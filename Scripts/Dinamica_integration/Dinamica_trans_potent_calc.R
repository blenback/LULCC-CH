#############################################################################
## Dinamica_trans_potent_calc: Prediction of cellular transition potential
## using fitted statistical models and optional implementation of spatial interventions
## Date: 25-02-2022, 09-11-2023
## Author: Ben Black, Carlson Büth
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

#values for testing purposes
wpath <- getwd()
Simulation_time_step <- 2020
Simulation_num <- "2"
File_path_simulated_LULC_maps <- "lulcc_output/SSP1/simulated_LULC_simID_SSP1_year_"
ProjCH <- "+proj=somerc +init=epsg:2056"

#set working directory
cat(paste0("Current working directory: ", getwd(), "\n"))

#Vector packages for loading
packs <- c("foreach", "data.table", "raster", "tidyverse",
           "testthat", "sjmisc", "callr", # "tictoc", "doParallel",
           "stringr", "readr", "xlsx", "randomForest", # "lulcc", "pbapply",
           "Dinamica", "future", "future.apply", "parallelly", "future.callr",
           "readxl", "terra", "yaml")

# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load function for spatial transition probability manipulation
invisible(source("Scripts/Functions/implement_spatial_interventions.R"))
invisible(source("Scripts/Functions/relative_prob_adjust.R"))
invisible(source("Scripts/Functions/absolute_prob_adjust.R"))

#Load in the grid file we are using for spatial extent and CRS

#swap '.gri in ref_grid_path with .grd
Ref_grid_path <- gsub("_grid.gri", "_grid.grd", Ref_grid_path)
Ref_grid <- rast(Ref_grid_path)
crs(Ref_grid) <- ProjCH #set CRS to Swiss projection

#load table of simulations subsetting to current simulation
Simulation_table <- read.csv(Sim_control_path)[Simulation_num,]

#Vector name of Scenario to be tested as string or numeric (i.e. "BAU" etc.)
Scenario_ID <- Simulation_table$Scenario_ID.string

#Vector name of climate scenario
Climate_ID <- Simulation_table$Climate_scenario.string

#Vector name of Economic scenario
Econ_ID <- Simulation_table$Econ_scenario.string

#Vector name of Population scenario
Pop_ID <- Simulation_table$Pop_scenario.string

#Vector ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Use parallel processing for transition potential calculation
Use_parallel <- Simulation_table$Parallel_TPC.string

#implement spatial interventions
Use_interventions <- Simulation_table$Spatial_interventions.string

#check normalisation of transition probabilities
Check_normalisation <- FALSE

cat(paste0("Starting transition potential calculation for ", Model_mode, ": ",
           Simulation_ID, ", with scenario: ", Scenario_ID,
           ", at time step: ", Simulation_time_step, "\n"))
cat(paste0(" - loading maps from: ", File_path_simulated_LULC_maps, "\n"))

#Convert model mode into a string of the dates calibration period being used
#this makes it easier to load files because they use this nomenclature

Calibration_periods <- unique(read.csv(Model_specs_path)[["Data_period_name"]])

Calibration_dates <- lapply(Calibration_periods, function(period) {
  dates <- as.numeric(str_split(period, "_")[[1]])
})

Period_tag <- if (grepl("calibration", Model_mode, ignore.case = TRUE)) {
  Period_log <- sapply(Calibration_dates, function(x) {
    if (Simulation_time_step > x[1] & Simulation_time_step <= x[2]) { TRUE }else { FALSE }
  })
  if (all(Period_log == FALSE) == TRUE) {
    #find the closet year
    closest_year <- unlist(Calibration_dates)[which.min(abs(unlist(Calibration_dates) - Simulation_time_step))]
    #match year to period name
    Period_log <- Calibration_periods[grepl(paste0(closest_year), Calibration_periods)]
  }else { Calibration_periods[Period_log == TRUE] }
}else if (grepl("simulation", Model_mode, ignore.case = TRUE)) { Calibration_periods[length(Calibration_periods)] }
#The last clause covers when calibration is occuring between 2009 and 2018
#and when the model is in simulation mode

#create folder for saving prediction probability maps
prob_map_folder <- paste0(wpath, "/Results/Pred_prob_maps/", Simulation_ID, "/", Simulation_time_step)
suppressWarnings(dir.create(prob_map_folder, recursive = TRUE))

cat(paste0(" - creating directory for saving probability maps: ", prob_map_folder, "\n"))


### =========================================================================
### B- Retrieve current LULC map
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- paste0(File_path_simulated_LULC_maps, Simulation_time_step, ".tif")

#load current LULC map
LULC_rast <- terra::rast(current_LULC_path)
names(LULC_rast) <- "LULC"
crs(LULC_rast) <- ProjCH #set CRS to Swiss projection

#load aggregation scheme
Aggregation_scheme <- read_excel(LULC_aggregation_path)

LULC_rat <- Aggregation_scheme %>% distinct(Aggregated_ID, .keep_all = TRUE)
LULC_rat <- LULC_rat[, c("Class_abbreviation", "Aggregated_ID")]

cat(paste0("Layerizing current LULC map: ", current_LULC_path, "\n"))

### =========================================================================
### C- Load Suitability and Accessibility predictors
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)) {
  cat("--- CALIBRATION MODE --- \n")

  #load sheet of predictor table for time point
  pred_details <- openxlsx::read.xlsx(Pred_table_path, sheet = paste(Period_tag))

  #load layers as raster::stack
  SA_pred_stack <- rast(c(pred_details$Prepared_data_path))

  #name layers in stack
  names(SA_pred_stack) <- pred_details$Covariate_ID

} #close if statement calibration

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)) {
  cat("--- SIMULATION MODE --- \n")

  #load sheet of predictor table for time point
  pred_details <- openxlsx::read.xlsx(Pred_table_path, sheet = paste(Simulation_time_step))

  #convert scenario column back to character vectors
  pred_details$Scenario_variant <- sapply(pred_details$Scenario_variant, function(x) unlist(strsplit(x, ",")))

  #first seperate static variables scenario = "All""
  preds_static <- pred_details[pred_details$Scenario_variant == "All",]

  #then those relevant for the scenario variant
  #first the climate predictors based on the RCP designation
  preds_climate <- pred_details[grep(Climate_ID, pred_details$Scenario_variant),]

  #Then the economic predictors
  preds_economic <- pred_details[grep(Econ_ID, pred_details$Scenario_variant),]

  #bind static and scenario specific preds
  simulation_preds <- rbind(preds_static, preds_climate, preds_economic)

  rm(preds_static, preds_climate, preds_economic, pred_details)

  #load layers as raster::stack
  SA_pred_stack <- rast(c(simulation_preds$Prepared_data_path))

  #name layers in stack
  names(SA_pred_stack) <- simulation_preds$Covariate_ID

} #close simulation if statement

  # check if crs matches ProjCH
  if (crs(SA_pred_stack) != ProjCH) {
    SA_pred_stack <- terra::project(SA_pred_stack, ProjCH)
    cat(" - Projected SA_pred_stack to ProjCH \n")
  } else {
    cat(" - SA_pred_stack already in ProjCH \n")
  }

cat(paste0("Loaded Suitability and Accessibility predictors \n"))

### =========================================================================
### E- Generate dynamic predictors
### =========================================================================

#-------------------------------------------------------------------------
# E.1- Dynamic predictors:  Municipal Population
#-------------------------------------------------------------------------

#for calibration the raster stacks already contain the dynamic predictor layers so there
#is nothing to be done

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)) {

  cat("Generating dynamic predictors: \n - Municipal population \n")

  #create population data layer
  #subset current LULC to just urban cells
  Urban_rast <- LULC_rast == 10

  #load canton shapefile
  Canton_shp <- vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

  #Zonal stats to get urban area per kanton
  Canton_urban_areas <- terra::extract(Urban_rast, Canton_shp, fun = sum, na.rm = TRUE)

  #append Kanton ID
  Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM

  #combine areas for cantons with multiple polygons
  Canton_urban_areas <- Canton_urban_areas %>%
    group_by(Canton_num) %>%
    dplyr::summarise(across(c(LULC), sum))

  #load the municipality shape file
  Muni_shp <- vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

  #filter out non-swiss municipalities
  Muni_shp <- Muni_shp[Muni_shp$ICC == "CH" & Muni_shp$OBJEKTART == "Gemeindegebiet",]

  #Zonal stats to get number of Urban cells per Municipality polygon
  #sum is used as a function because urban cells = 1 all others = 0
  Muni_urban_areas <- terra::extract(Urban_rast, Muni_shp, fun = sum, na.rm = TRUE)

  #append Kanton and Municipality IDs
  Muni_urban_areas$Canton_num <- Muni_shp$KANTONSNUM
  Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
  Muni_urban_areas$Perc_urban <- 0

  #loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
  for (i in Canton_urban_areas$Canton_num) {

    #vector kanton urban area
    Can_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "LULC"])

    #subset municipalities to this canton number
    munis_indices <- which(Muni_urban_areas$Canton_num == i)

    #loop over municipalities in the Kanton and calculate their urban areas as a % of the Canton's total
    for (muni in munis_indices) {
      Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "LULC"] / Can_urban_area) * 100
    } #close inner loop
  } #close outer loop

  #estimate % of predicted cantonal population per municipality
  #Load list of cantonal population models
  pop_models <- readRDS("Data/Preds/Tools/Dynamic_pop_models.rds")

  #add predicted % pop results column
  Muni_urban_areas$Perc_pop <- 0

  #estimate % of pop per municipality
  #loop over unique polygon IDs rather than BFS numbers to take into account that
  #multiple polygons have the same BFS number
  for (i in Muni_urban_areas$ID) {

    #seperate canton specific model
    canton_model <- pop_models[[Muni_urban_areas[Muni_urban_areas$ID == i, "Canton_num"]]]

    #perform prediction
    Muni_urban_areas$Perc_pop[[i]] <- predict(canton_model, newdata = Muni_urban_areas[Muni_urban_areas$ID == i,])
  }

  #load correct sheet of future population predictions according to scenario
  Pop_prediction_table <- openxlsx::read.xlsx("Data/Preds/Tools/Population_projections.xlsx", sheet = Pop_ID)

  #loop over unique kanton numbers, rescale the predicted population percentages
  #and calculate the estimated population per municipality as a % of the cantonal total

  #add results column
  Muni_urban_areas$Pop_est <- 0

  for (i in unique(Muni_urban_areas$Canton_num)) {

    #subset to predicted cantonal population percentages
    Canton_dat <- Muni_urban_areas[Muni_urban_areas$Canton_num == i, "Perc_urban"]

    #loop over the municipalites re-scaling the values
    Canton_preds_rescaled <- sapply(Canton_dat, function(y) {
      value <- y * 1 / sum(Canton_dat)
      value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
      return(value) }) #close inner loop

    #get the projected canton population value for this time point
    Canton_pop <- Pop_prediction_table[Pop_prediction_table$Canton_num == i, paste(Simulation_time_step)]

    #loop over the rescaled values calculating the estimated population
    Muni_indices <- which(Muni_urban_areas$Canton_num == i)
    Muni_urban_areas$Pop_est[Muni_indices] <- sapply(Canton_preds_rescaled, function(x) {
      pop_value <- Canton_pop * x #% already expressed as decimal so no need to /100
    }) #close loop over municipalities

  } #close loop over cantons

  #save the results
  #saveRDS(Muni_urban_areas, file = "Muni_urban_areas_terra.rds")

  #add estimated population to polygons and then rasterize
  Muni_shp$Pop_est <- Muni_urban_areas$Pop_est
  pop_raster <- terra::rasterize(x = Muni_shp, y = Ref_grid, field = "Pop_est", background = NA)
  names(pop_raster) <- "Muni_pop" #TO DO: THIS MUST BE THE LAYER NAME IN THE CALIBRATION STACKS/MODELS

  # check if the CRS matches ProjCH
  if (crs(pop_raster) != ProjCH) {
    pop_raster <- terra::project(pop_raster, ProjCH)
    cat(" - Projected pop_raster to ProjCH \n")
  } else {
    cat(" - pop_raster already in ProjCH \n")
  }


  #clean up
  rm(Canton_shp, Canton_urban_areas, Can_urban_area, canton_model, Muni_shp,
     Muni_urban_areas, munis_indices, pop_models, Pop_prediction_table,
     Urban_rast)

  #save Pop_raster
  # writeRaster(pop_raster, "Population_raster_terra.tif", overwrite = TRUE)

  #-------------------------------------------------------------------------
  # E.2- Dynamic predictors: Neighbourhood predictors
  #-------------------------------------------------------------------------

  cat(" - Neighbourhood predictors \n")

  #load matrices used to create focal layers
  Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

  #adjust matrix names
  names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) { split_name <- (str_split(x, "[.]"))[[1]][2] })

  #Load details of focal layers required for the model set being utilised
  Required_focals_details <- readRDS(list.files("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating", pattern = Period_tag, full.names = TRUE))

  #Loop over details of focal layers required creating a list of rasters from the current LULC map
  Nhood_rasters <- list()
  for (i in 1:nrow(Required_focals_details)) {

    #vector active class names
    Active_class_name <- Required_focals_details[i,]$active_lulc

    #get pixel values of active LULC class
    Active_class_value <- unlist(LULC_rat[LULC_rat$Class_abbreviation == Active_class_name, "Aggregated_ID"])

    #subset LULC raster by all Active_class_value
    Active_class_raster_subset <- LULC_rast == Active_class_value

    Focal_layer <- terra::focal(x = Active_class_raster_subset,
                         w = Focal_matrices[[Required_focals_details[i,]$matrix_id]],
                         na.rm = TRUE,
                         na.policy = "omit")

    #create file path for saving this layer
    Focal_name <- paste(Active_class_name, "nhood", Required_focals_details[i,]$matrix_id, sep = "_")
    Nhood_rasters[[Focal_name]] <- Focal_layer

    #steps for saving of rasters if needed
    #create a folder path using simulation ID and time step
    #Dynamic_focal_folder_path <- paste0("Data/Preds/Prepared/Stacks/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)

    #create directory
    #dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)

    #Focal_file_name <- paste(Scenario_ID, Simulation_time_step, Required_focals_details[i,]$active_lulc, "nhood", Focal_matrices[Required_focals_details[i,]$matrix_id], sep = "_")
    #Focal_full_path <- paste0(Dynamic_focal_folder_path, "/", Focal_file_name, ".grd") #create full folder path
    #writeRaster(Focal_layer, Focal_full_path ,datatype='INT2U', overwrite=TRUE) #save layer
  }

  rm(Focal_matrices, Focal_layer, Focal_name, Required_focals_details,
     Active_class_raster_subset, Active_class_name, Active_class_value)

} #close if statement for dynamic predictor prep

### =========================================================================
### F- Combine LULC, SA_preds and Nhood_preds and extract to dataframe
### =========================================================================

cat("Stacking LULC, SA_preds and Nhood_preds \n")

#Stack all rasters
#For calibration mode
if (grepl("calibration", Model_mode, ignore.case = TRUE)) {
  Trans_data_stack <- c(LULC_rast, SA_pred_stack)
  names(Trans_data_stack) <- c(names(LULC_rast), names(SA_pred_stack))
} else if (grepl("simulation", Model_mode, ignore.case = TRUE)) {
  #For simulation mode
  #also stack the Nhood_rasters here because otherwise they are not included
  #in the upper stack function

  #load the raster of Regions
  Region_rast <- rast("Data/Bioreg_CH/Bioreg_raster.grd")
  names(Region_rast) <- "Region"

  #check if the CRS matches ProjCH
  if (crs(Region_rast) != ProjCH) {
    Region_rast <- terra::project(Region_rast, ProjCH)
    cat(" - Projected Region_rast to ProjCH \n")
  } else {
    cat(" - Region_rast already in ProjCH \n")
  }

  #seperate attribute table
  Region_rat <- levels(Region_rast)[[1]]

  Trans_data_stack <- c(LULC_rast, SA_pred_stack, pop_raster, rast(Nhood_rasters), Region_rast)
  cat(" - Stacked all layers \n")
  names(Trans_data_stack) <- c(names(LULC_rast), names(SA_pred_stack), names(pop_raster), names(Nhood_rasters), names(Region_rast))
  cat(" - Renamed layers \n")
} else {
  stop("Model mode not recognised!")
}


### =========================================================================
### G- Run transition potential prediction for each transition
### =========================================================================

cat("Running transition potential prediction for each transition \n")
# parralel y/n
cat(paste0("Use parallel processing: ", Use_parallel, "\n"))

#saveRDS(Trans_dataset, "Data/Spat_prob_perturb_layers/EXP_trans_dataset.rds")
#Trans_dataset <- readRDS("Data/Spat_prob_perturb_layers/EXP_trans_dataset.rds")

#subsetting data indices for glacial modelling
#data_indices <- Trans_dataset[,c("ID", "x", "y")]
#write.csv(data_indices, "Data/Data_indices_full.csv", row.names = FALSE)
#write.table(data_indices, "Data/Data_indices_full.txt", row.names = FALSE)

#load model look up
Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)
cat(" - Loaded model lookup table \n")

#if statement to remove transitions if they are being implemented deterministically
if (grepl("simulation", Model_mode, ignore.case = TRUE) &
  grepl("Y", Simulation_table$Deterministic_trans.string, ignore.case = TRUE)) {

  #remove transitions with initial class == glacier
  cat(" - Removing transitions with initial class == glacier \n")
  Model_lookup <- Model_lookup[Model_lookup$Initial_LULC != "Glacier",]

} #close if statement

# seperate NA values in Trans_dataset_stack$LULC to a seperate raster
NA_rast <- ifel(is.na(Trans_data_stack[["LULC"]]), NA, 0)

#get coords of non-NA cells that need to be predicted
Predict_cells <- cells(NA_rast)

#extract LULC rast at these coords and add cell numbers
Prediction_probs <- terra::extract(LULC_rast, Predict_cells, xy = TRUE)
Prediction_probs$cell <- Predict_cells
row.names(Prediction_probs) <- Prediction_probs$cell

#add cols to both the complete and NA data to capture predict probabilities to each class
Final_LULC_classes <- unique(Model_lookup$Final_LULC)
for (i in Final_LULC_classes) {
  Prediction_probs[[paste0("Prob_", i)]] <- 0
}

cat(" - Created dataframe for storing prediction probabilities \n")

for (i in 1:nrow(Model_lookup)) {

  #vector details of transition
  Trans_name <- Model_lookup[i, "Trans_name"]
  Trans_ID <- Model_lookup[i, "Trans_ID"]
  Region <- Model_lookup[i, "Region"]
  Initial_LULC_class <- Model_lookup[i, "Initial_LULC"]
  Final_LULC_class <- Model_lookup[i, "Final_LULC"]
  Initial_LULC_ID <- unlist(LULC_rat[LULC_rat$Class_abbreviation == Initial_LULC_class, "Aggregated_ID"])
  Region_ID <- Region_rat[Region_rat$Class_Names == Region, "ID"]

  #detailed status message
  cat(paste0(" - predicting probabilities for transitions from ", Initial_LULC_class,
                " to ", Final_LULC_class, " within region: ", Region, "\n"))

  #load model
  Fitted_model <- readRDS(Model_lookup[i, "File_path"])

  #get names of preds removing 1st entry as it is the response variable
  pred_names <- c(colnames(Fitted_model[["call"]][["data"]])[-1])

  #subset Trans_data_stack to only include required predictors
  pred_data <- Trans_data_stack[[pred_names]]

  #identify cells for transition
  Trans_cells <- cells(ifel(Trans_data_stack[["LULC"]] == Initial_LULC_ID & Trans_data_stack[["Region"]] == Region_ID, 1, NA))

  #extract data for cells
  pred_data <- terra::extract(pred_data,Trans_cells)

  #predict using fitted model
  prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type = "prob"))
  names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC_class)
  row.names(prob_predicts) <- Trans_cells

  #append the predictions at the correct rows in the results df
  Prediction_probs[row.names(prob_predicts), paste0("Prob_", Final_LULC_class)] <- prob_predicts[paste0("Prob_", Final_LULC_class)]

  }

cat(" - Completed transition potential prediction \n")

### =========================================================================
### G- Re-scale predictions
### =========================================================================

cat("Re-scaling transition probabilities \n")

#replace any NAs with 0
Prediction_probs[is.na(Prediction_probs)] <- 0

#loop over rows and re-scale probability values so that they sum to 1
#(by dividing by multiplying by 1 and then dividing by the sum of the vector)

#Transition probability columns to re-scale
Pred_prob_columns <- grep("Prob_", names(Prediction_probs), value = TRUE)

#vector row indices with non-zero sums of transition probabilities
Non_zero_indices <- which(rowSums(Prediction_probs[, Pred_prob_columns]) > 1)

#Loop over rows performing re-scaling
Prediction_probs[Non_zero_indices, Pred_prob_columns] <- as.data.frame(t(apply(Prediction_probs[Non_zero_indices, Pred_prob_columns], MARGIN = 1, FUN = function(x) {
  sapply(x, function(y) {
    value <- y * 1 / sum(x)
    value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
    return(value)
  })
})))

#bind with NA values
NA_cells <- as.data.frame(NA_rast,
                          xy = TRUE,
                          cells= TRUE,
                          na.rm = FALSE)

NA_cells <- NA_cells[is.na(NA_cells$LULC), c("x", "y", "cell")]

#ensure that both data.frames have the same columns
NA_cells[setdiff(names(Prediction_probs), names(NA_cells))] <- NA

#bind together
Raster_prob_values <- rbind(Prediction_probs, NA_cells)

#sort by cell number (using row names)
Raster_prob_values <- Raster_prob_values[order(as.numeric(row.names(Raster_prob_values))),]

rm(Prediction_probs, NA_cells, Trans_data_stack)

# save for debugging
#saveRDS(Raster_prob_values, "Raster_prob_values.rds")
Raster_prob_values <- readRDS("Raster_prob_values.rds")

### =========================================================================
### H- Scenario specific trends spatially manipulating transition probabilities
### =========================================================================

if (grepl("simulation", Model_mode, ignore.case = TRUE)) {

  #If statement to implement spatial interventions
  if (Use_interventions == "Y") {

    cat("Implementing spatial interventions \n")

    #Use function to perform manipulation of spatial transition probabilities
    #according to scenario-specific interventions
    Raster_prob_values <- implement_spatial_interventions(
      interventions_dir = "Tools",
      scenario_ID = "SSP0",
      raster_prob_values = Raster_prob_values,
      simulation_time_step = paste(Simulation_time_step),
      LULC_rat = LULC_rat,
      Proj = ProjCH
      )


    ### =========================================================================
    ### I- Rescale following scenario trends/interventions
    ### =========================================================================

    cat("Performing re-scaling following scenario interventions \n")

    #vector row indices with non-zero sums of transition probabilities
    Non_zero_indices <- which(rowSums(Raster_prob_values[, Pred_prob_columns]) > 1)

    #Loop over rows performing re-scaling
    Raster_prob_values[Non_zero_indices, Pred_prob_columns] <- as.data.frame(t(apply(Raster_prob_values[Non_zero_indices, Pred_prob_columns], MARGIN = 1, FUN = function(x) {
      sapply(x, function(y) {
        value <- y * 1 / sum(x)
        value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
        return(value)
      })
    })))

  } #close if statement for spatial interventions
} #close simulation if statement

### =========================================================================
### L- Save transition rasters
### =========================================================================

cat("Saving transition rasters \n")

#subset model_lookup table to unique trans ID
Unique_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID),]

#Loop over unique trans using details to subset data and save Rasters
for (i in 1:nrow(Unique_trans)) {


  Trans_ID <- Unique_trans[i, "Trans_ID"]
  cat(paste0(" - Preparing layer ", Trans_ID, "\n"))
  Final_LULC_class <- Unique_trans[i, "Final_LULC"]
  Initial_LULC_class <- Unique_trans[i, "Initial_LULC"]
  Initial_LULC_ID <- unlist(LULC_rat[LULC_rat$Class_abbreviation == Initial_LULC_class, "Aggregated_ID"])

  #get indices of non_class cells
  non_initial_indices <- na.omit(Raster_prob_values[Raster_prob_values$LULC != Initial_LULC_ID, "cell"])

  #seperate Final class column
  Trans_raster_values <- Raster_prob_values[, c("cell", "x", "y", paste0("Prob_", Final_LULC_class))]

  #replace values of non-class cells with 0
  Trans_raster_values[non_initial_indices, paste0("Prob_", Final_LULC_class)] <- 0

  if (Check_normalisation) {
    #check that are Prob_ values are in [0, 1[ - otherwise warn
    for (col_name in paste0("Prob_", Final_LULC_class)) {
      col <- Trans_raster_values[, col_name]
      breaking <- FALSE
      if (any(col[is.finite(col)] < 0 | col[is.finite(col)] >= 1)) {
        # Raise warning for values outside [0, 1)
        warning("Raster warning: Probabilities (excluding NAs) are not in [0, 1[.")
        breaking <- TRUE
      } else if (any(is.na(col))) {
        # Raise warning for NAs
        warning("Raster warning: Probabilities contain NA values.")
        breaking <- TRUE
      }
      if (breaking) {
        break  # Stop checking further columns
      }
    }
  }

  #rasterize and save using Initial and Final class names
  Prob_raster <- rasterFromXYZ(
    Trans_raster_values[, c("x", "y", paste0("Prob_", Final_LULC_class))], crs = crs(LULC_rast)
  )

  #vector file path for saving probability maps
  prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC_class, "_to_", Final_LULC_class, ".tif")

  raster::writeRaster(Prob_raster, prob_map_path, overwrite = T)
} #close loop over transitions

### =========================================================================
### M- Return output to Dinamica
### =========================================================================

#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work
cat(paste0("Probability maps saved to: ", prob_map_folder, " (class: ", class(prob_map_folder), ") \n"))
