#############################################################################
## Dinamica_trans_potent_calc: Prediction of cellular transition potential
## using fitted statistical models and optional implementation of spatial interventions
## Date: 25-02-2022, 09-11-2023
## Author: Ben Black, Carlson Büth
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# #values for testing purposes
# wpath <- "E:/LULCC_CH_Ensemble"
# Simulation_time_step <- 2020
# Simulation_num <- "1"

#set working directory
cat(paste0("Current working directory: ", getwd(), "\n"))

#Vector packages for loading
packs <- c("foreach", "data.table", "raster", "tidyverse",
           "testthat", "sjmisc", "tictoc", "doParallel", "callr",
           "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest",
           "Dinamica", "future", "future.apply", "parallelly", "future.callr", "readxl")

#new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
#if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load function for spatial transition probability manipulation
invisible(source("Scripts/Functions/lulcc.spatprobmanipulation.R"))

#load function for ei interventions
invisible(source("Scripts/Functions/lulcc.eiintervention.R"))

#Load in the grid file we are using for spatial extent and CRS
Ref_grid <- raster(Ref_grid_path)

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

#EI_intervention_ID
EI_ID <- Simulation_table$EI_ID.string

#Vector ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Use parallel processing for transition potential calculation
Use_parallel <- Simulation_table$Parallel_TPC.string

#implement spatial interventions
Use_interventions <- Simulation_table$Spatial_interventions.string

#implement EI interventions
Use_EI_interventions <- Simulation_table$EI_interventions.string

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
### B- Retrieve current LULC map and layerize
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- paste0(File_path_simulated_LULC_maps, Simulation_time_step, ".tif")

#load current LULC map
Current_LULC <- raster(current_LULC_path)

#load aggregation scheme
Aggregation_scheme <- read_excel(LULC_aggregation_path)

LULC_rat <- Aggregation_scheme %>% distinct(Aggregated_ID, .keep_all = TRUE)
LULC_rat <- LULC_rat[, c("Class_abbreviation", "Aggregated_ID")]

cat(paste0("Layerizing current LULC map: ", current_LULC_path, "\n"))

#layerize data (columns for each LULC class)
LULC_data <- raster::layerize(Current_LULC)
names(LULC_data) <- sapply(str_remove_all(names(LULC_data), "X"), function(y) {
  LULC_rat[LULC_rat$Aggregated_ID == y, "Class_abbreviation"]
})

### =========================================================================
### C- Load Suitability and Accessibility predictors
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)) {
  cat("--- CALIBRATION MODE --- \n")

  #loading_dir <- "Data/Preds/Prepared/Stacks/Calibration"
  #SA_pred_stack <- readRDS(list.files(loading_dir, pattern = Period_tag, full.names = TRUE))

  #load sheet of predictor table for time point
  pred_details <- openxlsx::read.xlsx(Pred_table_path, sheet = paste(Period_tag))

  #load layers as raster::stack
  SA_pred_stack <- raster::stack(pred_details$Prepared_data_path)

  #name layers in stack
  names(SA_pred_stack@layers) <- pred_details$Covariate_ID

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
  preds_scenario <- rbind(preds_static, preds_climate, preds_economic)

  #load layers as raster::stack
  SA_pred_stack <- raster::stack(preds_scenario$Prepared_data_path)

  #name layers in stack
  names(SA_pred_stack@layers) <- preds_scenario$Covariate_ID

} #close simulation if statement

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
  Urban_rast <- Current_LULC == 10

  #load canton shapefile
  Canton_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

  #Zonal stats to get urban area per kanton
  Canton_urban_areas <- raster::extract(Urban_rast, Canton_shp, fun = sum, na.rm = TRUE, df = TRUE)

  #append Kanton ID
  Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM

  #combine areas for cantons with multiple polygons
  Canton_urban_areas <- Canton_urban_areas %>%
    group_by(Canton_num) %>%
    dplyr::summarise(across(c(layer), sum))

  #load the municipality shape file
  Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

  #filter out non-swiss municipalities
  Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet",]

  #Zonal stats to get number of Urban cells per Municipality polygon
  #sum is used as a function because urban cells = 1 all others = 0
  Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun = sum, na.rm = TRUE, df = TRUE)

  #append Kanton and Municipality IDs
  Muni_urban_areas$Canton_num <- Muni_shp@data[["KANTONSNUM"]]
  Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
  Muni_urban_areas$Perc_urban <- 0

  #loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
  for (i in Canton_urban_areas$Canton_num) {

    #vector kanton urban area
    Can_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "layer"])

    #subset municipalities to this canton number
    munis_indices <- which(Muni_urban_areas$Canton_num == i)

    #loop over municipalities in the Kanton and calculate their urban areas as a % of the Canton's total
    for (muni in munis_indices) {
      Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "layer"] / Can_urban_area) * 100
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

  #add estimated population to @data table of polygons and then rasterize
  Muni_shp@data$Pop_est <- Muni_urban_areas$Pop_est
  pop_raster <- raster::rasterize(x = Muni_shp, y = Ref_grid, field = "Pop_est", background = NAvalue(Ref_grid))
  names(pop_raster) <- "Muni_pop" #TO DO: THIS MUST BE THE LAYER NAME IN THE CALIBRATION STACKS/MODELS

  #clean up
  rm(Canton_shp, Canton_urban_areas, Can_urban_area, canton_model, Muni_shp,
     Muni_urban_areas, munis_indices, pop_models, Pop_prediction_table,
     Urban_rast)

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
    Active_class_raster_subset <- Current_LULC == Active_class_value

    #create focal layer using matrix
    Focal_layer <- focal(x = Active_class_raster_subset, w = Focal_matrices[[Required_focals_details[i,]$matrix_id]], na.rm = FALSE, pad = TRUE, padValue = 0, NAonly = FALSE)

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
  Trans_data_stack <- stack(LULC_data, SA_pred_stack)
  names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers))
} else if (grepl("simulation", Model_mode, ignore.case = TRUE)) {
  #For simulation mode
  #only stack the Nhood_rasters here because otherwise they were not included
  #in the upper stack function

  #load the raster of Bioregions
  Bioregion_rast <- raster("Data/Bioreg_CH/Bioreg_raster.gri")
  names(Bioregion_rast) <- "Bioregion"
  Trans_data_stack <- stack(LULC_data, SA_pred_stack, pop_raster, stack(Nhood_rasters), Bioregion_rast)
  cat(" - Stacked all layers \n")
  names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers), names(pop_raster), names(Nhood_rasters), names(Bioregion_rast))
  cat(" - Renamed layers \n")
} else {
  stop("Model mode not recognised!")
}

#Load raster stacks for testing
# SA_pred_stack <- readRDS("SA_pred_stack.rds")
# LULC_data <- readRDS("LULC_data.rds")
# pop_raster <- readRDS("pop_raster.rds")
# Nhood_rasters <- readRDS("Nhood_rasters.rds")
# Bioregion_rast <- readRDS("Bioregion_rast.rds")

#Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
Trans_dataset <- raster::as.data.frame(Trans_data_stack)

cat(" - Converted raster stack to dataframe \n")

#add ID column to dataset
#Trans_dataset$ID <- seq.int(nrow(Trans_dataset))
Trans_dataset$ID <- row.names(Trans_dataset)

#Get XY coordinates of cells
xy_coordinates <- coordinates(Trans_data_stack)

#cbind XY coordinates to dataframe and seperate rows where all values = NA
Trans_dataset <- cbind(Trans_dataset, xy_coordinates)

#release memory
rm(LULC_data, SA_pred_stack, Nhood_rasters, Trans_data_stack, xy_coordinates)

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

#seperate Trans_dataset into complete cases for prediction
#and NAs ()background values
Trans_dataset_complete <- Trans_dataset %>%
  filter(complete.cases(.))
row.names(Trans_dataset_complete) <- Trans_dataset_complete$ID

Trans_dataset_na <- Trans_dataset %>%
  filter(!complete.cases(.))
Trans_dataset_na <- Trans_dataset_na[, c("ID", "x", "y")]
row.names(Trans_dataset_na) <- Trans_dataset_na$ID

rm(Trans_dataset)

#seperate ID, x/y and Initial_LULC cols to append results to
Prediction_probs <- Trans_dataset_complete[, c("ID", "x", "y", unique(Model_lookup$Initial_LULC), "Static")]
row.names(Prediction_probs) <- Prediction_probs$ID

#add cols to both the complete and NA data to capture predict probabilities to each class
Final_LULC_classes <- unique(Model_lookup$Final_LULC)
for (i in Final_LULC_classes) {
  Prediction_probs[[paste0("Prob_", i)]] <- 0
  Trans_dataset_na[[paste0("Prob_", i)]] <- NA
}

cat(" - Created dataframe for storing prediction probabilities \n")

if (Use_parallel == "Y") {

  Par_start_time <- Sys.time()
  #set up cluster for parallel computation
  plan(future.callr::callr, workers = availableCores(omit = 2))

  size = 5000 * 1024^2
  options(future.globals.maxSize = size)

  #loop over transitions (currently takes 4 mins in parallel)
  results <- future.apply::future_lapply(1:nrow(Model_lookup),
                                         future.packages = packs,
                                         function(i) {

                                           #gc()

                                           #vector trans_name
                                           Trans_name <- Model_lookup[i, "Trans_name"]
                                           Trans_ID <- Model_lookup[i, "Trans_ID"]
                                           Region <- Model_lookup[i, "Region"]
                                           Final_LULC <- Model_lookup[i, "Final_LULC"]
                                           Initial_LULC <- Model_lookup[i, "Initial_LULC"]

                                           #load model
                                           Fitted_model <- readRDS(Model_lookup[i, "File_path"])

                                           #subset data for prediction
                                           pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                                                                 Trans_dataset_complete$Bioregion_Class_Names == Region,]

                                           #predict using fitted model
                                           prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type = "prob"))
                                           names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

                                           #bind to ID
                                           predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

                                           #gc()
                                           #return the prediction results to be bound together
                                           return(predict_ID)
                                         }) #close loop over Models
  plan(sequential)

  #Now the results need to be combined with other rows
  #results are being returned as a list
  for (i in results) {

    #get Prob_LULC column name to make appending simpler
    LULC_col_name <- colnames(i)[2]

    #Append prediction results
    Prediction_probs[which(Prediction_probs$ID %in% i[["ID"]]), LULC_col_name] <- i[[LULC_col_name]]
  }
  Par_end_time <- Sys.time()
  Par_time <- Par_end_time - Par_start_time  # Parallel time = 1.472859 mins

}else if (Use_parallel == "N") { #close parallel TPC chunk

  #Non_parallel TPC calculation:
  Non_par_start <- Sys.time()
  #loop over transitions
  for (i in 1:nrow(Model_lookup)) {

    #vector trans_name
    Trans_name <- Model_lookup[i, "Trans_name"]
    Trans_ID <- Model_lookup[i, "Trans_ID"]
    Region <- Model_lookup[i, "Region"]
    Final_LULC <- Model_lookup[i, "Final_LULC"]
    Initial_LULC <- Model_lookup[i, "Initial_LULC"]

    # detailed status message
    # cat(paste0(" - predicting probabilities for transitions from ", Initial_LULC,
    #            " to ", Final_LULC, " within region: ", Region, "\n"))

    #load model
    Fitted_model <- readRDS(Model_lookup[i, "File_path"])

    #subset data for prediction
    pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                          Trans_dataset_complete$Bioregion_Class_Names == Region,]

    #predict using fitted model
    prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type = "prob"))
    names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

    #bind to ID
    #predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

    #append the predictions at the correct rows in the results df
    #Prediction_probs[which(Prediction_probs$ID %in% predict_ID$ID), paste0("Prob_", Final_LULC)] <- predict_ID[paste0("Prob_", Final_LULC)]

    #alternative method of replacing prob prediction values
    Prediction_probs[row.names(prob_predicts), paste0("Prob_", Final_LULC)] <- prob_predicts[paste0("Prob_", Final_LULC)]

  } #close loop over Models
  Non_par_end <- Sys.time()
  Non_par_time <- Non_par_end - Non_par_start #sequential time = 2.937131 mins
} #Close non-parallel TPC chunk

cat(" - Completed transition potential prediction \n")

### =========================================================================
### G- Re-scale predictions
### =========================================================================

cat("Re-scaling transition probabilities \n")

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

#bind with background values
Trans_dataset_na[setdiff(names(Prediction_probs), names(Trans_dataset_na))] <- NA
Raster_prob_values <- rbind(Prediction_probs, Trans_dataset_na)

#sort by ID
Raster_prob_values <- Raster_prob_values[order(as.numeric(row.names(Raster_prob_values))),]

test_set <- Raster_prob_values
test_2 <- Raster_prob_values

#Save one copy of the raster probability values to be used to test
#spatial interventions, this file will be created during the running of the model
#to calibrate the Dinamica allocation parameters.
# if (file.exists("Data/Exemplar_data/EXP_raster_prob_values.rds") == FALSE) {
#   dir.create("Data/Exemplar_data")
#   saveRDS(Raster_prob_values, "Data/Exemplar_data/EXP_raster_prob_values.rds")
# }

### =========================================================================
### H- Scenario specific trends spatially manipulating transition probabilities
### =========================================================================

if (grepl("simulation", Model_mode, ignore.case = TRUE)) {

  #If statement to implement spatial interventions
  if (Use_interventions == "Y") {

    cat("Implementing spatial interventions \n")

    #Use function to perform manipulation of spatial transition probabilities
    #according to scenario-specific interventions
    Raster_prob_values <- lulcc.spatprobmanipulation(Intervention_table_path = Spat_ints_path,
                                                     Scenario_ID = Scenario_ID,
                                                     Raster_prob_values = Raster_prob_values,
                                                     Simulation_time_step = paste(Simulation_time_step))


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

### =========================================================================
### J- EI interventions spatially manipulating transition probabilities
### =========================================================================
  
  #If statement to implement spatial interventions
  if (Use_EI_interventions == "Y") {
    
    cat("Implementing EI interventions \n")
    
    #Use function to perform manipulation of spatial transition probabilities
    #according to scenario-specific interventions
    Raster_prob_values <- lulcc.eiintervention(Intervention_table_path = EI_ints_path,
                                                     EI_ID = EI_ID,
                                                     Raster_prob_values = Raster_prob_values,
                                                     Simulation_time_step = paste(Simulation_time_step))
  

### =========================================================================
### K- Rescale following EI interventions
### =========================================================================

cat("Performing additional re-scaling following EI interventions \n")

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
  Final_LULC <- Unique_trans[i, "Final_LULC"]
  Initial_LULC <- Unique_trans[i, "Initial_LULC"]


  #get indices of rows for cells of initial class
  Initial_indices <- na.omit(Raster_prob_values[Raster_prob_values[Initial_LULC] == 1, "ID"])

  #get indices of non_class cells
  non_initial_indices <- na.omit(Raster_prob_values[Raster_prob_values[Initial_LULC] == 0, "ID"])

  #seperate Final class column
  Trans_raster_values <- Raster_prob_values[, c("ID", "x", "y", paste0("Prob_", Final_LULC))]

  #replace values of non-class cells with 0
  Trans_raster_values[non_initial_indices, paste0("Prob_", Final_LULC)] <- 0

  if (Check_normalisation) {
    #check that are Prob_ values are in [0, 1[ - otherwise warn
    for (col_name in paste0("Prob_", Final_LULC)) {
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
    Trans_raster_values[, c("x", "y", paste0("Prob_", Final_LULC))], crs = crs(Current_LULC)
  )

  #vector file path for saving probability maps
  prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".tif")

  raster::writeRaster(Prob_raster, prob_map_path, overwrite = T)
} #close loop over transitions


### =========================================================================
### M- Return output to Dinamica
### =========================================================================

#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work
cat(paste0("Probability maps saved to: ", prob_map_folder, " (class: ", class(prob_map_folder), ") \n"))
