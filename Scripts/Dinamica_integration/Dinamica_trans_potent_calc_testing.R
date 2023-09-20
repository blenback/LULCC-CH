#############################################################################
## Dinamica_trans_potent_calc: Testing parallelization of transition 
## potential calculation
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

#receive working directory
#wpath <- s3
wpath <- getwd()
setwd(wpath)

#Vector packages for loading
packs<-c("foreach", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "doParallel", "callr",
         "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest",
         "Dinamica", "future", "future.apply", "parallelly", "future.callr", "readxl")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load function for spatial transition probability manipulation
invisible(source("Scripts/Functions/lulcc.spatprobmanipulation.R"))

#send model tool vars to global environment
list2env(readRDS("Tools/Model_tool_vars.rds"), .GlobalEnv)

#Load in the grid file we are using for spatial extent and CRS
Ref_grid <- raster(Ref_grid_path)

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

Sim_control_path <- "Tools/Simulation_control.csv"

#values for testing purposes
Simulation_time_step <- 2020
Simulation_num <- "1"
Control_table_path <- Sim_control_path
File_path_simulated_LULC_maps <- "Results/Dinamica_simulated_LULC/BAU/v6/simulated_LULC_scenario_BAU_simID_v6_year_"
Use_parallel <- "N"

#Receive current simulation time
#Simulation_time_step <- v1

#simulation number being performed
#Simulation_num <- v2

#load table of simulations
#Control_table_path <- s1
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Vector name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Vector an ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Receive folder path for loading simulated LULC maps 
#File_path_simulated_LULC_maps <- s2

#Use parallel processing for transition potential calculation
Use_parallel <- Simulation_table$Parallel_TPC.string

Use_interventions <- Simulation_table$Spatial_interventions.string

#Convert model mode into a string of the dates calibration period being used
#this makes it easier to load files because they use this nomenclature

Calibration_periods <- unique(readxl::read_excel(Model_specs_path)[["Data_period_name"]])

Calibration_dates <- lapply(Calibration_periods, function(period){
  dates <- as.numeric(str_split(period, "_")[[1]])
})

Period_tag <- if(grepl("calibration", Model_mode, ignore.case = TRUE)){
  Period_log <- sapply(Calibration_dates, function(x){
   if(Simulation_time_step > x[1] & Simulation_time_step <= x[2]){TRUE}else{FALSE}
  })
  if(all(Period_log == FALSE)==TRUE){
    #find the closet year
    closest_year <- unlist(Calibration_dates)[which.min(abs(unlist(Calibration_dates)-Simulation_time_step))]
    #match year to period name
    Period_log <- Calibration_periods[grepl(paste0(closest_year), Calibration_periods)]
    }else{Calibration_periods[Period_log==TRUE]}
  }else if (grepl("simulation", Model_mode, ignore.case = TRUE)){Calibration_periods[length(Calibration_periods)]}
#The last clause covers when calibration is occuring between 2009 and 2018
#and when the model is in simulation mode

#create folder for saving prediction probability maps
prob_map_folder <- paste0(wpath, "/Results/Pred_prob_maps/", Scenario_ID, "/", Simulation_ID, "/", Simulation_time_step)
suppressWarnings(dir.create(prob_map_folder, recursive = TRUE))

### =========================================================================
### C- Retrieve current LULC map and split into regionalized transition layers
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- paste0(File_path_simulated_LULC_maps, Simulation_time_step, ".tif")

#load current LULC map 
Current_LULC <- raster(current_LULC_path)

#load aggregation scheme
Aggregation_scheme <- read_excel(LULC_aggregation_path)

LULC_rat <- Aggregation_scheme%>% distinct(Aggregated_ID, .keep_all=TRUE)
LULC_rat <- LULC_rat[, c("Class_abbreviation", "Aggregated_ID")]

#layerize data (columns for each LULC class)
LULC_data <- raster::layerize(Current_LULC)
names(LULC_data) <- sapply(str_remove_all(names(LULC_data), "X"), function(y) {
  LULC_rat[LULC_rat$Aggregated_ID == y, "Class_abbreviation"]
})

### =========================================================================
### D- Load Suitability and Accessibility predictors and attach to LULC data
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Stacks/Calibration", pattern = Period_tag, full.names = TRUE))
} #close if statement calibration

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#get file path by matching on scenario ID and time step 
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Stacks/Simulation/SA_preds", pattern = paste0(Scenario_ID, "_", Simulation_time_step),  full.names = TRUE))
} #close simulation if statement

### =========================================================================
### E- Generate dynamic predictors
### =========================================================================

#-------------------------------------------------------------------------
# E.1- Dynamic predictors:  Municipal Population
#-------------------------------------------------------------------------

#for calibration the raster stacks already contain the dynamic predictor layers so there
#is nothing to be done

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){

  #create population data layer
  #subset current LULC to just urban cells
  Urban_rast <- Current_LULC == 10

  #load canton shapefile
  Canton_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

  #Zonal stats to get urban area per kanton
  Canton_urban_areas <- raster::extract(Urban_rast, Canton_shp, fun=sum, na.rm=TRUE, df=TRUE)

  #append Kanton ID 
  Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM

  #combine areas for cantons with multiple polygons
  Canton_urban_areas <- Canton_urban_areas %>%
  group_by(Canton_num) %>%
  dplyr::summarise(across(c(layer), sum))

  #load the municipality shape file
  Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

  #filter out non-swiss municipalities
  Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ] 

  #Zonal stats to get number of Urban cells per Municipality polygon
  #sum is used as a function because urban cells = 1 all others = 0
  Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun=sum, na.rm=TRUE, df=TRUE)
  
  #append Kanton and Municipality IDs
  Muni_urban_areas$Canton_num <- Muni_shp@data[["KANTONSNUM"]]
  Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
  Muni_urban_areas$Perc_urban <- 0
  
  #loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
  for(i in Canton_urban_areas$Canton_num){
  
  #vector kanton urban area
  Can_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "layer"])  

  #subset municipalities to this canton number 
  munis_indices <- which(Muni_urban_areas$Canton_num == i)
  
  #loop over municipalities in the Kanton and calculate their urban areas as a % of the Canton's total  
  for(muni in munis_indices){
  Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "layer"]/Can_urban_area)*100
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
  for(i in Muni_urban_areas$ID){
  
  #seperate canton specific model 
  canton_model <- pop_models[[Muni_urban_areas[Muni_urban_areas$ID == i, "Canton_num"]]]

  #perform prediction
  Muni_urban_areas$Perc_pop[[i]] <- predict(canton_model, newdata = Muni_urban_areas[Muni_urban_areas$ID == i, ]) 
  }

  #Our scenarios rely on specific population projects from FSO ("Ref", "High", "Low")
  #Identify which population scenario is required according to scenario being simulated
  Scenario_data_table <- openxlsx::read.xlsx("Tools/Scenario_specifications.xlsx", sheet = "Predictor_data")
  Pop_scenario <- Scenario_data_table[Scenario_data_table$Scenario_ID == Scenario_ID, "FSO_pop_scenario"]

  #load correct sheet of future population predictions according to scenario
  Pop_prediction_table <- openxlsx::read.xlsx("Data/Preds/Tools/Population_projections.xlsx", sheet = Pop_scenario)

  #loop over unique kanton numbers, rescale the predicted population percentages
  #and calculate the estimated population per municipality as a % of the cantonal total 

  #add results column
  Muni_urban_areas$Pop_est <- 0

  for(i in unique(Muni_urban_areas$Canton_num)){

    #subset to predicted cantonal population percentages
    Canton_dat <- Muni_urban_areas[Muni_urban_areas$Canton_num == i, "Perc_urban"]  

    #loop over the municipalites re-scaling the values
    Canton_preds_rescaled <- sapply(Canton_dat, function(y) {
    value <- y*1/sum(Canton_dat)
    value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
    return(value)}) #close inner loop

    #get the projected canton population value for this time point
    Canton_pop <- Pop_prediction_table[Pop_prediction_table$Canton_num == i, paste(Simulation_time_step)]

    #loop over the rescaled values calculating the estimated population
    Muni_indices <- which(Muni_urban_areas$Canton_num == i)
    Muni_urban_areas$Pop_est[Muni_indices] <- sapply(Canton_preds_rescaled, function(x) {
    pop_value <- Canton_pop*x #% already expressed as decimal so no need to /100  
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

  #load matrices used to create focal layers
  Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

  #adjust matrix names
  names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) {split_name <- (str_split(x, "[.]"))[[1]][2]})

  #Load details of focal layers required for the model set being utilised 
  Required_focals_details <- readRDS(list.files("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating", pattern = Period_tag, full.names = TRUE))

  #Loop over details of focal layers required creating a list of rasters from the current LULC map
  Nhood_rasters <- list()
  for(i in 1:nrow(Required_focals_details)){
  
    #vector active class names
    Active_class_name <- Required_focals_details[i,]$active_lulc   
  
    #get pixel values of active LULC class
    Active_class_value <- unlist(LULC_rat[LULC_rat$Class_abbreviation == Active_class_name, "Aggregated_ID"])

    #subset LULC raster by all Active_class_value
    Active_class_raster_subset <- Current_LULC == Active_class_value

    #create focal layer using matrix
    Focal_layer <- focal(x=Active_class_raster_subset, w= Focal_matrices[[Required_focals_details[i,]$matrix_id]], na.rm=FALSE, pad=TRUE, padValue=0, NAonly=FALSE) 

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

#Stack all rasters
#For calibration mode
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
Trans_data_stack <- stack(LULC_data, SA_pred_stack)
names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers))
} #close if statement calibration

#For simulation mode
#only stack the Nhood_rasters here because otherwise they were not included
#in the upper stack function
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#load the raster of Bioregions
Bioregion_rast <- raster("Data/Bioreg_CH/Bioreg_raster.gri")
names(Bioregion_rast) <- "Bioregion"
Trans_data_stack <- stack(LULC_data, SA_pred_stack, pop_raster, stack(Nhood_rasters), Bioregion_rast)
names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers), names(pop_raster), names(Nhood_rasters), names(Bioregion_rast))
} #close simulation if statement
  
#Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
Trans_dataset <- raster::as.data.frame(Trans_data_stack) 

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

#saveRDS(Trans_dataset, "Data/Spat_prob_perturb_layers/EXP_trans_dataset.rds")
#Trans_dataset <- readRDS("Data/Spat_prob_perturb_layers/EXP_trans_dataset.rds")

#subsetting data indices for glacial modelling
#data_indices <- Trans_dataset[,c("ID", "x", "y")]
#write.csv(data_indices, "Data/Data_indices_full.csv", row.names = FALSE)
#write.table(data_indices, "Data/Data_indices_full.txt", row.names = FALSE)

#load model look up
Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)

#if statement to remove transitions if they are being implemented deterministically
if(grepl("simulation", Model_mode, ignore.case = TRUE) &
    grepl("Y", Simulation_table$Deterministic_trans.string, ignore.case = TRUE)){

  #remove transitions with initial class == glacier
  Model_lookup <-Model_lookup[Model_lookup$Initial_LULC != "Glacier",]

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
for(i in Final_LULC_classes){
Prediction_probs[[paste0("Prob_", i)]] <- 0
Trans_dataset_na[[paste0("Prob_", i)]] <- NA
}

if(Use_parallel == "Y"){

Par_start_time <- Sys.time()  
#set up cluster for parallel computation
plan(future.callr::callr, workers=availableCores(omit = 2))

size = 5000*1024^2 
options(future.globals.maxSize= size)

#loop over transitions (currently takes 4 mins in parallel)
results <- future.apply::future_lapply(1:nrow(Model_lookup),
                                       future.packages = packs,
                                       function(i){

#gc()
                                         
#vector trans_name
Trans_name <- Model_lookup[i, "Trans_name"]
Trans_ID <- Model_lookup[i, "Trans_ID"]
Region <- Model_lookup[i,"Region"]
Final_LULC <- Model_lookup[i, "Final_LULC"]
Initial_LULC <- Model_lookup[i, "Initial_LULC"]

#load model
Fitted_model <- readRDS(Model_lookup[i, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioregion_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
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
for(i in results){

#get Prob_LULC column name to make appending simpler
LULC_col_name <- colnames(i)[2]

#Append prediction results   
Prediction_probs[which(Prediction_probs$ID %in% i[["ID"]]), LULC_col_name] <- i[[LULC_col_name]]
}
Par_end_time <- Sys.time()
Par_time <- Par_end_time - Par_start_time  # Parallel time = 1.472859 mins

}else if (Use_parallel == "N"){ #close parallel TPC chunk

#Non_parallel TPC calculation: 
Non_par_start <- Sys.time() 
#loop over transitions
for(i in 1:nrow(Model_lookup)){

#vector trans_name
Trans_name <- Model_lookup[i, "Trans_name"]
Trans_ID <- Model_lookup[i, "Trans_ID"]
Region <- Model_lookup[i,"Region"]
Final_LULC <- Model_lookup[i, "Final_LULC"]
Initial_LULC <- Model_lookup[i, "Initial_LULC"]

#print status message
cat(paste0("predicting probabilities for transitions from ", Initial_LULC, " to ", Final_LULC, " within region: ", Region, "\n"))

#load model
Fitted_model <- readRDS(Model_lookup[i, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioregion_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

#bind to ID
#predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

#append the predictions at the correct rows in the results df
#Prediction_probs[which(Prediction_probs$ID %in% predict_ID$ID), paste0("Prob_", Final_LULC)] <- predict_ID[paste0("Prob_", Final_LULC)]

#alternative method of replacing prob prediction values
Prediction_probs[row.names(prob_predicts),paste0("Prob_", Final_LULC)] <- prob_predicts[paste0("Prob_", Final_LULC)]

} #close loop over Models
Non_par_end <- Sys.time()
Non_par_time <- Non_par_end - Non_par_start #sequential time = 2.937131 mins
  } #Close non-parallel TPC chunk 

### =========================================================================
### G- Re-scale predictions
### =========================================================================

#loop over rows and re-scale probability values so that they sum to 1
#(by dividing by multiplying by 1 and then dividing by the sum of the vector)

#Transition probability columns to re-scale
Pred_prob_columns <- grep("Prob_", names(Prediction_probs), value = TRUE)

#vector row indices with non-zero sums of transition probabilities
Non_zero_indices <- which(rowSums(Prediction_probs[,Pred_prob_columns]) >1)

#Loop over rows performing re-scaling
Prediction_probs[Non_zero_indices ,Pred_prob_columns] <- as.data.frame(t(apply(Prediction_probs[Non_zero_indices,Pred_prob_columns], MARGIN = 1, FUN = function(x) {
sapply(x, function(y) {
value <- y*1/sum(x)
value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
return(value)
})
})))

#bind with background values
Trans_dataset_na[setdiff(names(Prediction_probs), names(Trans_dataset_na))] <- NA
Raster_prob_values <- rbind(Prediction_probs, Trans_dataset_na)

#sort by ID
Raster_prob_values <- Raster_prob_values[order(as.numeric(row.names(Raster_prob_values))), ]

#Save one copy of the raster probability values to be used to test
#spatial interventions, this file will be created during the running of the model
#to calibrate the Dinamica allocation parameters.
if(file.exists("Data/Exemplar_data/EXP_raster_prob_values.rds") == FALSE){
    dir.create("Data/Exemplar_data")
    saveRDS(Raster_prob_values, "Data/Exemplar_data/EXP_raster_prob_values.rds")
  }

### =========================================================================
### H- Spatial manipulations of transition probabilities
### =========================================================================

if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
  #If statement to implement spatial interventions
  if(Use_interventions == "Y"){
    
  #load table of scenario interventions
  Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")  
    
  #Use function to perform manipulation of spatial transition probabilities
  #according to scenario-specific interventions 
  Raster_prob_values <- lulcc.spatprobmanipulation(Interventions = Interventions,
                                                  Scenario_ID = Scenario_ID,
                                                  Raster_prob_values = Raster_prob_values,
                                                  Simulation_time_step = paste(Simulation_time_step))
  } #close if statement for spatial interventions

} #close simulation if statement

### =========================================================================
### I- Final rescaling
### =========================================================================

#vector row indices with non-zero sums of transition probabilities
Non_zero_indices <- which(rowSums(Raster_prob_values[,Pred_prob_columns]) > 1)

#Loop over rows performing re-scaling
Raster_prob_values[Non_zero_indices ,Pred_prob_columns] <- as.data.frame(t(apply(Raster_prob_values[Non_zero_indices,Pred_prob_columns], MARGIN = 1, FUN = function(x) {
sapply(x, function(y) {
value <- y*1/sum(x)
value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
return(value)
})
})))

### =========================================================================
### J- Save transition rasters
### =========================================================================

#subset model_lookup table to unique trans ID
Unique_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID), ]

#Loop over unique trans using details to subset data and save Rasters
for(i in 1:nrow(Unique_trans)){

Trans_ID <- Unique_trans[i, "Trans_ID"]
Final_LULC <- Unique_trans[i, "Final_LULC"]
Initial_LULC <- Unique_trans[i, "Initial_LULC"]

#get indices of rows for cells of initial class
Initial_indices <- na.omit(Raster_prob_values[Raster_prob_values[Initial_LULC] == 1,"ID"])

#get indices of non_class cells
non_initial_indices <- na.omit(Raster_prob_values[Raster_prob_values[Initial_LULC] == 0,"ID"])

#seperate Final class column
Trans_raster_values <- Raster_prob_values[, c("ID", "x", "y", paste0("Prob_", Final_LULC))]

#replace values of non-class cells with 0
Trans_raster_values[non_initial_indices, paste0("Prob_", Final_LULC)] <- 0

#rasterize and save using Initial and Final class names
Prob_raster <- rasterFromXYZ(Trans_raster_values[,c("x", "y", paste0("Prob_", Final_LULC))], crs = crs(Current_LULC))

#vector file path for saving probability maps
prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".tif")

writeRaster(Prob_raster, prob_map_path, overwrite=T)
} #close loop over transitions 

#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work
outputString("probmap_folder_path", prob_map_folder)

