#############################################################################
## Dinamica_trans_potent_calc: Calculating transition potential maps using random forests
## for use in LULCC allocation within Dinamica EGO
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "parallel",
         "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest", "Dinamica")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

#Receive current simulation time
Simulation_time_step <- v1

#simulation number being performed
Simulation_num <- v2

#load table of simulations
Simulation_table <- read.csv("Tools/Simulation_control.csv")[Simulation_num,]

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Enter an ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Receive folder path for loading simulated LULC maps 
File_path_simulated_LULC_maps <- s2
#File_path_simulated_LULC_maps <- "Results/Dinamica_simulated_LULC/BAU/V1/simulated_LULC_scenario_BAU_simID_V1_year_<v1>"


#Convert model mode into a string related to the model period being used
#this makes it easier to load files because they use this nomenclature

Period_tag <- if(grepl("calibration", Model_mode, ignore.case = TRUE)&
                 Simulation_time_step <= 1997
                 ){"1985_1997"}else if (grepl("calibration", Model_mode, ignore.case = TRUE)&
                 Simulation_time_step >= 1997 &
                 Simulation_time_step <= 2009
                 ){"1997_2009"}else{"2009_2018"} 
#The last clause covers when calibration is occuring between 2009 and 2018
#and when the model is in simulation mode

#create folder for saving prediction probability maps
prob_map_folder <- paste0(wpath, "/Results/Pred_prob_maps/", Scenario_ID, "/", Simulation_ID, "/", Simulation_time_step)
dir.create(prob_map_folder, recursive = TRUE)

### =========================================================================
### C- Retrieve current LULC map and split into regionalized transition layers
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- str_replace(File_path_simulated_LULC_maps, "<v1>", paste0(Simulation_time_step))

#load current LULC map 
Current_LULC <- raster(current_LULC_path)

#vector LULC labels
LULC_labels <- c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier")
names(LULC_labels) <- c(10,11,12,13,14,15,16,17,18,19)

#vector LULC values that are present in the current map and get the class names
#(in case not all classes are present anymore during simulation)
LULC_values_current<- unique(Current_LULC)
LULC_labels_current <- LULC_labels[which(LULC_values_current %in% names(LULC_labels))]

#convert to ObsLulcRasterStack object using function from lulcc package
obs <- ObsLulcRasterStack(x= Current_LULC,
                           categories= LULC_values_current, 
                           labels= LULC_labels_current, 
                         t= Simulation_time_step)

#layerize data (columns for each LULC class) and convert to DF
LULC_data <- raster::layerize(obs@layers[[1]])
names(LULC_data) <- obs@labels
#LULC_df <- as.data.frame(raster::extract(x=LULC_data, y=part[["all"]]))

### =========================================================================
### D- Load Suitability and Accessibility predictors and attach to LULC data
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Calibration", pattern = Period_tag, full.names = TRUE))
} #close if statement calibration

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#get file path by matching on scenario ID and time step 
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Simulation/SA_preds/SA_pred_stacks", pattern = paste0(Scenario_ID, "_", Simulation_time_step),  full.names = TRUE))
} #close simulation if statement

### =========================================================================
### E- Create dynamic predictors
### =========================================================================

#create population data layer


### =========================================================================
### E- Load or create neighbourhood predictors
### =========================================================================

#for calibration the raster stacks already contain the nhood layers so there
#is nothing to be done

#for simulation the nhood layers need to be created from the current LULC map
#Two possibilities: 1. create all realisations 
#or 2. create only those realisations that are needed i.e. were retained after feature selection  

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#for both approaches:
#load matrices used to create focal layers
Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

#adjust matrix names
names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) {split_name <- (str_split(x, "[.]"))[[1]][2]})

#create a folder path using simulation ID and time step 
Dynamic_focal_folder_path <- paste0("Data/Preds/Prepared/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)
    
#create directory
dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)

# Approach 1
#provide vector of active LULC class names
#Active_class_names <- c('Urban', 'Int_AG', 'Alp_Past', 'Grassland', 'Perm_crops')

#generate rasters and save
# lulcc.generatenhoodrasters(LULC_raster = Current_LULC,
#                            Data_period = paste0(Scenario_ID, "_", Simulation_time_step),
#                            Neighbourhood_matrices = Focal_matrices,
#                            Active_LULC_class_names = Active_class_names,
#                            Nhood_folder_path = paste0(Dynamic_focal_folder_path, "/"))


#approach 2
#Load details of focal layers required for the model set being utilised 
Required_focals_details <- readRDS(list.files("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating", pattern = Period_tag, full.names = TRUE))

for (i in 1:nrow(Required_focals_details)){
  lulcc.generatenhoodrasters(LULC_raster = Current_LULC,
                             Neighbourhood_matrices = Focal_matrices[Required_focals_details[i,]$matrix_id],
                             Active_LULC_class_names = Required_focals_details[i,]$active_lulc,
                             Data_period = paste0(Scenario_ID, "_", Simulation_time_step),
                             Nhood_folder_path = paste0(Dynamic_focal_folder_path, "/"))
  }

#load raster stacks for each period
nhood_pred_stack <- stack(list.files(Dynamic_focal_folder_path, full.names = TRUE, pattern = ".gri"))

#alter name to generalized form
redact <- paste(c(paste0(Scenario_ID, "_", Simulation_time_step, "_"), ".gri"), collapse = "|")
names(nhood_pred_stack@layers) <- str_remove_all(list.files(Dynamic_focal_folder_path, full.names = FALSE, pattern = ".gri"), redact)
} #close simulation if statement

### =========================================================================
### F- Combine LULC, SA_preds and Nhood_preds and extract to dataframe
### =========================================================================

#Stack all rasters
#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
Trans_data_stack <- stack(LULC_data, SA_pred_stack)
names(Trans_data_stack@layers) <- c(names(LULC_data), names(SA_pred_stack))
} #close if statement calibration

names(Trans_data_stack@layers)

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  Trans_data_stack <- stack(LULC_data, SA_pred_stack, nhood_pred_stack)
names(Trans_data_stack@layers) <- c(names(LULC_data), names(SA_pred_stack), names(nhood_pred_stack))
} #close simulation if statement
  
#Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
Trans_dataset <- raster::as.data.frame(Trans_data_stack) 

#rename the LULC cols 
names(Trans_dataset)[1:10] <- names(Trans_data_stack@layers[1:10])

#add ID column to dataset
Trans_dataset$ID <- seq.int(nrow(Trans_dataset))

#Get XY coordinates of cells
xy_coordinates <- coordinates(Trans_data_stack) 

#cbind XY coordinates to dataframe and seperate rows where all values = NA
Trans_dataset <- cbind(Trans_dataset, xy_coordinates) 

#release memory
rm(LULC_data, SA_pred_stack, Trans_data_stack, points, region_table, obs, xy_coordinates)

### =========================================================================
### G- Run transition potential prediction with RF for each transition
### =========================================================================

#load model look up
Model_lookup <- read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)

#seperate Trans_dataset into complete cases for prediction
#and NAs ()background values
Trans_dataset_complete <- Trans_dataset %>%
  filter(complete.cases(.))

Trans_dataset_na <- Trans_dataset %>%
  filter(!complete.cases(.))  

Trans_dataset_na <- Trans_dataset_na[, c("x", "y", "ID")]

#lulcc.createprobmaps <- function(Model_lookup, Trans_dataset){}

#vector unique transition names
Trans_names <- unique(Model_lookup$Trans_name)

#loop over transitions
sapply(Trans_names, function(Transition){

#vector transition ID for file naming
Trans_ID <- unique(Model_lookup[Model_lookup$Trans_name == Transition, "Trans_ID"])

#subset model lookup table by transition name
Regional_trans_models <- Model_lookup[Model_lookup$Trans_name == Transition,]
Final_LULC <- Regional_trans_models[1, "Final_LULC"]
Initial_LULC <- Regional_trans_models[1, "Initial_LULC"]

#capture non-prediction rows (i.e. other initial LULC classes)
non_pred_data <- Trans_dataset_complete[!Trans_dataset_complete[Initial_LULC] == 1,]

#add final lulc class column to non-pred data
non_pred_data <- non_pred_data[,c("x", "y", "ID")]
non_pred_data[Final_LULC] <- 0

#loop over regional transition models, predicting probabilities and combine output as DF
Regional_predictions <- rbindlist(lapply(1:nrow(Regional_trans_models), function(trans_model){

#vector aspects of the iteration for readability
Region <- Regional_trans_models[trans_model, "Region"]

#print status message
cat(paste0("predicting probabilities for transitions from ", Initial_LULC, " to ", Final_LULC, " within region: ", Region, "\n"))

#load model
Fitted_model <- readRDS(Regional_trans_models[trans_model, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioreg_raster_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- Final_LULC

#bind to ID
predict_ID <- cbind(pred_data[,c("x","y", "ID")], prob_predicts[Final_LULC])

return(predict_ID)
}), idcol = NULL) #close loop over regional transition models

#append predictions with non_pred_data
Prediction_values <- rbind(Regional_predictions, non_pred_data)

#bind with NAs
background_NAs <- Trans_dataset_na
background_NAs[setdiff(names(Prediction_values), names(background_NAs))] <- NA
Raster_prob_values <- rbind(Prediction_values, background_NAs)

#sort by ID and then remove
Raster_prob_values[order(Raster_prob_values$ID),]
Raster_prob_values$ID <- NULL

#rasterize and save using Initial and Final class names
Prob_raster <- rasterFromXYZ(Raster_prob_values, crs = crs(Current_LULC))

#vector file path for saving probability maps
prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".tif")

writeRaster(Prob_raster, prob_map_path, overwrite=T)
}) #close loop over transitions

#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work

outputString("probmap_folder_path", prob_map_folder)

