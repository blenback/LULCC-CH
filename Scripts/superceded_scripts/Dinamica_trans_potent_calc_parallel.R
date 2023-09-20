#############################################################################
## Dinamica_trans_potent_calc_parallel: Testing parallelization of transition 
## potential calculation
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
         "testthat", "sjmisc", "tictoc", "doParallel",
         "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest", "Dinamica")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

#Receive current simulation time
Simulation_time_step <- v1
#Simulation_time_step <- 2020

#simulation number being performed
Simulation_num <- v2
#Simulation_num <- "1"

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
#File_path_simulated_LULC_maps <- "E:/LULCC_CH/Results/Dinamica_simulated_LULC/BAU/V1/simulated_LULC_scenario_BAU_simID_V1_year_"


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
current_LULC_path <- paste0(File_path_simulated_LULC_maps, Simulation_time_step, ".tif")

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
#if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#for both approaches:
#load matrices used to create focal layers
Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

#adjust matrix names
names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) {split_name <- (str_split(x, "[.]"))[[1]][2]})

#create a folder path using simulation ID and time step 
Dynamic_focal_folder_path <- paste0("Data/Preds/Prepared/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)
    
#create directory
dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)

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

#} #close simulation if statement

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
rm(LULC_data, SA_pred_stack, Trans_data_stack, obs, xy_coordinates)

### =========================================================================
### G- Run transition potential prediction with RF for each transition
### =========================================================================

Trans_dataset <- readRDS("E:/LULCC_CH/Data/Trans_dataset_testing.rds")

#load model look up
Model_lookup <- read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)

#seperate Trans_dataset into complete cases for prediction
#and NAs ()background values
Trans_dataset_complete <- Trans_dataset %>%
  filter(complete.cases(.))

Trans_dataset_na <- Trans_dataset %>%
  filter(!complete.cases(.))  
Trans_dataset_na <- Trans_dataset_na[, c("x", "y", "ID")]

rm(Trans_dataset)

#set up cluster for parallel computation
no_cores <- detectCores()-2  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

#loop over transitions (currently takes 4 mins in parallel)
results <- foreach(i = 1:nrow(Model_lookup),
                   .packages =c("randomForest"),
                   .noexport = c("Trans_dataset_na") ) %dopar%{

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
                                      Trans_dataset_complete$Bioreg_raster_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

#bind to ID
predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

#return the prediction results to be bound together
return(predict_ID)
} #close loop over Models
stopCluster(cl)

#seperate ID, x/y and Initial_LULC cols to append results to
Prediction_probs <- Trans_dataset_complete[, c("ID", "x", "y", unique(Model_lookup$Initial_LULC), "Static")]
rm(Trans_dataset_complete)

#add cols to both the complete and NA data to capture predict probabilities to each class
Final_LULC_classes <- unique(Model_lookup$Final_LULC)
for(i in Final_LULC_classes){
Prediction_probs[[paste0("Prob_", i)]] <- 0
Trans_dataset_na[[paste0("Prob_", i)]] <- NA
}

#Now the results need to be combine with other rows
#results are being returned as a list
for(i in results){

#get Prob_LULC column name to make appending simpler
LULC_col_name <- colnames(i)[2]

#Append prediction results   
Prediction_probs[which(Prediction_probs$ID %in% i[["ID"]]), LULC_col_name] <- i[[LULC_col_name]]
}

### =========================================================================
### G- Re-scale predictions
### =========================================================================

#loop over rows and re-scale probability values so that they sum to 1
#(by dividing by multiplying by 1 and then dividing by the sum of the vector)

#vector row indices with non-zero sums of transition probabilities
Non_zero_indices <- which(rowSums(Prediction_probs[,grep("Prob_", names(Prediction_probs), value = TRUE)]) != 0)

#Transition probability columns to re-scale
Pred_prob_columns <- grep("Prob_", names(Prediction_probs), value = TRUE)

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
Raster_prob_values[order(Raster_prob_values$ID),]

### =========================================================================
### H- Spatially adjust transition probabilities
### =========================================================================

#NOW WOULD BE THE TIME TO SPATIALLY ADJUST PROBABILITY VALUES USING POLYGONS/RASTERS
#BEFORE SAVING THEM BECAUSE RE-SCALING WILL NEED TO BE REPEATED AFTER ADJUSTMENT.   

### =========================================================================
### I- Save transition rasters
### =========================================================================

#subset model_lookup table to unique trans ID
Unique_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID), ]

#Loop over unique trans using details to subset data and save Rasters
system.time({
for(i in 1:nrow(Unique_trans)){
Trans_ID <- Unique_trans[i, "Trans_ID"]
Final_LULC <- Unique_trans[i, "Final_LULC"]
Initial_LULC <- Unique_trans[i, "Initial_LULC"]

#subset data
Trans_raster_values <- Raster_prob_values[,c("x", "y", paste0("Prob_", Final_LULC))]

#rasterize and save using Initial and Final class names
Prob_raster <- rasterFromXYZ(Trans_raster_values, crs = crs(Current_LULC))

#vector file path for saving probability maps
prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".tif")

writeRaster(Prob_raster, prob_map_path, overwrite=T)
} #close loop over transitions 
})
  
#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work

outputString("probmap_folder_path", prob_map_folder)