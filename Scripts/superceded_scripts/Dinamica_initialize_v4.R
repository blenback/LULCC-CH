#############################################################################
## Dinamica_intialize: Initialize model run specifications in Dinamica  
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl", "ggpubr",
         "rlist", "tidyverse", "rstatix", "Dinamica", "raster", "openxlsx")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

### =========================================================================
### B- Enter model specifications
### =========================================================================

#simulation number being performed
Simulation_num <- v4
#Simulation_num <- 1

#load table of simulations
#TO DO: ADD IN A CONDITION TO CHOOSE BETWEEN SIMULATION CONTROL AND CALIBRATION CONTROL TABLES  
#Simulation_table <- read.csv("Tools/Simulation_control.csv")[Simulation_num,]
Simulation_table <- read.csv("Tools/Calibration_control.csv")[Simulation_num,]

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Enter an ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Enter start and end dates of scenario (numeric)
Scenario_start <- Simulation_table$Scenario_start.real

Scenario_end <- Simulation_table$Scenario_end.real 

#Enter duration of time step for modelling
Step_length <- Simulation_table$Step_length.real

#Specify base folder for tables of allocation parameters
Params_folder <- "E:/LULCC_CH/Data/Allocation_parameters/Simulation"

#specify save location for simulated LULC maps (replace quoted section)
#folder path based upon Scenario and Simulation ID's
simulated_LULC_folder_path <- paste(wpath, "Results/Dinamica_simulated_LULC", Scenario_ID, Simulation_ID, sep = "/") 

### =========================================================================
### C- Work Dir and model mode initialization
### =========================================================================

{ # Open bracket to allows 'stop' calls to operate if invalid model specifications have been given
  
# Check all required inputs have been defined
# if(all(sapply(c("Scenario_ID", 
#      "Simulation_ID",
#      "Model_mode",
#      "Scenario_start",
#      "Scenario_end",
#      "Step_length",
#      "simulated_LULC_folder_path",
#      "wpath"), exists)) == "FALSE") {stop("All required inputs not found, correct before proceding")}  
  
#Change working directory for Dinamica if desired otherwise current WD (replace wpath with new location)
outputString("work_dir_path", wpath)

#send step length
outputDouble("Step_length", Step_length)  
  
#Send Scenario ID
#outputString("Scenario_id", Scenario_ID)

#Send Simulation ID
outputString("Sim_id", Simulation_ID)
  
#send Model mode
outputString("Model_mode", Model_mode)

#Finalize string relevant for the selection of covariates and
#transition models to be used under calibration or Simulation

Precise_model_mode <- if (Model_mode == "Calibration" & Scenario_start == 1985) {
  "calibration_start_1985"} else if (Model_mode == "Calibration" & Scenario_start == 1997) {
    "calibration_start_1997"} else if (Model_mode == "Calibration" & Scenario_start == 2009) {
      "calibration_start_2009"} else if (Model_mode == "Simulation") {
        "Simulation"} else {NULL}



#TO DO: add warning message if model mode does not match 'precise model mode'  
  
### =========================================================================
### D- Model time step 
### =========================================================================
  
#use start and end time to generate a lookup table of dates seperated by 5 years 
#model_time_steps <- list( Keys = c(1:(((Scenario_end - Scenario_start)/Step_length)+1)),
#                          Values = c(seq(Scenario_start, Scenario_end, Step_length)))

#use start and end time to generate a lookup table of dates seperated by 5 years 
model_time_steps <- list( Keys = c(seq(Scenario_start, Scenario_end, Step_length)),
                          Values = c(seq((Scenario_start +5), (Scenario_end+5), Step_length)))

#send Model time step table to Dinamica receiver: simulation_time_steps
outputLookupTable("simulation_time_steps", model_time_steps$Keys, model_time_steps$Values)

### =========================================================================
### E- Transition matrix initialization 
### =========================================================================

#TO DO make an if else system based on if the model mode is calibration or simulation

#use scenario ID to grab folder path of scenario specific transition tables
Scenario_transition_matrix_folder <- str_replace((list.files("E:/LULCC_CH/Data/Transition_tables/prepared_trans_tables", pattern = Scenario_ID, full.names = TRUE)), paste0(wpath, "/"), "")

#Use folder path to create a generic scenario specific transition matrix file path
Scenario_transition_matrix_file <- paste0(Scenario_transition_matrix_folder, "/", Scenario_ID, "_", "trans_table", "_")

#For simulation mode especially two checks need to be made:
#1. That there are sufficient transition matrices for the time steps specified
#2. That the matrices contain all of the required transitions
#if not abort.
if (Model_mode == "Simulation") {
  #load the list of viable transitions under simulation
  viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[["2009_2018"]]

  #Create a list of the hypothetical transition matrix files that should exist given the time steps specified.
  list_of_trans_matrix_files <- lapply(model_time_steps$Keys, function(x) paste0(Scenario_transition_matrix_file, x, ".csv"))
  
  #run through list and check that all files exist return a vector of TRUE/FALSE and evaluate if all are TRUE
  All_trans_matrixs_exist <- all(sapply(list_of_trans_matrix_files, function(x) file.exists(x)))
  
  #If the test returns false throw an error message that stops the script initialising Dinamica
  if (All_trans_matrixs_exist == FALSE) {stop("Transition tables are missing, correct before proceeding")}
  
  #loop over the file checking that they include the same From/To class values as the viable trans list
  viable_trans_list$From_To <- paste(viable_trans_list$From., viable_trans_list$To., sep = "_")
  
  Trans_tables_correct <- all(sapply(list_of_trans_matrix_files, function(trans_table_path){
    trans_table <- read.csv(trans_table_path)
    trans_table_from_to <- paste(trans_table$From., trans_table$To., sep = "_")
    identical(viable_trans_list$From_To, trans_table_from_to)
  }))
  if (Trans_tables_correct == FALSE) {stop("Transition tables do not match the transitions to be modelled, correct before proceeding")}
  } #close simulation if statement
 
#append the suffix necessary for Dinamica to alter strings (<v1>) to the file name
Scenario_transition_matrix_file_Dinamica <- paste0(Scenario_transition_matrix_file, "<v1>.csv")

#send folder path as string to Dinamica receiver: trans_matrix_folder_path
outputString("trans_matrix_folder_path", Scenario_transition_matrix_file_Dinamica)

### =========================================================================
### F- LULC map initialization   
### =========================================================================

#Check if directory for saving LULC maps exists, if not create it.
#requires use of absolute paths
if (dir.exists(simulated_LULC_folder_path) == TRUE) {"LULC folder already exists"} else {
  dir.create(simulated_LULC_folder_path, recursive = TRUE)}

#Create relative file path for simulated LULC maps, building on folder path 
# no need to include Dinamica's escape string because an R script is used to modify for the correct time step
simulated_LULC_file_path <- paste0(simulated_LULC_folder_path, "/", "simulated_LULC_scenario_", Scenario_ID, "_simID_", Simulation_ID, "_year_")

#send simulated LULC folder path to Dinamica receiver: sim_lulc_folder_path
outputString("sim_lulc_folder_path", simulated_LULC_file_path)

#use Simulation start time to select file path of initial LULC map and load it
Obs_LULC_paths <- list.files("E:/LULCC_CH/Data/Historic_LULC", full.names = TRUE, pattern = ".gri")

#extract numerics
Obs_LULC_years <- unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", Obs_LULC_paths)))

#identify closest start year
if(Scenario_start >= 2020){LULC_start_year <- 2018}else{LULC_start_year <- base::which.min(abs(Obs_LULC_years - Scenario_start))} 

#subset to correct LULC path and load
Initial_LULC_raster <- raster(Obs_LULC_paths[LULC_start_year])

#vector file path for saving raster
save_raster_path <- paste0(simulated_LULC_file_path, Scenario_start, ".tif")

#create a copy of the initial LULC raster files in the Simulation output folder so that it can be called within Dinamica, 
#it should be named using the file path for simulated_LULC maps (see above) and the Simulation start year
writeRaster(Initial_LULC_raster, save_raster_path, overwrite = TRUE, datatype ="INT1U")

#use grep to select only the file with the .grd extension
#then remove the working directory to make it a relative path
#LULC_rel_path <- str_remove(save_raster_path, paste0(wpath, "/"))
LULC_rel_path <- save_raster_path

#send initial LULC map file path to Dinamica receiver:  
outputString("initial_lulc_path", LULC_rel_path)

### =========================================================================
### G- Send allocation parameter table folder path   
### =========================================================================

#append the suffix necessary for Dinamica to alter strings (<v1>) to the file name
if(Model_mode == "Simulation"){
Params_folder_Dinamica <- paste0(Params_folder, "/Allocation_param_table_<v1>.csv")
} else if(Model_mode == "Calibration"){
Params_folder_Dinamica <- paste0(Params_folder, "/", Simulation_ID, "/Allocation_param_table_<v1>.csv")  
}

#check if all allocation parameter tables exist and
#contain the correct transitions, if not abort.
if (Model_mode == "Simulation") {
  #load the list of viable transitions under simulation
  viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[["2009_2018"]]

  #Create a list of the hypothetical param table files that should exist given the time steps specified.
  list_of_param_table_files <- lapply(model_time_steps$Keys, function(x) str_replace(Params_folder_Dinamica, "<v1>", paste(x)))
  
  #run through list and check that all files exist return a vector of TRUE/FALSE and evaluate if all are TRUE
  All_param_tables_exist <- all(sapply(list_of_param_table_files, function(x) file.exists(x)))
  
  #If the test returns false throw an error message that stops the script initialising Dinamica
  if (All_param_tables_exist == FALSE) {stop("Allocation parameter tables are missing, correct before proceeding")}
  
  #loop over the file checking that they include the same From/To class values as the viable trans list
  viable_trans_list$From_To <- paste(viable_trans_list$From., viable_trans_list$To., sep = "_")
  
  Param_tables_correct <- all(sapply(list_of_param_table_files, function(param_table_path){
    param_table <- read.csv(param_table_path)
    param_table_from_to <- paste(param_table$From., param_table$To., sep = "_")
    identical(viable_trans_list$From_To, param_table_from_to)
  }))
  if (Param_tables_correct == FALSE) {stop("Allocation parameter tables do not match the transitions to be modelled, correct before proceeding")}
  } #close simulation if statement

#send folder path as string to Dinamica receiver: trans_matrix_folder_path
outputString("Allocation_params_folder_path", Params_folder_Dinamica)

### =========================================================================
### H- Check model look up table  
### =========================================================================

#check if the model look up table contains all of the transition IDs present
#in the viable trans lists and that all models in the table exist if not abort.
if (Model_mode == "Simulation") {
  #load the list of viable transitions under simulation
  viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[["2009_2018"]]

  #load model look up table
  Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = "2009_2018")  
  unique_trans_IDs <- unique(Model_lookup$Trans_ID)
  All_trans_have_models <- all(viable_trans_list$Trans_ID %in% unique_trans_IDs)
  #If the test returns false throw an error message that stops the script initialising Dinamica
  if (All_trans_have_models == FALSE) {stop("Some transitions do not have corresponding models in the model lookup table, correct before proceeding")}

  #Check that all model files containe din the look up table exist
  All_models_exist <- all(sapply(Model_lookup$File_path, function(x) file.exists(x)))
  if (All_models_exist == FALSE) {stop("Some models in lookup table do not have existing files, correct before proceeding")}
  } #close simulation if statement

### =========================================================================
### I- Check predictor data preparation   
### =========================================================================

#three checks:
#1. That predictor tables contain all of the predictors for the models
#2. That predictors in each table exist and there are no duplicates
#3. That all SA predictor stacks exist to be loaded

if (Model_mode == "Simulation") {
###Check 1. ###
#Use results from the end of feature selection
#to get a list of unique predictors across all models 
Feature_selection_results <- readRDS("E:/LULCC_CH/Results/Model_tuning/Covariate_selection/GRRF_embedded_selection/Period_2009_2018_GRRF_embedded_filtered_covs_regionalized")
Unique_preds <- unique(Reduce(c, unlist(sapply(Feature_selection_results, function(x) x[["var"]]), recursive = FALSE)))

#remove any focal preds because these are only created during prediction
SA_vars <- grep("nhood", Unique_preds, invert = TRUE, value = TRUE)

#loop over predictor tables ensuring that all SA vars are present
All_preds_in_tables <- all(sapply(model_time_steps$Keys, function(Time_step){
#load predictor table
Predictor_table <- openxlsx::read.xlsx("E:/LULCC_CH/Data/Preds/Predictor_table.xlsx", sheet = paste(Time_step))
all(SA_vars %in% Predictor_table$Covariate_ID)
}))
if (All_preds_in_tables == FALSE) {stop("Some predictors required for models are not contained in the predictor tables used to produce stacks, correct before proceeding")}

### Check 2.###
#Get file path of all unique predictors in tables
Pred_raster_paths <- unique(unlist(sapply(model_time_steps$Keys, function(Time_step){
#load predictor table
Predictor_table <- openxlsx::read.xlsx("E:/LULCC_CH/Data/Preds/Predictor_table.xlsx", sheet = paste(Time_step))
Predictor_table$File_name
}, simplify = TRUE)))

#check if they exist
All_pred_rasters_exist <- all(sapply(Pred_raster_paths, function(x) file.exists(x)))
if(All_pred_rasters_exist == FALSE) {stop("Some of the predictors required are missing corresponding raster files, correct before proceeding")}

#check that there are no duplicates
No_duplicate_preds <- sapply(model_time_steps$Keys, function(Time_step){
#load predictor table
Predictor_table <- openxlsx::read.xlsx("E:/LULCC_CH/Data/Preds/Predictor_table.xlsx", sheet = paste(Time_step))
any(duplicated(Predictor_table$Covariate_ID))
}, simplify = TRUE)
if(any(No_duplicate_preds)== TRUE){stop("Some of the predictor stacks contain duplicate predictors, correct before proceeding")}


### Check 3.###
# Create list of hypothetical predictor stacks required for this simulation
Pred_stack_paths <- sapply(model_time_steps$Keys, function(Time_step){
paste0("Data/Preds/Prepared/Stacks/Simulation/SA_preds/SA_pred_stacks/SA_pred_", Scenario_ID, "_", Time_step, ".rds")})

#check that they exist
All_pred_stacks_exist <- all(sapply(Pred_stack_paths, function(x) file.exists(x)))
if(All_pred_stacks_exist == FALSE) {stop("Some of the predictor stacks required do not exist, correct before proceeding")} 
} #close simulation if statement

} #close function that allows 'stop' calls to operate

