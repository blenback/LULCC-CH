#############################################################################
## Dinamica_intialize: Initialize model run specifications in Dinamica  
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl", "ggpubr",
         "rlist", "tidyverse", "rstatix", "Dinamica")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#Open Dinamica session (session name specified within Parentheses
#as per Dinamica/Options/Integration/Active session name)
Dinamica::openSession("DinamicaEGO")
?Dinamica::openSession
### =========================================================================
### B- Enter model specifications
### =========================================================================

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- "BAU"

#Enter an ID for this run of the scenario (e.g V1)
Simulation_ID <- "V1"

#Define model_mode: Calibration or Simulation
Model_mode <- "Calibration"

#Enter start and end dates of scenario (numeric)
Scenario_start <- 1985
Scenario_end <- 1997 

#Enter duration of time step for modelling
Step_length <- 5

#specify save location for simulated LULC maps (replace quoted section)
#folder path based upon Scenario and Simulation ID's
simulated_LULC_folder_path <- paste("Results/Dinamica_simulated_LULC", Scenario_ID, Simulation_ID, sep = "/") 

### =========================================================================
### C- Work Dir and model mode initialization
### =========================================================================

{ # Open bracket to allows 'stop' calls to operate if invalid model specifications have been given
  
# Check all required inputs have been defined
if(all(sapply(c("Scenario_ID", 
     "Simulation_ID",
     "Model_mode",
     "Scenario_start",
     "Scenario_end",
     "Step_length",
     "simulated_LULC_folder_path",
     "wpath"), exists)) == "FALSE") {stop("All required inputs not found, correct before proceding")}  
  
#Change working directory for Dinamica if desired otherwise current WD (replace wpath with new location)
Dinamica::sendNamedString("work_dir_path", wpath)

#Send Scenario ID
Dinamica::sendNamedString("Scenario_id", Scenario_ID)

#Send Simulation ID
Dinamica::sendNamedString("Sim_id", Simulation_ID)
  
#Finalize string relevant for the selection of covariates and
#transition models to be used under calibration or Simulation

Precise_model_mode <- if (Model_mode == "Calibration" & Scenario_start == 1985) {
  "calibration_start_1985"} else if (Model_mode == "Calibration" & Scenario_start == 1997) {
    "calibration_start_1997"} else if (Model_mode == "Calibration" & Scenario_start == 2009) {
      "calibration_start_2009"} else if (Model_mode == "Simulation") {
        "Simulation"} else {NULL}

if (is.null(Precise_model_mode)) {stop("Invalid model mode settings, correct before continuing")} else
 {Dinamica::sendNamedString("Model_mode", Precise_model_mode)}

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
Dinamica::sendNamedLookupTable("simulation_time_steps", model_time_steps$Keys, model_time_steps$Values)

### =========================================================================
### E- Transition matrix initialization 
### =========================================================================

#TO DO make an if else system based on if the model mode is calibration or simulation

#use scenario ID to grab folder path of scenario specific transition tables
Scenario_transition_matrix_folder <- str_replace((list.files("E:/LULCC_CH/Data/Transition_tables/prepared_trans_tables", pattern = Scenario_ID, full.names = TRUE)), paste0(wpath, "/"), "")

#Use folder path to create a generic scenario specific transition matrix file path
Scenario_transition_matrix_file <- paste0(Scenario_transition_matrix_folder, "/", Scenario_ID, "_", "trans_table", "_")

#Check that there are sufficient transition matrices for the time steps specified if not abort. 

#Create a list of the hypothetical transition matrix files that should exist given the time steps specified.
list_of_trans_matrix_files <- lapply(model_time_steps$Values, function(x) paste0(Scenario_transition_matrix_file, x))

#run through list and check that all files exist return a vector of TRUE/FALSE and evaluate if all are TRUE
All_trans_matrixs_exist <- all(sapply(list_of_trans_matrix_files, function(x) file.exists(x)))

#If the test returns false throw an error message that stops the script initialising Dinamica
#if (All_trans_matrixs_exist == FALSE) {stop("Transition matrices are missing, correct before proceeding")}

#append the suffix necessary for Dinamica to alter strings (<v1>) to the file name
Scenario_transition_matrix_file_Dinamica <- paste0(Scenario_transition_matrix_file, "<v1>.csv")

#send folder path as string to Dinamica receiver: trans_matrix_folder_path
Dinamica::sendNamedString("trans_matrix_folder_path", Scenario_transition_matrix_file_Dinamica)

### =========================================================================
### F- LULC map initialization   
### =========================================================================

#Check if directory for saving LULC maps exists, if not create it.
#requires use of absolute paths
if (dir.exists(paste(wpath, simulated_LULC_folder_path, sep = "/")) == TRUE) {"LULC folder already exists"} else {
  dir.create(paste(wpath, simulated_LULC_folder_path, sep = "/"), recursive = TRUE)}

#Create relative file path for simulated LULC maps, building on folder path and including '_<v1>' necessary for Dinamica's escape string
simulated_LULC_file_path <- paste0(simulated_LULC_folder_path, "/", "simulated_LULC_scenario_", Scenario_ID, "_simID_", Simulation_ID, "_year_<v1>")

#send simulated LULC folder path to Dinamica receiver: sim_lulc_folder_path
Dinamica::sendNamedString("sim_lulc_folder_path", simulated_LULC_file_path)

#use Simulation start time to select file path of initial LULC map
Initial_LULC_path <- list.files("E:/LULCC_CH/Data/Historic_LULC", pattern = paste(Scenario_start), full.names = TRUE)

#vector file extensions (.gri, .grd)
file_suffixes <- lapply(Initial_LULC_path, function(x) str_split(x, "_agg")[[1]][2])

#vector new file names using suffixes
new_file_names <- c(sapply(file_suffixes, function(x) {file_path <- 
  str_replace(paste0(wpath, "/", simulated_LULC_file_path), "<v1>", paste0(Scenario_start, x))}))

#create a copy of the initial LULC raster files in the Simulation output folder so that it can be called within Dinamica, 
#it should be named using the file path for simulated_LULC maps (see above) and the Simulation start year
file.copy(from = Initial_LULC_path, to = new_file_names)

#use grep to select only the file with the .grd extension
#then remove the working ddirectory to make it a relative path
LULC_rel_path <- str_remove(grep(".grd",new_file_names, value = TRUE), paste0(wpath, "/"))

#send initial LULC map file path to Dinamica receiver:  
Dinamica::sendNamedString("initial_lulc_path", LULC_rel_path)

} #close function that allows 'stop' calls to operate

