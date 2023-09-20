#############################################################################
## Dinamica_intialize: Initialize model run specifications in Dinamica  
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

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

#receive working directory
wpath <- s2
setwd(wpath)

#simulation number being performed
Simulation_num <- v1
#Simulation_num <- 2

#load table of simulations
Control_table_path <- s1
#Control_table_path <- "E:/LULCC_CH/Tools/Calibration_control.csv"
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Enter an ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Get start and end dates of scenario (numeric)
Scenario_start <- Simulation_table$Scenario_start.real
Scenario_end <- Simulation_table$Scenario_end.real 

#Enter duration of time step for modelling
Step_length <- Simulation_table$Step_length.real

#Specify base folder for tables of allocation parameters
Params_folder <- paste(wpath, "/Data/Allocation_parameters")

#specify save location for simulated LULC maps (replace quoted section)
#folder path based upon Scenario and Simulation ID's
simulated_LULC_folder_path <- paste(wpath, "Results/Dinamica_simulated_LULC", Scenario_ID, Simulation_ID, sep = "/") 

### =========================================================================
### C- Work Dir and model mode initialization
### =========================================================================

#send step length
outputDouble("Step_length", Step_length)  
  
#Send Simulation ID
outputString("Sim_id", Simulation_ID)
  
#send Model mode
outputString("Model_mode", Model_mode)

### =========================================================================
### D- Model time step 
### =========================================================================
  
#use start and end time to generate a lookup table of dates seperated by 5 years 
model_time_steps <- list( Keys = c(seq(Scenario_start, Scenario_end, Step_length)),
                          Values = c(seq((Scenario_start+5), (Scenario_end+5), Step_length)))

#send Model time step table to Dinamica receiver: simulation_time_steps
outputLookupTable("simulation_time_steps", model_time_steps$Keys, model_time_steps$Values)

### =========================================================================
### E- Transition matrix initialization 
### =========================================================================

#use scenario ID to grab folder path of scenario specific transition tables
Scenario_transition_matrix_folder <- str_replace((list.files("E:/LULCC_CH/Data/Transition_tables/prepared_trans_tables", pattern = Scenario_ID, full.names = TRUE)), paste0(wpath, "/"), "")

#Use folder path to create a generic scenario specific transition matrix file path
Scenario_transition_matrix_file <- paste0(Scenario_transition_matrix_folder, "/", Scenario_ID, "_", "trans_table", "_")

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
if(Scenario_start >= 2020){LULC_start_year <- 2018}else{LULC_start_year <- Obs_LULC_years[base::which.min(abs(Obs_LULC_years - Scenario_start))]} 

#subset to correct LULC path and load
Initial_LULC_raster <- raster(Obs_LULC_paths[grep(LULC_start_year, Obs_LULC_paths)])

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
if(grepl("simulation", Model_mode, ignore.case = TRUE)){
Params_folder_Dinamica <- paste0(Params_folder, "/Simulation/Allocation_param_table_<v1>.csv")
} else if(grepl("calibration", Model_mode, ignore.case = TRUE)){
Params_folder_Dinamica <- paste0(Params_folder, "/Calibration/", Simulation_ID, "/Allocation_param_table_<v1>.csv")  
}

#send folder path as string to Dinamica receiver: trans_matrix_folder_path
outputString("Allocation_params_folder_path", Params_folder_Dinamica)
