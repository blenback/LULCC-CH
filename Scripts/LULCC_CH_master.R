#############################################################################
## LULCC_CH_master: Master script for whole process of LULCC data preparation,
## statistical modelling and preparing model inputs required by Dinamica EGO
##
## Note: scripts must be sourced in the order presented in order to work,
## adjust model_specs table to:
## 1. control time periods to be modelled
## 2. Specify statistical modelling technique
## 3. Whether or not regionalized datasets and models should be created
##
## Date: 25-10-2022
## Author: Ben Black
#############################################################################

# Install and load packages

#install Dinamica from source
#install.packages("Model/dinamica_1.0.4.tar.gz", repos=NULL, type="source")

#SDMtools is depreciated and needs to be installed from source
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

#vector other required packages
packs<-c("data.table", "raster", "tidyverse", "SDMTools", "doParallel", 
"sf", "tiff", "igraph", "readr", "foreach", "testthat", 
"sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", 
"rgeos", "bfsMaps", "rjstat", "future.apply", "future", "stringr", 
"stringi", "readxl", "rlist", "rstatix", "openxlsx", "pxR", "zen4R", 
"rvest", "viridis", "sp", "jsonlite", "httr", "xlsx", "callr",
"gdata", "landscapemetrics", "randomForest", "RRF", "future.callr", 
"ghibli", "ggpattern", "butcher", "ROCR", "ecospat", "caret", "Dinamica", 
"gridExtra", "extrafont", "ggpubr", "ggstatsplot","PMCMRplus", "reshape2",
"ggsignif", "ggthemes", "ggside", "gridtext", "grid", "rstudioapi", "landscapemetrics")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",
                            pattern = ".R",
                            full.names = TRUE,
                            recursive = TRUE),source))

#TO DO: Check if Dinamica EGO is already installed
# Diego.installed <- system(comannd = paste('*dinamica7* -v'))==0
# executable <- "*Dinamica*"
# test <- system2("where", args = c("-v", executable))
# print(test)


#Install Dinamica EGO using included installer (Windows)
#create string for installer
#install_path <- paste0(getwd(), "/Model/SetupDinamicaEGO-720.exe")
#install_path <- gsub("/", "\\\\", install_path) #replace "/" with "\\"

#system command
#system2(command = paste(install_path))

#set environment path for Dinamica log/debug files
#create a temporary dir for storing the Dinamica output files
# Logdir <- "Model/Dinamica_models/Model_log_files"
# dir.create(Logdir)
# Win_logdir <- paste0(getwd(), "/", Logdir)
# Win_logdir <- gsub('(*/)\\1+', '\\1', Win_logdir) #remove instances of double "/"
# Win_logdir <- gsub("/", "\\\\", Win_logdir) #replace "/" with "\\"
# Sys.setenv(DINAMICA_EGO_7_LOG_PATH = paste(Win_logdir))

#create table for controlling simulations

#User enter scenario names to model 
#vector abbreviations of scenario's for folder/file naming
Scenario_names <- c("BAU", "EI_NAT", "EI_CUL", "EI_SOC", "GR_EX")

#User enter start and end dates for the scenarios
#either enter a single number value or a vector of values the same length as the number of scenarios
#earliest possible model start time is 1985 and end time is 2060,
#simulations begin from 2020 and we have initially agreed to use 5 year time steps

Scenario_start <- 2020
Scenario_end <- 2060 
Step_length <- 5

#User enter number of runs to perform for each simulation
reps <- 1

#Threshold for identifying transitions: This represents the number of transition
#instances from class X -> Y as a % of the the total area of class X
#a good value for this threshold is 0.5 such that if the number of cells
#transitioning <0.5% of the total number of cells of the initial class then the
#transition is not included. The rationale for this is that the statistical model
#produced for the transition will be too weak due to high-imbalance
Inclusion_thres <- 0.5

### =========================================================================
### Simulation control table prep
### =========================================================================

#vector save path
Sim_control_path <- "Tools/Simulation_control.csv"

# Simulation_control_table <- data.frame(matrix(ncol = 11, nrow = 0))
# colnames(Simulation_control_table) <- c("Simulation_num.",
#                                          "Scenario_ID.string",
#                                          "Simulation_ID.string",
#                                          "Model_mode.string",
#                                          "Scenario_start.real",
#                                          "Scenario_end.real",
#                                          "Step_length.real",
#                                          "Parallel_TPC.string",
#                                          "Spatial_interventions.string",
#                                          "Deterministic_trans.string",
#                                          "Completed.string")
# 
# #expand vector of scenario names according to number of repetitions and add to table
# Scenario_IDs <- c(sapply(Scenario_names, function(x) rep(x, reps), simplify = TRUE))
# Simulation_control_table[1:length(Scenario_IDs), "Scenario_ID.string"] <- Scenario_IDs
# 
# #fill other columns
# Simulation_control_table$Simulation_ID.string <- rep(paste0("v", seq(1, reps, 1)), length(Scenario_names))
# Simulation_control_table$Scenario_start.real <- if(length(unique(Scenario_start)) == 1){Scenario_start} else {c(rep(Scenario_start, length(Scenario_names)))}
# Simulation_control_table$Scenario_end.real <- if(length(unique(Scenario_end)) == 1){Scenario_end} else {c(rep(Scenario_end, length(Scenario_names)))}
# Simulation_control_table$Step_length.real <- Step_length
# Simulation_control_table$Model_mode.string <- "Simulation"
# Simulation_control_table$Simulation_num. <- seq(1, nrow(Simulation_control_table),1)
# Simulation_control_table$Parallel_TPC.string <- "N"
# Simulation_control_table$Spatial_interventions.string <- "Y"
# Simulation_control_table$Deterministic_trans.string <- "Y"
# Simulation_control_table$Completed.string <- "N"
# 
# #save the table
# write_csv(Simulation_control_table, Sim_control_path)

### =========================================================================
### Modelling set-up
### =========================================================================

#TO DO: Document how users should set up the various 'tools' tables that control
#the creation of transition datasets and the tp models. 

#Get the file path of the Dinamica console executable 
# DC_path <- list.files("C:/", recursive = TRUE, full.names = TRUE, pattern = ".*DinamicaConsole.*\\.exe")
# DC_path <- gsub('(*/)\\1+', '\\1', DC_path) #remove instances of double "/"
# DC_path <- gsub("/", "\\\\", DC_path) #replace "/" with "\\"
DC_path <- "C:\\Program Files\\Dinamica EGO 7\\DinamicaConsole7.exe"

#create directory to store simulation logs
Sim_log_dir <- "Results/Simulation_notifications"
dir.create(Sim_log_dir)

#list objects required for modelling
Model_tool_vars <- list(LULC_aggregation_path = "Tools/LULC_class_aggregation.xlsx",#Path to LULC class aggregation table
                          Model_specs_path = "Tools/model_specs.xlsx", #Path to model specifications table
                          Param_grid_path = "Tools/param-grid.xlsx", #Path to model hyper parameter grids
                          Pred_table_path = "Tools/Predictor_table.xlsx", #Path to predictor table
                          Ref_grid_path = "Data/Ref_grid.gri",
                          Scenario_specs_path = "Tools/Scenario_specifications.xlsx",
                          Calibration_param_dir = "Data/Allocation_parameters/Calibration",
                          Simulation_param_dir= "Data/Allocation_parameters/Simulation",
                          Trans_rate_table_dir = "Data/Transition_tables/prepared_trans_tables",
                          Sim_control_path = Sim_control_path, #Path to simulation control table
                          Sim_log_dir = Sim_log_dir,
                          Step_length= Step_length,
                          Scenario_names = Scenario_names,
                          Inclusion_thres = Inclusion_thres,
                          DC_path = DC_path) #Path to grid to standardise spatial data

#Import model specifications table
model_specs <- read_excel(Model_tool_vars$Model_specs_path)

# Vector data periods contained in model specifications table
Model_tool_vars$Data_periods <- unique(model_specs$Data_period_name)

#attach string to env. indicating whether regionalized datasets should be produced
if(any(grep(model_specs$Model_scale,
        pattern = "regionalized",
        ignore.case = TRUE)) == TRUE){
Model_tool_vars$Regionalization <- TRUE  
} else{
Model_tool_vars$Regionalization <- FALSE  
}

#save the list of model tools to be used during simulations
saveRDS(Model_tool_vars, "Tools/Model_tool_vars.rds")

#Create a seperate environment for storing output of sourced scripts
scripting_env <- new.env() 

#send objects to global and scipting environment
list2env(Model_tool_vars, .GlobalEnv)
list2env(Model_tool_vars, scripting_env)

### =========================================================================
<<<<<<< HEAD
### Download research data
### =========================================================================

#Data that is not created programmatically has been provided as a Zenodo record.
#under the following DOI and URL
Zenodo_record_doi <- "10.5281/zenodo.8263509"

#This can be downloaded using the Zenodo API service.

#Connect to Zenodo API service
=======
### Download and unpack data
### =========================================================================

#download raw predictor data using Zenodo API service to get URLs for file downloads

#connect to Zenodo API
>>>>>>> 01677fe38d086bd8a3841fb7af8c0148fbeb4daa
zenodo <- ZenodoManager$new()

#Get record info
#TO DO: won't work until record is made open access 
<<<<<<< HEAD
rec <- zenodo$getRecordByDOI(Zenodo_record_doi)
files <- rec$listFiles(pretty = TRUE)
files <- my_rec$listFiles(pretty = TRUE)

#increase timeout limit for downloading folder  
options(timeout=6000)

#create a temporary directory to store the zipped folder
tmpdir <- tempdir()

#Download to tmpdir
#Option 1: using the download link in the Zenodo record object
my_rec$downloadFiles(path = tmpdir)

#Option 2: using R's download.file function
#download.file(files$download, paste0(tmpdir, "/", files$filename), mode = "wb")

#WARNING: unzipping the folder programatically can be temperamental for various
#reasons. As such, you may need to navigate to the tmpdir and manually unzip
#the folder there using the standard process for your operating system.

#Option 1: Custom function for unzipping large files using system2 command
=======
rec <- zenodo$getRecordByDOI("10.5281/zenodo.7590103")
files <- rec$listFiles(pretty = TRUE)
files <- my_rec$listFiles(pretty = TRUE)

#increase timeout limit for downloading file  
options(timeout=6000)

#create a temporary directory to store the zipped file
tmpdir <- tempdir()

#Download to tmpdir
my_rec$downloadFiles(path = tmpdir)
download.file(files$download, paste0(tmpdir, "/", files$filename), mode = "wb")

#unzip (this can be temperamental may need to manually unzip)
#function for unzipping large files using system
>>>>>>> 01677fe38d086bd8a3841fb7af8c0148fbeb4daa
decompress_file <- function(directory, file, .file_cache = FALSE) {

    if (.file_cache == TRUE) {
       print("decompression skipped")
    } else {

      # Set working directory for decompression
      # simplifies unzip directory location behavior
      wd <- getwd()
      setwd(directory)

      # Run decompression
      decompression <-
        system2("unzip",
                args = c("-o", # include override flag
                         file),
                stdout = TRUE)

      # uncomment to delete archive once decompressed
      # file.remove(file) 

      # Reset working directory
      setwd(wd); rm(wd)

      # Test for success criteria
      # change the search depending on 
      # your implementation
      if (grepl("Warning message", tail(decompression, 1))) {
        print(decompression)
      }
    }
} 

<<<<<<< HEAD
#Applying custom function
decompress_file(tmpdir, file = paste0(tmpdir, "\\", files$filename), .file_cache = FALSE)

#Option 2: using r utils::unzip
unzip(paste0(tmpdir, "/", files$filename), exdir = str_remove(paste0(tmpdir, "/", files$filename), ".zip")) 

#Some of the files and dirs needed to be unpacked to different locations
#within the project dir

#Move the raw data dir:
raw_data_path <- str_replace(paste0(tmpdir, "/", files$filename), ".zip", "/Data/Raw")
file.copy(raw_data_path, "Data/Preds", recursive=TRUE)

#Moving the spatial reference grid
raw_data_path <- str_replace(paste0(tmpdir, "/", files$filename), ".zip", "/Data/Raw")

=======
#using function
decompress_file(tmpdir, file = paste0(tmpdir, "\\", files$filename), .file_cache = FALSE)

#using r utils::unzip
unzip(paste0(tmpdir, "/", files$filename), exdir = str_remove(paste0(tmpdir, "/", files$filename), ".zip")) 

#TO DO: update path when Manuel has finished Zenodo upload.
#select just the raw data
raw_data_path <- str_replace(paste0(tmpdir, "/", files$filename), ".zip", "/Data/Raw")

#Move files into project structure
file.copy(raw_data_path, "Data/Preds", recursive=TRUE)

>>>>>>> 01677fe38d086bd8a3841fb7af8c0148fbeb4daa
#remove the zipped folder in temp dir 
unlink(paste0(tmpdir, "/", files$filename)) 

### =========================================================================
### A- Prepare LULC/region data
### =========================================================================

#Prepare LULC data layers
source("Scripts/Preparation/LULC_data_prep.R", local = scripting_env)

#Prepare raster of Swiss Bioregions
source("Scripts/Preparation/Region_prep.R", local = scripting_env)

### =========================================================================
### B- Prepare predictor data
### =========================================================================

#Start from a basic table of predictor names and details
#that cannot be created programmatically and expand this
#when data layers are created

#Prepare suitability and accessibility predictors
source("Scripts/Preparation/Calibration_predictor_prep.R", local = scripting_env)

### =========================================================================
### C- Identify LULC transitions and create transition datasets
### =========================================================================

source("Scripts/Preparation/Transition_identification.R", local = scripting_env)

### =========================================================================
### D- Create transition datasets
### =========================================================================

source("Scripts/Preparation/Transition_dataset_prep.R", local = scripting_env)

### =========================================================================
### E- Predictor variable selection on LULCC transition datasets
### =========================================================================

source("Scripts/Preparation/Transition_feature_selection.R", local = scripting_env) 

### =========================================================================
### F- Statistical modelling of LULCC transition datasets
### =========================================================================

#TO DO: USER CREATE TABLE OF MODEL SPECIFCATIONS AND PARAM GRID TO BE TESTED

source("Scripts/Preparation/Trans_modelling.R", local = scripting_env)

### =========================================================================
### G- Summarizing model validation results
### =========================================================================

#The results comparing the performance of different transition model
#specifications require manual interpretation as the choice of optimal model
#must balance numerous aspects: accuracy, overfitting, computation time etc.
source("Scripts/Preparation/Transition_model_evaluation.R", local = scripting_env)

#Enter choice of optimal model specifcations
#Model_type <- "rf"
#Model_scale <- "regionalized"
#Feature_selection_employed <- "TRUE"
#Balance_adjustment <- "TRUE"

#adjust contents of model_specs table to only optimal specifcations
lulcc.finalisemodelspecifications(Model_specs_path = Model_specs_path,
                                  Param_grid_path = Param_grid_path)

### =========================================================================
### H- Re-fitting optimal model specifications on full data
### =========================================================================

source("Scripts/Preparation/Trans_model_finalization.R", local = scripting_env)

### =========================================================================
### I- Prepare data for deterministic transitions (e.g glacier -> Non-glacier)
### =========================================================================
      
source("Scripts/Preparation/Deterministic_trans_prep.R", local = scripting_env)

### =========================================================================
### I- Prepare tables of transition rates for scenarios
### =========================================================================
      
source("Scripts/Preparation/Simulation_trans_tables_prep.R", local = scripting_env)

### =========================================================================
### J- Prepare predictor data for scenarios
### =========================================================================
      
source("Scripts/Preparation/Simulation_predictor_prep.R", local = scripting_env)

### =========================================================================
### K- Calibrate allocation parameters for Dinamica
### =========================================================================

#1. Estimate values for the allocation parameters and then apply random perturbation
#to generate sets of values to test with monte-carlo simulation
#2. perform simulations with all parameter sets
#3. Identify best performing parameter sets and save copies of tables
#to be used in scenario simulations

source("Scripts/Preparation/Calibrate_allocation_parameters.R", local = scripting_env)

### =========================================================================
### L- Prepare scenario specific spatial interventions
### =========================================================================

source("Scripts/Preparation/Spatial_interventions_prep.R", local = scripting_env)

### =========================================================================
### M- Run Dinamica simulations over scenarios
### =========================================================================



#Note this path needs to include the working directory because it is used
#in the windows system command
Control_table_path <- paste0(getwd(),"/", Sim_control_path)

#Perform pre-checks to make sure that all element required for Dinamica modelling
#are prepared
Pre_check_result <- lulcc.modelprechecks(Control_table_path, Param_dir = Simulation_param_dir)

#Run the Dinamica simulation model
#Fail pre-check condition
if(Pre_check_result == FALSE){print("Some elements required for modelling are not present/incorrect,
        consult the pre-check results object")} else if(Pre_check_result == TRUE){

  #Read in Model.ego file
  Model_text <- try(readLines("Model/Dinamica_models/LULCC_CH.ego"))

  #Replace dummy string for working directory path path
  Model_text <- str_replace(Model_text, "=====WORK_DIR=====", getwd())

  #Replace dummy string for control table file path
  Model_text <- str_replace(Model_text, "=====TABLE_PATH=====", Control_table_path)

  #save a temporary copy of the model.ego file to run
  print('Creating a copy of the Dinamica model using the current control table')
  Temp_model_path <- gsub(".ego", paste0("_simulation_", Sys.Date(), ".ego"), "Model/Dinamica_models/LULCC_CH.ego")
  writeLines(Model_text, Temp_model_path)  

  #vector a path for saving the output text of this simulation
  #run which indicates any errors
  output_path <- paste0(Sim_log_dir, "/Simulation_output_", Sys.Date(), ".txt")

  print('Starting to run model with Dinamica EGO')
  system2(command = paste(DC_path),
        #args = c("-processors 10","-memory-allocation-policy 4", Temp_model_path),
        args = c("-disable-parallel-steps",Temp_model_path),
       wait = TRUE,
       stdout= output_path,
       stderr = output_path)
  
  #because the simulations may fail without the system command returning an error 
  #(if the error occurs in Dinamica) then check the simulation control table to see
  #if/how many simulations have failed
  Updated_control_tbl <- read.csv(Control_table_path)
  
  if(any(Updated_control_tbl$Completed.string == "ERROR")){
    print(paste(length(which(Updated_control_tbl$Completed.string == "ERROR")), "of", nrow(Updated_control_tbl),
                 "simulations have failed to run till completion, check simulation output .txt file for details of errors"))
  }else{
    #Send completion message
    print('All simulations completed sucessfully')
  
    #Delete the temporary model file
    #unlink(Temp_model_path)
    
    #clean up log and debug files created by Dinamica as their output
    #is stored in the .txt file anyway
    #unlink(list.files(pattern = paste0(c("log_","debug_"),collapse="|"), full.names = TRUE))
    
  }
} #close if statement running simulation
