#############################################################################
## Dinamica_load_trans_matrix: Load Scenario/time specific transition matrix 
## and update to reflect any deterministic transitions
## Date: 03-05-2023
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# #wpath <- getwd()
# setwd(wpath)

#Vector packages for loading
packs <- c("data.table", "raster", "tidyverse", "stringr", "readr", "xlsx",
           "Dinamica", "readxl")

# new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
# if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load table of simulations
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Vector name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Vector ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

### =========================================================================
### B- Load Transition rate table and update to reflect any deterministic transitions
### =========================================================================

#use scenario ID to grab folder path of scenario specific transition tables
Scenario_trans_table_dir <- str_replace((list.files(Trans_rate_table_dir, pattern = Scenario_ID, full.names = TRUE)), paste0(wpath, "/"), "")

#Use folder path to create a generic scenario specific transition matrix file path
Scenario_trans_table_file <- paste0(Scenario_trans_table_dir, "/", Scenario_ID, "_", "trans_table", "_", Simulation_year, ".csv")

#load the table
Trans_table <- read_csv(Scenario_trans_table_file)

#if statement to remove transitions if they are being implemented deterministicly
if (grepl("simulation", Model_mode, ignore.case = TRUE) &
  grepl("Y", Simulation_table$Deterministic_trans.string, ignore.case = TRUE)) {

  #remove transitions with initial class == glacier
  Trans_table <- Trans_table[Trans_table$`From*` != 19,]

} #close if statement
