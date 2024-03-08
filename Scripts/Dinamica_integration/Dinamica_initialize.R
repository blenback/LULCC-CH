#############################################################################
## Dinamica_intialize: Initialize model run specifications in Dinamica  
## Date: 25-02-2022
## Author: Ben Black
#############################################################################


### =========================================================================
### A- Preparation
### =========================================================================

# wpath <- "E:/LULCC_CH_HPC"
# setwd(wpath)
# Simulation_num <- 1

# Install packages if they are not already installed
packs <- c("data.table", "stringi", "stringr", "plyr", "readxl", #"ggpubr",
           "rlist", "tidyverse", "rstatix", "Dinamica", "raster", "openxlsx")

# new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
# if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load table of simulations
Simulation_table <- read.csv(Sim_control_path)[Simulation_num,]

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.)
Scenario_ID <- Simulation_table$Scenario_ID.string

#Vector simulation ID
Simulation_ID <- as.character(Simulation_table$Simulation_ID.string)

#Vector name of Climate scenario
Climate_ID <- Simulation_table$Climate_scenario.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Get start and end dates of scenario (numeric)
Scenario_start <- Simulation_table$Scenario_start.real
Scenario_end <- Simulation_table$Scenario_end.real

#Enter duration of time step for modelling
Step_length <- Simulation_table$Step_length.real

#specify save location for simulated LULC maps for this simulation
#LULCC_output_dir is created in Dinamica_get_env_vars.R
simulation_output_dir <- paste(LULCC_output_dir, Simulation_ID, sep = "/")
cat("Simulation output directory: ", simulation_output_dir, "\n")

### =========================================================================
### B- Generate table of simulation time steps
### =========================================================================

#use start and end time to generate a lookup table of dates seperated by 5 years
model_time_steps <- list(Keys = c(seq(Scenario_start, Scenario_end - 5, Step_length)),
                         Values = c(seq((Scenario_start + 5), (Scenario_end), Step_length)))

# ### =========================================================================
# ### C- LULC map initialization + glacier conversion
# ### =========================================================================

#Check if directory for saving LULC maps exists, if not create it.
#requires use of absolute paths
if (dir.exists(simulation_output_dir) == TRUE) { "LULC folder already exists" } else {
  dir.create(simulation_output_dir, recursive = TRUE) }

#Create relative file path for simulated LULC maps, building on folder path
# no need to include Dinamica's escape string because an R script is used to modify for the correct time step
simulated_LULC_file_path <- paste0(simulation_output_dir, "/", "simulated_LULC_simID_", Simulation_ID, "_year_")

#use Simulation start time to select file path of initial LULC map
Obs_LULC_paths <- list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".gri")

#extract numerics
Obs_LULC_years <- unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", Obs_LULC_paths)))

#vector file path for saving raster
save_raster_path <- paste0(simulated_LULC_file_path, Scenario_start, ".tif")

#if Scenario_start year is <= 2020 then it probably hasn't been run before so we
#need to create a copy of the initial LULC map to start the simulation with
# vice versa if Scenario_start year is >2020 then the scenario may have
#been run previously or have been interrupted by an error so there is no need
#to copy the start map because one will exist but this still needs to be checked

if (Scenario_start <= 2020) {

  #Identify start year
  LULC_start_year <- Obs_LULC_years[base::which.min(abs(Obs_LULC_years - Scenario_start))]

  #subset to correct LULC path and load
  Initial_LULC_raster <- raster(Obs_LULC_paths[grep(LULC_start_year, Obs_LULC_paths)])

  #convert raster to dataframe
  LULC_dat <- raster::as.data.frame(Initial_LULC_raster)

  #add ID column to dataset
  LULC_dat$ID <- seq.int(nrow(LULC_dat))

  #Get XY coordinates of cells
  xy_coordinates <- coordinates(Initial_LULC_raster)

  #cbind XY coordinates to dataframe and seperate rows where all values = NA
  LULC_dat <- cbind(LULC_dat, xy_coordinates)

  #For the simulations in order for the transition rates for glaciers to be
  #accurate we need to make sure that the initial LULC map has the correct
  #number of glacier cells according to glacial modelling
  if (grepl("simulation", Model_mode, ignore.case = TRUE)) {

    #load scenario specific glacier index
    Glacier_index <- readRDS(file = list.files("Data/Glacial_change/Scenario_indices",
                                               full.names = TRUE,
                                               pattern = Climate_ID))[, c("ID_loc", paste(Scenario_start))]

    #seperate vector of cell IDs for glacier and non-glacer cells
    Non_glacier_IDs <- Glacier_index[Glacier_index[[paste(Scenario_start)]] == 0, "ID_loc"]
    Glacier_IDs <- Glacier_index[Glacier_index[[paste(Scenario_start)]] == 1, "ID_loc"]

    #replace the 1's and 0's with the correct LULC
    LULC_dat[LULC_dat$ID %in% Non_glacier_IDs, "Pixel_value"] <- 11
    LULC_dat[LULC_dat$ID %in% Glacier_IDs, "Pixel_value"] <- 19

    #2nd step ensure that other glacial cells that do not match the glacier index
    #are also changed to static so that the transition rates calculate the
    #correct number of cell changes
    LULC_dat[which(LULC_dat$Pixel_value == 19 & !(LULC_dat$ID %in% Glacier_IDs)), "Pixel_value"] <- 11

    #convert back to raster
    Initial_LULC_raster <- rasterFromXYZ(LULC_dat[, c("x", "y", "Pixel_value")])
  } #close if statement for glacial modification

  #create a copy of the initial LULC raster files in the Simulation output folder so that it can be called within Dinamica,
  #it should be named using the file path for simulated_LULC maps (see above) and the Simulation start year
  writeRaster(Initial_LULC_raster, save_raster_path, overwrite = TRUE, datatype = "INT1U")

} #close if statement for copying initial LULC raster

### =========================================================================
### D- Send allocation parameter table folder path   
### =========================================================================

#append the suffix necessary for Dinamica to alter strings (<v1>) to the file name
if (grepl("simulation", Model_mode, ignore.case = TRUE)) {
  Params_folder_Dinamica <- paste0(Simulation_param_dir, "/", Scenario_ID, "/Allocation_param_table_<v1>.csv")
} else if (grepl("calibration", Model_mode, ignore.case = TRUE)) {
  Params_folder_Dinamica <- paste0(Calibration_param_dir, "/", Simulation_ID, "/Allocation_param_table_<v1>.csv")
}


