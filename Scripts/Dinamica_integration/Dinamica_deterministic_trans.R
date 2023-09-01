#############################################################################
## Dinamica_deterministic_trans: Implement deterministic transitions 
## following statistical transition potential during during simulation step
## Date: 03-05-2023
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

#receive working directory
wpath <- s2
#wpath <- getwd()
setwd(wpath)

#Vector packages for loading
packs<-c("data.table", "raster", "tidyverse","stringr", "readr", "xlsx",
         "Dinamica", "readxl")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#send model tool vars to global environment
list2env(readRDS("Tools/Model_tool_vars.rds"), .GlobalEnv)

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

#values for testing purposes
# Simulated_lulc_year <- "2025"
# Simulation_num <- "1"
# File_path_simulated_LULC_maps <- "Results/Dinamica_simulated_LULC/CALIBRATION/v1/simulated_LULC_scenario_CALIBRATION_simID_v1_year_2010.tif"

#Receive current simulation time
Simulated_lulc_year <- s3

#simulation number being performed
Simulation_num <- v2

#load table of simulations
Control_table_path <- s4
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Vector name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Vector ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Receive file path for loading simulated LULC maps
File_path_simulated_LULC_maps <- s1

#temp path for testing
#Updated_LULC_path <- str_replace(File_path_simulated_LULC_maps, ".tif", "_DTupdate.tif")

#if the model is in simulation mode and the value in the deterministic 
#transitions column of the control table is not "N" then update the current 
#simulated LULC map with the deterministic transitions
if(grepl("simulation", Model_mode, ignore.case = TRUE) &
    grepl("Y", Simulation_table$Deterministic_trans.string, ignore.case = TRUE)){
  
    #Load simulated LULC map for time step
    Current_lulc <- raster(File_path_simulated_LULC_maps)
    
    #convert raster to dataframe
    LULC_dat <- raster::as.data.frame(Current_lulc)
    
    #get name of first column
    Value_col <- colnames(LULC_dat)[1]

    #add ID column to dataset
    LULC_dat$ID <- seq.int(nrow(LULC_dat))

    #Get XY coordinates of cells
    xy_coordinates <- coordinates(Current_lulc) 

    #cbind XY coordinates to dataframe and seperate rows where all values = NA
    LULC_dat <- cbind(LULC_dat, xy_coordinates)
  
    #load scenario specific glacier index
    Glacier_index <- readRDS(file = list.files("Data/Glacial_change/Scenario_indices",
                                                 full.names = TRUE,
                                                 pattern = Scenario_ID))[,c("ID_loc", paste(Simulated_lulc_year))]

    #seperate vector of cell IDs for glacier and non-glacer cells
    Non_glacier_IDs <- Glacier_index[Glacier_index[[paste(Simulated_lulc_year)]]==0, "ID_loc"]
    Glacier_IDs <- Glacier_index[Glacier_index[[paste(Simulated_lulc_year)]]==1, "ID_loc"] 
    
    #replace the 1's and 0's with the correct LULC 
    LULC_dat[LULC_dat$ID %in% Non_glacier_IDs, Value_col] <- 11
    LULC_dat[LULC_dat$ID %in% Glacier_IDs, Value_col] <- 19
      
    #convert back to raster
    Updated_raster <- rasterFromXYZ(LULC_dat[,c("x", "y", Value_col)]) 
  
    #save updated LULC raster
    writeRaster(Updated_raster, File_path_simulated_LULC_maps, overwrite = TRUE, datatype ="INT1U")
  }

#send file path to dinamica
outputString("updated_lulc_map_path", File_path_simulated_LULC_maps)
