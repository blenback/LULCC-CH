#############################################################################
## Dinamica_deterministic_trans: Implement deterministic transitions 
## following statistical transition potential during during simulation step
## Date: 03-05-2023
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# setwd(wpath)
#
# #Vector packages for loading
packs <- c("data.table", "raster", "tidyverse", "stringr", "readr", "xlsx",
           "Dinamica", "readxl")

# new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
# if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load table of simulations
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Vector name of Climate scenario
Climate_ID <- Simulation_table$Climate_scenario.string

#Vector ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#load the ref_grid to use the crs
ref_grid <- raster(Ref_grid_path)


### =========================================================================
### B- Implement deterministic glacial transitions
### =========================================================================

#if the model is in simulation mode and the value in the deterministic 
#transitions column of the control table is not "N" then update the current 
#simulated LULC map with the deterministic transitions
if (grepl("simulation", Model_mode, ignore.case = TRUE) &
  grepl("Y", Simulation_table$Deterministic_trans.string, ignore.case = TRUE)) {

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
                                             pattern = Climate_ID))[, c("ID_loc", paste(Simulated_lulc_year))]

  #seperate vector of cell IDs for glacier and non-glacer cells
  Non_glacier_IDs <- Glacier_index[Glacier_index[[paste(Simulated_lulc_year)]] == 0, "ID_loc"]
  Glacier_IDs <- Glacier_index[Glacier_index[[paste(Simulated_lulc_year)]] == 1, "ID_loc"]

  #replace the 1's and 0's with the correct LULC
  LULC_dat[LULC_dat$ID %in% Non_glacier_IDs, Value_col] <- 11
  LULC_dat[LULC_dat$ID %in% Glacier_IDs, Value_col] <- 19

  #convert back to raster
  Updated_raster <- rasterFromXYZ(LULC_dat[, c("x", "y", Value_col)])

  #add the project CRS
  crs(Updated_raster) <- crs(ref_grid)

  #save updated LULC raster
  writeRaster(Updated_raster, File_path_simulated_LULC_maps, overwrite = TRUE, datatype = "INT1U")
}


