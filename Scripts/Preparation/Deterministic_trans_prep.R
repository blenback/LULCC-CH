#############################################################################
## Deterministic_trans_prep: Prepare spatial data for locations of deterministic
## trans to implement during simulations
##
## Date: 03-05-2023
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
#Vector packages for loading
# packs<-c("data.table", "raster", "tidyverse",
#          "lulcc", "stringr", "readr", "xlsx", "gdata")
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))

LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#Vector time Data_periods for calibration
#Inherit from master
#Data_periods <- c("1985_1997", "1997_2009", "2009_2018")

#The model lookup table specifies which transitions are modelled and
#should be used to subset the transition rates tables

#Load Model lookup tables for each period and subset to just transition names
Periodic_trans_names <- lapply(Data_periods, function(Period){
  full_table <- read.xlsx("Tools/Model_lookup.xlsx", sheetIndex = Period)
  trans_names <- unique(full_table[["Trans_name"]])
  })
names(Periodic_trans_names) <- Data_periods

#identify final year of calibration periods
Final_calib_year <- max(c(sapply(names(Periodic_trans_names), function(x)as.numeric(str_split(x, "_")[[1]]), simplify = TRUE)))

#load simulation control table
Sim_control_table <- read.csv(Sim_control_path)

#vector all time steps in calibration and simulation
All_time_steps <- seq(min(LULC_years), max(Sim_control_table$Scenario_end.real), Step_length)

#vector simulation time steps 
#(i.e. only the time steps until the end, omitting the start year)
Sim_time_steps <- All_time_steps[between(All_time_steps,(min(Sim_control_table$Scenario_start.real)+Step_length), max(Sim_control_table$Scenario_end.real))]

#vector all simulation years (i.e. including initial year)
Sim_years <- c(round(as.numeric(max(LULC_years))/Step_length)*Step_length, paste(Sim_time_steps))

#use simulation control table to get names of Scenarios
Scenario_names <- unique(Sim_control_table[["Scenario_ID.string"]])

### =========================================================================
### B - calculate glacial change rates and wrangle indices of change locations
### =========================================================================

#The files provided by Farinotti et al. contained the locations
#of glacier (1) and absence of glacier (0) according to the index of cells in
#our spatial grid under the different RCPs. 

#We need to use these indices of glacier locations to calculate
#glacial coverage in each time step for each scenario as input
#for the calculation of modified transition rates. 

#Wrangle glacial location data for each RCP
Glacier_indices <- lapply(list.files("Data/Glacial_change/median_scenarios", full.names = TRUE),function(x) {
  
  #load
  Glacier_index <- read.table(file = x, skip = 10,header = TRUE)
  
  #adjust column names to reflect years
  colnames(Glacier_index)[2:ncol(Glacier_index)] <- seq(from = 2005, to= 2100, by =5)
  
  #subset to simulation years
  Glacier_index_sim <- Glacier_index[,c("ID_loc", Sim_years)]
  })

#extract RCP designation between other strings
names(Glacier_indices) <- lapply(list.files("Data/Glacial_change/median_scenarios", full.names = FALSE), function(x) str_match(x, "series_\\s*(.*?)\\s*_median")[,2]) 

#calculate glacial change area per time step and combine to single DF 
Glacial_change <- rbindlist(lapply(Glacier_indices, function(x){
  
  #calculate col sums
  Area_per_year <- colSums(x[,2:ncol(x)])
  
  #calculate change in area between each time point
  Areal_change <- data.frame(t(sapply(1:(length(Area_per_year)-1), function(i){
    chg <- Area_per_year[i] - Area_per_year[i+1]
  })))
  
  colnames(Areal_change) <- names(Area_per_year)[2:length(Area_per_year)]
  return(Areal_change)
}), idcol = "RCP")

#save a table of glacial change areas with a row for each scenario matched by RCP
#and at the same time save a table of the glacial indexs for each scenario

#load scenario specifications
Scenario_specs <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Predictor_data")

#create a df to capture results
Scenario_glacial_change <- data.frame(matrix(nrow = length(Scenario_names), ncol = ncol(Glacial_change)))
names(Scenario_glacial_change) <- c("Scenario", paste(Sim_time_steps))

#create directory for scenario specific indices
Glacial_scenario_dir <- "Data/Glacial_change/Scenario_indices"
dir.create(Glacial_scenario_dir)

#loop over scenario names filling df
for(i in 1:length(Scenario_names)){
  Scenario <- Scenario_names[i]
  Scenario_glacial_change[i,"Scenario"] <- Scenario
  Scenario_RCP <- Scenario_specs[Scenario_specs$Scenario_ID == Scenario, "Climate_RCP"]
  Scenario_glacial_change[i,2:ncol(Scenario_glacial_change)] <-  Glacial_change[Glacial_change$RCP == Scenario_RCP,2:ncol(Glacial_change)]

  #seperate glacial change index for scenario
  Scenario_index <- Glacier_indices[[Scenario_RCP]]
  
  #save scenario specific index
  saveRDS(Scenario_index, paste0(Glacial_scenario_dir, "/", Scenario, "_glacial_change.rds"))
  }

#save areal change across scenario's table
write.xlsx(Scenario_glacial_change, "Tools/Glacial_area_change.xlsx", row.names = FALSE)
