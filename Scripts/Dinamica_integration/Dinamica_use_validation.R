#############################################################################
## Dinamica_use_validation: Detrmine condition for 'if else' function to perform
## validation on simulation results  
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Determine if validation is required
### =========================================================================

# setwd(wpath)

#If model mode is simulation then return '0' as validation is not required
#else if calibration ,then validation is required so return '1' 
#and generate file paths for saving validation results

#vector folder path for validation results
Val_res_folder <- paste0(wpath, "/Results/Validation/", Sim_ID)
Val_res_path <- paste0(Val_res_folder, "/", paste("Simulation", Sim_ID, "from", Simulation_time_steps[1, "Keys"], "to", Simulation_time_steps[nrow(Simulation_time_steps), "Values"], sep = "_"))

if(grepl("simulation", Model_mode, ignore.case = TRUE)){
validation_condition <- 0
Validation_map_path <- "NA"
Validation_result_path <- "NA"
} else  {
validation_condition <- 1

#vector file paths for results
dir.create(Val_res_folder, recursive = TRUE)

#adjust to file path
Validation_map_path <- paste0(Val_res_path, "_map.tif")
Validation_result_path <- paste0(Val_res_path, "_similarity_value.csv") 
} 


### =========================================================================
### B- Identify file path for relevant year Observed LULC
### =========================================================================

#gather file path for observed LULC year that is closest to that of the final simulation year
Obs_LULC_paths <- list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".grd")

#extract numerics
Obs_LULC_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", Obs_LULC_paths))

#identify closest LULC year
desired_lulc_year <- which.min(abs(Obs_LULC_years - Simulation_time_steps[nrow(Simulation_time_steps), "Keys"]))

#subset to correct LULC path
Final_LULC_path <- Obs_LULC_paths[desired_lulc_year]


### =========================================================================
### C- Identify file path for final simulation year Simulated LULC
### =========================================================================

#alter file path for simulated LULC map for final simulation year
Sim_final_LULC_path <- paste0(Sim_LULC_path_gen, Simulation_time_steps[nrow(Simulation_time_steps), "Values"], ".tif")



