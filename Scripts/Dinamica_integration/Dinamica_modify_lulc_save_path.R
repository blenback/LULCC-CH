#############################################################################
## Dinamica_modify_lulc_save_path: Finalize save path for simulated LULC map
## according to time step
## Date: 10-10-2022
## Author: Ben Black
#############################################################################

#receive basic path from Dinamica (produced by script Dinamica_initialize)
LULC_base_path <- s1
#LULC_base_path <- "E:/LULCC_CH/Results/Dinamica_simulated_LULC/BAU/v2/simulated_LULC_scenario_BAU_simID_v2_year_"

#receive time step
Time_step <- v1
#Time_step <- 2020
Step_length <- v2 

#add step length to current time step to get simulation year
Simulated_lulc_year <-  paste((Time_step + Step_length))

#paste together and include '.tif' as the file type
Final_lULC_path <- paste0(LULC_base_path, Simulated_lulc_year, ".tif")

#Output the path
outputString("simulated_lulc_save_path", Final_lULC_path)

#output the simulated map year for use in the implementation of deterministic transitions
outputString("Simulated_LULC_year", Simulated_lulc_year)
