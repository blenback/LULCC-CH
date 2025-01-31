#############################################################################
## Dinamica_modify_lulc_save_path: Finalize save path for simulated LULC map
## according to time step
## Date: 10-10-2022
## Author: Ben Black
#############################################################################

#add step length to current time step to get simulation year
Simulated_lulc_year <- paste((Time_step + Step_length))

#paste together and include '.tif' as the file type
Final_lULC_path <- paste0(LULC_base_path, Simulated_lulc_year, ".tif")


