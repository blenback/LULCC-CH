
#read raster of simulation results
Sim_lulc <- raster("Results/Dinamica_simulated_LULC/BAU/v1/simulated_LULC_scenario_BAU_simID_v1_year_2025.tif")
initial_lulc <- raster("Results/Dinamica_simulated_LULC/BAU/v1/simulated_LULC_scenario_BAU_simID_v1_year_2020.tif")

freq(Sim_lulc)

lulc_dat_sim  <- raster::as.data.frame(Sim_lulc) 
lulc_dat_int  <- raster::as.data.frame(initial_lulc) 

#add ID column to dataset
lulc_dat_sim$ID <- seq.int(nrow(lulc_dat_sim))
lulc_dat_int$ID <- seq.int(nrow(lulc_dat_int))

#load scenario specific glacier index
Glacier_index <- readRDS("Data/Glacial_change/Scenario_indices/BAU_glacial_change.rds")

table(Glacier_index[["2025"]])

#check for glacier locations
Glacier_ncells_sim <- length(which(Glacier_index[["2025"]]==1))
Glacier_ncells_int<- length(which(Glacier_index[["2020"]]==1))
Glacier_cells_sim_lulc <- length(which(lulc_dat_sim$layer==19))
Glacier_cells_int_lulc <- length(which(lulc_dat_int$layer==19))


Glacier_ncells_int-Glacier_ncells_sim

#checking that non glacial areas match
table(Glacier_index[["2025"]])
non_glacier_IDs <- Glacier_index[Glacier_index[["2025"]] == 0, "ID_loc"]
glacier_IDs <- Glacier_index[Glacier_index[["2025"]] == 1, "ID_loc"]

non_glacier_cell_values <- lulc_dat_sim[lulc_dat_sim$ID %in% non_glacier_IDs, "layer"]
table(non_glacier_cell_values)

glacier_cell_values <- lulc_dat_sim[lulc_dat_sim$ID %in% glacier_IDs, "layer"]
table(glacier_cell_values)

#right now there are some cells in the simulated LULC map that should be non-glacier
#which are still glacier and vice versa some that should be glacier but are instead static
#so change is occurring in the wrong places and to the wrong amount... 

glacial_chg_rate <- 0.03279866397390088
ncells_change <- length(which(lulc_dat_int$layer==19))*glacial_chg_rate

ncells_diff <- length(which(lulc_dat_int$layer==19))-length(which(lulc_dat_sim$layer==19))

length(which(lulc_dat_int$layer==19))
length(which(lulc_dat_sim$layer==19))
