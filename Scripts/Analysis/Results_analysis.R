#############################################################################
## Results_analysis: wrangling/visualisation of simulated LULC maps
##
## Date: 25-10-2022
## Author: Ben Black
#############################################################################

#get file paths of end of simulation rasters
Final_raster_paths <- list.files("Results/Dinamica_simulated_lulc",
           full.names = TRUE,
           recursive = TRUE,
           pattern = paste(Scenario_end))

#remove tif.aux files
Final_raster_paths <- Final_raster_paths[!grepl(".aux", Final_raster_paths)]

#load as stack
Final_lulc_stack <- stack(Final_raster_paths)
names(Final_lulc_stack) <- str_match(Final_raster_paths, paste0(Scenario_names, collapse = "|"))

#get frequency tables
Rast_freq <- freq(Final_lulc_stack, merge=TRUE)

#remove row for NA
Rast_freq <- Rast_freq[1:(nrow(Rast_freq)-1),]

#add column for class names
LULC_agg_scheme <- readxl::read_excel(LULC_aggregation_path)

Rast_freq$LULC_name <- sapply(Rast_freq$value, function(x){
  unique(LULC_agg_scheme[LULC_agg_scheme$Aggregated_ID == x, "Class_abbreviation"])})

#convert cell counts to %'s
Total_area <- sum(Rast_freq$BAU)

LULC_percs <- Rast_freq
LULC_percs[,Scenario_names] <- apply(LULC_percs[,Scenario_names], c(1,2), function(x){(x/Total_area)*100})

#check expected glacier amount in 2060
Glacier_index <- readRDS("Data/Glacial_change/Scenario_indices/GR_EX_glacial_change.rds")
Glacier_ncells_2060 <- length(which(Glacier_index[["2060"]]==1))


