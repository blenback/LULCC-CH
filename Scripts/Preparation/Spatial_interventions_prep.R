#############################################################################
## Spatial_prob_perturb: Prepare spatial layers for perturbation of cellular
##transition probabilities in simulation steps
##
## Date: 18-11-2021
## Author: Ben Black
#############################################################################

#Vector packages for loading
# packs<-c("foreach", "data.table", "raster", "tidyverse", "testthat",
#          "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", "rgeos",
#          "sf", "tiff", "bfsMaps", "rjstat", "future.apply", "future", "stringr",
#          "stringi" ,"readxl","rlist", "rstatix", "openxlsx", "pxR", "rvest", "landscapemetrics")
# 
# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))
#
# Ref_grid <- raster(Ref_grid_path)
# Ref_crs <- crs(Ref_grid)
# 
# #need to reload the model tool vars because it has been updated
# #during the process of preparing the calibration allocation parameters
# Model_tool_vars <- readRDS("Tools/Model_tool_vars.rds")
# list2env(Model_tool_vars, .GlobalEnv)
#
# #load table of scenario specific spatial interventions
# #Spat_ints_path <- "E:/LULCC_CH_Ensemble/Tools/Spatial_interventions.csv"
# Interventions <- read.csv(Spat_ints_path)
# 
# #convert Time_step and Target_classes columns back to character vectors
# Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
#   x <- str_remove_all(x, " ")
#   rep <- unlist(strsplit(x, ","))
#   },simplify=FALSE)
#   
# Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
#   x <- str_remove_all(x, " ")
#   rep <- unlist(strsplit(x, ","))
#   },simplify=FALSE)

### =========================================================================
### A- Prepare building zone data
### =========================================================================

# #list URLs to be downloaded
# Data_URLs <- c("https://www.kgk-cgc.ch/download_file/1018/239", #building zones 2022
#                "https://www.kgk-cgc.ch/download_file/1019/239") #undeveloped areas in building zones 2022
# 
# #To Do: rewrite this codes as a function that searches for multiple file extensions for spatial data
# #create dir
# BZ_dir <- "Data/Spat_prob_perturb_layers/Bulding_zones"
# dir.create(BZ_dir, recursive = TRUE)
# 
# #download directly from website
# tmpdir <- tempdir()
# url <- "https://www.kgk-cgc.ch/download_file/1018/239.zip"
# file <- basename(url)
# download.file(url, file, mode = "wb")
# zip::unzip(zipfile = file, exdir = tmpdir)
# unlink(file)
# 
# gpkg <- list.files(tmpdir, pattern = ".gpkg", full.names = TRUE)
# 
# #load shapefile from geopackage
# shp_file <- sf::st_read(gpkg)
# 
# #re-project to research CRS
# shp_file <- sf::st_transform(shp_file, crs = Ref_crs)
# 
# #convert shapefile to raster
# BZ_rast <- rasterize(shp_file, Ref_grid, field= shp_file$CH_CODE_HN)
# #BZ_rast <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.gri")
# 
# #create a raster attribute table (RAT)
# BZ_rast <- ratify(BZ_rast)
# BZ_rat <- levels(BZ_rast)[[1]]
# 
# #get the german zone names
# BZ_IDs <- unique(shp_file$CH_BEZ_D)
# 
# #add name column to RAT
# BZ_rat$Class_Names <- str_replace_all(BZ_IDs, " ", "_")
# 
# #Convert names to english
# BZ_rat$Class_Names <- c("Residential zones","Mixed zones",                          
# "Zones_for_public_uses", "Restricted_building_zones",             
# "Work_zones","Centre_zones",                       
# "Other_Building_Zones", "Tourism_and_Recreation_Zones",        
# "Traffic_zones")
# 
# #add RAT to raster object
# levels(BZ_rast) <- BZ_rat
#  
# BZ_reclass_mat <- BZ_rat
# BZ_reclass_mat$Class_Names <- 1
# 
# #convert raster to binary values (0 or 1 and NA)
# BZ_reclass <- reclassify(BZ_rast, rcl = BZ_reclass_mat)
# 
# #saving the raster in R's native .grd format which preserves the attribute table
# writeRaster(BZ_rast, filename= "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster_all_classes.grd", overwrite = TRUE)
# writeRaster(BZ_reclass, filename = "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.grd", overwrite = TRUE)
# 
# #create a distance to building zones raster
# BZ_distance <- distance(BZ_rast)
# 
# #load in land use raster to mask distance raster
# LULC_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".grd")))
# Final_lulc <- raster(grep(paste(max(LULC_years)), list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".gri"), value = TRUE))
# 
# #convert all non-NA values to 1
# Final_lulc[!is.na(Final_lulc)] <- 1
# 
# #mask distance raster
# BZ_distance_masked <- mask(BZ_distance, Final_lulc)
# 
# #save
# writeRaster(BZ_distance_masked, filename = "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif", overwrite = TRUE)
# 
# #remove downloaded shape files
# unlink(list(file, tmpdir))

### =========================================================================
### B- Typology of municipalities
### =========================================================================

# #load municipalities shapefile downloaded in SA_var_prep.R
# Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
# 
# #filter out non-swiss municipalities
# Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]
# 
# #Import data of typology of municipalities from FSO web service using condition:
# #1. Municipality designations as of 01/05/2022 (to match mutations)
# 
# #scrape content from html address
# Muni_type_content <- read_html("https://www.agvchapp.bfs.admin.ch/de/typologies/results?SnapshotDate=01.05.2022&SelectedTypologies%5B0%5D=HR_GDETYP2012")
# muni_typology <- as.data.frame(html_table(Muni_type_content, fill = TRUE)[[1]][-1,]) #remove duplicate row names
# 
# #remove columns 7 and 8 which specify the other typologies (3 and 9 categories)
# #and rename remaining col
# muni_typology <- muni_typology[,-c(7,8)]
# colnames(muni_typology)[[7]] <- "muni_type_ID"
# muni_typology$`BFS-Gde Nummer` <- as.numeric(muni_typology$`BFS-Gde Nummer`)
# 
# #create manual legend (data documentation only specifies categories in German/French)
# Muni_typ_legend <- data.frame(ID = sort(unique(muni_typology$muni_type_ID)),
#                               type = c("City-center_large_agglomeration",
# "Urban_employment_municipality_large_agglomeration",
# "Residential_urban_municipality_large_agglomeration",
# "City-centre_medium_agglomeration",
# "Urban_employment_municipality_medium_agglomeration",
# "Residential_urban_municipality_medium_agglomeration",
# "Urban_tourist_municipality_of_a_small_agglomeration",
# "Industrial_urban_municipality_of_a_small_agglomeration",
# "Tertiary_urban_municipality_of_a_small_agglomeration",
# "High-density_industrial_peri-urban_municipality",
# "High-density_tertiary_peri-urban_municipality",
# "Mid-density_industrial_peri-urban_municipality",
# "Medium-density_tertiary_peri-urban_municipality",
# "Low-density_agricultural_peri-urban_municipality",
# "Low-density_industrial_peri-urban_municipality",
# "Low_density_tertiary_peri-urban_municipality",
# "Tourist_town_of_a_rural_center",
# "Industrial_municipality_of_a_rural_center",
# "Tertiary_municipality_of_a_rural_center",
# "Rural_agricultural_municipality_in_a_central_location",
# "Rural_industrial_municipality_in_a_central_location",
# "Tertiary_rural_municipality_in_a_central_location",
# "Peripheral_rural_tourist_municipality",
# "Peripheral_agricultural_rural_municipality",
# "Peripheral_mixed_rural_municipality"))
# 
# #add municipality type to data
# muni_typology$muni_type <- sapply(muni_typology$muni_type_ID, function(x){
#   Muni_typ_legend[Muni_typ_legend$ID == x, "type"]
# })
# 
# #add Muni_type_ID to shapefile
# Muni_shp@data$muni_type_ID <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
#   type <- muni_typology[muni_typology$`BFS-Gde Nummer` == Muni_num, "muni_type_ID"]
#   }, simplify = TRUE))
# 
# #rasterize
# Muni_type_rast <- rasterize(Muni_shp, Ref_grid, field = "muni_type_ID")
# 
# #link raster attribute table
# levels(Muni_type_rast) <- Muni_typ_legend
# 
# #save
# save_dir <- "Data/Spat_prob_perturb_layers/Municipality_typology/"
# dir.create(save_dir)
# raster::writeRaster(Muni_type_rast, filename = paste0(save_dir, "Muni_type_raster.grd"), overwrite = TRUE)
# 
# Muni_type_spatrast <- rast(Muni_type_rast)
# freq(Muni_type_spatrast)
# 
# # subset the raster to the municipality types of interest
# 
# # For the urban_migration and rural_migration interventions we use the remote rural municipalities
# rural_munis <- c(325, 326, 327, 335, 338)
# 
# # subset the raster to the rural municipalities
# Muni_rural_rast <- Muni_type_spatrast
# 
# # set all values that are not in rural_munis to NA
# Muni_rural_rast[!(values(Muni_rural_rast) %in% rural_munis)] <- NA
# Muni_rural_rast[values(Muni_rural_rast) %in% rural_munis] <- 1
# 
# # save
# terra::writeRaster(Muni_rural_rast, filename = paste0(save_dir, "Muni_rural_raster.tif"), overwrite = TRUE)
# 
# # For the mountain_delineation interventions we use the mountain municipalities
# mountain_munis <- c(314,334)
# 
# # subset the raster to the mountain municipalities
# Muni_mountain_rast <- Muni_type_spatrast
# # set all values that are not in mountain_munis to NA
# Muni_mountain_rast[!(values(Muni_mountain_rast) %in% mountain_munis)] <- NA
# Muni_mountain_rast[values(Muni_mountain_rast) %in% mountain_munis] <- 1
# # save
# terra::writeRaster(Muni_mountain_rast, filename = paste0(save_dir, "Muni_mountain_raster.tif"), overwrite = TRUE)

### =========================================================================
### C- Mountain areas
### =========================================================================

# #Import data of municipalities in mountainous areas from FSO web service using condition:
# #1. Municipality designations as of 01/05/2022 (to match mutations)
# 
# #scrape content from html address
# Mount_content <- read_html("https://www.agvchapp.bfs.admin.ch/de/typologies/results?SnapshotDate=01.05.2022&SelectedTypologies%5B0%5D=HR_MONT2019")
# muni_mountains <- as.data.frame(html_table(Mount_content, fill = TRUE)[[1]][-1,]) #remove duplicate row names
# 
# #rename column of interest
# colnames(muni_mountains)[[7]] <- "mountainous"
# muni_mountains$`BFS-Gde Nummer` <- as.numeric(muni_mountains$`BFS-Gde Nummer`)
# 
# #create attribute table: 0:non-mountain, 1:Mountainous
# Muni_mount_legend <- data.frame(ID = c(0,1),
#                               type = c("Non-mountainous", "Mountainous"))
# 
# #add Muni_type_ID to shapefile
# Muni_shp@data$mountainous <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
#   type <- muni_mountains[muni_typology$`BFS-Gde Nummer` == Muni_num, "mountainous"]
#   }, simplify = TRUE))
# 
# #rasterize
# Muni_mount_rast <- rasterize(Muni_shp, Ref_grid, field = "mountainous")
# 
# #link raster attribute table
# levels(Muni_mount_rast) <- Muni_mount_legend
# 
# #save
# save_dir <- "Data/Spat_prob_perturb_layers/Mountainous_municipalities/"
# dir.create(save_dir)
# raster::writeRaster(Muni_mount_rast, filename = paste0(save_dir, "Muni_mountainous_raster.grd"), overwrite = TRUE)

### =========================================================================
### D-  Representation of land marginality
### =========================================================================

# # For the interventions of Agri_maintenance, Agri_abandonment and Agri_expansion
# # we use a layer that represents the marginality of agricultural land based upon
# # the model used by Gellrich et al. 2007 that considers distance to roads, slope
# # and distance to building zones as a measure of 'marginality'.
# 
# #load the static predictor layers and re-scale between 0-1
# 
# #helper function for min-max rescaling:
# rescale <- function(x, x.min, x.max, new.min = 0, new.max = 1) {
#   if (is.null(x.min)) { x.min = min(x) }
#   if (is.null(x.max)) { x.max = max(x) }
#   new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
#   }
# 
# #distance to roads
# Dist2rds <- raster("Data/Preds/Prepared/Layers/Transport/Distance_to_roads_mean_100m.tif")
# Dist2rds <- calc(Dist2rds, function(x) rescale(x, x.min = minValue(Dist2rds),
#                                                        x.max = maxValue(Dist2rds)))
# 
# #Slope
# Slope <- raster("Data/Preds/Prepared/Layers/Topographic/Slope_mean_100m.tif")
# Slope <- calc(Slope, function(x) rescale(x, x.min = minValue(Slope),
#                                                  x.max = maxValue(Slope)))
# 
# #elevation
# Elev <- raster("Data/Preds/Prepared/Layers/Topographic/Elevation_mean_100m.tif")
# Elev <- calc(Elev, function(x) rescale(x, x.min = minValue(Elev),
#                                                x.max = maxValue(Elev)))
# 
# # Distance to building zones
# # This layer needs to be inverted when re-scaling because
# #greater distance from building zones means lower land cost
# #which means less likely to abandon hence x.min and x.max values swapped
# Dist2BZ <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif")
# Dist2BZ <- calc(Dist2BZ, function(x) rescale(x, x.min = maxValue(Dist2BZ),
#                                                      x.max = minValue(Dist2BZ)))
# 
# #stack dist2rds, slope and forest_dist layers and sum values as raster
# Marginality_rast <- calc(stack(Dist2rds, Slope, Elev, Dist2BZ), mean)
# 
# # save
# Save_dir <- "Data/Spat_prob_perturb_layers/Land_marginality"
# dir.create(Save_dir, recursive = TRUE)
# writeRaster(Marginality_rast, filename = file.path(Save_dir, "Marginality_raster.tif"))
# 
# # The final step for the agri_expansion intervention is to mask out the
# # Muni_rural_rast areas from the marginality raster to ensure that city centers
# # do not get converted to agricultural land
# Marginality_rast <- rast(file.path(Save_dir, "Marginality_raster.tif"))
# 
# # load the Muni_rural_rast raster
# Muni_rural_rast <- rast("Data/Spat_prob_perturb_layers/Municipality_typology/Muni_rural_raster.tif")
# 
# # mask the Marginality_rast raster to the Muni_rural_rast areas
# Marginality_rast_masked <- mask(Marginality_rast, Muni_rural_rast == 1)
# 
# # save the masked raster
# writeRaster(Marginality_rast_masked, filename = file.path(Save_dir, "Marginality_raster_urban_centers_masked.tif"), overwrite = TRUE)


### =========================================================================
### D-  Agricultural areas
### =========================================================================

# #path to Biodiversity promotion areas .gpkg file
# BPA_path <- "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/Agri_bio_areas.gpkg"
# 
# #get layer names
# BPA_layers <- st_layers(BPA_path)
# 
# #read in correct layer
# BPAs <- st_read(BPA_path,layer = BPA_layers$name[[1]], geometry_column = "wkb_geometry")
# 
# #remove entries with empty geometries
# BPAs <- BPAs[!st_is_empty(BPAs),,drop=FALSE]
# 
# #re-project to research CRS
# BPAs <- sf::st_transform(BPAs, crs = Ref_crs)
# 
# #convert to spat vector
# BPAs <- vect(BPAs)
# 
# #add ID col
# BPAs$ID <- seq(1:nrow(BPAs))
# 
# #check for invalid polygons (i.e. holes)
# polys_invalid <- any(is.valid(BPAs, messages=FALSE, as.points=FALSE)==FALSE)
# 
# #if invalid polygons then makeValid
# if(polys_invalid == TRUE){
# BPAs <- makeValid(BPAs)
# }
# 
# #save shapefile
# writeVector(BPAs, "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPAs.shp", overwrite=TRUE)
# 
# #rasterize using the most recent LULC layer as a mask
# Mask_rast <- rast("E:/LULCC_CH/Data/Historic_LULC/LULC_2018_agg.grd")
# BPA_raster <- terra::mask(Mask_rast, BPAs)
# 
# #change non-NA values to 1
# BPA_raster <- ifel(!is.na(BPA_raster), 1, NA)
# writeRaster(BPA_raster, "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPA_raster.tif")

### =========================================================================
### E - Prepare Protected area maps
### =========================================================================

  # ------------------------------------------------------------------------
  # E.1 Data preparation
  # -------------------------------------------------------------------------

#create CRS object
ProjCH <- "+proj=somerc +init=epsg:2056"

#terra won't read rasters from '.gri' extension only '.grd' update path
Ref_grid_path <- str_replace(Ref_grid_path, "\\.gri", "\\.grd")

#re-load ref_grid as terra::rast 
Ref_grid <- rast(Ref_grid_path)

# #vector dir of raw data
CA_raw_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/raw_data"
# 
# #create dir for intermediate data layers produced
# EI_int_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Int_data"
# dir.create(EI_int_dir, recursive = TRUE)
# 
# New_EI_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Hypothetical_PAs"
# dir.create(New_EI_dir, recursive = TRUE)
# 
# EI_final_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Simulation_PAs"
# dir.create(EI_final_dir, recursive = TRUE)


# load the pre-prepared shapefile that contains the various types of PAs including:
# Ramsar
# Swiss_National_Park
# Unesco_BiosphereReserve
# Unesco_NaturalSites
# ProNatura_reserves
# Emeraude
# Amphibian_spawning
# Bird_reserves
# PA_cantons
# Dry_pastures
# Dry_pastures_appendix2
# Fens
# Floodplains
# Floodplains_appendix2
# Forest_reserves
# Hunting_areas
# Raised_bog
# Biodiversity_promotion_areas
# CA_vect <- vect("Data/Spat_prob_perturb_layers/Protected_areas/raw_data/PA_SWISS.shp")
# 
# # Load the layer of Biodiversity promotion Areas on Agricultural land
# BPA_vect <- vect("Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPAs.shp")
# 
# # add a column of Res_Type to the Biodiversity promotion areas vector
# # to match the naming column in the Protected Areas vector
# BPA_vect$Res_Type <- "Biodiversity_promotion_areas"
# 
# # append the Biodiversity promotion areas to the Protected Areas vector
# CA_vect <- vect(c(CA_vect, BPA_vect))
# 
# # get the unique types of protected areas
# CA_types <- unique(CA_vect$Res_Type)
# 
# # replace any whitespaces with '_'
# CA_types <- str_replace_all(CA_types, " ", "_")
# 
# # update the Res_Type column in the Protected Areas vector
# CA_vect$Res_Type <- str_replace_all(CA_vect$Res_Type, " ", "_")
# 
# # save the CA_vect
# writeVector(CA_vect, file.path(CA_raw_dir, "CAs_complete.shp"))



#-------------------------------------------------------------------------
# Preparing maps of prioritization for conservation
#-------------------------------------------------------------------------

   #-------------------------------------------------------------------------
   # Biodiversity prioritization map
   #-------------------------------------------------------------------------

    #load biodiversity prioritization map
    Bio_prio <- rast(paste0(CA_raw_dir, "/Bio_prio.tif"))
    crs(Bio_prio) <- ProjCH

   #-------------------------------------------------------------------------
   # NCP prioritization map
   #-------------------------------------------------------------------------
 
   #load NCP prioritization map
   NCP_prio <- rast(paste0(CA_raw_dir, "/NCP_prio.tif"))
   crs(NCP_prio) <- ProjCH
   ext(NCP_prio) <- ext(Ref_grid)
   NCP_prio <- terra::resample(NCP_prio, Ref_grid, method= "bilinear")
 
   #-------------------------------------------------------------------------
   #  Cultural prioritization
   #-------------------------------------------------------------------------
 
   #This is the exact same approach as with the biodiversity,
   #except that the proximity to BLN areas is considered
 
   # load BLN areas (for cultural landscapes)
   BLN <- vect(paste0(CA_raw_dir, "/N2017_Revision_landschaftnaturdenkmal_20170727_20221110.shp"))
   crs(BLN) <- ProjCH
 
   buffer <- terra::buffer(BLN, width = 2830)
 
   #-------------------------------------------------------------------------
   # Agricultural unproductive land prioritization
   #-------------------------------------------------------------------------
 
   # Reload the land marginality ratser from above
   Marginality_rast <- rast("Data/Spat_prob_perturb_layers/Land_marginality/Marginality_raster.tif")
 
   # reproject to ProjCH CRS
   Marginality_rast <- terra::project(Marginality_rast, ProjCH)
   
   # save the Marginality raster
   writeRaster(Marginality_rast, filename = "Data/Spat_prob_perturb_layers/Land_marginality/Marginality_raster.tif", overwrite = TRUE)
   
   #subset the marginality raster to only the pixels of the agricultural
   #land types (Int_AG, Alp_Past)
 
   #load the land use raster in the year closest the simulation start date
   Sim_start <- min(as.numeric(Sim_control_temp$Scenario_start.real))
   Sim_start <- 2020
 
   #load in land use raster to mask distance raster
   LULC_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".grd")))
 
   #identify which LULC year is closest to Sim_start
   LULC_year <- LULC_years[order(abs(LULC_years - Sim_start))][1]
 
   #load rast
   LULC <- rast(grep(paste(LULC_year), list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".grd"), value = TRUE))
 
   #seperate only the pixels of the classes 16,17,18
   Agri_rast <- LULC %in% c(14,16,17,18)
   Agri_rast[Agri_rast == 0] <- NA
 
   #mask the marginality raster with the agricultural land pixels
   Agri_prio <- mask(Marginality_rast, Agri_rast)


#-------------------------------------------------------------------------
# Subsetting PAs for each scenario
#-------------------------------------------------------------------------

# in LULC replace all values that are not NA with 1
LULC[LULC > 0] <- 1
# project to the ProjCH CRS
PA_mask <- terra::project(LULC, ProjCH)

# save as a raster called PA_mask.tif
writeRaster(PA_mask, filename = file.path(CA_raw_dir, "PA_mask.tif"), overwrite = TRUE)
   
   
# Load shapeile of all current CAs 
CA_vect <- vect(file.path(CA_raw_dir, "CAs_complete.shp"))
   
# create a list of the mask vectors
expansion_mask_paths <- c("Data/Spat_prob_perturb_layers/Protected_areas/raw_data/Railways_mask.shp",
                          "Data/Spat_prob_perturb_layers/Protected_areas/raw_data/Roads_mask.shp",
                          "Data/Spat_prob_perturb_layers/Protected_areas/raw_data/Settlements_mask.shp"
                          )
 
# Apply function to identify and prepare masks for conservation interventions under each scenario 
prepare_conservation_intervention_masks(
    Scenario_names = c("SSP0", "SSP3", "SSP4", "SSP5"),
    interventions_dir = "Tools",
    CA_vect = CA_vect,
    Proj = ProjCH,
    CA_mask_path = "Data/Spat_prob_perturb_layers/Protected_areas/raw_data/PA_mask.tif",
    intervention_mask_dir = "Data/Spat_prob_perturb_layers/Protected_areas/Simulation_PAs",
    expansion_mask_paths = expansion_mask_paths,
    Ref_grid = Ref_grid,
    Use_parallel = TRUE,
    n_cores = 4,
    Save_dir = "Data/Spat_prob_perturb_layers/Protected_areas",
    Recalc_results = FALSE
)







