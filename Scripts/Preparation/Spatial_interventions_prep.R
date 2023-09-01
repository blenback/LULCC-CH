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

Ref_grid <- raster(Ref_grid_path)
Ref_crs <- crs(Ref_grid)

#need to reload the model tool vars because it has been updated
#during the process of preparing the calibration allocation parameters
Model_tool_vars <- readRDS("Tools/Model_tool_vars.rds")
list2env(Model_tool_vars, .GlobalEnv)

#load table of scenario interventions
Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")

#convert Time_step and Target_classes columns back to character vectors
Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
  x <- str_remove_all(x, " ")
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)
  
Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
  x <- str_remove_all(x, " ")
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)

### =========================================================================
### A- Prepare building zone data
### =========================================================================

#list URLs to be downloaded
Data_URLs <- c("https://www.kgk-cgc.ch/download_file/1018/239", #building zones 2022
               "https://www.kgk-cgc.ch/download_file/1019/239") #undeveloped areas in building zones 2022

#To Do: rewrite this codes as a function that searches for multiple file extensions for spatial data
#create dir
BZ_dir <- "Data/Spat_prob_perturb_layers/Bulding_zones"
dir.create(BZ_dir, recursive = TRUE)

#download directly from website
tmpdir <- tempdir()
url <- "https://www.kgk-cgc.ch/download_file/1018/239.zip"
file <- basename(url)
download.file(url, file, mode = "wb")
zip::unzip(zipfile = file, exdir = tmpdir)
unlink(file)

gpkg <- list.files(tmpdir, pattern = ".gpkg", full.names = TRUE)

#load shapefile from geopackage
shp_file <- sf::st_read(gpkg)

#re-project to research CRS
shp_file <- sf::st_transform(shp_file, crs = Ref_crs)

#convert shapefile to raster
BZ_rast <- rasterize(shp_file, Ref_grid, field= shp_file$CH_CODE_HN)
#BZ_rast <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.gri")

#create a raster attribute table (RAT)
BZ_rast <- ratify(BZ_rast)
BZ_rat <- levels(BZ_rast)[[1]]

#get the german zone names
BZ_IDs <- unique(shp_file$CH_BEZ_D)

#add name column to RAT
BZ_rat$Class_Names <- str_replace_all(BZ_IDs, " ", "_")

#Convert names to english
BZ_rat$Class_Names <- c("Residential zones","Mixed zones",                          
"Zones_for_public_uses", "Restricted_building_zones",             
"Work_zones","Centre_zones",                       
"Other_Building_Zones", "Tourism_and_Recreation_Zones",        
"Traffic_zones")

#add RAT to raster object
levels(BZ_rast) <- BZ_rat
 
BZ_reclass_mat <- BZ_rat
BZ_reclass_mat$Class_Names <- 1

#convert raster to binary values (0 or 1 and NA)
BZ_reclass <- reclassify(BZ_rast, rcl = BZ_reclass_mat)

#saving the raster in R's native .grd format which preserves the attribute table
writeRaster(BZ_rast, filename= "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster_all_classes.grd", overwrite = TRUE)
writeRaster(BZ_reclass, filename = "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.grd", overwrite = TRUE)

#create a distance to building zones raster
BZ_distance <- distance(BZ_rast)

#load in land use raster to mask distance raster
LULC_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".grd")))
Final_lulc <- raster(grep(paste(max(LULC_years)), list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".gri"), value = TRUE))

#convert all non-NA values to 1
Final_lulc[!is.na(Final_lulc)] <- 1

#mask distance raster
BZ_distance_masked <- mask(BZ_distance, Final_lulc)

#save
writeRaster(BZ_distance_masked, filename = "Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif", overwrite = TRUE)

#remove downloaded shape files
unlink(list(file, tmpdir))

### =========================================================================
### B- Typology of municipalities
### =========================================================================

#load municipalities shapefile downloaded in SA_var_prep.R
Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]

#Import data of typology of municipalities from FSO web service using condition:
#1. Municipality designations as of 01/05/2022 (to match mutations)

#scrape content from html address
Muni_type_content <- read_html("https://www.agvchapp.bfs.admin.ch/de/typologies/results?SnapshotDate=01.05.2022&SelectedTypologies%5B0%5D=HR_GDETYP2012")
muni_typology <- as.data.frame(html_table(Muni_type_content, fill = TRUE)[[1]][-1,]) #remove duplicate row names

#remove columns 7 and 8 which specify the other typologies (3 and 9 categories)
#and rename remaining col
muni_typology <- muni_typology[,-c(7,8)]
colnames(muni_typology)[[7]] <- "muni_type_ID"
muni_typology$`BFS-Gde Nummer` <- as.numeric(muni_typology$`BFS-Gde Nummer`)

#create manual legend (data documentation only specifies categories in German/French)
Muni_typ_legend <- data.frame(ID = sort(unique(muni_typology$muni_type_ID)),
                              type = c("City-center_large_agglomeration",
"Urban_employment_municipality_large_agglomeration",
"Residential_urban_municipality_large_agglomeration",
"City-centre_medium_agglomeration",
"Urban_employment_municipality_medium_agglomeration",
"Residential_urban_municipality_medium_agglomeration",
"Urban_tourist_municipality_of_a_small_agglomeration",
"Industrial_urban_municipality_of_a_small_agglomeration",
"Tertiary_urban_municipality_of_a_small_agglomeration",
"High-density_industrial_peri-urban_municipality",
"High-density_tertiary_peri-urban_municipality",
"Mid-density_industrial_peri-urban_municipality",
"Medium-density_tertiary_peri-urban_municipality",
"Low-density_agricultural_peri-urban_municipality",
"Low-density_industrial_peri-urban_municipality",
"Low_density_tertiary_peri-urban_municipality",
"Tourist_town_of_a_rural_center",
"Industrial_municipality_of_a_rural_center",
"Tertiary_municipality_of_a_rural_center",
"Rural_agricultural_municipality_in_a_central_location",
"Rural_industrial_municipality_in_a_central_location",
"Tertiary_rural_municipality_in_a_central_location",
"Peripheral_rural_tourist_municipality",
"Peripheral_agricultural_rural_municipality",
"Peripheral_mixed_rural_municipality"))

#add municipality type to data
muni_typology$muni_type <- sapply(muni_typology$muni_type_ID, function(x){
  Muni_typ_legend[Muni_typ_legend$ID == x, "type"]
})

#add Muni_type_ID to shapefile
Muni_shp@data$muni_type_ID <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
  type <- muni_typology[muni_typology$`BFS-Gde Nummer` == Muni_num, "muni_type_ID"]
  }, simplify = TRUE))

#rasterize
Muni_type_rast <- rasterize(Muni_shp, Ref_grid, field = "muni_type_ID")

#link raster attribute table
levels(Muni_type_rast) <- Muni_typ_legend

#save
save_dir <- "Data/Spat_prob_perturb_layers/Municipality_typology/"
dir.create(save_dir)
raster::writeRaster(Muni_type_rast, filename = paste0(save_dir, "Muni_type_raster.grd"), overwrite = TRUE)


### =========================================================================
### C- Mountain areas
### =========================================================================

#Import data of municipalities in mountainous areas from FSO web service using condition:
#1. Municipality designations as of 01/05/2022 (to match mutations)

#scrape content from html address
Mount_content <- read_html("https://www.agvchapp.bfs.admin.ch/de/typologies/results?SnapshotDate=01.05.2022&SelectedTypologies%5B0%5D=HR_MONT2019")
muni_mountains <- as.data.frame(html_table(Mount_content, fill = TRUE)[[1]][-1,]) #remove duplicate row names

#rename column of interest
colnames(muni_mountains)[[7]] <- "mountainous"
muni_mountains$`BFS-Gde Nummer` <- as.numeric(muni_mountains$`BFS-Gde Nummer`)

#create attribute table: 0:non-mountain, 1:Mountainous
Muni_mount_legend <- data.frame(ID = c(0,1),
                              type = c("Non-mountainous", "Mountainous"))

#add Muni_type_ID to shapefile
Muni_shp@data$mountainous <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
  type <- muni_mountains[muni_typology$`BFS-Gde Nummer` == Muni_num, "mountainous"]
  }, simplify = TRUE))

#rasterize
Muni_mount_rast <- rasterize(Muni_shp, Ref_grid, field = "mountainous")

#link raster attribute table
levels(Muni_mount_rast) <- Muni_mount_legend

#save
save_dir <- "Data/Spat_prob_perturb_layers/Mountainous_municipalities/"
dir.create(save_dir)
raster::writeRaster(Muni_mount_rast, filename = paste0(save_dir, "Muni_mountainous_raster.grd"), overwrite = TRUE)

### =========================================================================
### D-  Agricultural areas
### =========================================================================

#path to Biodiversity promotion areas .gpkg file
BPA_path <- "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/Agri_bio_areas.gpkg"

#get layer names
BPA_layers <- st_layers(BPA_path)

#read in correct layer
BPAs <- st_read(BPA_path,layer = BPA_layers$name[[1]], geometry_column = "wkb_geometry")

#remove entries with empty geometries
BPAs <- BPAs[!st_is_empty(BPAs),,drop=FALSE]

#re-project to research CRS
BPAs <- sf::st_transform(BPAs, crs = Ref_crs)

#convert to spat vector
BPAs <- vect(BPAs)

#add ID col
BPAs$ID <- seq(1:nrow(BPAs))

#check for invalid polygons (i.e. holes)
polys_invalid <- any(is.valid(BPAs, messages=FALSE, as.points=FALSE)==FALSE)

#if invalid polygons then makeValid
if(polys_invalid == TRUE){
BPAs <- makeValid(BPAs)
}

#save shapefile
writeVector(BPAs, "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPAs.shp", overwrite=TRUE)

#rasterize using the most recent LULC layer as a mask
Mask_rast <- rast("E:/LULCC_CH/Data/Historic_LULC/LULC_2018_agg.grd")
BPA_raster <- terra::mask(Mask_rast, BPAs)

#change non-NA values to 1
BPA_raster <- ifel(!is.na(BPA_raster), 1, NA)
writeRaster(BPA_raster, "Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPA_raster.tif")



### =========================================================================
### E- Protected areas
### =========================================================================

  #-------------------------------------------------------------------------
  # E.1 Data preparation
  #-------------------------------------------------------------------------

#create CRS object
ProjCH <- "+proj=somerc +init=epsg:2056"
#Ref_grid_path <- ("Data/Ref_grid.grd")

#terra won't read rasters from '.gri' extension only '.grd' update path
Ref_grid_path <- str_replace(Ref_grid_path, "\\.gri", "\\.grd")

#re-load ref_grid as terra::rast 
Ref_grid <- rast(Ref_grid_path)

#vector dir of raw data
PA_raw_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/raw_data"

#create dir for intermediate data layers produced
PA_int_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Int_data"
dir.create(PA_int_dir)

New_PA_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/New_PAs"
dir.create(New_PA_dir)

PA_final_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Future_PAs"
dir.create(PA_final_dir)

#load PAs shapefile of SwissPAs layer compiled by Louis-Rey
PA <- vect(paste0(PA_raw_dir, "/SwissPA.shp"))

#Load cantonal PA layer provided by BAFU (cleaned by us)
PA_cantons <- vect(paste0(PA_raw_dir, "/PA_cantons.shp"))

#load the vector land cover data from Swiss TLM regio to identify settlement areas
LC <- vect(paste0(PA_raw_dir, "/swissTLMRegio_LandCover.shp"))
settlement <- subset(LC, LC$OBJVAL == "Siedl")
crs(settlement) <- ProjCH

#load Swiss TLM region roads and railways layers to exclude
road <- vect(paste0(PA_raw_dir, "/swissTLMRegio_Road.shp"))
crs(road) <- ProjCH
road <- subset(road, road$CONSTRUCT == 'Keine Kunstbaute')

railway <- vect(paste0(PA_raw_dir, "/swissTLMRegio_Railway.shp"))
railway <- subset(railway, railway$CONSTRUCT == 'Keine Kunstbaute')
crs(railway) <- ProjCH

# load BLN areas (for cultural landscapes)
BLN <- vect(paste0(PA_raw_dir, "/N2017_Revision_landschaftnaturdenkmal_20170727_20221110.shp"))
crs(BLN) <- ProjCH

#load biodiversity prioritization map
Biodiv_prio <- rast(paste0(PA_raw_dir, "/Bio_prio.tif"))
crs(Biodiv_prio) <- ProjCH

#load NCP prioritization map
NCP_prio <- rast(paste0(PA_raw_dir, "/NCP_prio.tif"))
crs(NCP_prio) <- ProjCH
ext(NCP_prio) <- ext(Ref_grid)
NCP_prio <- terra::resample(NCP_prio, Ref_grid, method= "bilinear")

  #-------------------------------------------------------------------------
  # E.2 Spatially identify PAs included in national targets
  #-------------------------------------------------------------------------

#Subset the PAs data to only the types supposedly included in the calculation of
#the national coverage estimates 
subset_rows <- PA$Res_Type %in% c("Ramsar",
                                  "Swiss National Park",
                                  "Unesco_BiosphereReserve",
                                  "Unesco_CulturalSites",
                                  "Unesco_NaturalSites",
                                  "ProNatura reserves",
                                  "Emeraude")
PA_BAFU <- PA[subset_rows, ]
PA_BAFU <- terra::project(PA_BAFU, ProjCH)
PA_BAFU_df <- as.data.frame(PA_BAFU)
writeVector(PA_BAFU, paste0(PA_int_dir, "/PA_BAFU.shp"), overwrite=TRUE)

#merge PAs from BAFU with the cantonal PA provided by FOEN, name PA_BAFU is kept
#in order not to change all the dependencies below
PA_BAFU <- rbind(PA_BAFU, PA_cantons)
#writeVector(PA_BAFU, "Data/Spat_prob_perturb_layers/Protected_areas/PA_SWISS.shp", overwrite=TRUE)

#check for invalid polygons (i.e. holes)
polys_invalid <- any(is.valid(PA_BAFU, messages=FALSE, as.points=FALSE)== FALSE)

#if invalid polygons then makeValid
if(polys_invalid == TRUE){
PA_BAFU <- makeValid(PA_BAFU)
}

#Rasterize the combined BAFU and cantonal PAs
#Preferred approach with mask() which results in same num of PA cells
#as the original approach but keeps the ncells and extent consistent
PA_BAFU_raster <- mask(Biodiv_prio, PA_BAFU) 
# global(PA_BAFU_raster, fun="notNA")
# global(PA_BAFU_raster, fun="isNA")
# ncell(PA_BAFU_raster)
# plot(PA_BAFU_raster)
# ext(PA_BAFU_raster)

#change non-NA values to 1
PA_BAFU_raster <- ifel(!is.na(PA_BAFU_raster), 1, 0)
#writeRaster(PA_BAFU_raster,paste0(PA_int_dir, "/PA_BAFU_raster.tif"), overwrite=TRUE)
#PA_BAFU_raster <- rast(paste0(PA_int_dir, "/PA_BAFU_raster.tif"))

#combine PA raster with raster of Biodiversity promotion areas
BPA_raster <- rast("Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPA_raster.tif")
BPA_raster <- ifel(!is.na(BPA_raster), 1, 0)
PA_total_rast <- PA_BAFU_raster+BPA_raster 

#addition results in 2's for overlap and 1 for non-overlapping BPAs
#convert all values greater than 0 to 1 and the rest back to NA
PA_total_rast <- ifel(PA_total_rast == 0, NA, 1)
#global(PA_total_rast, fun="notNA")
#global(PA_total_rast, fun="isNA")
#confirming that the correct number of PA cells have been changed to NA
#NA_confirm should match number of non NA cells in PA_total_raster
#NA_confirm = global(Biodiv_prio, fun="isNA")-global(Biodiv_prio_wo_pa, fun="isNA")
writeRaster(PA_total_rast, paste0(PA_int_dir, "/PA_combined.tif"), overwrite = TRUE)
#PA_total_rast <- rast(paste0(PA_int_dir, "/PA_combined.tif"))

  #-------------------------------------------------------------------------
  # E.3 Calculate current PA areal coverage
  #-------------------------------------------------------------------------

#The BAFU give a figure of 12% PA coverage but this is very instransparent and
#should only be considered as an approximation.

#Two components required each with possibilities to calculate differently:
# 1. Area of Switzerland: raw area vs. raster area according to our grid/CRS 
# 2. Area of PAs: Areas from rasters or polygonal areas

#1. Area of Switzerland:
area_ch_raw = 41285*1000000 

#use area of biodiversity prioritization raster as it is binary and projected
#to our CRS and extent
area_ch_raster <- expanse(Biodiv_prio, unit="m")

#2. Area of PAs:
#if we use the area of the rasterized layer of PAs we will overestimate current
#coverage because many BPAs for example only occupy portions of 100m cells 
#hence we should calculate areas from the Spatvectors

#For the BPAs this is easy as there is an area attribute with values for all polygons
#but for the PA shapefile there is several incomplete area columns so we will
#need to estimate from the polygons however some are overlapping so first 
#we need to aggregate to non-overlapping areas only 

#2.1 calculate discrepancy between area of BPAs from polygons vs. BPA raster cells
#that do not overlap with other PAs
#set 0's back to NA in BPA and PA_BAFU rasters
BPA_raster <- ifel(BPA_raster == 0, NA, 1)
PA_BAFU_raster <- ifel(PA_BAFU_raster==0, NA, 1)

#Identify BPA cells not in PAs (inverse masking)
Non_PA_BPAs_rast <- mask(BPA_raster, PA_BAFU_raster, inverse = TRUE) 
writeRaster(Non_PA_BPAs_rast,paste0(PA_int_dir, "/Non_PA_BPA_raster.tif"), overwrite=TRUE)

#load shp file of BPAs as Spatvector
BPAs <- vect("Data/Spat_prob_perturb_layers/Agriculture_bio_areas/BPAs.shp")

#filter Spatvector of BPAs by first intersecting with the BAFU PAs then subsetting
#the Spatvector by the intersecting polygons it would be faster to use
#terra::crop but it is throwing an error: "TopologyException: Input geom 1 is invalid" 
#I have check geometry validity using terra::is.valid and apparently all are valid?
#note: intersecting splits polygons meaning that there are rows with non-unique IDs
intersecting_BPAs <- terra::intersect(BPAs, PA_BAFU)
Non_PA_BPAs <- BPAs[which(!BPAs$ID %in% unique(intersecting_BPAs$ID)), ]
writeVector(Non_PA_BPAs, paste0(PA_int_dir, "/Non_PA_BPA.shp"), overwrite=TRUE)
Non_PA_BPAs <- vect("Data/Spat_prob_perturb_layers/Protected_areas/Int_data/Non_PA_BPA.shp")

#calculate area of BPA cells not in PAs
cell_area_BPA <- expanse(Non_PA_BPAs, unit="m")

#sum up areas of remaining BPA polygons
poly_area_BPA <- sum(Non_PA_BPAs$flaeche_m2)

#cell area minus polygon area equates to the areal overestimation
BPA_area_overestimate <- cell_area_BPA-poly_area_BPA

#2.2 Out of interest, calculate discrepancy between area of PA polygons minus
#the overlapping BPA areas and the raster cell total area

#combine overlapping polygons
PA_agg <- terra::aggregate(PA_BAFU, dissolve = TRUE)
writeVector(PA_agg, paste0(PA_int_dir, "/PA_agg.shp"), overwrite=TRUE)
PA_agg <- vect(paste0(PA_int_dir, "/PA_agg.shp"))

#calc areas of remaining polygons
PA_poly_area <- expanse(PA_agg)

#calc raster cell area
PA_cell_area <- expanse(PA_BAFU_raster)

#calculate overestimation of raster
PA_area_overestimate <- PA_cell_area-PA_poly_area

#3 calculate total PA coverage using the raw vs. raster areas of Switzerland
#and the raster vs. polygonal areas of PAs

#directly calculate area of protected cells and subtract the overestimation of BPAs
area_prot_raster <- expanse(PA_total_rast, unit="m")
area_prot_poly <- PA_poly_area+poly_area_BPA

#current coverage estimate from polygons vs. raster under the raster CH area
cover_raster <- area_prot_raster/area_ch_raster #28.8%
cover_poly <- area_prot_poly/area_ch_raster #18.3%

#under the raw CH area
cover_raster_raw <- area_prot_raster/area_ch_raw #28.1%
cover_poly_raw <- area_prot_poly/area_ch_raw #17.8%
cover_poly_raw <- 0.178721

  #-------------------------------------------------------------------------
  # E.4 Calculate additional PA areal coverage required under scenarios
  #-------------------------------------------------------------------------

#vector of percent of total area of Switzerland that should be protected by 2060
#under each scenario
perc_goals <- c(0.22,0.25,0.30)
names(perc_goals) <- c("EI_SOC", "EI_CUL", "EI_NAT")


#The raw CH area produces the estimated coverage that is closest to that 
#reported by the BAFU hence lets use that
#additional % area of switzerland required to meet goal
perc_todo <- perc_goals-cover_poly_raw
 
#number of additional cells to protect in order to meet goal
n_cells <- ceiling(perc_todo*area_ch_raw/prod(res(Biodiv_prio))) 

  #-------------------------------------------------------------------------
  # E.5 Identify locations for new PAs based on biodiversity prioritization map
  #-------------------------------------------------------------------------

#mask Biodiv_prio map so that values inside PAs are 0
Biodiv_prio_wo_pa <- mask(Biodiv_prio, PA_total_rast, maskvalue=1)
#writeRaster(Biodiv_prio_wo_pa,file=paste0(getwd(),"/Prio_without_PA.tif"), overwrite=TRUE)

#Exclude urban areas/roads and railways
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, settlement, inverse=TRUE)
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, road, inverse=TRUE)
Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, railway, inverse=TRUE)

#two approaches to trial for patch identification
#1. Simple approach: terra::patches, Detect patches (clumps) i.e. groups of 
# cells that are surrounded by cells that are NA. For this the prioritization 
#maps should be subsetted to only the n_cells with the highest priority values

#2. landscapemetrics::get_patches which forms patches based on class values 
#which could be used to better identify patches of high priority 
#however we would need to discretize the continuous cell values into bins

# 1. simple approach, 
#Equal amount of additional patches per timestep, using overall the best patches
#Get the n_cells with highest value
#(using the max value in n_cells so there will always be sufficient
# n_cells_values <- sort(values(Biodiv_prio_wo_pa), decreasing = TRUE)[1:max(n_cells)]
# 
# # Set the rest of the cells to NA
# Biodiv_prio_wo_pa[!(Biodiv_prio_wo_pa %in% n_cells_values)] <- NA
# 
# #Get patches
# Patches_terra <- patches(Biodiv_prio_wo_pa)
# writeRaster(Patches_terra,file= paste0(PA_int_dir, "/Patches_terra.tif"), overwrite=TRUE) 
# Patches_terra <- rast(paste0(PA_int_dir, "/Patches_terra.tif"))
# Terra_num_patches <- length(unlist(terra::unique(Patches_terra)))

#Raster with all the best cells, that would be enough to reach the desired share of protected area
#writeRaster(Biodiv_prio_wo_pa,file= paste0(PA_int_dir, "/Prio_bestpatches.tif"), overwrite=TRUE) 
#Biodiv_prio_wo_pa <- rast(paste0(PA_int_dir, "/Prio_bestpatches.tif"))

#2. Complex approach with landscape metrics

#reclassify raster to discrete
Bio_prio_hist <- terra::hist(Biodiv_prio_wo_pa, maxcell = ncell(Biodiv_prio_wo_pa))

#because we don't need to add that much PA cells it makes sense to create patches
#using only the highest priority breaks that give sufficent cells counts to
#cover the required amount  

#take the counts and iteratively sum from the last value until the max(n_cells) is exceeded
Bio_running_sum <- cumsum(rev(Bio_prio_hist$counts)) 

# identify the first cumsum that exceeds the required n_cells and add 1 to 
#include the lower bound of the break points
Bio_min_ind <- min(which(Bio_running_sum > n_cells[names(n_cells)=="EI_NAT"]))

#subset only the high prioirty breaks that satisfy the desired n_cells
Bio_cuts <- c(0, Bio_prio_hist$breaks[(length(Bio_prio_hist$breaks)-Bio_min_ind):length(Bio_prio_hist$breaks)])
  
#reclassify using the break values
Bio_prio_discrete <- classify(Biodiv_prio_wo_pa, Bio_cuts)

#create patches
Bio_prio_patches <- get_patches(Bio_prio_discrete,
                           directions = 8,
                           return_raster = TRUE)

#convert patch rasters to terra:rast and sum values excluding the first layer
# (i.e. excluding the values of 0-min break)
Bio_prio_patch_sum <- sum(rast(lapply(Bio_prio_patches$layer_1[2:length(Bio_prio_patches$layer_1)], function(x){
class_rast <- rast(x)
class_rast <- ifel(!is.na(class_rast), 1, 0)
})))

#convert 0 to NA for patch identification
Bio_prio_patch_sum[Bio_prio_patch_sum == 0] <- NA

#Now we have patches based on high priority values now delineate them spatially
#using terra::patches
Bio_patches <- patches(Bio_prio_patch_sum)
writeRaster(Bio_patches,file= paste0(PA_int_dir, "/Bio_patches.tif"), overwrite=TRUE) 
Bio_patches <- rast(paste0(PA_int_dir, "/Bio_patches.tif"))

  #-------------------------------------------------------------------------
  # E.6 Zonal statistics for each biodiversity patches
  #-------------------------------------------------------------------------

#small function calculating stats on patches
Patch_stats <- function(Patch_raster, Val_raster){

  #calculate the area of all patches
  area_df <- as.data.frame(freq(Patch_raster))

  #subset cols and rename
  area_df <- area_df[c('value','count')]
  colnames(area_df) <- c("patch_id", "num_cells")

  # stack with the prioritization map
  r_stack <- c(Patch_raster, Val_raster)

  # Use terra's zonal function to compute the median for each unique value in Patch_rast
  zonal_median <- terra::zonal(r_stack, Patch_raster, fun = function(x) { median(x, na.rm = TRUE) })
  zonal_median <- zonal_median[c(2,3)]

  # Rename the columns of zonal_median
  colnames(zonal_median) <- c("patch_id", "median")

  #merge dfs with median of prio and area of each patch
  df_merge <- merge(area_df,zonal_median,by="patch_id")
}

#calculate stats for each patch generation approach
#Terra_patch_stats <- Patch_stats(Patch_raster = Patches_terra, Val_raster = Biodiv_prio_wo_pa)
Bio_patch_stats <- Patch_stats(Patch_raster = Bio_patches, Val_raster = Biodiv_prio_wo_pa)

#count number of single cell patches
#Terra_num_SC <- nrow(Terra_patch_stats[Terra_patch_stats$num_cells ==1,])
Bio_num_SC <- nrow(Bio_patch_stats[Bio_patch_stats$num_cells <2,])

#remove patches that are 2 cells or less
#Terra_non_SC <- Terra_patch_stats[Terra_patch_stats$num_cells > 2,]
Bio_patch_stats <- Bio_patch_stats[Bio_patch_stats$num_cells > 2,]
row.names(Bio_patch_stats) <- 1:nrow(Bio_patch_stats)

#calculate average median patch priority
#Terra_mean <- mean(Terra_patch_stats$median, na.rm = TRUE)
Bio_mean <- mean(Bio_patch_stats$median, na.rm = TRUE)

#subset the patches raster and save along with the patch stats
Bio_patches_subset <- ifel(Bio_patches %in% Bio_patch_stats$patch_id, Bio_patches, NaN)
writeRaster(Bio_patches_subset,file= paste0(PA_int_dir, "/Bio_patches_subset.tif"), overwrite=TRUE)
saveRDS(Bio_patch_stats,file= paste0(PA_int_dir, "/Bio_patch_stats.rds"))

#The LSM approach delivers a much high average median priority value per patch
#whether or not single cells patches are excluded
#Terra: 0.54
#LSM: 0.80
#Hence we should use the LSM patches

  #-------------------------------------------------------------------------
  # E.7 Identify locations for new PAs based on NCP prioritization map
  #-------------------------------------------------------------------------

#mask NCP_prio map so that values inside PAs are 0
NCP_prio_wo_pa <- mask(NCP_prio, PA_total_rast, maskvalue=1)
#writeRaster(NCP_prio_wo_pa, paste0(PA_int_dir, "/NCP_prio_without_PAs.tif"), overwrite = TRUE)

#Exclude urban areas/roads and railways
NCP_prio_wo_pa <- mask(NCP_prio_wo_pa, settlement, inverse=TRUE)
NCP_prio_wo_pa <- mask(NCP_prio_wo_pa, road, inverse=TRUE)
NCP_prio_wo_pa <- mask(NCP_prio_wo_pa, railway, inverse=TRUE)

#reclassify raster to discrete
NCP_prio_hist <- terra::hist(NCP_prio_wo_pa, maxcell = ncell(NCP_prio_wo_pa))

#because we don't need to add that much PA cells it makes sense to create patches
#using only the highest priority breaks that give sufficent cells counts to
#cover the required amount  

#take the counts and iteratively sum from the last value until the max(n_cells) is exceeded
NCP_running_sum <- cumsum(rev(NCP_prio_hist$counts)) 

# identify the first cumsum that exceeds the required n_cells and add 1 to 
#include the lower bound of the break points
NCP_min_ind <- min(which(NCP_running_sum > (n_cells[names(n_cells) == "EI_SOC"])*1.5))

#subset only the high prioirty breaks that satisfy the desired n_cells
NCP_cuts <- c(0, NCP_prio_hist$breaks[(length(NCP_prio_hist$breaks)-NCP_min_ind):length(NCP_prio_hist$breaks)])
  
#reclassify using the break values
NCP_prio_discrete <- classify(NCP_prio_wo_pa, NCP_cuts)

#create patches
NCP_prio_patches <- get_patches(NCP_prio_discrete,
                           directions = 8,
                           return_raster = TRUE)

#convert patch rasters to terra:rast and sum values excluding the first layer
# (i.e. excluding the values of 0-min break)
NCP_prio_patch_sum <- sum(rast(lapply(NCP_prio_patches$layer_1[2:length(NCP_prio_patches$layer_1)], function(x){
class_rast <- rast(x)
class_rast <- ifel(!is.na(class_rast), 1, 0)
})))

#convert 0 to NA for patch identification
NCP_prio_patch_sum[NCP_prio_patch_sum == 0] <- NA

#Now we have patches based on high priority values now delineate them spatially
#using terra::patches
NCP_patches <- patches(NCP_prio_patch_sum)
writeRaster(NCP_patches,file= paste0(PA_int_dir, "/NCP_patches.tif"), overwrite=TRUE) 
NCP_patches <- rast(paste0(PA_int_dir, "/NCP_patches.tif"))

#calculate stats for NCP patches
NCP_patch_stats <- Patch_stats(Patch_raster = NCP_patches, Val_raster = NCP_prio_wo_pa)

#count number of single cell patches
#Terra_num_SC <- nrow(Terra_patch_stats[Terra_patch_stats$num_cells ==1,])
NCP_num_SC <- nrow(NCP_patch_stats[NCP_patch_stats$num_cells <2,])

#remove patches that are 2 cells or less
#Terra_non_SC <- Terra_patch_stats[Terra_patch_stats$num_cells > 2,]
NCP_patch_stats <- NCP_patch_stats[NCP_patch_stats$num_cells > 2,]
row.names(NCP_patch_stats) <- 1:nrow(NCP_patch_stats)

#calculate average median patch priority
#Terra_mean <- mean(Terra_patch_stats$median, na.rm = TRUE)
NCP_mean <- mean(NCP_patch_stats$median, na.rm = TRUE)

#subset the patches raster and save
NCP_patches_subset <- ifel(NCP_patches %in% NCP_patch_stats$patch_id, NCP_patches, NaN)
writeRaster(NCP_patches_subset,file= paste0(PA_int_dir, "/NCP_patches_subset.tif"), overwrite=TRUE)
saveRDS(NCP_patch_stats,file= paste0(PA_int_dir, "/NCP_patch_stats.rds"))

  #-------------------------------------------------------------------------
  # E.8 Identify locations for new PAs based on cultural importance map
  #-------------------------------------------------------------------------

#This is the exact same approach as with the biodiversity, except that the proximity to BLN areas is considered
#mask Biodiv_prio map so that values inside PAs are 0
#Bio_prio_wo_pa <- mask(Biodiv_prio, PA_total_rast, maskvalue=1)
#writeRaster(Biodiv_prio_wo_pa,file=paste0(getwd(),"/Prio_without_PA.tif"), overwrite=TRUE)

#Exclude urban areas/roads and railways
#Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, settlement, inverse=TRUE)
#Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, road, inverse=TRUE)
#Biodiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, railway, inverse=TRUE)

buffer <- terra::buffer(BLN, width = 2830)
CULdiv_prio_wo_pa <- mask(Biodiv_prio_wo_pa, buffer, inverse=TRUE)

#reclassify raster to discrete
CUL_prio_hist <- terra::hist(CULdiv_prio_wo_pa, maxcell = ncell(Biodiv_prio_wo_pa))

#because we don't need to add that much PA cells it makes sense to create patches
#using only the highest priority breaks that give sufficent cells counts to
#cover the required amount  

#take the counts and iteratively sum from the last value until the max(n_cells) is exceeded
CUL_running_sum <- cumsum(rev(CUL_prio_hist$counts)) 

# identify the first cumsum that exceeds the required n_cells and add 1 to 
#include the lower bound of the break points
CUL_min_ind <- min(which(CUL_running_sum > n_cells[names(n_cells)=="EI_NAT"]))

#subset only the high prioirty breaks that satisfy the desired n_cells
CUL_cuts <- c(0, CUL_prio_hist$breaks[(length(CUL_prio_hist$breaks)-CUL_min_ind):length(CUL_prio_hist$breaks)])

#reclassify using the break values
CUL_prio_discrete <- classify(CULdiv_prio_wo_pa, CUL_cuts)

#create patches
CUL_prio_patches <- get_patches(CUL_prio_discrete,
                                directions = 8,
                                return_raster = TRUE)

#convert patch rasters to terra:rast and sum values excluding the first layer
# (i.e. excluding the values of 0-min break)
CUL_prio_patch_sum <- sum(rast(lapply(CUL_prio_patches$layer_1[2:length(CUL_prio_patches$layer_1)], function(x){
  class_rast <- rast(x)
  class_rast <- ifel(!is.na(class_rast), 1, 0)
})))

#convert 0 to NA for patch identification
CUL_prio_patch_sum[CUL_prio_patch_sum == 0] <- NA

#Now we have patches based on high priority values now delineate them spatially
#using terra::patches
CUL_patches <- patches(CUL_prio_patch_sum)
writeRaster(CUL_patches,file= paste0(PA_int_dir, "/CUL_patches.tif"), overwrite=TRUE) 
CUL_patches <- rast(paste0(PA_int_dir, "/CUL_patches.tif"))

#calculate patch stats
CUL_patch_stats <- Patch_stats(Patch_raster = CUL_patches, Val_raster = CULdiv_prio_wo_pa)

#count number of single cell patches
CUL_num_SC <- nrow(CUL_patch_stats[CUL_patch_stats$num_cells <2,])

#remove patches that are 2 cells or less
#Terra_non_SC <- Terra_patch_stats[Terra_patch_stats$num_cells > 2,]
CUL_patch_stats <- CUL_patch_stats[CUL_patch_stats$num_cells > 2,]
row.names(CUL_patch_stats) <- 1:nrow(CUL_patch_stats)

#calculate average median patch priority
CUL_mean <- mean(CUL_patch_stats$median, na.rm = TRUE)

#subset the patches raster and save along with the patch stats
CUL_patches_subset <- ifel(CUL_patches %in% CUL_patch_stats$patch_id, CUL_patches, NaN)
writeRaster(CUL_patches_subset,file= paste0(PA_int_dir, "/CUL_patches_subset.tif"), overwrite=TRUE)
saveRDS(CUL_patch_stats,file= paste0(PA_int_dir, "/CUL_patch_stats.rds"))

  #-------------------------------------------------------------------------
  # E.9 Identify subsets of best patches to meet areal demand and seperate patches
  # according to scenario time steps 
  #-------------------------------------------------------------------------

#solver function from: https://stackoverflow.com/questions/69608840/selecting-such-vector-elements-so-that-the-sum-of-elements-is-exactly-equal-to-t
#for identifying combination of patches whose sum total area
#exceeds a specifcied amount
findSumm <- function(xy, sfind, nmax=10000, tmax=100000000000000000000000000){
  
  #sort xy according to target variable
  xy <- xy[order(xy$num_cells, decreasing = TRUE),]
  
  #seperate patch areas
  x = xy[, "num_cells"]
  
  #stop if the sum of all patches does not exceed the desired area
  if(sum(x)<sfind) stop("Impossible solution! sum(x)<sfind!")
  
  #helper function to calculate difference from start time
  fTimeSec <- function() as.numeric(Sys.time()-l$tstart, units="secs")
  
  #Create a vector the same length as the num of patches to start the loop,
  #first entry TRUE all the subsequent entries FALSE
  #updated iteratively in loop
  sel = c(TRUE, rep(FALSE, length(x)-1))
  
  #List of intermediate states of the vector sel
  lsel = list()
  
  #List with a collection of parameters and results
  l = list(
    patch_ids = list(),
    x = x,
    tstart = Sys.time(),
    chosen = list(),
    xfind = list(),
    time = c(),
    stop = FALSE,
    reason = "")
  
  while(TRUE) {
    #Maximum Runtime Test
    if(fTimeSec()>tmax) {
      l$reason = "Calculation time is greater than tmax.\n"
      l$stop = TRUE
      break
    }
    
    #Record the solution and test the number of solutions
    if(sum(l$x[sel])==sfind){
      #Save solution
      l$chosen[[length(l$chosen)+1]] = sel
      l$xfind[[length(l$xfind)+1]] = l$x[sel]
      l$patch_ids[[length(l$patch_ids)+1]] = xy[sel, "patch_id"]
      l$time = c(l$time, fTimeSec())
      
      #Test the number of solutions
      if(length(l$chosen)==nmax){
        l$reason = "Already found nmax solutions.\n"
        l$stop = TRUE
        break
      }
    }
    
    idx = which(sel)
    if(idx[length(idx)]==length(sel)) {
      if(length(lsel)==0) break
      sel=lsel[[length(lsel)]]
      idx = which(sel)
      lsel[length(lsel)]=NULL
      sel[idx[length(idx)]]=FALSE
      sel[idx[length(idx)]+1]=TRUE
      next
    }
    
    if(sum(l$x[sel])>=sfind){
      sel[idx[length(idx)]]=FALSE
      sel[idx[length(idx)]+1]=TRUE
      next
    } else {
      lsel[[length(lsel)+1]] = sel  #Save the current state of sel vector
      sel[idx[length(idx)]+1]=TRUE
      next
    }
  }
  if(length(l$chosen)==0 & !l$stop) stop("No solutions!")
  
  #vector summary of result
  l$reason = paste(l$reason, "Found", length(l$chosen),
                   "solutions in time", signif(fTimeSec(), 3), "seconds.\n")
  
  #print summary of combinatorial step
  cat(l$reason)
  
  #return results object
  return(l)
}     

#vector scenario names and layer keys to match on
Scenario_keys <- c("NCP", "CUL","Bio")
names(Scenario_keys) <- names(n_cells)

#Scenario_keys <- Scenario_keys[2]
#n_cells <- n_cells[2]

#subset interventions table
PA_interventions <- Interventions[Interventions$Intervention_ID == "Protection",]

#loop function over cell targets for scenarios
for(i in 1:length(n_cells)){

  #Load the layer of patchs and patch stats appropriate for the scenario 
  Scenario_patch_stats <- readRDS(paste0(PA_int_dir, "/", Scenario_keys[i], "_patch_stats.rds"))
  Scenario_patches <- rast(paste0(PA_int_dir, "/", Scenario_keys[i], "_patches_subset.tif"))

   Solutions <- findSumm(xy = Scenario_patch_stats,
                       sfind = n_cells[i])
   saveRDS(Solutions, paste0(PA_int_dir, "/Solutions_10k_", names(n_cells)[i], ".rds"))
  
  browser()
  Solutions <- readRDS(paste0(PA_int_dir, "/Solutions_10k_", names(n_cells)[i], ".rds"))

  #rank solutions according greatest value of the sum of median patch priority * patch size
  size = 5000*1024^2 
  options(future.globals.maxSize= size)
  #plan(multisession) #use parallel for larger solution sets
  Solution_scores <- rbindlist(future.apply::future_lapply(1:length(Solutions$chosen),
                                       function(x){
                                         
    #seperate vector of whether patches were chosen (by ID)  
    idx = Solutions$chosen[[x]]
  
    #calculate sum of median patch priority * patch size for solution
    return(list("Solution_num" = x,
    "Patch_prio_sum" = sum(Scenario_patch_stats$median[idx] * Scenario_patch_stats$num_cells[idx])))
    }))
  #plan(sequential)                                  

  #sort results
  Ranked_solutions <- Solution_scores[order(Solution_scores$Patch_prio_sum, decreasing = TRUE),]
  Best_solution_ID <- Ranked_solutions[[1,"Solution_num"]]

  #get best patches via id
  Patch_ids <- Solutions$patch_ids[[Best_solution_ID]]
  rm(Solutions, Solution_scores, Ranked_solutions)
  
  #subset patch stats to check total area
  Best_patch_stats <- Scenario_patch_stats[Scenario_patch_stats$patch_id %in% Patch_ids,]

  #sort patches by median priority
  Best_patch_stats <- Best_patch_stats[order(Best_patch_stats$median, decreasing = TRUE),]
  #sum(Solutions$xfind[[Best_solution_ID]])
  #sum(Best_patch_stats$num_cells)

  #seperate best patches in raster and save
  Best_patches <-  ifel(Scenario_patches %in% Patch_ids, Scenario_patches, NaN)
  writeRaster(Best_patches,file= paste0(New_PA_dir, "/", names(n_cells)[i],"_all_future_PAs.tif"), overwrite=TRUE)

  #grab intervention time steps
  Scenario_time_steps <- unlist(PA_interventions[PA_interventions$Scenario_ID == names(n_cells)[i], "Time_step"])
  Scenario_path <- unlist(PA_interventions[PA_interventions$Scenario_ID == names(n_cells)[i], "Intervention_data"])

  #becuase it is unlikely that new protection areas will be established before
  #2025 and also because the scenario statements specify that PA targets be met by 2060 
  #we will use the assumption that the addition of new patches takes places
  #between  2025-2055 however we still need to include the protective effects 
  #of the existing PAs in the 2020 time point
  
  #seperate patches into groups according to number of time steps 
  #(group sizes may be unqual in the case of non-integer division
  #of number of patches/time steps)
  
  PA_exp_steps <- Scenario_time_steps[-1] #exclude 2020 time step 
  Split_ind  <- rep(1:length(PA_exp_steps),each=ceiling(nrow(Best_patch_stats)/length(PA_exp_steps)))[1:nrow(Best_patch_stats)]
  Time_grouped_patches  <- split(Best_patch_stats[,"patch_id"],Split_ind)
  names(Time_grouped_patches) <- PA_exp_steps

  #This has split patches into unique groups for each time step
  #but every subsequent time step needs to contain the patches from the
  #previous time step as well
  Time_cum_patches <-sapply(1:length(Time_grouped_patches), function(x){
    if(x == 1){patches <- Time_grouped_patches[[x]]}else{
      patches <- append(Time_grouped_patches[[x]], Time_grouped_patches[[(x-1)]])
    }
    })
  names(Time_cum_patches) <- PA_exp_steps
  

  #loop over time step patches saving as rasters 
  for(step in Scenario_time_steps){
    
    if(step == "2020"){
    Updated_PAs <- PA_total_rast
    }else{

    #identify patches for time step in raster (setting values to 1 otherwise NaN)
    Time_step_patches <-  ifel(Best_patches %in% Time_cum_patches[[step]], 1, 0)
    
    #save a layer of just the new patches without combining with the existing PAs
    writeRaster(Time_step_patches,file= paste0(New_PA_dir, "/", names(n_cells)[i],"_new_PAs_", step, ".tif"), overwrite=TRUE)
    
    #replace NA values in current PA layer for 0
    Current_PAs <- ifel(is.na(PA_total_rast), 0, 1)
    
    #combine with existing PAs
    Updated_PAs <- Current_PAs + Time_step_patches
    
    #set 0's to NA
    Updated_PAs <- ifel(Updated_PAs == 0, NA, 1)
    }
    
    #save
    writeRaster(Updated_PAs,file= paste0(PA_final_dir, "/", names(n_cells)[i],"_PAs_", step, ".tif"), overwrite=TRUE)
    } #close loop over scenario time steps

} #close loop over scenario cell targets

### =========================================================================
### Intervention in allocation params
### =========================================================================

#load table of scenario interventions
Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")

#convert Time_step and Target_classes columns back to character vectors
Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
  x <- str_remove_all(x, " ")
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)
  
Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
  x <- str_remove_all(x, " ")
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)

#test to see if spatial zoning 
if(any(Interventions$Intervention_type == "Param_adjust")){
  
  #subset to interventions involving parameter adjustment
  Param_ints <- Interventions[Interventions$Intervention_type == "Param_adjust",]
  
  #load the LULC aggregation scheme
  LULC_agg <- openxlsx::read.xlsx(LULC_aggregation_path)
  
  #swap the target classes for class numbers
  Param_ints$Target_classes <- sapply(Param_ints$Target_classes, function(x){
    class_nums <- unique(LULC_agg[LULC_agg$Class_abbreviation %in%x, "Aggregated_ID"])
  })
  
  #loop over interventions adjust param tables
  sapply(1:nrow(Param_ints), function(i){
    
    #get paths of param tables for relevant scenario and time points
    Param_table_paths <- list.files(paste0(Simulation_param_dir, "/", Param_ints[i, "Scenario_ID"]),
               pattern = paste0(Param_ints[[i, "Time_step"]], collapse = "|"),
               full.names = TRUE) 
  
    #loop over paths adjusting tables
    sapply(Param_table_paths, function(tbl_path){
    
      #load table
      param_table <- read.csv(tbl_path)
    
      #adjust column names
      colnames(param_table) <- c("From*","To*"," Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry", "Perc_expander", "Perc_patcher")
    
      #alter rows for Target_classes
      param_table[param_table$`To*` %in% Param_ints[[i, "Target_classes"]], "Perc_expander"] <- 1
      param_table[param_table$`To*` %in% Param_ints[[i, "Target_classes"]], "Perc_patcher"] <- 0
    
      #save table
      write_csv(param_table, file = tbl_path)
      }) #close loop over tables
  
    }) #close loop over intervention rows

} #close if statement 



    

