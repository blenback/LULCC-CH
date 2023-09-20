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

PA_final_dir <- "Data/Spat_prob_perturb_layers/Protected_areas/Future_PAs"
dir.create(PA_final_dir)

#load PAs shapefile of SwissPAs layer compiled by Louis-Rey
PA <- vect(paste0(PA_raw_dir, "/SwissPA.shp"))

#Load cantonal PA layer provided by BAFU (cleaned by us)
PA_cantons <- vect(paste0(PA_raw_dir, "/PA_cantons.shp"))

#load the vector land cover data from Swiss TLM regio to identify settlement areas
LC <- vect(paste0(PA_raw_dir, "/swissTLMRegio_LandCover.shp"))
settlement <- subset(LC, LC$OBJVAL == "Siedl")

#load Swiss TLM region roads and railways layers to exclude
road <- vect(paste0(PA_raw_dir, "/swissTLMRegio_Road.shp"))
road <- subset(road, road$CONSTRUCT == 'Keine Kunstbaute')
railway <- vect(paste0(PA_raw_dir, "/swissTLMRegio_Railway.shp"))
railway <- subset(road, road$CONSTRUCT == 'Keine Kunstbaute')

#load biodiversity prioritization map
Biodiv_prio <- rast(paste0(PA_raw_dir, "/Bio_prio.tif"))
crs(Biodiv_prio) <- ProjCH

#load NCP prioritization map
NCP_prio <- rast(paste0(PA_raw_dir, "/NCP_prio.tif"))
crs(NCP_prio) <- ProjCH
ext(NCP_prio) <- ext(Ref_grid)
NCP_prio <- terra::resample(NCP_prio, Ref_grid, method= "bilinear")

#load table of scenario interventions
Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")

#convert Time_step and Target_classes columns back to character vectors
Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)
  
Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
  x <- str_remove_all(x, " ")
  rep <- unlist(strsplit(x, ","))
  },simplify=FALSE)

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

# TO DO: wait for approach from Manuel K. 

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
Scenario_keys <- c("NCP", "Cul","Bio")
names(Scenario_keys) <- names(n_cells)

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
  writeRaster(Best_patches,file= paste0(PA_final_dir, "/", names(n_cells)[i],"_all_future_PAs.tif"), overwrite=TRUE)

  #grab intervention time steps
  Scenario_time_steps <- unlist(PA_interventions[PA_interventions$Scenario_ID == names(n_cells)[i], "Time_step"])
  Scenario_path <- unlist(PA_interventions[PA_interventions$Scenario_ID == names(n_cells)[i], "Intervention_data"])

  #seperate patches into groups according to number of time steps 
  #(group sizes may be unqual in the case of non-integer division
  #of number of patches/time steps)
  Split_ind  <- rep(1:length(Scenario_time_steps),each=ceiling(nrow(Best_patch_stats)/length(Scenario_time_steps)))[1:nrow(Best_patch_stats)]
  Time_grouped_patches  <- split(Best_patch_stats[,"patch_id"],Split_ind)
  names(Time_grouped_patches) <- Scenario_time_steps

  #This has split patches into unique groups for each time step
  #but every subsequent time step needs to contain the patches from the
  #previous time step as well
  Time_cum_patches <-sapply(1:length(Time_grouped_patches), function(x){
    if(x == 1){patches <- Time_grouped_patches[[x]]}else{
      patches <- append(Time_grouped_patches[[x]], Time_grouped_patches[[(x-1)]])
    }
    })
  names(Time_cum_patches) <- Scenario_time_steps
  
  #loop over time step patches saving as rasters 
  for(step in Scenario_time_steps){

    #identify patches for time step in raster (setting values to 1 otherwise NaN)
    Time_step_patches <-  ifel(Best_patches %in% Time_cum_patches[[step]], 1, 0)
    
    #replace NA values in current PA layer for 0
    Current_PAs <- ifel(is.na(PA_total_rast), 0, 1)
    
    #combine with existing PAs
    Updated_PAs <- Current_PAs + Time_step_patches
    
    #set 0's to NA
    Updated_PAs <- ifel(Updated_PAs == 0, NA, 1)
    
    #save
    writeRaster(Updated_PAs,file= paste0(PA_final_dir, "/", names(n_cells)[i],"_PAs_", step, ".tif"), overwrite=TRUE)
    } #close loop over scenario time steps

} #close loop over scenario cell targets


### =========================================================================
### Load testing_data
### =========================================================================

#TO DO: determine whether it is faster to perform the probability perturbation
#operations on dataframes or rasters:
#Pro's of dataframe: smaller size (not including NAs) however identifying cells
#based on XY values may be slower than raster operations.
#Con's of dataframe: requires converting every mechanism layer into dataframe

#Pro's of raster: possibly faster operations and can calculate focal values
#and euclidean distances etc.
#Con's of raster: requires converting prob predictions into a raster brick and
#then back to dataframe in order to be re-scaled. 

#dataframe of predicted probabilities not including NA cells
#Prediction_probs <- readRDS("Data/Spat_prob_perturb_layers/EXP_pred_probs_rescaled.rds")

#dataframe of NA cells to be combined with predicted probabilities
#Trans_dataset_na <- readRDS("Data/Spat_prob_perturb_layers/EXP_trans_dataset_NA_values.rds")

#process for binding NA values with data
#Trans_dataset_na[setdiff(names(Prediction_probs), names(Trans_dataset_na))] <- NA
#Raster_prob_values <- rbind(Prediction_probs, Trans_dataset_na)

#Dataframe of combined predicted probability values and NA values
Raster_prob_values <- readRDS("Data/Exemplar_data/EXP_raster_prob_values.rds")

# #sort by ID
Raster_prob_values[order(Raster_prob_values$ID),]

Simulation_time_step <- "2020"
Scenario_ID <- "EI_CUL"

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


### =========================================================================
### Function to perform perturbations (raster version)
### =========================================================================

#if Perc_diff is too small then the effect will likely not be achieved
#set a threshold of minimum probability perturbation

lulcc.spatprobperturbation <- function(Scenario_ID, Raster_prob_values, Simulation_time_step){}

  #vector names of columns of probability predictions (matching on Prob_)
  Pred_prob_columns <- grep("Prob_", names(Raster_prob_values), value = TRUE)

  #convert probability table to raster stack
  Prob_raster_stack <- stack(lapply(Pred_prob_columns, function(x) rasterFromXYZ(Raster_prob_values[,c("x", "y", x)])))
  names(Prob_raster_stack@layers) <- Pred_prob_columns

  #load table of scenario interventions
  Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")

  #convert Time_step and Target_classes columns back to character vectors
  Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)
  
  Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
    x <- str_remove_all(x, " ")
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)
  
  #subset interventions to scenario
  Scenario_interventions <- Interventions[Interventions$Scenario_ID == Scenario_ID,] 
  
  #subset to interventions for current time point
  Time_step_rows <- sapply(Scenario_interventions$Time_step, function(x) any(grepl(Simulation_time_step, x)))
  Current_interventions <- Scenario_interventions[Time_step_rows, ]
  
  #loop over rows
  if(nrow(Current_interventions) !=0){}
  for(i in nrow(Current_interventions)){}

    i = 5
    
    Intervention_ID <- Current_interventions[i, "Intervention_ID"]
    Target_classes <- paste0("Prob_", Current_interventions[[i, "Target_classes"]])
    Intervention_data <- Current_interventions[i, "Intervention_data"]
    Prob_perturb_thresh <- as.numeric(Current_interventions[i, "Prob_perturb_threshold"]) 
    
    #--------------------------------------------------------------------------
    # Urban_densification intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_densification"){

    #load building zone raster
    Intervention_rast <- raster(Intervention_data)
  
    #identify pixels inside of building zones 
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #increase probability to one
    Intersecting[Intersecting > 0] <- 1
    
    #index which cells need to have value updated
    ix <- Intersecting == 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
    
    }#close Urban_densification chunk
    
    
    #--------------------------------------------------------------------------
    #Urban_sprawl intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_sprawl"){

    #load building zone raster
    Intervention_rast <- raster(Intervention_data)
  
    #identify pixels inside of building zones 
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of building zones
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #calculate 90th percentile values of probability for pixels inside vs.outside
    #excluding those with a value of 0
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the 90th percentile
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean
    
    #Average of means
    Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
    #calculate percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #The intended effect of the intervention is to increase the probability of
    #urban development outside the building zone, however depending on the
    #valency of the Perc_diff values this needs to be implemented differently
    
    #If Perc_diff is >0 then increase the probability of instances above the
    #90th percentile for the outside pixels by the percentage difference
    #between the means
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}
      
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      positive_test <- Prob_raster_stack@layers[[Target_classes]]
    
      #else if Perc_diff is <0 then decrease the probability of instances above the
      #90th percentile for the inside pixels by the percentage difference
      #between the means
      }else if(Perc_diff <0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
    
      Intersecting[Intersecting > Intersect_percentile] <- Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting < 0] <- 0
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
      
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      negative_test <- Prob_raster_stack@layers[[Target_classes]]
      } #close else if statement
    
    }#close Urban_sprawl chunk
    
    #--------------------------------------------------------------------------
    # Urban_migration intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_migration"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    #for this intervention: 325, 326, 327, 335, 338
    Leg <- Intervention_rast@data@attributes[[1]]
    Leg[Leg$ID %in% c(325, 326, 327, 335, 338), "type"] <- 1
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of remote rural municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  

    #identify pixels outside of remote rural municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to decrease the 
    #probability of urban development in the remote rural municipalities
    #calculate 90th percentile value of probability for pixels inside
    #and the 80th percentile value for pixels outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.80, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean
    
    #Average of means
    Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is < 0 then decrease the probability of instances above the
    #90th percentile for the pixels in remote rural municipalities by the percentage difference
    #between the means
    if(Perc_diff <0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
      
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
    
      #else if Perc_diff is >0 then increase the probability of instances above the
      #90th percentile for the outside pixels by the percentage difference
      #between the means
      }else if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
    
    }#close Urban_migration chunk
    
    #--------------------------------------------------------------------------
    #Mountain_development intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Mountain_development"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    Leg <- Intervention_rast@data@attributes[[1]]
    
    #For this intervention there are two different specs for scenarios
    #EI_NAT: 314
    #EI_SOC: 314,334
    if(Scenario_ID == "EI_NAT"){
    Leg[Leg$ID == 314, "type"] <- 1
    } else if(Scenario_ID == "EI_SOC"){
    Leg[Leg$ID %in% c(314, 334), "type"] <- 1  
    }
    
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of mountainous remote municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of mountainous remote municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to increase the 
    #probability of urban development in the mountainous municipalities
    
    #calculate 90th percentile value of probability for pixels inside and outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean 
    
    #Average of means
    Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is > 0 then increase the probability of instances above the
    #90th percentile for the pixels in mountainous municipalities by the
    #percentage difference between the means (or the threshold value)
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      #increase the values
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      
    
      #else if Perc_diff is >0 then increase the probability of instances above the
      #90th percentile for the outside pixels by the percentage difference
      #between the means
      }else if(Perc_diff <0){
        
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -c(Prob_perturb_thresh)} 
        
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
    
    }#close Mountain_development chunk

    #--------------------------------------------------------------------------
    # Rural_migration intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Rural_migration"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    Leg <- Intervention_rast@data@attributes[[1]]
    Leg[Leg$ID %in% c(325, 326, 327, 335, 338), "type"] <- 1
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of remote rural municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of remote rural municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to increase the 
    #probability of urban development in the remote rural municipalities
    
    #calculate 90th percentile value of probability for pixels inside and outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean 
    
    #Average of means
    Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is > 0 then increase the probability of instances above the
    #90th percentile for the pixels in remote rural municipalities by the
    #percentage difference between the means (or the threshold value)
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      #increase the values
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      
    
      #else if Perc_diff is >0 then increase the probability of instances above the
      #90th percentile for the pixels outside the remote rural municipalities
      #by the percentage difference between the means
      }else if(Perc_diff <0){
        
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -c(Prob_perturb_thresh)} 
        
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
  
    }#close Rural_migration chunk
    
    #--------------------------------------------------------------------------
    # Agri_abandonment intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Agri_abandonment"){
      
    #The predicted probability of cells to transition from agriculture to other
    #LULC classes already uses accessibility based predictors such as
    #distance to roads/slope however other variables e.g climaticor soil may be having
    #a larger effect hence we should apply a simple analysis based upon the 
    #model used by Gellrich et al. 2007 that considers distance to roads, 
    #slope and distance to building zones as a measure of 'marginality' and 
    #then select the 90th percentile of pixels according to this value
    
    #load the static predictor layers and re-scale between 0-1
    
    #function for rescaling:
    rescale <- function(x, x.min, x.max, new.min = 0, new.max = 1) {
    if(is.null(x.min)) {x.min = min(x)}
    if(is.null(x.max)) {x.max = max(x)}
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
    }
    
    #distance to roads
    Dist2rds <- raster("Data/Preds/Prepared/Layers/Transport/Distance_to_roads_mean_100m.tif")
    Dist2rds <- calc(Dist2rds, function(x) rescale(x,x.min= minValue(Dist2rds),
                                       x.max = maxValue(Dist2rds)))
    
    #Slope
    Slope <- raster("Data/Preds/Prepared/Layers/Topographic/Slope_mean_100m.tif")
    Slope <- calc(Slope, function(x) rescale(x, x.min= minValue(Slope),
                                       x.max = maxValue(Slope)))
    
    #elevation
    Elev <- raster("Data/Preds/Prepared/Layers/Topographic/Elevation_mean_100m.tif")
    Elev <- calc(Elev, function(x) rescale(x, x.min= minValue(Elev),
                                       x.max = maxValue(Elev)))
    
    # Distance to building zones
    #This layer needs to be inverted when re-scaling because
    #greater distance from building zones means lower land cost 
    #which means less likely to abandon hence x.min and x.max values swapped
    Dist2BZ <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif")
    Dist2BZ <- calc(Dist2BZ, function(x) rescale(x, x.min= maxValue(Dist2BZ),
                                       x.max = minValue(Dist2BZ)))
    
    #stack dist2rds, slope and forest_dist layers and sum values as raster
    Marginality_rast <- calc(stack(Dist2rds, Slope, Elev, Dist2BZ), mean)
    
    #subset the marginality raster to only the pixels of the agricultural
    #land types (Int_AG, Alp_Past)
    Agri_rast <- rasterFromXYZ(Raster_prob_values[,c("x", "y", "Alp_Past")])
    Agri_rast[Agri_rast == 0] <- NA
    Agri_marginality <- mask(Marginality_rast, Agri_rast)

    
    #calculate the upper quartile value of marginality for the agricultural cells
    Marginality_percentile <- quantile(Agri_marginality@data@values, probs = 0.75, na.rm=TRUE)

    #indexes of all cells above/below the upper quartile
    marginal_index <- Agri_marginality > Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    non_marginal_index <- Agri_marginality < Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    
    #loop over target classes gathering probability values for marginal vs. non-marginal cells
    for(class in Target_classes){
    
    #calculate the 90th percentile value of probability of transition to the
    #target class in the marginal agricultural cells vs. non-marginal
    marginal_cells <- Prob_raster_stack@layers[[class]][marginal_index]
    marginal_percentile <- quantile(marginal_cells[marginal_cells >0],probs = 0.9, na.rm=TRUE)
    marginal_cells_abv_percentile <- marginal_cells[marginal_cells > marginal_percentile]
    m_ix <- marginal_cells > marginal_percentile
    
    non_marginal_cells <- Prob_raster_stack@layers[[class]][non_marginal_index]
    nonmarginal_percentile <- quantile(non_marginal_cells[non_marginal_cells >0], probs = 0.9, na.rm=TRUE)
    non_marginal_cells_abv_percentile <- non_marginal_cells[non_marginal_cells > nonmarginal_percentile] 
    nm_ix <- non_marginal_cells > nonmarginal_percentile 
    
    #increase the probability values above the 90th percentile of the marginal cells
    marginal_cells[m_ix] <- marginal_cells[m_ix] + (marginal_cells[m_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    marginal_cells[marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][marginal_index] <- marginal_cells
    
    #decrease the probability values above the 90th percentile of the non_marginal cells
    non_marginal_cells[nm_ix] <- non_marginal_cells[nm_ix] - (non_marginal_cells[nm_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    non_marginal_cells[non_marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][non_marginal_index] <- non_marginal_cells
    
    } #close loop over target classes
    
    } #close Agri_abandonment chunk
    
    #--------------------------------------------------------------------------
    # Agri_maintenance intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Agri_maintenance"){
    
    #Use the same approach as the Agri_abandonment intervention to identify 
    #most marginal agricultural pixels and decrease their probability of 
    #transitioning to the target classes (i.e. away from agriculture)
    
    #load the static predictor layers and re-scale between 0-1
    
    #function for rescaling:
    rescale <- function(x, x.min, x.max, new.min = 0, new.max = 1) {
    if(is.null(x.min)) {x.min = min(x)}
    if(is.null(x.max)) {x.max = max(x)}
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
    }
    
    #distance to roads
    Dist2rds <- raster("Data/Preds/Prepared/Layers/Transport/Distance_to_roads_mean_100m.tif")
    Dist2rds <- calc(Dist2rds, function(x) rescale(x,x.min= minValue(Dist2rds),
                                       x.max = maxValue(Dist2rds)))
    
    #Slope
    Slope <- raster("Data/Preds/Prepared/Layers/Topographic/Slope_mean_100m.tif")
    Slope <- calc(Slope, function(x) rescale(x, x.min= minValue(Slope),
                                       x.max = maxValue(Slope)))
    
    #elevation
    Elev <- raster("Data/Preds/Prepared/Layers/Topographic/Elevation_mean_100m.tif")
    Elev <- calc(Elev, function(x) rescale(x, x.min= minValue(Elev),
                                       x.max = maxValue(Elev)))
    
    # Distance to building zones
    #This layer needs to be inverted when re-scaling because
    #greater distance from building zones means lower land cost 
    #which means less likely to abandon hence x.min and x.max values swapped
    Dist2BZ <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif")
    Dist2BZ <- calc(Dist2BZ, function(x) rescale(x, x.min= maxValue(Dist2BZ),
                                       x.max = minValue(Dist2BZ)))
    
    #stack dist2rds, slope and forest_dist layers and sum values as raster
    Marginality_rast <- calc(stack(Dist2rds, Slope, Elev, Dist2BZ), mean)
    
    #subset the marginality raster to only the pixels of the agricultural
    #land types (Int_AG, Alp_Past)
    Agri_rast <- rasterFromXYZ(Raster_prob_values[,c("x", "y", "Alp_Past")])
    Agri_rast[Agri_rast == 0] <- NA
    Agri_marginality <- mask(Marginality_rast, Agri_rast)

    #calculate the upper quartile value of marginality for the agricultural cells
    Marginality_percentile <- quantile(Agri_marginality@data@values, probs = 0.75, na.rm=TRUE)

    #indexes of all cells above/below the upper quartile
    marginal_index <- Agri_marginality > Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    non_marginal_index <- Agri_marginality < Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    
    #loop over target classes gathering probability values for marginal vs. non-marginal cells
    for(class in Target_classes){
    
    #calculate the 90th percentile value of probability of transition to the
    #target class in the marginal agricultural cells vs. non-marginal
    marginal_cells <- Prob_raster_stack@layers[[class]][marginal_index]
    non_marginal_cells <- Prob_raster_stack@layers[[class]][non_marginal_index]
    nonmarginal_percentile <- quantile(non_marginal_cells[non_marginal_cells >0], probs = 0.95, na.rm=TRUE)
    m_ix <- marginal_cells > nonmarginal_percentile
    marginal_cells_abv_percentile <- marginal_cells[marginal_cells > nonmarginal_percentile]
    
    #decrease the probability values above the 90th percentile of the marginal cells
    marginal_cells[m_ix] <- marginal_cells[m_ix] - (marginal_cells[m_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    marginal_cells[marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][marginal_index] <- marginal_cells

    } #close loop over target classes
    
    } #close Agri_maintenance chunk
    
    #--------------------------------------------------------------------------
    #Protection intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Protection"){}
    
    #Load intervention raster
    Intervention_rast <- raster(str_replace(Intervention_data, "X", Simulation_time_step))
    
    #loop over target classes
    for(class in Target_classes){}
    
    class <- Target_classes[2]
      
      #identify pixels of target class inside of protected areas
      Intersecting <- raster::mask(Prob_raster_stack@layers[[class]], Intervention_rast == 1)
      
      if(Scenario_ID == "EI_NAT" || Scenario_ID== "EI_SOC"){
        
        #decrease probability to zero
        Intersecting[Intersecting > 0] <- 0
    
        #index which cells need to have value updated
        ix <- Intersecting == 0
    
        #replace values in target raster
        Prob_raster_stack@layers[[class]][ix] <- Intersecting[ix]
        
      }else if(Scenario_ID == "EI_CUL"){}
        
        #identify pixels outside of PAs
        non_intersecting <- overlay(Prob_raster_stack@layers[[class]],Intervention_rast,fun = function(x, y) {
          x[y==1] <- NA
        return(x)
        })
    
        #calculate 90th percentile values of probability for pixels inside vs.outside
        #excluding those with a value of 0
        
        #if statement for the condition that all pixels of the target class 
        #inside or outside the protected areas have prob values of 0 i.e not possible to calculate a percentile value. 
        if(length(unique(Intersecting))>1 & length(unique(non_marginal_cells)) >1){
          Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
          Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
          #get the means of the values above the 90th percentile
          Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
          Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
        #mean difference
        Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean
    
        #Average of means
        Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
        #calculate percentage difference
        Perc_diff <- (Mean_diff/Average_mean)*100
        
        #The goal is to reduce the likelihood of non-natural land transitions
        #inside protected areas. If Perc_diff is < 0 then this already
        #implies that the average probability of transition inside the PAs
        #is less than outside but we still want to decrease cellular probability
        #in general. Hence, decrease the probability of instances above the 
        #90th percentile for the pixels in PAs by the % difference
        #between the means unless the % diff between the means is less than 
        #the specificed threshold value
        if(Perc_diff <0){
      
          #check threshold
          if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
      
          Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
          #replace any values greater than 1 with 1
          Intersecting[Intersecting > 1] <- 1
 
          #index which cells need to have value updated
          ix <- Intersecting > Intersect_percentile
    
          #replace values in target raster
          Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
    
          #else if Perc_diff is >0 then increase the probability of instances 
          #above the 90th percentile for the pixels outside of the PAs by the
          #% difference between the means unless the % diff between the means
          #is less than the specificed threshold value
          }else if(Perc_diff >0){
      
            #check threshold
            if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
            non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
            #replace any values greater than 1 with 1
            non_intersecting[non_intersecting > 1] <- 1
 
            #index which cells need to have value updated
            ix <- non_intersecting > Nonintersect_percentile
    
            #replace values in target raster
            Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
          } #close if statement for Perc_diff >0
        }
        
        
        } #close if statement fro EI_CUL scenario
    
      } #close loop over target classes
    } #close Protection chunk
  


#} close function


    

