#############################################################################
## LULC_data_prep: Preparing land use land cover rasters for all historic periods in the 
## the Swiss Areal Statistiks and aggregating LULC rasters to new classification scheme
## Date: 01-08-2021
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat",
#          "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal",
#          "rgeos", "sf", "tiff")
# 
# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))
# 
# # Source custom functions
# invisible(sapply(list.files("Scripts/Functions",pattern = ".R", 
# full.names = TRUE, recursive=TRUE), source))

#Load in the grid file we are using for spatial extent and CRS
Ref_grid <- raster(Ref_grid_path)

#Create objects for spatial extents
prj_95 <- "+init=epsg:2056" ## CH1903+ in which AS data is needed

### =========================================================================
### B- Creating numerical rasters of LULC in NOAs04 classification for each period
### =========================================================================

# Read in table of Areal Statistik LULC data from all historic period

#possible to read directly from URL but this exhibits strange behaviour
#sometimes differing number of rows
#AS_table <- read.csv2(url("https://dam-api.bfs.admin.ch/hub/api/dam/assets/20104753/master"), sep = ";") 

#read from file for security
AS_table <- read.csv2("Data/Historic_LULC/NOAS04_LULC/raw/AREA_NOAS04_72_LATEST.csv", sep = ";")

#splitting into a list of tables for separate periods
# Keep only columns of coordinates and LULC classes under the 72 categories scheme
AS_tables_seperate_periods <- list(
NOAS04_1985 = AS_table[,c("E", "N", "AS85_72")],
NOAS04_1997 = AS_table[,c("E", "N", "AS97_72")],
NOAS04_2009 = AS_table[,c("E", "N", "AS09R_72")],
NOAS04_2018 = AS_table[,c("E", "N", "AS18_72")])

rm(AS_table)

#Create raster for each period

#instantiate small function for raster creation
create.reproject.save.raster <- function(table_for_period, raster_name){
 sp::coordinates(table_for_period) <- ~E+N
 gridded(table_for_period) <- TRUE 
 Raster_for_period <- raster(table_for_period, values=TRUE)
 projection(Raster_for_period) <- prj_95 # define current projection
 cropped_raster_for_period <- terra::crop(Raster_for_period, Ref_grid)
 reprojected_raster_for_period <- projectRaster(cropped_raster_for_period, Ref_grid, method = 'ngb') # project them to new coordinate reference system
 writeRaster(reprojected_raster_for_period, filename =  paste0("Data/Historic_LULC/NOAS04_LULC/rasterized/", raster_name, ".tif"), overwrite=T)
 }

#Loop function over tables 
NOAS04_periods_rasters <- mapply(create.reproject.save.raster,
                                 table_for_period = AS_tables_seperate_periods,
                                 raster_name = names(AS_tables_seperate_periods))  


### =========================================================================
### B- Preparing aggregated LULC Rasters for each period 
### =========================================================================

#Aggregated categories and numerical ID's for the purpose of the LULC modeling:
#10 Settlement/urban/amenities 	
#11 Static class 
#12 Open Forest	
#13 Closed forest
#14 Overgrown/shrubland/unproductive vegetation	
#15 Intensive agriculture	
#16 Alpine pastures	
#17 Grassland or meadows	
#18 Permanent crops	
#19 Glacier	

#Preparing rasters using a 2 col (initial value, new value) matrix

#load aggregation scheme
Aggregation_scheme <- read_excel(LULC_aggregation_path)

#subset to just the ID cols
Agg_matrix <- as.matrix(Aggregation_scheme[,c("NOAS04_ID", "Aggregated_ID")])

Reclassified_rasters <- lapply(NOAS04_periods_rasters, function(x) reclassify(x, rcl = Agg_matrix))
names(Reclassified_rasters) <- c("LULC_1985_agg", "LULC_1997_agg", "LULC_2009_agg", "LULC_2018_agg")

#add raster attribute table (rat)
LULC_IDs <- unique(Aggregation_scheme$Aggregated_ID)
names(LULC_IDs) <- sapply(LULC_IDs, function(x){unique(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == x, "Class_abbreviation"])})
LULC_IDs <- sort(LULC_IDs)

LULC_rat <- data.frame(
  ID= LULC_IDs, 
  Pixel_value= LULC_IDs,
  lulc_name = names(LULC_IDs)
)

Reclassified_rasters <- lapply(Reclassified_rasters, function(x) {
  raster_with_att <- ratify(x)
  levels(raster_with_att) <- LULC_rat
  return(raster_with_att)})

#save
mapply(FUN = writeRaster,
       x = Reclassified_rasters,
       filename =  paste0("Data/Historic_LULC/", names(Reclassified_rasters) , ".grd"),datatype='INT2U', overwrite=T)

