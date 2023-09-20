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
Ref_grid <- raster("Data/Ref_grid.gri")

#Create objects for spatial extents
prj_95 <- "+init=epsg:2056" ## CH1903+ in which AS data is needed

### =========================================================================
### B- Creating numerical rasters of LULC in NOAs04 classification for each period
### =========================================================================

# Read in table of Areal Statistik LULC data from all historic period
AS_table <- read.csv2(url("https://dam-api.bfs.admin.ch/hub/api/dam/assets/20104753/master"), sep = ";") 

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
 coordinates(table_for_period) <- ~E+N
 gridded(table_for_period) <- TRUE 
 Raster_for_period <- raster(table_for_period, values= TRUE)
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

### Preparing numerical Rasters ###

#3 possible approaches to re-classifying rasters:
#Method 1. Using a 3 column (From, to, value) matrix
#Method 2. Using a 2 col (initial value, new value) matrix
#Method 3. using seperately specificed vectors of categories and breaks. 

#1 or 2 are preferable because they can be performed directly on rasters themselves whereas 3 is a tabular operation. 

#Method 1. 3 col matrix based re-classification
# create classification matrix
reclass_num <- c(0, 14, 10,
                 14, 18, 11,
                 18, 19, 10,
                 19, 28, 11,
                 28, 36, 10,
                 36, 40, 18,
                 40, 41, 15,
                 41, 45, 17,
                 45, 49, 16,
                 49, 53, 13,
                 53, 56, 12,
                 56, 57, 13,
                 57, 60, 12,
                 60, 63, 11,
                 63, 65, 14,
                 65, 71, 11,
                 71, 72, 19,
                 72, Inf, NA)

reclass_num_matrix <- matrix(reclass_num,
                ncol = 3,
                byrow = TRUE)

#reclassify
Reclassified_rasters_3col <- lapply(NOAS04_periods_rasters, function(x) reclassify(x, reclass_num_matrix))

#rename
names(Reclassified_rasters_3col) <- paste0(str_replace_all(names(NOAS04_periods_rasters), "NOAS04", "LULC"), "_agg")
  
#add raster attribute table (rat)
LULC_rat <- data.frame(
  ID=10:19, 
  Pixel_value= 10:19,
  lulc_name = c("Urban", "Static", "Open_Forest",
  "Closed_Forest","Shrubland", "Int_AG",
  "Alp_Past", "Grassland", "Perm_crops", "Glacier")
)

Reclassified_rasters_3col_rat <- lapply(Reclassified_rasters_3col, function(x) {
  raster_with_att <- ratify(x)
  levels(raster_with_att) <- LULC_rat
  return(raster_with_att)})

#save
mapply(FUN = writeRaster,
       x = Reclassified_rasters_3col_rat,
       filename =  paste0("Data/Historic_LULC/", names(Reclassified_rasters_3col) , ".grd"),datatype='INT2U', overwrite=T)

#Method 2. 2 col matrix based re-classification  
#Aggregation_scheme <- read.csv("Data/LULC/Dry_run_agg_scheme.csv")
#Reclassified_rasters_2col <- lapply(NOAS04_periods_rasters, function(x) reclassify(x, Aggregation_scheme))
#names(Reclassified_rasters_2col) <- c("LULC_1985_agg", "LULC_1997_agg", "LULC_2009_agg", "LULC_2018_agg")

#Method 3. approach using categories and breaks
#Here the categories object links each aggregated LULC class to the fine scale classes through the
#breaks contained in the cut function below i.e. the first break denoted by the comma selects the NOAS04 classes 1 and 2
#and turns them into the aggregated category 10 which according to my aggregated classes is: Settlement/Urban/Amenities.
#Repeats of the aggregated categories in the list are necessary because the fine categories are not necessarily
#grouped how we would like them to be.

#categories <- c(10,11,10,11,10,18,15,17,16,13,12,13,12,11,14,11,19)

#create seperate data frame for aggregations (requires the .csv file to be read in in section A)     
#Agg_cat <- AS_72

#Assign aggregated classes to each column of historic data
#Agg_cat$AS85R_72 <- categories[as.numeric(cut(Agg_cat$AS85R_72,
#                                             breaks =c(0,14,18,19,28,36,40,41,45,49,53,56,57,60,63,65,71,72)))]
#Agg_cat$AS97R_72 <- categories[as.numeric(cut(Agg_cat$AS97R_72,
#                                             breaks =c(0,14,18,19,28,36,40,41,45,49,53,56,57,60,63,65,71,72)))]
#Agg_cat$AS09_72 <- categories[as.numeric(cut(Agg_cat$AS09_72,
#                                            breaks =c(0,14,18,19,28,36,40,41,45,49,53,56,57,60,63,65,71,72)))]

#Now follow procedure of creating rasters for each period as for NOAS04 rasters above
