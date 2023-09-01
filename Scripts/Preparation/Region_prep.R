#############################################################################
## Region_prep: Preparing Rasters of Bioregions in Switzerland to 
## sub-divide data for regional modelling 
## This script is adapted from the process of Gerecke et al. 2019
## Date: 06-10-2021
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat",
#          "sjmisc", "tictoc", "parallel", "terra", "pbapply", "sp", "rgdal")
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

#define projection systems
Ref_grid <- raster(Ref_grid_path)
Ref_crs <- crs(Ref_grid)

### =========================================================================
### B- Load data and Rasterize
### =========================================================================

#Load in an exemplar Raster of the correct res/extent etc. 
LULC_2009 <- raster("Data/LULC/NOAS04_LULC/rasterized/NOAS04_2009.tif")

#Download data
Bioreg_dir <- "Data/Bioreg_CH"
lulcc.downloadunzip(url = "https://data.geo.admin.ch/ch.bafu.biogeographische_regionen/data.zip",
                    save_dir = Bioreg_dir)

#Load in shapefile of Bioregions of Switzerland
bioreg.shp <- readOGR(dsn="Data/Bioreg_CH/BiogeographischeRegionen", layer = "N2020_Revision_BiogeoRegion")

#project to new CRS
bioreg.shp <- spTransform(bioreg.shp, CRSobj = Ref_crs)

#save shapefile in new CRS as an .rds file for easy use in R
write_rds(bioreg.shp, file = "Data/Bioreg_CH/Bioreg_shape.rds")

#necessary to set the regionNumm column as numeric data type in order to Rasterise
bioreg.shp$RegionNumm <- as.numeric(bioreg.shp$RegionNumm)

bioreg_6.r <- rasterize(bioreg.shp, LULC_2009, field=bioreg.shp$RegionNumm)
bioreg_6.r2 <- mask(bioreg_6.r,LULC_2009)
bioreg_6.r3 <- focal(bioreg_6.r2, w=matrix(1,3, 3), fun=modal, na.rm=TRUE, NAonly=TRUE)
bioreg_6.r3 <- focal(bioreg_6.r3, w=matrix(1,3, 3), fun=modal, na.rm=TRUE, NAonly=TRUE)
bioreg_6.r4 <- mask(bioreg_6.r3,LULC_2009)

#writing the raster as a .tif file because .grd files cannot be loaded into Dinamica
writeRaster(bioreg_6.r4, file="Data/Bioreg_CH/Bioreg_raster.tif", overwrite=T) 

#Create a table from the shape file columns to link region numbers with their descriptions
Region_names <- data.frame(bioreg.shp$RegionNumm, bioreg.shp$DERegionNa)

#remove duplicates
Region_names %>% distinct(bioreg.shp.RegionNumm, .keep_all = TRUE)

#using these names to create a raster attribute table (rat)
bioreg_6.r4 <- ratify(bioreg_6.r4)
rat_bioregions <- levels(bioreg_6.r4)[[1]]
rat_bioregions$Pixel_Values <- c(1, 2, 3, 4, 5, 6)
rat_bioregions$Class_Names <- c("Jura", "Plateau", "Northern_Prealps", "Southern_Prealps", "Western_Central_Alps", "Eastern_Central_Alps")
levels(bioreg_6.r4) <- rat_bioregions

#saving the rat as .csv in  case it needs to be loaded in with the tif. later
write.csv(rat_bioregions, file="Data/Bioreg_CH/Bioregion_rat", row.names = F)

#saving the raster in R's native .grd format which preserves the attribute table
writeRaster(bioreg_6.r4, filename="Data/Bioreg_CH/Bioreg_raster.grd", overwrite = TRUE)

cat(paste0(' Preparation of Bio-regions raster complete \n'))

