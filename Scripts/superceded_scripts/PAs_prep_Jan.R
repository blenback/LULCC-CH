## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Jan Streit
##
## Date Created: 2023-02-07
##
## Copyright (c) Jan Streit, 2023
## Email: jstreit@ethz.ch
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

### =========================================================================
### Selection of Functions
### =========================================================================

PA_prio_crop <- crop(PA_prio, PA)
PA_prio_crop <- crop(PA_prio, extent(PA))
PA_prio_crop <- mask(PA_prio_crop, PA)
PA_prio_crop <- mask(PA_prio_crop, PA, inverse = TRUE)
PA_prio_crop <- gDifference(PA_prio_crop, PA)

### =========================================================================
### Set working directory
### =========================================================================
# mac wd  (USE THIS CODE ON MAC)
setwd("/Volumes/plus_projects/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC_CH/Data/Spat_prob_perturb_layers/Protected_areas")

# Setting the working directory on windows (USE THIS CODE ON WINDOWS)
#setwd("")

### =========================================================================
### Package Set-UP
### =========================================================================

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl","rlist", "tidyverse",
         "rstatix", "Dinamica", "raster", "openxlsx", "pxR", "bfsMaps", "mapview", 
         "sf", "basemaps", "leaflet", "leaflet.providers", "shiny", "shinysurveys", "sfheaders")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

### =========================================================================
### Import Data
### =========================================================================

# Import the shapefiles of the protected areas
library(rgdal)
PA <- readOGR("SwissPA.shp")
PA <- PA[PA$Res_Type == "Regional Nature Park", ]

st_crs(PA)
crs(PA)


# Import the raster file of the prioritization map
PA_prio <- raster(x = "DHM200.asc")

st_crs(PA_prio)
crs(PA_prio) <- "+init=EPSG:4150"

# Import the the perimeter of Switzerland
ogrListLayers(dsn="/Volumes/plus_projects/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC_CH/Data/Spat_prob_perturb_layers/Protected_areas/swissBOUNDARIES3D_1_4_LV95_LN02.gdb")

swiss_perimeter <- readOGR(dsn = "/Volumes/plus_projects/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC_CH/Data/Spat_prob_perturb_layers/Protected_areas/swissBOUNDARIES3D_1_4_LV95_LN02.gdb",
                           layer = "TLM_LANDESGEBIET")

mapview(PA_prio) + mapview(PA) + mapview(swiss_perimeter)

### =========================================================================
### Crop PA Priority with Existing PA's
### =========================================================================

library(rgdal)

PA_prio_crop <- mask(PA_prio, PA, inverse = TRUE)

PA_prio_cut <- mask(PA_prio, PA, inverse=TRUE, updatevalue=NA, updateNA=FALSE)

PA_prio_crop <- terra::mask(PA_prio_crop, swiss_perimeter, inverse = TRUE)

mapview(PA_prio_crop) + mapview(PA) + mapview(swiss_perimeter)


mapview(PA_prio) + mapview(PA)
mapview(PA_prio_crop)

?mask


PA_prio_crop <- crop(PA_prio, PA)
PA_prio_crop <- crop(PA_prio, extent(PA))

PA_prio_crop <- gDifference(PA_prio_crop, PA)





### =========================================================================
### Manuel
### =========================================================================

library(terra)
setwd("E:/LULCC_CH/Data/Spat_prob_perturb_layers/Protected_areas")

ProjCH <- "+proj=somerc +init=epsg:2056"
PA <- vect("SwissPA.shp")

Biodiv_grid_path <- paste0("prioritization_no_PAs.tif")
Biodiv_prio <- rast(Biodiv_grid_path)

#Because of different resolutions and projections I'm using prioritzation as ref_grid too
#Ref_grid_path <- ("Data/Ref_grid.tif")
Ref_grid_path <- "prioritization_no_PAs.tif"
Ref_grid <- rast(Ref_grid_path)

#Get the PA-categories and subset the shp file accordingly
subset_rows <- PA$Res_Type %in% c("Ramsar","Swiss National Park","Unesco_BiosphereReserve","Unesco_CulturalSites","Unesco_NaturalSites","ProNatura reserves", "Emeraude")
PA_BAFU <- PA[subset_rows, ]
PA_BAFU = project(PA_BAFU, ProjCH)

#Make a rasterized version of the protected areas (cropped twice because of different extents)
x <- crop(Ref_grid, ext(PA_BAFU))
PA_BAFU_raster <- mask(x, PA_BAFU)
Biodiv_prio <- crop(Biodiv_prio, ext(PA_BAFU_raster))

#Set cellvalues to zero if they are in protected area
Biodiv_prio_wo_pa <- ifel(is.na(PA_BAFU_raster), Biodiv_prio, 0)
#writeRaster(Biodiv_prio_wo_pa,file=paste0(getwd(),"/Prio_without_PA.tif"), overwrite=TRUE)
#writeRaster(PA_BAFU_raster,file=paste0(getwd(),"/PA_BAFU_raster.tif"), overwrite=TRUE)

#Calculating the number of cells needed for protection goals
n_prot <- global(PA_BAFU_raster, fun="notNA")
area_ch = 41285*1000000
n_goal = 0.30 #percent of total area that should be protected (not 100% certain about 18%)
n_todo = (n_goal - (n_prot*10000)/area_ch)$notNA
n_cells = ceiling(n_todo*area_ch/10000) #used for calculation below

#dataframe for visualization is not needed in calculation
t = data.frame(table(cut(values(Biodiv_prio_wo_pa), breaks=seq(0.85, 1.0, by=0.01))))
t$area=t$Freq*10000
t$share=t$area/area_ch
t$current_sum <- rev(cumsum(rev(t$share)))
t$diff <- n_todo$notNA-t$current_sum #shows in which value range the goal can be reached


#Equal amount of additional patches per timestep, using overall the best patches
#Get the n_cells with highest value
n_cells_values <- sort(values(Biodiv_prio_wo_pa), decreasing = TRUE)[1:n_cells]
Biodiv_prio_wo_pa[!(Biodiv_prio_wo_pa %in% n_cells_values)] <- NA # Set the rest of the cells to NA
writeRaster(Biodiv_prio_wo_pa,file=paste0(getwd(),"/Prio_bestpatches.tif"), overwrite=TRUE) #Raster with all the best cells, that would be enough to reach the desired share of protected area




