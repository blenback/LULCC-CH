#############################################################################
## Muni_urban_area_calc: Prep work calculating %of urban areas in municipalities
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl","rlist", "tidyverse",
         "rstatix", "Dinamica", "raster", "openxlsx")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Load one of the historic LULC maps to use as a dummy layer
current_LULC <- raster("E:/LULCC_CH/Data/Historic_LULC/LULC_2018_agg.gri")

#Load in the grid file we are using for spatial extent and CRS
Ref_grid <- raster("Data/Ref_grid.gri")

### SIMULATION STEP PROCESS BEGIN HERE ###

### =========================================================================
### A- Population: calculate % urban area per municipality
### =========================================================================

#subset current LULC to just urban cells
Urban_rast <- current_LULC == 10

#load canton shapefile
Canton_shp <- shapefile("PopulationModel_Valpar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

#Zonal stats to get urban area per kanton
Canton_urban_areas <- raster::extract(Urban_rast, Canton_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Kanton ID 
Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM

#combine areas for cantons with multiple polygons
Canton_urban_areas <- Canton_urban_areas %>%
  group_by(Canton_num) %>%
  summarise(across(c(layer), sum))

#load the municipality shape file
Muni_shp <- shapefile("PopulationModel_Valpar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ] 

#Zonal stats to get number of Urban cells per Municipality polygon
#sum is used as a function because urban cells = 1 all others = 0
Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Kanton and Municipality IDs
Muni_urban_areas$Canton_num <- Muni_shp@data[["KANTONSNUM"]]
Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
Muni_urban_areas$Perc_urban <- 0

#loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
for(i in Canton_urban_areas$Canton_num){

#vector kanton urban area
Can_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "layer"])  

#subset municipalities to this canton number 
munis_indices <- which(Muni_urban_areas$Canton_num == i)

#loop over municipalities in the Kanton and calculate their urban areas as a % of the Canton's total  
for(muni in munis_indices){
Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "layer"]/Can_urban_area)*100
  } #close inner loop 
} #close outer loop

### =========================================================================
### B- Population: estimate % of predicted cantonal population per municipality
### =========================================================================

#Load list of cantonal population models
pop_models <- readRDS("Data/Preds/Tools/Dynamic_pop_models.rds")

#add predicted % pop results column
Muni_urban_areas$Perc_pop <- 0

#estimate % of pop per municipality
#loop over unique polygon IDs rather than BFS numbers to take into account that
#multiple polygons have the same BFS number
for(i in Muni_urban_areas$ID){

#seperate canton specific model 
canton_model <- pop_models[[Muni_urban_areas[Muni_urban_areas$ID == i, "Canton_num"]]]

#perform prediction
Muni_urban_areas$Perc_pop[[i]] <- predict(canton_model, newdata = Muni_urban_areas[Muni_urban_areas$ID == i, ]) 
}

#Our scenarios rely on specific population projects from FSO ("Ref", "High", "Low")
# Identify which population scenario is required according to scenario being simulated
Pop_scenario <- if(grepl("BIOPRO", Scenario_ID, ignore.case = TRUE)){"Low"} else if(
  grepl("SHAD", Scenario_ID, ignore.case = TRUE)){"High"}else{"Ref"} #Final else clause covers scenarios: "DIV", "BAU", "FUTEI"

#load correct sheet of future population predictions according to scenario
Pop_prediction_table <- read.xlsx("Data/Preds/Tools/Population_projections.xlsx", sheet = Pop_scenario)

#loop over unique kanton numbers, rescaled the predicted population percentages
#and calculate the estimated population per municipality as a % of the cantonal total 

#add results column
Muni_urban_areas$Pop_est <- 0

for(i in unique(Muni_urban_areas$Canton_num)){

#subset to predicted cantonal population percentages
Canton_dat <- Muni_urban_areas[Muni_urban_areas$Canton_num == i, "Perc_urban"]  

#loop over the municipalites re-scaling the values
Canton_preds_rescaled <- sapply(Canton_dat, function(y) {
value <- y*1/sum(Canton_dat)
value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
return(value)}) #close inner loop

#get the projected canton population value for this time point
Canton_pop <- Pop_prediction_table[Pop_prediction_table$Canton_num == i, paste(Simulation_time_step)]

#loop over the rescaled values calculating the estimated population
Muni_indices <- which(Muni_urban_areas$Canton_num == i)
Muni_urban_areas$Pop_est[Muni_indices] <- sapply(Canton_preds_rescaled, function(x) {
pop_value <- Canton_pop*x #% already expressed as decimal so no need to /100  
}) #close loop over municipalities

} #close loop over cantons

### =========================================================================
### C- Population: Spatialize municipality population predictions
### =========================================================================

#add estimated population to @data table of polygons and then rasterize
Muni_shp@data$Pop_est <- Muni_urban_areas$Pop_est
pop_raster <- rasterize(Muni_shp, Ref_grid, field = "Pop_est")


