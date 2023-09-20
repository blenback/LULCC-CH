#############################################################################
## Dinamica_trans_potent_calc_parallel: Testing parallelization of transition 
## potential calculation
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "doParallel",
         "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest", "Dinamica")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Load in the grid file we are using for spatial extent and CRS
Ref_grid <- raster("Data/Ref_grid.gri")

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

#Receive current simulation time
Simulation_time_step <- v1
#Simulation_time_step <- 2020

#simulation number being performed
Simulation_num <- v2
#Simulation_num <- "1"

#load table of simulations
Control_table_path <- s1
#Control_table_path <- "E:/LULCC_CH/Tools/Calibration_control.csv"
Simulation_table <- read.csv(Control_table_path)[Simulation_num,]

#Enter name of Scenario to be tested as string or numeric (i.e. "BAU" etc.) 
Scenario_ID <- Simulation_table$Scenario_ID.string

#Enter an ID for this run of the scenario (e.g V1)
Simulation_ID <- Simulation_table$Simulation_ID.string

#Define model_mode: Calibration or Simulation
Model_mode <- Simulation_table$Model_mode.string

#Receive folder path for loading simulated LULC maps 
File_path_simulated_LULC_maps <- s2
#File_path_simulated_LULC_maps <- "E:/LULCC_CH/Results/Dinamica_simulated_LULC/CALIBRATION/v1/simulated_LULC_scenario_CALIBRATION_simID_v1_year_"

#Convert model mode into a string related to the model period being used
#this makes it easier to load files because they use this nomenclature

Period_tag <- if(grepl("calibration", Model_mode, ignore.case = TRUE)&
                 Simulation_time_step <= 1997
                 ){"1985_1997"}else if (grepl("calibration", Model_mode, ignore.case = TRUE)&
                 Simulation_time_step >= 1997 &
                 Simulation_time_step <= 2009
                 ){"1997_2009"}else{"2009_2018"} 
#The last clause covers when calibration is occuring between 2009 and 2018
#and when the model is in simulation mode

#create folder for saving prediction probability maps
prob_map_folder <- paste0(wpath, "/Results/Pred_prob_maps/", Scenario_ID, "/", Simulation_ID, "/", Simulation_time_step)
dir.create(prob_map_folder, recursive = TRUE)

### =========================================================================
### C- Retrieve current LULC map and split into regionalized transition layers
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- paste0(File_path_simulated_LULC_maps, Simulation_time_step, ".tif")

#load current LULC map 
Current_LULC <- raster(current_LULC_path)

#vector LULC labels
LULC_labels <- c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier")
names(LULC_labels) <- c(10,11,12,13,14,15,16,17,18,19)

#vector LULC values that are present in the current map and get the class names
#(in case not all classes are present anymore during simulation)
LULC_values_current<- unique(Current_LULC)
LULC_labels_current <- LULC_labels[which(LULC_values_current %in% names(LULC_labels))]

#layerize data (columns for each LULC class)
LULC_data <- raster::layerize(Current_LULC)
names(LULC_data) <- LULC_labels_current


### =========================================================================
### D- Load Suitability and Accessibility predictors and attach to LULC data
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Stacks/Calibration", pattern = Period_tag, full.names = TRUE))
} #close if statement calibration

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#get file path by matching on scenario ID and time step 
SA_pred_stack <- readRDS(list.files("Data/Preds/Prepared/Stacks/Simulation/SA_preds/SA_pred_stacks", pattern = paste0(Scenario_ID, "_", Simulation_time_step),  full.names = TRUE))
} #close simulation if statement

### =========================================================================
### E.1- Dynamic predictors: Municipal population
### =========================================================================

#for calibration the raster stacks already contain the dynamic predictor layers so there
#is nothing to be done

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){

#create population data layer
#subset current LULC to just urban cells
Urban_rast <- Current_LULC == 10

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

#estimate % of predicted cantonal population per municipality
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
Pop_prediction_table <- openxlsx::read.xlsx("Data/Preds/Tools/Population_projections.xlsx", sheet = Pop_scenario)

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

#add estimated population to @data table of polygons and then rasterize
Muni_shp@data$Pop_est <- Muni_urban_areas$Pop_est
pop_raster <- raster::rasterize(x = Muni_shp, y = Ref_grid, field = "Pop_est", background = NAvalue(Ref_grid))
names(pop_raster) <- "Muni_pop" #TO DO: THIS MUST BE THE LAYER NAME IN THE CALIBRATION STACKS/MODELS

#clean up
rm(Canton_shp, Canton_urban_areas, Can_urban_area, canton_model, Muni_shp,
   Muni_urban_areas, munis_indices, pop_models, Pop_prediction_table,
   Urban_rast)

### =========================================================================
### E.2- Dynamic predictors: Neighbourhood predictors
### =========================================================================

#load matrices used to create focal layers
Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

#adjust matrix names
names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) {split_name <- (str_split(x, "[.]"))[[1]][2]})

#create a folder path using simulation ID and time step 
Dynamic_focal_folder_path <- paste0("Data/Preds/Prepared/Stacks/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)
    
#create directory
dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)

#Load details of focal layers required for the model set being utilised 
Required_focals_details <- readRDS(list.files("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating", pattern = Period_tag, full.names = TRUE))

#Loop over details of focal layers required creating a list of rasters from the current LULC map
Nhood_rasters <- list()
for(i in 1:nrow(Required_focals_details)){
#vector active class names
Active_class_name <- Required_focals_details[i,]$active_lulc   
  
#get pixel values of active LULC class
Active_class_value <- as.numeric(names(LULC_labels[LULC_labels == Active_class_name]))

#subset LULC raster by all Active_class_value
Active_class_raster_subset <- Current_LULC == Active_class_value

#create focal layer using matrix
Focal_layer <- focal(x=Active_class_raster_subset, w= Focal_matrices[[Required_focals_details[i,]$matrix_id]], na.rm=FALSE, pad=TRUE, padValue=0, NAonly=FALSE) 

#create file path for saving this layer  
Focal_name <- paste(Active_class_name, "nhood", Required_focals_details[i,]$matrix_id, sep = "_") 
Nhood_rasters[Focal_name] <- Focal_layer

#steps for saving of rasters if needed  
#Focal_file_name <- paste(Scenario_ID, Simulation_time_step, Required_focals_details[i,]$active_lulc, "nhood", Focal_matrices[Required_focals_details[i,]$matrix_id], sep = "_")
#Focal_full_path <- paste0(Dynamic_focal_folder_path, "/", Focal_file_name, ".grd") #create full folder path
#writeRaster(Focal_layer, Focal_full_path ,datatype='INT2U', overwrite=TRUE) #save layer
}

rm(Focal_matrices, Focal_layer, Focal_name, Required_focals_details,
   Active_class_raster_subset, Active_class_name, Active_class_value)
} #close if statement for dynamic predictor prep
  
### =========================================================================
### F- Combine LULC, SA_preds and Nhood_preds and extract to dataframe
### =========================================================================

#Stack all rasters
#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
Trans_data_stack <- stack(LULC_data, SA_pred_stack)
names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers))
} #close if statement calibration

#For simulation mode
#only stack the Nhood_rasters here because otherwise they were not including in the upper stack function
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#load the raster of Bioregions
Bioregion_rast <- raster("Data/Bioreg_CH/Bioreg_raster.gri")
names(Bioregion_rast) <- "Bioregion"
Trans_data_stack <- stack(LULC_data, SA_pred_stack, pop_raster, stack(Nhood_rasters), Bioregion_rast)
names(Trans_data_stack) <- c(names(LULC_data), names(SA_pred_stack@layers), names(pop_raster), names(Nhood_rasters), names(Bioregion_rast))
} #close simulation if statement
  
#Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
Trans_dataset <- raster::as.data.frame(Trans_data_stack) 

#add ID column to dataset
Trans_dataset$ID <- seq.int(nrow(Trans_dataset))

#Get XY coordinates of cells
xy_coordinates <- coordinates(Trans_data_stack) 

#cbind XY coordinates to dataframe and seperate rows where all values = NA
Trans_dataset <- cbind(Trans_dataset, xy_coordinates) 

#release memory
rm(LULC_data, SA_pred_stack, Nhood_rasters, Trans_data_stack, obs, xy_coordinates)

### =========================================================================
### G- Run transition potential prediction with RF for each transition
### =========================================================================

#load model look up
Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)

#seperate Trans_dataset into complete cases for prediction
#and NAs ()background values
Trans_dataset_complete <- Trans_dataset %>%
  filter(complete.cases(.))

Trans_dataset_na <- Trans_dataset %>%
  filter(!complete.cases(.))  
Trans_dataset_na <- Trans_dataset_na[, c("x", "y", "ID")]

rm(Trans_dataset)

#seperate ID, x/y and Initial_LULC cols to append results to
Prediction_probs <- Trans_dataset_complete[, c("ID", "x", "y", unique(Model_lookup$Initial_LULC), "Static")]

#add cols to both the complete and NA data to capture predict probabilities to each class
Final_LULC_classes <- unique(Model_lookup$Final_LULC)
for(i in Final_LULC_classes){
Prediction_probs[[paste0("Prob_", i)]] <- 0
Trans_dataset_na[[paste0("Prob_", i)]] <- NA
}

#loop over transitions
for(i in 1:nrow(Model_lookup)){

#vector trans_name
Trans_name <- Model_lookup[i, "Trans_name"]
Trans_ID <- Model_lookup[i, "Trans_ID"]
Region <- Model_lookup[i,"Region"]
Final_LULC <- Model_lookup[i, "Final_LULC"]
Initial_LULC <- Model_lookup[i, "Initial_LULC"]

#print status message
cat(paste0("predicting probabilities for transitions from ", Initial_LULC, " to ", Final_LULC, " within region: ", Region, "\n"))

#load model
Fitted_model <- readRDS(Model_lookup[i, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioregion_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

#bind to ID
predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

#append the predictions at the correct rows in the results df
Prediction_probs[which(Prediction_probs$ID %in% predict_ID$ID), paste0("Prob_", Final_LULC)] <- predict_ID[paste0("Prob_", Final_LULC)]
} #close loop over Models


### =========================================================================
### G- Re-scale predictions
### =========================================================================

#loop over rows and re-scale probability values so that they sum to 1
#(by dividing by multiplying by 1 and then dividing by the sum of the vector)

#vector row indices with non-zero sums of transition probabilities
Non_zero_indices <- which(rowSums(Prediction_probs[,grep("Prob_", names(Prediction_probs), value = TRUE)]) != 0)

#Transition probability columns to re-scale
Pred_prob_columns <- grep("Prob_", names(Prediction_probs), value = TRUE)

#Loop over rows performing re-scaling
Prediction_probs[Non_zero_indices ,Pred_prob_columns] <- as.data.frame(t(apply(Prediction_probs[Non_zero_indices,Pred_prob_columns], MARGIN = 1, FUN = function(x) {
sapply(x, function(y) {
value <- y*1/sum(x)
value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
return(value)
})
})))

#bind with background values
Trans_dataset_na[setdiff(names(Prediction_probs), names(Trans_dataset_na))] <- NA
Raster_prob_values <- rbind(Prediction_probs, Trans_dataset_na)

#sort by ID
Raster_prob_values[order(Raster_prob_values$ID),]

### =========================================================================
### H- Spatially adjust transition probabilities
### =========================================================================

#NOW WOULD BE THE TIME TO SPATIALLY ADJUST PROBABILITY VALUES USING POLYGONS/RASTERS
#BEFORE SAVING THEM BECAUSE RE-SCALING WILL NEED TO BE REPEATED AFTER ADJUSTMENT.   

### =========================================================================
### I- Save transition rasters
### =========================================================================

#subset model_lookup table to unique trans ID
Unique_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID), ]

#Loop over unique trans using details to subset data and save Rasters
for(i in 1:nrow(Unique_trans)){
Trans_ID <- Unique_trans[i, "Trans_ID"]
Final_LULC <- Unique_trans[i, "Final_LULC"]
Initial_LULC <- Unique_trans[i, "Initial_LULC"]

#subset data
Trans_raster_values <- Raster_prob_values[,c("x", "y", paste0("Prob_", Final_LULC))]

#rasterize and save using Initial and Final class names
Prob_raster <- rasterFromXYZ(Trans_raster_values, crs = crs(Current_LULC))

#vector file path for saving probability maps
prob_map_path <- paste0(prob_map_folder, "/", Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".tif")

writeRaster(Prob_raster, prob_map_path, overwrite=T)
} #close loop over transitions 


#Return the probability map folder path as a string to
#Dinamica to indicate completion
#Note strings must be vectorized for 'outputString to work

outputString("probmap_folder_path", prob_map_folder)