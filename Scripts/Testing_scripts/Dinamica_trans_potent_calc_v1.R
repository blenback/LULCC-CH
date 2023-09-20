#############################################################################
## Dinamica_trans_potent_calc: Calculating transition potential maps using random forests
## for use in LULCC allocation within Dinamica EGO
## Date: 25-02-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
system.time(wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "parallel",
         "lulcc", "pbapply", "stringr", "readr", "xlsx", "randomForest")

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

### =========================================================================
### B- Receive information from Dinamica
### =========================================================================

#Receive Scenario ID
#Scenario_ID <- s3
Scenario_ID <- "BAU"

#Receive current simulation time
#Simulation_time_step <- v1
Simulation_time_step <- 1985

#Receive simulation ID
#Simulation_ID <- s4 
Simulation_ID <- "V1"

#Receive folder path for loading simulated LULC maps 
#File_path_simulated_LULC_maps <- s2
File_path_simulated_LULC_maps <- "Results/Dinamica_simulated_LULC/BAU/V1/simulated_LULC_scenario_BAU_simID_V1_year_<v1>"

#Receive model mode: Calibration or simulation
#Model_mode <- s1
Model_mode <- "calibration"

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
prob_map_folder <- paste0("Results/Pred_prob_maps/", Scenario_ID, "/", Simulation_ID, "/", Simulation_time_step, "/")
dir.create(prob_map_folder, recursive = TRUE)

### =========================================================================
### C- Retrieve current LULC map and split into regionalized transition layers
### =========================================================================

#replace Dinamica escape character in file path with current time step
current_LULC_path <- str_replace(File_path_simulated_LULC_maps, "<v1>", paste0(Simulation_time_step))

#load current LULC map 
Current_LULC <- raster(current_LULC_path)

#vector LULC labels
LULC_labels <- c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier")
names(LULC_labels) <- c(10,11,12,13,14,15,16,17,18,19)


#Load Bioregions raster
# Region_raster <- raster(list.files("Data/Bioreg_CH",pattern = ".grd", full.names = TRUE, include.dirs = TRUE, recursive=TRUE))
# 
# #Bioregions raster already has an attribute table so no need to create one
# #split Bioregional raster by using values from attribute table
# Sep_region_rasters <- lapply(Region_raster@data@attributes[[1]][["ID"]], function(x) mask(Region_raster, Region_raster != x, maskvalue=1))
# names(Sep_region_rasters) <- Region_raster@data@attributes[[1]][["Class_Names"]]
# 
# #split Current LULC by Bioregion 
# LULC_by_Bioregion <- lapply(Sep_region_rasters, function(x) mask(Current_LULC, x))

# #split Bioregional rasters by LULC classes and unlist to a single list
# Bioregion_rasters_by_LULC <- unlist(lapply(LULC_by_Bioregion, function(y) {
#   sep_lulc_rasters <- lapply(Current_LULC@data@attributes[[1]][["ID"]], function(x) mask(y, y != x, maskvalue=1))
#   names(sep_lulc_rasters) <- Current_LULC@data@attributes[[1]][["lulc_name"]]
#   return(sep_lulc_rasters)}), use.names = TRUE, recursive = FALSE)
# 
# #remove rasters where all values are NA (i.e. LULC class does not occur in region e.g glacier)
# NA_identify <- lapply(Bioregion_rasters_by_LULC, function(individual_raster) {
#   if(all(is.na(individual_raster@data@values))) {individual_raster <- NULL}
#      return(individual_raster)
#      })
# 
# Bioregion_rasters_by_LULC <- NA_identify[-which(sapply(NA_identify, is.null))]
# 
# rm(NA_identify)

#vector LULC values that are present in the current map and get the class names
#(in case not all classes are present anymore during simulation)
LULC_values_current<- unique(Current_LULC)
LULC_labels_current <- LULC_labels[which(LULC_values_current %in% names(LULC_labels))]

#convert to ObsLulcRasterStack object using function from lulcc package
obs <- ObsLulcRasterStack(x= Current_LULC,
                           categories= LULC_values_current, 
                           labels= LULC_labels_current, 
                         t= Simulation_time_step)

#create spatial points from data (currently un-needed)
#spat_points <- rasterToPoints(obs[[1]], spatial = TRUE)

#layerize data (columns for each LULC class) and convert to DF
LULC_data <- raster::layerize(obs@layers[[1]])
names(LULC_data) <- obs@labels
#LULC_df <- as.data.frame(raster::extract(x=LULC_data, y=part[["all"]]))

### =========================================================================
### D- Load Suitability and Accessibility predictors and attach to LULC data
### =========================================================================

#use model mode string to select correct folder

#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
SA_pred_stack <- readRDS(list.files("Data/Preds/Calibration/SA_preds/SA_pred_stacks", pattern = Period_tag, full.names = TRUE))
} #close if statement calibration

# OLD VERSION BEFORE MODIFICATION OF PERIOD TAG
#using time step and numeric between 
# if (grepl("calibration", Model_mode, ignore.case = TRUE)){
# SA_stack_names <- list.files("Data/Preds/Calibration/SA_preds/SA_pred_stacks", full.names = TRUE)
# 
# #identify which historic data period is required
# Select_period <- sapply(SA_stack_names, function(x) {
#   start_end <- as.numeric(str_extract_all(x, "\\d+", simplify = TRUE))
# if(Simulation_time_step >= start_end[1] && Simulation_time_step <= start_end[2]){TRUE} else{FALSE}
# }, simplify = FALSE)
# 
# #load raster stack 
# SA_pred_stack <- readRDS(names(Select_period[which(Select_period== TRUE)]))
# } #close if statement calibration
# 

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#get file path by matching on scenario ID and time step 
SA_pred_stack <- readRDS(list.files("Data/Preds/Simulation/SA_preds/SA_pred_stacks", pattern = paste0(Scenario_ID, "_", Simulation_time_step),  full.names = TRUE))
} #close simulation if statement

### =========================================================================
### E- Load or create neighbourhood predictors
### =========================================================================

#for calibration the raster stacks already contain the nhood layers so there
#is nothing to be done

#for simulation the nhood layers need to be created from the current LULC map
#Two possibilities: 1. create all realisations 
#or 2. create only those realisations that are needed i.e. were retained after feature selection  

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  
#for both approaches:
#load matrices used to create focal layers
Focal_matrices <- unlist(readRDS("Data/Preds/Tools/Neighbourhood_matrices/ALL_matrices"), recursive = FALSE)

#adjust matrix names
names(Focal_matrices) <- sapply(names(Focal_matrices), function(x) {split_name <- (str_split(x, "[.]"))[[1]][2]})

#create a folder path using simulation ID and time step 
Dynamic_focal_folder_path <- paste0("Data/Preds/Simulation/NH_preds", "/", Scenario_ID, "/", Simulation_time_step)
    
#create directory
dir.create(paste(wpath, Dynamic_focal_folder_path, sep = "/"), recursive = TRUE)

# Approach 1
#provide vector of active LULC class names
#Active_class_names <- c('Urban', 'Int_AG', 'Alp_Past', 'Grassland', 'Perm_crops')

#generate rasters and save
# lulcc.generatenhoodrasters(LULC_raster = Current_LULC,
#                            Data_period = paste0(Scenario_ID, "_", Simulation_time_step),
#                            Neighbourhood_matrices = Focal_matrices,
#                            Active_LULC_class_names = Active_class_names,
#                            Nhood_folder_path = paste0(Dynamic_focal_folder_path, "/"))


#approach 2
#Load details of focal layers required for the model set being utilised 
Required_focals_details <- readRDS(list.files("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating", pattern = Period_tag, full.names = TRUE))

for (i in 1:nrow(Required_focals_details)){
  lulcc.generatenhoodrasters(LULC_raster = Current_LULC,
                             Neighbourhood_matrices = Focal_matrices[Required_focals_details[i,]$matrix_id],
                             Active_LULC_class_names = Required_focals_details[i,]$active_lulc,
                             Data_period = paste0(Scenario_ID, "_", Simulation_time_step),
                             Nhood_folder_path = paste0(Dynamic_focal_folder_path, "/"))
  }

#load raster stacks for each period
nhood_pred_stack <- stack(list.files(Dynamic_focal_folder_path, full.names = TRUE, pattern = ".gri"))

#alter name to generalized form
redact <- paste(c(paste0(Scenario_ID, "_", Simulation_time_step, "_"), ".gri"), collapse = "|")
names(nhood_pred_stack@layers) <- str_remove_all(list.files(Dynamic_focal_folder_path, full.names = FALSE, pattern = ".gri"), redact)
} #close simulation if statement

### =========================================================================
### F- Combine LULC, SA_preds and Nhood_preds and extract to dataframe
### =========================================================================

#Stack all rasters
#For calibration mode (matching on Period_tag)
if (grepl("calibration", Model_mode, ignore.case = TRUE)){
Trans_data_stack <- stack(LULC_data, SA_pred_stack)
names(Trans_data_stack@layers) <- c(names(LULC_data), names(SA_pred_stack))
} #close if statement calibration

names(Trans_data_stack@layers)

#For simulation mode
if (grepl("simulation", Model_mode, ignore.case = TRUE)){
  Trans_data_stack <- stack(LULC_data, SA_pred_stack, nhood_pred_stack)
names(Trans_data_stack@layers) <- c(names(LULC_data), names(SA_pred_stack), names(nhood_pred_stack))
} #close simulation if statement
  
#extract to dataframe

#OLD: Points based extraction method
#Trans_dataset <- as.data.frame(raster::extract(x=Trans_data_stack, y= spat_points))

#Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
Trans_dataset <- raster::as.data.frame(Trans_data_stack) 

#rename the LULC cols 
names(Trans_dataset)[1:10] <- names(Trans_data_stack@layers[1:10])

#add ID column to dataset
Trans_dataset$ID <- seq.int(nrow(Trans_dataset))

#Get XY coordinates of cells
xy_coordinates <- coordinates(Trans_data_stack) 

#cbind XY coordinates to dataframe and seperate rows where all values = NA
Trans_dataset <- cbind(Trans_dataset, xy_coordinates) 

#seperate Bioregion raster attribute table
#region_table <- Trans_data_stack@layers[["Bioreg_raster"]]@data@attributes[[1]]

#convert Bioregion class values into names
#not necessary with dataframe conversion as the raster attribute table gets used
#Trans_dataset$Bioregion_class <- sapply(Trans_dataset$Bioreg_raster, function(x) region_table$Class_Names[region_table$Pixel_Values == x])

#release memory
rm(LULC_data, SA_pred_stack, Trans_data_stack, points, region_table, obs, xy_coordinates)

#temporarily save a copy of the df for testing
#saveRDS(Trans_dataset, "Data/Temporary/Trans_dataset_test.rds")
#Trans_dataset <- readRDS("Data/Temporary/Trans_dataset_test.rds")

### =========================================================================
### G- Run transition potential prediction with RF for each transition
### =========================================================================

#load model look up
Model_lookup <- read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period_tag)

#exclude persistence models
#Model_lookup <- Model_lookup[!Model_lookup$Initial_LULC == Model_lookup$Final_LULC,]

#seperate Trans_dataset into complete cases for prediction
#and NAs ()background values
Trans_dataset_complete <- Trans_dataset %>%
  filter(complete.cases(.))

Trans_dataset_na <- Trans_dataset %>%
  filter(!complete.cases(.))  

Trans_dataset_na <- Trans_dataset_na[, c("x", "y", "ID")]

#lulcc.createprobmaps <- function(Model_lookup, Trans_dataset){}

#vector unique transition names
Trans_names <- unique(Model_lookup$Trans_name)

#loop over transitions
sapply(Trans_names, function(Transition){

#vector transition ID for file naming
Trans_ID <- unique(Model_lookup[Model_lookup$Trans_name == Transition, "Trans_ID"])

#subset model lookup table by transition name
Regional_trans_models <- Model_lookup[Model_lookup$Trans_name == Transition,]
Final_LULC <- Regional_trans_models[1, "Final_LULC"]
Initial_LULC <- Regional_trans_models[1, "Initial_LULC"]

#capture non-prediction rows (i.e. other initial LULC classes)
non_pred_data <- Trans_dataset_complete[!Trans_dataset_complete[Initial_LULC] == 1,]

#add final lulc class column to non-pred data
non_pred_data <- non_pred_data[,c("x", "y", "ID")]
non_pred_data[Final_LULC] <- 0

#loop over regional transition models, predicting probabilities and combine output as DF
Regional_predictions <- rbindlist(lapply(1:nrow(Regional_trans_models), function(trans_model){

#vector aspects of the iteration for readability
Region <- Regional_trans_models[trans_model, "Region"]

#print status message
cat(paste0("predicting probabilities for transitions from ", Initial_LULC, " to ", Final_LULC, " within region: ", Region, "\n"))

#load model
Fitted_model <- readRDS(Regional_trans_models[trans_model, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioreg_raster_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- Final_LULC

#bind to ID
predict_ID <- cbind(pred_data[,c("x","y", "ID")], prob_predicts[Final_LULC])

return(predict_ID)
}), idcol = NULL) #close loop over regional transition models

#append predictions with non_pred_data
Prediction_values <- rbind(Regional_predictions, non_pred_data)

#bind with NAs
background_NAs <- Trans_dataset_na
background_NAs[setdiff(names(Prediction_values), names(background_NAs))] <- NA
Raster_prob_values <- rbind(Prediction_values, background_NAs)

#sort by ID and then remove
Raster_prob_values[order(Raster_prob_values$ID),]
Raster_prob_values$ID <- NULL

#rasterize and save using Initial and Final class names
Prob_raster <- rasterFromXYZ(Raster_prob_values, crs = crs(Current_LULC))

#create folder for saving probability maps
prob_map_path <- paste0(prob_map_folder, Trans_ID, "_probability_", Initial_LULC, "_to_", Final_LULC, ".grd")

writeRaster(Prob_raster, prob_map_path, datatype='INT2U', overwrite=T)
})) #close loop over transitions




### =========================================================================
### Approach for probability by class (not by transition)
### =========================================================================

#subset trans data to just ID and x y
prediction_table <- Trans_dataset[, c("x", "y", "ID")]

#vector all LULC classes
All_LULC_classes <-unique(Model_lookup$Final_LULC, Model_lookup$Initial_lookup)

#add columns for all LULC classes with NA values
prediction_table[, All_LULC_classes] <- NA

system.time(for (trans_model in 1:nrow(Model_lookup)) {

Final_LULC <- Model_lookup[trans_model, "Final_LULC"]
Initial_LULC <- Model_lookup[trans_model, "Initial_LULC"]
Region <- Model_lookup[trans_model, "Region"]

cat(paste0("'\n)", "predicting for transition from ", Initial_LULC, " to ", Final_LULC, "\n'"))

#load model
Fitted_model <- readRDS(Model_lookup[trans_model, "File_path"])

#subset data
pred_data <- Trans_dataset[Trans_dataset[Initial_LULC] == 1 & Trans_dataset$Bioreg_raster_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- Final_LULC

#bind to ID
predict_ID <- cbind(ID = pred_data[,c("ID")], prob_predicts[Final_LULC])

#append predictions into prediction table
prediction_table[match(predict_ID$ID, prediction_table$ID), Final_LULC] <- predict_ID[,2]

}) #close for loop

#replace any remaining NAs with 0
prediction_table[is.na(prediction_table)] <- 0

#recombine with NA values
Trans_dataset_na[setdiff(names(prediction_table), names(Trans_dataset_na))] <- NA
prediction_values <- rbind(prediction_table, Trans_dataset_na)
prediction_values[order(prediction_values$ID),]
prediction_values$ID <- NULL

#rasterize predicted probability values for each final LULC class and save
prediction_brick <- rasterFromXYZ(prediction_values, crs = crs(Current_LULC))

#create folder for saving probability maps

#to use the Dinamica submodel Create cube of probability maps, the rasters in
#the folder need to have pre-fix according to their row number in the transition matrix 
#create a transition matrix to extract row numbers and then add a column
#to the model look up table with the row numbers
prob_map_path <- paste0("Results/Pred_prob_maps/", Scenario_ID, "/", Simulation_ID, "/", Simulation_time_step, "/",  )

#save layers from raster brick seperately

### =========================================================================
### Testing rasterization of point data
### =========================================================================

#seperate one variable for testing
test_data <- as.data.frame(Trans_dataset$Soil_PH)

#create spatial points dataframe using points from original LULC raster
data_points <- SpatialPointsDataFrame(spat_points, test_data)

#rasterize
r <- raster::rasterize(x= data_points, y=Current_LULC, field = names(data_points), update = TRUE)
#This is causing issues trying to allocate a very large amount of memory

#alternative is to test raster<-dataframe with xy (my previous approach) and then function rasterfromxyz() 
df_conversion <- raster::as.data.frame(Trans_data_stack) #Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
xy_coordinates <- coordinates(Trans_data_stack) #Get XY coordinates of cells
df_with_xy <- cbind(df_conversion, xy_coordinates) #cbind XY coordinates to dataframe and remove NAs

#seperate one variable for testing
test_data_xy <- as.data.frame(df_with_xy[,c("x", "y", "Soil_nutrients", "Soil_PH")])  

#outputs a raster brick when xyz df contains multiple columns
raster_xy_test <- rasterFromXYZ(test_data_xy, crs = crs(Current_LULC))

#access names and save seperate rasters
names(raster_xy_test)

#comparing raster attributes
compareRaster(Trans_data_stack$Soil_PH, raster_xy_test, crs=TRUE, res=FALSE,
         rotation=TRUE, values=TRUE, stopiffalse=TRUE, showwarning=FALSE)
























#mapply through the three lists and at each item have an internal loop through transition modelling for each class-class transition

#Internal loop for each transition
#1. load correct covariates 


#2. stack LULC raster and covariates


#3. convert to dataframe, remove NAs and append transition cols

#seperate covariates cols and XY/LULC cols and transition cols into seperate dataframes in a list

#vector names for transition potential columns from LULC class names in LULC_rat
Transition_potential_cols <- sapply(LULC_rat$LULCclass, function(x) paste0(x, ".transprob"))

#convert all rasters to dataframes and add  10 columns for transition potential
#to each class to all datasets that have greater than 0 rows 
regional_lulc_dataframes <- lapply(Bioregion_rasters_by_LULC, function(x) {
  LULC_dataframe <- raster::as.data.frame(x, xy=TRUE, na.rm= TRUE)
  if (nrow(LULC_dataframe) > 0) {LULC_dataframe[Transition_potential_cols] <- NA}
  return(LULC_dataframe)
  })

#remove datasets that have no rows indicating no LULC class in given region
regional_lulc_dataframes <- regional_lulc_dataframes[sapply(regional_lulc_dataframes, nrow)>0]

#5. load RF model

#6. Use DF in call to RF model

#7. Append RF prediction to DF in column named after specific transition. 

#8. fill empty columns (for non-possible transitions) with 0 values

#9. re-combine the XY/LULC cols and transitions cols, dropping the covariate cols 


#Closing external loop
#1. rbind all dataframes back together

#2. Produce a raster stack of probability maps for transition to each LULC type
# i.e. loop through transition cols creating a raster for each and then stack. 


