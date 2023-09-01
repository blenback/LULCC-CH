#############################################################################
## Trans_model_finalization: Re-fit LULCC transition models using optimal specifcation
## Date: 15-02-2023
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("data.table","stringi","stringr","rlist",  
#          "randomForest", "RRF", "butcher", 
#          "ROCR","ecospat","caret", "foreach", "doMC", "data.table", "raster", "tidyverse",
#          "testthat", "sjmisc", "tictoc", "lulcc", "pbapply", "stringr", "readr", "openxlsx", "readxl") 
# 
# new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
# 
# if (length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))
# 
# # Source custom functions
# invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

#Provide base folder paths for saving models for simulation
model_base_folder <- "Data/Transition_models/Prediction_models"
dir.create(model_base_folder, recursive = TRUE)

#File path for table of model parameters   
param_grid <- "Tools/Predict_param-grid.xlsx"

#load table of model specifications
model_specs <- read_excel("Tools/Predict_model_specs.xlsx")

#Filter for models already completed
models_specs <- model_specs[model_specs$Modelling_completed == "N",]

#split into named list
model_list <- lapply(split(models_specs, seq(nrow(models_specs))), as.list)
names(model_list) <- models_specs$Detail_model_tag

#FOR TESTING DELETE AFTERWARDS
#model_specs <- model_list[[1]]
#Dataset_path <- Data_paths_for_period[1]

#Instantiate wrapper function over process of modelling prep, fitting,
#evaluation, saving and completeness checking
lulcc.multispectransmodelling <- function(model_specs){

### =========================================================================
### A- Prepare model specifications 
### =========================================================================  

#vector model specifcations    
Data_period <- model_specs$Data_period
Model_type <- model_specs$Model_type
Model_scale <- model_specs$Model_scale
Feature_selection_employed <- model_specs$Feature_selection_employed
Nhood_extent <- model_specs$Nhood_extent
Correct_balance <- model_specs$Balance_adjustment

cat(paste0('Conducting modelling under specification ',model_specs$Detail_model_tag , '...\n'))

#finalise folder paths
model_folder <- paste0(model_base_folder, "/", Data_period)
dir.create(model_folder, recursive = TRUE)


#Get file paths of transition datasets for period
Data_paths_for_period <-if(Feature_selection_employed == FALSE) {list.files(paste0("Data/Transition_datasets/Pre_predictor_filtering/", Data_period), pattern = Model_scale, full.names = TRUE)}else if(
Feature_selection_employed == TRUE) {list.files(paste0("Data/Transition_datasets/Post_predictor_filtering/", Data_period), pattern = Model_scale, full.names = TRUE)}

### =========================================================================
### B- Performing modelling 
### =========================================================================

#Now opening loop over datasets
future::plan(multisession(workers = availableCores()-2))
Predict_model_paths <- future_lapply(Data_paths_for_period, function(Dataset_path) {
gc()
  
#Source custom functions
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))  

#load dataset
Trans_dataset <- readRDS(Dataset_path)
Trans_name <- str_remove_all(basename(Dataset_path), paste(c(".rds", paste0("_", Model_scale)), collapse = "|"))
  
### =========================================================================
### B.1 - Attach model parameters
### =========================================================================

#Attach  a list of model parameters('model_settings') 
#for each type of model specified in the parameter grid
model_settings <- lulcc.setparams(transition_result = Trans_dataset$trans_result,
                                    covariate_names= names(Trans_dataset$cov_data),
                                    model_name = Model_type,
                                    param_grid= param_grid, # Parameter tuning grid (all possible combinations will be evaluated)
                                    weights= 1)

Trans_dataset <- list.append(Trans_dataset, model_settings = model_settings)  
rm(model_settings)

### =========================================================================
### B.2- Fit model
### =========================================================================

Downsampling_bounds <- list(lower = 0.05, upper = 60)

#Fit model
mod <-try(lulcc.fitmodel(trans_result = Trans_dataset$trans_result, #transitions data
                        cov_data = Trans_dataset$cov_data, #covariate data
                        replicatetype = "none", # cross-validation strategy
                        reps= 1, # Number of replicates 
                        mod_args = Trans_dataset$model_settings,
                        path = NA,
                        Downsampling = if (Correct_balance == TRUE & Trans_dataset$imbalance_ratio <= Downsampling_bounds$lower | Trans_dataset$imbalance_ratio >= Downsampling_bounds$upper) {TRUE} else {FALSE} # utilise downsampling based on imbalance ratio 
                     ), TRUE)  #Supply model arguments

#extract only the part of the model that is needed (see below)
model_extract <- mod@fits[["replicate_01"]][[1]]

#save in final model location
model_save_path <- paste0(model_folder, "/", Trans_name, ".", Data_period, ".", Model_type, ".rds")
saveRDS(model_extract, model_save_path)

gc()

#return save path for model look up table
return(model_save_path)

}, future.seed=TRUE, future.packages = packs) #close loop over transition datasets
plan(sequential)

### =========================================================================
### B.3- Update model specification table to reflect that this specification
### of models is complete
### =========================================================================

#load model spec table and replace the values in the 'Completed' column
model_spec_table <- readxl::read_excel("Tools/Predict_model_specs.xlsx")

#find the correct row
model_spec_table$Modelling_completed[model_spec_table$Detail_model_tag == model_specs$Detail_model_tag] <- "Y"

openxlsx::write.xlsx(model_spec_table, file = "Tools/Predict_model_specs.xlsx", overwrite = TRUE) 

cat(paste0('Prediction model fitting finished \n'))

return(Predict_model_paths)
} #close wrapper function

#loop wrapper function over list of models
All_predict_model_paths <- lapply(model_list[2:3], function(model){
lulcc.multispectransmodelling(model)
})

### =========================================================================
### F- create a model look up table
### =========================================================================

Model_periods <- unique(models_specs$Data_period_name)

#load list of viable transitions for each time period
Viable_transitions_lists <- readRDS("Tools/Viable_transitions_lists.rds")
names(Viable_transitions_lists) <- Model_periods

#create a df with info for each period
Model_lookups <- lapply(Model_periods, function(x) {
  
  #Get file paths for models and uncertainty tables and convert to DF adding columns for file_path, model_name, unc_table_path
  model_df <- data.frame(File_path = list.files(paste0(model_base_folder, "/", x), full.names = TRUE),
                         Model_name = list.files(paste0(model_base_folder, "/", x), full.names = FALSE))
                         #Unc_table_path = list.files(paste0("Data/Uncertainty_tables/", x), full.names = TRUE)) #adding uncertainty table as a list object if desired

 
  #model_region (split on 1st period)
  model_df$Region <- sapply(model_df$Model_name, function(x) str_split(x, "\\.")[[1]][1], simplify = TRUE)

  #Initial LULC class (split on 2nd period)
  model_df$Initial_LULC <- sapply(model_df$Model_name, function(x) str_split(x, "\\.")[[1]][2], simplify = TRUE)
  
  #Final LULC class (split on 3rd period)
  model_df$Final_LULC <- sapply(model_df$Model_name, function(x) str_split(x, "\\.")[[1]][3], simplify = TRUE)

  #transition names (concatenating Initial and Final_class)
  model_df$Trans_name <- sapply(model_df$Model_name, function(x) {
    Initial <- str_split(x, "\\.")[[1]][2]
    Final <- str_split(x, "\\.")[[1]][3]
    Trans_name <- paste0(Initial, "_", Final) 
    }
    , simplify = TRUE)
  
  #Model_period (split on 4th period)
  model_df$Model_period <-sapply(model_df$Model_name, function(x) str_split(x, "\\.")[[1]][4], simplify = TRUE)  
  
  #Model_type (split on 5th period)
  model_df$Model_type <-sapply(model_df$Model_name, function(x) str_split(x, "\\.")[[1]][5], simplify = TRUE)

  #reorder columns
  col_order <- c("Trans_name", "Region", "Initial_LULC", "Final_LULC", "Model_type", "Model_period", "Model_name", "File_path") #"Unc_table_path" to vector if included
  model_df <- model_df[, col_order]
  
  #remove rows for persistence models which are not required by Dinamica
  model_df <- model_df[!model_df$Initial_LULC == model_df$Final_LULC,]
                                
  return(model_df)
  })
names(Model_lookups) <- Model_periods
#adding ID column using lists of viable transitions
Model_lookups_with_ID <- mapply(function(Model_lookup, trans_table){
  Model_lookup$Trans_ID <- sapply(Model_lookup$Trans_name, function(x) trans_table[trans_table$Trans_name == x, "Trans_ID"])
return(Model_lookup)
  }, Model_lookup = Model_lookups,
trans_table = Viable_transitions_lists,
SIMPLIFY = FALSE)

#save DFs for each periods as sheets in a xlsx. 
unlink("Tools/Model_lookup.xlsx")
mapply(function(model_table, model_name) 
  xlsx::write.xlsx(model_table, file="Tools/Model_lookup.xlsx", sheet= model_name, row.names=FALSE, append = TRUE),
         model_table = Model_lookups_with_ID,
         model_name = names(Model_lookups_with_ID))
