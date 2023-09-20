#############################################################################
## Trans_modelling: Fit and evaluate models of LULC transitions
## under multiple model specifications 
## Date: 08-04-2022
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

#Provide base folder paths for saving fitted_models
model_base_folder <- "Data/Transition_models"
eval_results_base_folder <- "Results/Transition_model_evaluation"

#File path for table of model parameters   
param_grid <- Param_grid_path

dir.create(model_base_folder, recursive = TRUE)
dir.create(eval_results_base_folder, recursive = TRUE)

#load table of model specifications
# Import model specifications table
model_specs <- read_excel(Model_specs_path)

#Filter for models already completed
models_specs <- model_specs[model_specs$Modelling_completed == "N",]

#split into named list
model_list <- lapply(split(models_specs, seq(nrow(models_specs))), as.list)
names(model_list) <- models_specs$Detail_model_tag

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
FS_string <- if(Feature_selection_employed == TRUE) {"filtered"} else if (Feature_selection_employed == FALSE){"unfiltered"}

if(Correct_balance == TRUE){
model_folder <- paste0(model_base_folder, "/", toupper(Model_type) , "_models", "/", Model_scale, "_", FS_string, "/")
eval_results_folder <- paste0(eval_results_base_folder, "/", toupper(Model_type) , "_model_evaluation_downsampled", "/", Model_scale, "_", FS_string, "/")
}else if(Correct_balance == FALSE){
model_folder <- paste0(model_base_folder, "/", toupper(Model_type) , "_models_non_adjusted", "/", Model_scale, "_", FS_string, "/")
eval_results_folder <- paste0(eval_results_base_folder, "/", toupper(Model_type) , "_model_evaluation_non_adjusted", "/", Model_scale, "_", FS_string, "/")  
}

#Get file paths of transition datasets for period
Data_paths_for_period <-if(Feature_selection_employed == FALSE) {list.files(paste0("Data/Transition_datasets/Pre_predictor_filtering/", Data_period), pattern = Model_scale, full.names = TRUE)}else if(
Feature_selection_employed == TRUE) {list.files(paste0("Data/Transition_datasets/Post_predictor_filtering/", Data_period), pattern = Model_scale, full.names = TRUE)}
names(Data_paths_for_period) <- sapply(Data_paths_for_period, function(x) str_remove(basename(x), ".rds"))

### =========================================================================
### B- Performing modelling 
### =========================================================================

#Now opening loop over datasets
future::plan(multisession(workers = availableCores()-2))
Modelling_outputs <- future_lapply(Data_paths_for_period, function(Dataset_path) {
gc()

# Source custom functions
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))  
  
cat(paste0('Modelling transition: ', str_remove(basename(Dataset_path), ".rds"), '\n'))

#load dataset
Trans_dataset <- readRDS(Dataset_path)
Trans_name <- str_remove(basename(Dataset_path), ".rds")
  
### =========================================================================
### B.1 - Attach model parameters
### =========================================================================

#Attach  a list of model parameters('model_settings') 
#for each type of model specifcied in the parameter grid
model_settings <- lulcc.setparams(transition_result = Trans_dataset$trans_result,
                                    covariate_names= names(Trans_dataset$cov_data),
                                    model_name = Model_type,
                                    param_grid= param_grid, # Parameter tuning grid (all possible combinations will be evaluated)
                                    weights= 1)

Trans_dataset <- list.append(Trans_dataset, model_settings = model_settings)  
rm(model_settings)
cat(paste0('Modelling parameters defined \n'))

### =========================================================================
### B.2- Fit, evaluate and save models
### =========================================================================

#Apply function for fitting and evaluating models and saving results
#the output returned is a list of errors caught by try()
Trans_model_capture <- lulcc.fitevalsave(Transition_dataset = Trans_dataset,
                                      Transition_name = Trans_name,
                                      replicatetype= "splitsample", 
                                      reps=5,
                                      balance_class = Correct_balance,
                                      Downsampling_bounds = list(lower = 0.05, upper = 60),
                                      Data_period = Data_period,
                                      model_folder = model_folder,
                                      eval_results_folder = eval_results_folder,
                                      Model_type = Model_type) 

gc()
return(Trans_model_capture)
}, future.seed=TRUE, future.packages = packs
) #close loop over trnasition datasets
plan(sequential)

### =========================================================================
### B.3- Update model specification table to reflect that this specification
### of models is complete
### =========================================================================

#check for failures in the Modelling outputs
Modelling_check <- unlist(Modelling_outputs)

if(all(Modelling_check == "Success") == TRUE){
  
  #load model spec table and replace the values in the 'Completed' column
  model_spec_table <- readxl::read_excel(Model_specs_path)

  #find the correct row
  model_spec_table$Modelling_completed[model_spec_table$Detail_model_tag == model_specs$Detail_model_tag] <- "Y"

  openxlsx::write.xlsx(model_spec_table, file = Model_specs_path, overwrite = TRUE) 

  cat(paste0('Model fitting and evaluation for:', model_specs$Detail_model_tag, 'completed without errors'))
} else if(all(Modelling_check == "Success") == FALSE){
  
  #count number of errors
  Num_errors <- length(Modelling_check[Modelling_check != "Success"])
  
   #print error message
  cat(paste0(Num_errors, ' errors occurred in model fitting and evaluation for the model specification: \n',
             model_specs$Detail_model_tag,
             '\n please consult saved modelling output file: \n',
             paste0(eval_results_folder, model_specs$Detail_model_tag, "_modelling_output_summary.rds")
             ))
  
  if(Num_errors <10){
    
    cat("As the number of erros was <10 the model spec table has been updated
        to indicate that this specification has been completed, the likely cause
        of error is transition dataset with insufficient number of transition
        instances (1's) or where feature selection has reduced to a single predictor variable")
    #load model spec table and replace the values in the 'Completed' column
    model_spec_table <- readxl::read_excel(Model_specs_path)

    #find the correct row
    model_spec_table$Modelling_completed[model_spec_table$Detail_model_tag == model_specs$Detail_model_tag] <- "Y"
    
    #add a warning
    model_spec_table$Num_errors <- Num_errors
    
    #save
    openxlsx::write.xlsx(model_spec_table, file = Model_specs_path, overwrite = TRUE)
  }
  
  
  #save modelling outputs for user inspection
  saveRDS(Modelling_outputs, paste0(eval_results_folder, model_specs$Detail_model_tag, "_modelling_output_summary.rds"))

}

} #close wrapper function

#loop wrapper function over list of models
lapply(model_list, function(model){lulcc.multispectransmodelling(model)})

### =========================================================================
### C- Extract model objects and save in seperate folder structure
### =========================================================================

#This process requires the user to have checked the model evaluation results
#and determined which is the preferred model specification to use moving forward
#with the LULCC modelling i.e model spec with the best performance

#This code section is to extract the model objects for the desired specification
#and save them in a seperate location and at the same time create a model lookup
#table to be used by Dinamica when predicting transition potential
#at future time points

#vector model periods
Model_periods <- unique(model_specs$Data_period)

lapply(Model_periods , function(Model_period){
#paste together path
Model_folder_path <- paste0("Data/Fitted_models/RF_models/regionalized_filtered/Period_", Model_period, "_rf_models") 

#list model file paths
model_paths <- as.list(list.files(Model_folder_path, recursive = TRUE, full.names = TRUE))

#rename
names_w_dir <- sapply(as.list(list.files(Model_folder_path, recursive = TRUE, full.names = FALSE)),function(x) 
  str_remove_all(str_split(x, "/")[[1]][2], "_rf-1.rds")
                 )

#load viable trans_list for period
Initial_LULC_classes <-  unique(readRDS("Tools/Viable_transitions_lists.rds")[[Model_period]][["Initial_class"]])
Initial_LULC_classes <- paste0(Initial_LULC_classes, "_")

#replacing the "_" between LULC classes with a '.'
names_w_dir <- sapply(names_w_dir, function(name){
Initial_class <- Initial_LULC_classes[sapply(Initial_LULC_classes, function(class) {grepl(class, name)}, simplify = TRUE)]
new_name <- gsub('^\\_|\\_$', '.', Initial_class)
renamed <- gsub(Initial_class, new_name, name)
})

names(model_paths) <- names_w_dir

extract_save_model <- function(model_file_path, model_name){
  
  #load model
  model_object <- readRDS(model_file_path)
  
  #extract one of the model objects
  model_extract <- model_object[["model"]]@fits[["replicate_01"]][["rf-1"]]

  #create a folder path based on time period
  folder_path <- paste0("Data/Fitted_models/", Model_period) 
  dir.create(folder_path, recursive = TRUE)
  
  #expand to file path using model name
  file_path <- paste0(folder_path, "/", model_name, ".", Model_period, ".", "RF.rds")
  
  #save the model
  saveRDS(model_extract, file = file_path)
  
  #return the predictors 
  
}

mapply(extract_save_model, 
       model_file_path = model_paths,
       model_name = names(model_paths))

}) #close loop over periods

### =========================================================================
### F- create a model look up table
### =========================================================================

#load list of viable transitions for each time period
Viable_transitions_lists <- readRDS("Tools/Viable_transitions_lists.rds")
names(Viable_transitions_lists) <- Model_periods

#create a df with info for each period
Model_lookups <- lapply(Model_periods, function(x) {
  
  #get folder for fitted models
  #model_folders <- as.list(list.files("Data/Fitted_models", recursive = FALSE, full.names = TRUE))
  
  #Get file paths for models and uncertainty tables and convert to DF adding columns for file_path, model_name, unc_table_path
  model_df <- data.frame(File_path = list.files(paste0("Data/Fitted_models/", x), full.names = TRUE),
                         Model_name = list.files(paste0("Data/Fitted_models/", x), full.names = FALSE),
                         Unc_table_path = list.files(paste0("Data/Uncertainty_tables/", x), full.names = TRUE))

 
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
  col_order <- c("Trans_name", "Region", "Initial_LULC", "Final_LULC", "Model_type", "Model_period", "Model_name", "File_path", "Unc_table_path")
  model_df <- model_df[, col_order]
  
  #remove rows for persistence models which are not required by Dinamica
  model_df <- model_df[!model_df$Initial_LULC == model_df$Final_LULC,]
                                
  return(model_df)
  })

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
