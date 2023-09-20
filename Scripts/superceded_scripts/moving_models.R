### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "parallel", "terra",
         "lulcc", "pbapply", "stringr", "readr", "xlsx")

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("E:/Dry_run/Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#vector model periods
Model_periods <- c("1985_1997", "1997_2009", "2009_2018")

### =========================================================================
### B- Extract just model objects and save in seperate folder structure
### =========================================================================

lapply(Model_periods , function(Model_period){
#paste together path
Model_folder_path <- paste0("Data/Fitted_models/Fitted_RF_models/regionalized_filtered/Period_", Model_period, "_rf_models") 

#list model file paths
model_paths <- as.list(list.files(Model_folder_path, recursive = TRUE, full.names = TRUE))

#rename
names_w_dir <- sapply(as.list(list.files(Model_folder_path, recursive = TRUE, full.names = FALSE)),function(x) 
  str_remove_all(str_split(x, "/")[[1]][2], "_rf-1.rds")
                 )

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
  
}

mapply(extract_save_model, 
       model_file_path = model_paths,
       model_name = names(model_paths))

}) #close loop over periods

### =========================================================================
### C- create a model look up table
### =========================================================================

#load list of viable transitions for each time period
Viable_transitions_lists <- readRDS("Tools/Viable_transitions_lists.rds")
names(Viable_transitions_lists) <- Model_periods

#list folders for fitted models
model_folders <- as.list(list.files("Data/Fitted_models", recursive = FALSE, full.names = TRUE))

#remove folder of full model objects (i.e. including test/training data etc/)
model_folders <- model_folders[1:3]
names(model_folders) <- Model_periods

#create a df with info for each period
Model_lookups <- lapply(model_folders, function(x) {
  
  #convert file paths to DF adding columns for file_path and model_name
  model_df <- data.frame(File_path = list.files(x, full.names = TRUE),
                         Model_name = list.files(x, full.names = FALSE))

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
  col_order <- c("Trans_name", "Region", "Initial_LULC", "Final_LULC", "Model_type", "Model_period", "Model_name", "File_path")
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
mapply(function(model_table, model_name) write.xlsx(model_table, file="Tools/Model_lookup.xlsx", sheetName= model_name, row.names=FALSE, append = TRUE, ),
         model_table = Model_lookups_with_ID,
         model_name = names(Model_lookups_with_ID))
  





  