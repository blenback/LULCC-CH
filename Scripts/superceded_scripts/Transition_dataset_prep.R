#############################################################################
## Transition_dataset_creation: Script for gathering the layers of LULC
## (dependent variable) and predictors for each historic period then seperating
## to viable LULC transitions at the scale of Switzerland and Bioregions 
## Date: 25-09-2021
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat",
#          "sjmisc", "tictoc", "parallel", "pbapply", "stringr", "openxlsx")
# 
# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))
# 
# # Source custom functions
# invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#Historic LULC data folder path
LULC_folder <- "Data/Historic_LULC"

#character string for data period inherited from LULCC_CH_master
#otherwise uncomment here: 
Data_periods <- c("1985_1997", "1997_2009", "2009_2018")

#Produce regionalized datasets?: inherited from LULCC_CH_master
Regionalization <- TRUE

#If Regionalization = TRUE provide regional raster file path
Region_raster_path <- "Data/Bioreg_CH"

#create save dir
save_dir <- "Data/Transition_datasets/Pre_predictor_filtering"
dir.create(save_dir, recursive = TRUE)

### =========================================================================
### B- Load data and filter for each time period
### =========================================================================

#The predictor data table is used to identify the file names of variables that
#are to be included in the stack for each time period

#Predictor table file path
Pred_table_path <- "Tools/Predictor_table.xlsx"

#load tables as list
predictor_tables <- lapply(Data_periods, function(x) data.table(openxlsx::read.xlsx(Pred_table_path, sheet = x)))
names(predictor_tables) <- Data_periods

#subsetting to only the necessary columns
Preds_by_period <- lapply(predictor_tables, function(x) {
Pred_subset <- x[,c("Prepared_data_path", "Covariate_ID")]
Pred_subset$File_name <- Pred_subset$Prepared_data_path
Pred_subset$Prepared_data_path <- NULL
names(Pred_subset)[names(Pred_subset) == "Covariate_ID"] <- "Layer_name"
return(Pred_subset)
})

#Appending the initial LULC classes (1st year of period) and the
#outcome LULC (last year of period) as well regional designation

#create a list of the file paths of the historic LULC rasters
#pattern matching on the .gri extension first and then
#excluding the accompanying .ovr files with grep
LULC_raster_paths <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(LULC_raster_paths) <- c("File_name", "Layer_name")
LULC_raster_paths["File_name"] <- as.data.frame(list.files(LULC_folder, pattern = ".gri", full.names = TRUE))
LULC_raster_paths["Layer_name"] <- str_remove(str_extract(LULC_raster_paths$File_name, "(?<=/)[^/]*$"), ".gri") #extract everything that begins with / and runs to the end of the string.

#Collect file path for regional raster
Region_path <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(Region_path) <- c("File_name", "Layer_name")
Region_path["File_name"] <- list.files(Region_raster_path,pattern = ".gri", full.names = TRUE)
Region_path["Layer_name"] <- "Bioregion"

LULC_period_regexes <- lapply(Data_periods, function(x) {
  Regex <- str_replace(x, pattern = "_","|")
  })
names(LULC_period_regexes) <- Data_periods

#filtering the LULC file paths for each period
LULC_paths_by_period <- lapply(LULC_period_regexes, function(x) filter(LULC_raster_paths, grepl(x, File_name)))

#Change layer names to Initial and Final for easier splitting later
LULC_paths_by_period <- lapply(LULC_paths_by_period, function(x) {
x$Layer_name[1] <- "Initial_class"
x$Layer_name[2] <- "Final_class"
return(x)})

#combining lists of predictor paths and LULC rasters optionally adding the regional raster
if(Regionalization == TRUE){
Combined_paths_by_period <- lapply((mapply(rbind, Preds_by_period, LULC_paths_by_period, SIMPLIFY = FALSE)), function(x) rbind(x, Region_path))
}else{Combined_paths_by_period <- mapply(rbind, Preds_by_period, LULC_paths_by_period, SIMPLIFY = FALSE)}

#read in all rasters in the list to check compatibility before stacking
Rasters_by_periods <- lapply(Combined_paths_by_period, function(x) {
  raster_list <- lapply(x$File_name, function(y) raster(y))
  names(raster_list) <- x$Layer_name
  return(raster_list)
   })

### =========================================================================
### C- Confirm Raster compatibility for stacking
### =========================================================================

#Use a function to test rasters in the list against an 'exemplar'
#which has the extent, crs and resolution that we want
#in this case the Ref_grid file used for re-sampling some of the predictors.

#exemplar raster for comparison
Exemplar_raster <- raster("Data/Ref_grid.gri")

#TO DO:add a stop code to stop script if rasters are not compatible

Raster_comparison_results <- lapply(Rasters_by_periods, function(raster_list)
  lulcc.TestRasterCompatibility(raster_list, Exemplar_raster))

if ((is_empty(unlist(Raster_comparison_results), first.only = FALSE, all.na.empty = TRUE)) == FALSE) {
    stop("Differences in Raster characteristics means they are unsuitable for stacking
         refer to object Raster_comparison_results to locate problems")
  }

#creating raster stacks of all predictors and LULC data for each time period
Rasterstacks_by_periods <- mapply(function(Raster_list, Period_name){
  raster_stack_for_period <- stack(Raster_list)
  names(raster_stack_for_period@layers) <- names(Raster_list)
  Data_period <- str_remove(Period_name, "Period_")
  saveRDS(raster_stack_for_period, file = paste0("Data/Preds/Prepared/Stacks/Calibration/Pred_stack_", Data_period, ".rds"))
  return(raster_stack_for_period)
  }, Raster_list = Rasters_by_periods,
  Period_name = names(Rasters_by_periods),
  SIMPLIFY = FALSE)

rm(Rasters_by_periods)

### =========================================================================
### C.1- Data extraction
### =========================================================================

#for efficiency all subsequent processes are looped over each period in parallel
future::plan(multisession(workers = availableCores()-2))
future_lapply(Data_periods, function(period){})

period <- "1985_1997"
  
period_data <- Rasterstacks_by_periods[[paste(period)]]

#Apply function to convert Rasterstacks to dataframes and then split by region 
Trans_data_full_extent <- lulcc.extractsplitrasterstack(Rasterstack_for_splitting = period_data,
       Split_by = "NONE")

if(Regionalization == TRUE){
Trans_data_regionalized <- lulcc.extractsplitrasterstack(Rasterstack_for_splitting = period_data,
       Split_by = "Bioregion_Class_Names")} 

### =========================================================================
### D. Transition dataset creation
### ========================================================================= 

#Datasets still contain instances of all class-class transitions which is not
#useful for modelling because some are not viable. Thus we need to filter out 
#these transitions to give separate data sets for each initial LULC class to
#final LULC class transition but each needs to contain all of the data points
#of the other transitions from the same initial class to the other final classes.
#Given the total number of class-class transitions this could be a
#lengthy process so first we filter out the transitions that are not viable.

#This should be a two step process:
#1. Filtering out entries based on Initial LULC classes that do not
#undergo transitions (static class)
#2. Use a list of viable transitions to separate out transitions that do not
#occur at a sufficient rate. 

#read in list of viable transition for period
#created in script 'Transition_identification'
viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[[paste(period)]]

#loop over viable transitions and add column for each to the data with the values: 
#1: If the row is positive for the given transition: If both Initial and Final classes match that of the transition) 
#0 :if the row is negative for this transition: If the initial class matches but the final class does not); 
#NA :if the row is Not applicable: neither the initial or final class match that of the transition)

cat(paste0("Creating transition datasets for period ", period)) 

# Load the predictor data table that will be used to identify the categories of predictors
predictor_table <- openxlsx::read.xlsx(Pred_table_path, sheet = period)

#Create binarized transition datasets for each transition
Binarized_datasets_for_period_full <- lulcc.binarizetransitiondatasets(data_set_for_splitting = Trans_data_full_extent,
                                                                  viable_trans_list = viable_trans_list)

if(Regionalization == TRUE){
Binarized_datasets_for_period_regionalized <- unlist(lapply(Trans_data_regionalized,
                                        function(x) lulcc.binarizetransitiondatasets(data_set_for_splitting = x,
                                        viable_trans_list = viable_trans_list)), recursive = FALSE)
}

#Loop over transition datasets splitting each into:
#the transition result column 
#non-transition columns,
#weight vector, 
#measure of class imbalance
#number of units in the dataset

Trans_datasets_full <- lapply(Binarized_datasets_for_period_full, function(x)
  lulcc.splitforcovselection(x, predictor_table = predictor_table))

Trans_datasets_regionalized <- lapply(Binarized_datasets_for_period_regionalized, function(x)
  lulcc.splitforcovselection(x, predictor_table = predictor_table))

#loop over dataset lists saving individual datasets
lapply(names(Trans_datasets_full), function(dataset_name){
Full_save_path <- paste0(save_dir, "/", period, "_", dataset_name, "_full_ch") 
saveRDS(Trans_datasets_seperated, Full_save_path)
})

lapply(names(Trans_datasets_regionalized), function(dataset_name){
Full_save_path <- paste0(save_dir, "/", period, "_", dataset_name, "_regionalized")
saveRDS(Trans_datasets_seperated, Full_save_path)
})

cat(paste((paste0("Transition Datasets for:", period, "complete")), "", sep = "\n"))
#}) #close loop over periods

plan(sequential) #close parallel clusters

cat(paste0(' Preparation of transition datasets complete \n'))


