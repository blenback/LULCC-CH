#############################################################################
## Feature_selection: Performing collinearity based and
## embedded feature selection with Guided Regularized Random Forests
## Date: 25-09-2021
## Author: Ben Black
#############################################################################

# All packages and functions are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("data.table", "raster", "tidyverse", "testthat", "sjmisc", "tictoc", "randomForest", "RRF", "rlist", "purrr",
#            "doParallel", "future.apply", "ghibli", "readxl", "openxlsx", "ggpattern")
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

# Import model specifications table
model_specs <- read_excel("Tools/Model_specs.xlsx")

#Filter for models with feature selection not required
Filtering_required <- model_specs[model_specs$Feature_selection_employed == "TRUE",]%>%
  group_by(model_scale)%>%
  distinct(Data_period_name)

#add tag column
Filtering_required$tag <- paste0(Filtering_required$Data_period_name, "_", Filtering_required$model_scale)

#split into named list
Datasets_for_FS <- lapply(split(Filtering_required, seq(nrow(Filtering_required))), as.list)
names(Datasets_for_FS) <- Filtering_required$tag

#set folder paths 
Pre_FS_folder <- "Data/Transition_datasets/Pre_predictor_filtering" #Pre feature selection datasets folder
collinearity_folder_path <- "Results/Model_tuning/Covariate_selection/Covariate_collinearity_filtering"
embedded_folder_path <- "Results/Model_tuning/Covariate_selection/GRRF_embedded_selection"
Combined_results_folder_path <- "Results/Model_tuning/Covariate_selection/Cov_selection_summaries"
Filtered_datasets_folder_path <- "Data/Transition_datasets/Post_predictor_filtering"
FS_results_folder <- "Results/Model_tuning/Covariate_selection/Cov_selection_summaries" #Feature selection results folder

#loop through folders and create any that do not exist
lapply(list(Pre_FS_folder,
                     collinearity_folder_path,
                     embedded_folder_path,
                     Combined_results_folder_path,
                     Filtered_datasets_folder_path,
                     FS_results_folder), function(x) dir.create(x, recursive = TRUE))

#Predictor table file path
Pred_table_path <- "Tools/Predictor_table.xlsx"

### =========================================================================
### Perform feature selection
### =========================================================================

#wrapper function to run feature selection over multiple scales and datasets
lulcc.featureselection <- function(Dataset_details){

### =========================================================================
### A- Load data
### =========================================================================  
Data_period_name <- Dataset_details$Data_period_name
Dataset_scale <- Dataset_details$model_scale 
  
# Load the predictor data table that will be used to identify the categories of covariates
#and perform collinearity testing seperately
Predictor_table <- read.xlsx(Pred_table_path, sheet = Data_period_name)

file_match_regex <- glob2rx(paste0(Data_period_name, "*", Dataset_scale))

#load in the data for the period specified
Datasets_for_period <- readRDS(list.files(Pre_FS_folder, pattern = file_match_regex, full.names = TRUE))

cat(paste0(' Transition datasets loaded \n'))

### =========================================================================
### B- Stage 1: Collinearity based feature selection
### =========================================================================

collin_selection_results <- lapply(Datasets_for_period, function(z) {
    lulcc.filtersel(
      transition_result = z[["trans_result"]],
      cov_data = z[["cov_data"]], 
      categories =  Predictor_table$CA_category[which(Predictor_table$Covariate_ID %in% names(z[["cov_data"]]))],
      collin_weight_vector = z[["collin_weights"]],
      embedded_weight_vector = z[["embed_weights"]],
      focals= c("Neighbourhood"),
      method="GLM",
      corcut=0.7)
}) #close loop over transition datasets

# save the output
Save_path_collinearity <- paste0(collinearity_folder_path, "/", Data_period_name, "_collinearity_filtered_covs_", Dataset_scale)
#saveRDS(collin_selection_results, Save_path_collinearity)

cat(paste0(' Collinearity based covariate selection complete \n'))

rm(Datasets_for_period)

### =========================================================================
### C- Stage 2: GRRF Embedded feature selection
### =========================================================================

  GRRF_selection_results <- lapply(collin_selection_results, function(x) {
  GRRF_results_dataset <- try(lulcc.grrffeatselect(
  transition_result = x$transition_result,
                     cov_data = x$covdata_collinearity_filtered,
                     weight_vector = x$embedded_weight_vector,
                      gamma = 0.5), TRUE)
  })
  
#save output
Save_path_embedded <- paste0(embedded_folder_path, "/", Data_period_name, "_GRRF_embedded_filtered_covs_", Dataset_scale)
saveRDS(GRRF_selection_results, file =  Save_path_embedded)

cat(paste0(' GRRF embedded covariate selection done \n'))

### =========================================================================
### D- Combine results of covariate selection procedures:
### Collinearity filtering + Embedded covariate selection
### =========================================================================

#merge the two lists of results based on names
Combined_covariate_selection_results <- list.merge(collin_selection_results, GRRF_selection_results) 

#Drop unnecessary list items such as weight vector and transition result and rename results
Combined_covariate_selection_results_final <- lapply(Combined_covariate_selection_results, function(x){
  collinearity_filtered_covs <- colnames(x[["covdata_collinearity_filtered"]])
  GRRF_embedded_selected_covs <- do.call(cbind.data.frame, (covariate = list(x[["var"]], 
                                                            Mean_decrease_Gini = x[["MeanDecreaseGini"]],
                                                            Rank = x[["rank"]])))
  output <- list(collinearity_filtered_covs, GRRF_embedded_selected_covs)
  names(output) <- c("collinearity_filtered_covs", "GRRF_embedded_selected_covs")
  return(output)
})

Combined_results_save_path <- paste0(Combined_results_folder_path, "/", Data_period_name, "_combined_FS_results_", Dataset_scale)
saveRDS(Combined_covariate_selection_results_final, Combined_results_save_path)

cat(paste0(' Results of covariate selection combined and saved \n'))

### =========================================================================
### E- Subsetting datasets with results of covariate selection
### =========================================================================

#Reload full datasets  
Datasets_for_period <- readRDS(list.files(Pre_FS_folder, pattern = file_match_regex, full.names = TRUE))

# merge them 
Datasets_filtering_results_combined <- list.merge(Datasets_for_period, GRRF_selection_results)

# Perform subsetting
Filtered_covariates_final <- lapply(Datasets_filtering_results_combined, function(x) { 
  output <- list(trans_result = x[["trans_result"]], #vector of transitions unchanged
                 cov_data = x[["cov_data"]][, .SD, .SDcols = unlist(x["var"])], #data.table of cov_data subsetted by the names of variables returned by the GRRF embedded selection
                 non_cov_data = x[["non_cov_data"]], #non_cov_data unchanged
                 collinearity_weight_vector = x[["collin_weights"]], #collinearity weight_vector unchanged
                 embedded_weight_vector = x[["embed_weights"]], #embed weight_vector unchanged
                imbalance_ratio =  x[["imbalance_ratio"]], #class imbalance ratio
                num_units = x[["num_units"]]) #number of units
  return(output)
  })

#save final filtered datasets
Filtered_datasets_save_path <- paste0(Filtered_datasets_folder_path, "/", Data_period_name, "_filtered_predictors_", Dataset_scale) 
saveRDS(Filtered_covariates_final, Filtered_datasets_save_path)

cat(paste0(' Transitions datasets subsetted to filtered covariates \n'))


### =========================================================================
### F- Identifying focal layers in final covariate selection for dynamic updating in simulations
### =========================================================================

lulcc.identifyfocalsforupdating(Final_cov_selection = GRRF_selection_results,
                               Data_period_name = Data_period_name,
                               Dataset_scale = Dataset_scale)

cat(paste0(' Focal layers identified for updating during simulation \n'))

### =========================================================================
### G- Summarise results of feature selection
### =========================================================================

#vector describing summary levels
summary_levels <- c("across_trans", "Bioregion", "Initial_lulc", "Final_lulc")

#use Wrapper function to summarise feature selection results
lulcc.evalfeatureselection(Data_period_name = Data_period_name,
                                                           Dataset_scale = Dataset_scale,
                                                           Pre_FS_folder = Pre_FS_folder,
                                                           FS_results_folder = FS_results_folder,
                                                           summary_levels = summary_levels)

cat(paste0(' Results of covariate selection summarized \n'))

#} #close wrapper function

lapply(Datasets_for_FS[3], function(x) lulcc.featureselection(Dataset_details = x))

cat(paste0(' Covariate selection complete \n'))




