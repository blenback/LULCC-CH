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

#Import model specifications table
model_specs <- read_excel(Model_specs_path)

#Filter for models with feature selection not required
Filtering_required <- model_specs[model_specs$Feature_selection_employed == "TRUE",]%>%
  group_by(Model_scale)%>%
  distinct(Data_period_name)

#add tag column
Filtering_required$tag <- paste0(Filtering_required$Data_period_name, "_", Filtering_required$Model_scale)

#split into named list
Datasets_for_PS <- lapply(split(Filtering_required, seq(nrow(Filtering_required))), as.list)
names(Datasets_for_PS) <- Filtering_required$tag

#set folder paths 
Pre_PS_folder <- "Data/Transition_datasets/Pre_predictor_filtering" #Pre Predictor selection datasets folder
collinearity_folder_path <- "Results/Model_tuning/Predictor_selection/Collinearity_filtering"
grrf_folder_path <- "Results/Model_tuning/Predictor_selection/GRRF_embedded_selection"
Filtered_datasets_folder_path <- "Data/Transition_datasets/Post_predictor_filtering"
PS_results_folder <- "Results/Model_tuning/Predictor_selection/Predictor_selection_summaries" #Predictor selection results folder

#loop through folders and create any that do not exist
lapply(list(Pre_PS_folder,
                     collinearity_folder_path,
                     grrf_folder_path,
                     Filtered_datasets_folder_path,
                     PS_results_folder), function(x) dir.create(x, recursive = TRUE))

#Predictor table file path (received from output_env, only uncomment here for testing)
#Pred_table_path <- "Tools/Predictor_table.xlsx"

### =========================================================================
### Perform feature selection
### =========================================================================

#wrapper function to run feature selection over multiple scales and datasets
lulcc.featureselection <- function(Dataset_details){

### =========================================================================
### A- Preparation
### =========================================================================  

Data_period <- Dataset_details$Data_period_name
Dataset_scale <- Dataset_details$model_scale
  
# Load the predictor data table that will be used to identify the categories of covariates
#and perform collinearity testing seperately
Predictor_table <- openxlsx::read.xlsx(Pred_table_path, sheet = Data_period)

#vector file paths for the data for the period specified
Data_paths <- list.files(paste0(Pre_PS_folder, "/", Data_period), pattern = Dataset_scale, full.names = TRUE)
names(Data_paths) <- sapply(Data_paths, function(x) basename(x))

### =========================================================================
### B- Stage 1: Collinearity based 'filter' feature selection
### =========================================================================

future::plan(multisession(workers = availableCores()-2))
collin_selection_results <- future_lapply(Data_paths, function(z) {
  
    gc()  
    #load dataset
    Trans_data <- readRDS(z)

    #perform filter based feature selection
    Collin_filtered_data <- try(lulcc.filtersel(
    transition_result = Trans_data[["trans_result"]],
    cov_data = Trans_data[["cov_data"]], 
    categories =  Predictor_table$CA_category[which(Predictor_table$Covariate_ID %in% names(Trans_data[["cov_data"]]))],
    collin_weight_vector = Trans_data[["collin_weights"]],
    embedded_weight_vector = Trans_data[["embed_weights"]],
    focals= c("Neighbourhood"),
    method="GLM",
    corcut=0.7))
    
    #save the result
    Dataset_name <- str_remove(basename(z), ".rds")
    Save_dir <- paste0(collinearity_folder_path, "/", Data_period)
    dir.create(Save_dir, recursive = TRUE)
    Save_path_collinearity <- paste0(Save_dir, "/", Dataset_name, "_collin_filtered.rds")
    saveRDS(Collin_filtered_data, Save_path_collinearity)
    
    gc()
    return(Save_path_collinearity)
}) #close loop over transition datasets
plan(sequential)

cat(paste0(' Collinearity based covariate selection complete \n'))

### =========================================================================
### C- Stage 2: GRRF Embedded feature selection
### =========================================================================

  future::plan(multisession(workers = availableCores()-2))
  GRRF_selection_results <- future_lapply(collin_selection_results, function(x) {
    
  gc()  
  #load dataset
  collin_filtered_data <- readRDS(x)
  
  GRRF_filtered_data <- try(lulcc.grrffeatselect(
  transition_result = collin_filtered_data$transition_result,
                     cov_data = collin_filtered_data$covdata_collinearity_filtered,
                     weight_vector = collin_filtered_data$embedded_weight_vector,
                      gamma = 0.5), TRUE)
  
  #save the result
  Dataset_name <- str_remove(basename(x), ".rds")
  Save_dir <- paste0(grrf_folder_path, "/", Data_period)
  dir.create(Save_dir, recursive = TRUE)
  Save_path_grrf <- paste0(Save_dir, "/", str_replace(Dataset_name, "_collin_filtered", "_GRRF_filtered.rds"))
  saveRDS(GRRF_filtered_data, Save_path_grrf)
  
  gc()
  return(Save_path_grrf)
  })
  plan(sequential)
cat(paste0(' GRRF embedded covariate selection done \n'))

### =========================================================================
### D- Summarize results of predictor selection procedures
### =========================================================================

#loop over the lists of results extracting names of remaining predictors
Filtered_predictors <- lapply(1:length(collin_selection_results),function(i){

  #load each dataset and extract predictor names
  output <- list(collinearity_preds = colnames(readRDS(collin_selection_results[[i]])[["covdata_collinearity_filtered"]]),
                 GRRF_preds = try(readRDS(GRRF_selection_results[[i]])[["var"]]))

})

#rename with transition names
names(Filtered_predictors) <- names(collin_selection_results)

# #get vector of unique predictors in each pre-predictor selection datasets
# Unique_preds <- sapply(Data_paths, function(x){
# 
#   #remove cols which only have 1 unique value
#   cov_data <- Filter(function(y)(length(unique(y))>2), readRDS(x)[["cov_data"]])
#   covariate_names <- colnames(cov_data)
#   return(covariate_names)
#   })
# 
# #vector describing summary levels
# summary_levels <- c("across_trans", "Bioregion", "Initial_lulc", "Final_lulc")
# 
# #loop over summary levels
# Predictor_selection_summaries <- lapply(summary_levels, function(summarize_by){
# 
# # TO DO: fIX CODE FOR SUMMARISING RESULTS OF PREDICTOR SELECTION
# # iNTEGRATING THE PLOT CODE BELOW INTO ONE OF THE FUNCTIONS
#   
# # PS_summary <- lulcc.analysecovselection(All_pred_names = Unique_preds,
# #                                         Filtered_predictors = Filtered_predictors,
# #                                         summary_level = summarize_by)
# #  
# # 
# #  if(summarize_by != "across_trans"){
# #  PS_summary <- lulcc.summarisecovselection(nested_list_of_trans = PS_summary, split_by = summarize_by)  
# #  }
# #  return(PS_summary)  
#   })
# 
# #rename
# names(Predictor_selection_summaries) <- lapply(summary_levels, function(x) paste0("FS_summary_by_", x))
# 
# #save results
# saveRDS(Predictor_selection_summaries, file = paste0(PS_results_folder, "/", Data_period_name, "_", Dataset_scale, "_feature_selection_summary"))
# 
# 
# #final bar chart from summary
# Collin_results <- Feature_selection_summaries[["FS_summary_by_Bioregion"]][["Cov_occurence_summary"]][["Cov_occurence_tables"]][["Cov_occurence_after_collinearity_selection"]][,c("Covariate", "total_occurences")]
# embedded_results <- Feature_selection_summaries[["FS_summary_by_Bioregion"]][["Cov_occurence_summary"]][["Cov_occurence_tables"]][["Cov_occurence_after_embedded_selection"]][,c("Covariate", "total_occurences")]
# 
# Combined_results <- merge(Collin_results, embedded_results, by= "Covariate", all.x = TRUE)
# Combined_results[is.na(Combined_results)] <- 0
# colnames(Combined_results) <- c("covariate", "Collinearity", "Embedded")
# Filtered_results <- Combined_results[(Combined_results$Collinearity > 10 | Combined_results$Embedded >10),]
# 
# Long_results <- pivot_longer(Filtered_results, cols = c('Collinearity', 'Embedded'), names_to = "FS_stage", values_to = "num_covs")
# Long_results$Clean_cov_name <- str_remove_all(Predictor_table$Covariate_ID[match(Long_results$covariate, Predictor_table$Layer_name)], paste(c("_mean_100m", "_100m", "_nhood"), collapse = "|"))
# Long_results$cov_group <- Predictor_table$CA_category[match(Long_results$covariate, Predictor_table$Layer_name)]
# 
# FS_predictor_frequency_bar_chart <- 
#     Long_results %>% ggplot(aes(y = fct_reorder(Clean_cov_name,
#                                                 num_covs,
#                                                 .desc = FALSE),
#                                 x = num_covs,
#                                 fill= FS_stage),
#                                 alpha = 0.5)+
#     geom_bar(position = position_dodge(), stat = "identity")+
#     scale_fill_discrete(type = ghibli_palettes$TotoroMedium[5:6],
#                         name = "Feature selection stage:",
#                         labels = c("Collinearity selection","Embedded selection"))+
#     labs(y = "Predictor",
#        x= "Frequency of inclusion")+
#     theme(text = element_text(family = "Times New Roman"),
#         plot.title = element_text(size = rel(1.1), hjust = 0.5),
#         axis.line = element_line(1),
#         strip.background = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(colour = "black"),
#         axis.text.x = element_text(angle = 90, size =12),
#         axis.text.y = element_text(size =12),
#         legend.title = element_text(size=12, face = "bold"), #change legend title font size
#         legend.text = element_text(size=12), #change legend text font size
#         legend.key = element_rect(fill = "white", colour = "white"),
#         legend.title.align=0.5,
#         legend.position = c(x=0.1, y = 0.85))+coord_flip()
# 
# dir.create("Results/Figures/Covariate_selection", recursive = TRUE)
# 
# ggsave(plot = FS_predictor_frequency_bar_chart,
#        filename = paste0("Results/Figures/Covariate_selection/",
#                          Data_period_name,
#                          "_bar_plot_frq_cov_occurence"),
#        device='tiff',
#        dpi=300,
#        width = 28,
#        height = 20,
#        units = "cm")
# 
# cat(paste0(' Results of covariate selection summarized \n'))

### =========================================================================
### E- Subsetting datasets with results of predictor filtering
### =========================================================================

Filtered_predictors <- lapply(GRRF_results, function(x) {
  output <- list(collinearity_preds = "test",
                 GRRF_preds = readRDS(x)[["var"]])
})


#Loop over the pre-filtering dataset paths subsetting
#the data by the filtering results and at the same time identifying 
future::plan(multisession(workers = availableCores()-2))
future_lapply(1:length(Data_paths),function(i){
  gc()
  
  #read pre-filtering data
  Pre_PS_dat <- readRDS(Data_paths[[i]])
  
  #subset the cov_data component by the names of the
  #remaining predictors following GRRF
  
  Pred_names <- Filtered_predictors[[i]][["GRRF_preds"]]
  Pre_PS_dat[["cov_data"]] <- as.data.frame(Pre_PS_dat[["cov_data"]][,Pred_names])
  colnames(Pre_PS_dat[["cov_data"]]) <- Pred_names
  Post_PS_dat <- Pre_PS_dat
  
  #save
  Post_PS_dir <- paste0(Filtered_datasets_folder_path, "/", Data_period)
  dir.create(Post_PS_dir)
  Post_PS_dat_save_path <- paste0(Post_PS_dir, "/", basename(Data_paths[[i]])) 
  saveRDS(Post_PS_dat, Post_PS_dat_save_path)
  
  gc()
})
plan(sequential)
cat(paste0(' Transitions datasets subsetted to filtered covariates \n'))

### =========================================================================
### F- Identifying focal layers in final covariate selection for dynamic updating in simulations
### =========================================================================

#Identify focal variables
Focal_preds_remaining <- unique(unlist(sapply(Filtered_predictors, function(x){
  Focals <- grep("nhood",x[["GRRF_preds"]] , value = TRUE)
})))

#load focal layer look up table
Focal_lookup <- readRDS("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating/Focal_layer_lookup.rds")

#subset by the current data period
Focal_lookup <- Focal_lookup[Focal_lookup$period == Data_period,]

#subset the focal look up table by the list of focals required for the transition models for this period
Focal_subset <- Focal_lookup[grep(paste(Focal_preds_remaining,collapse="|"), Focal_lookup$layer_name),]

#save the Focal layer details for this period 
saveRDS(Focal_subset, file = paste0("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating/", Data_period, "_", Dataset_scale, "_focals_for_updating"))

cat(paste0(' Focal layers identified for updating during simulation \n'))

#return list of predictors for each dataset as this will also capture try errors
return(Filtered_predictors)
} #close wrapper function

Filtering_overview <- lapply(Datasets_for_PS[2], function(x) lulcc.featureselection(Dataset_details = x))

cat(paste0(' Covariate selection complete \n'))




