### =========================================================================
### lulcc.evalfeatureselection: Wrapper function to summarise the results of 
### the feature selection process according to different aspects of the 
### transitions datasets 
### =========================================================================
#' @param Data_period_name Character name of dataset period to be used in file saving.
#' @param Dataset_scale Character, scale of datasets to be used in file saving.
#' @param Pre_FS_folder Character, folder path for pre-covariate selection datasets. 
#' @param FS_results_folder Character folder path to save summaries.
#' @param summary_levels vector of characters describing aspect of transition
#'   datasets to summarize by accepts:
#'   "across_trans", "Bioregion", "Initial_lulc", "Final_lulc".
#'
#' @author Ben Black
#' @export


lulcc.evalfeatureselection <- function(Predictor_table_path,
                                       Data_period_name,
                                       Dataset_scale,
                                       Pre_FS_folder,
                                       FS_results_folder,
                                       summary_levels){
  
### =========================================================================
### A - Load results
### =========================================================================  

#load covariate table to use the names for splitting
Covariate_table <-  data.table(read.xlsx(Predictor_table_path, sheet = paste0(Data_period_name)))  

#load in results of 2 step covariate selection 
Feature_selection_results <- unlist(readRDS(list.files(FS_results_folder, full.names = TRUE, pattern = paste0(Data_period_name, "_combined"))), recursive = FALSE)

#Within the Feature_selection results convert the dataframe of embedded covariate selection results into just a vector of the variables selected. 
FS_results<- lapply(Feature_selection_results, function(x) {
    collinearity_filtered_covs <- x[["collinearity_filtered_covs"]]
  GRRF_embedded_selected_covs <- x[["GRRF_embedded_selected_covs"]][, 1]
  output <- list(collinearity_filtered_covs = collinearity_filtered_covs, GRRF_embedded_selected_covs = GRRF_embedded_selected_covs)
  return(output)
})

#regex to load pre selection datasets
dataset_regex <- glob2rx(paste0(Data_period_name, "*", Dataset_scale))

### =========================================================================
### B - extract a list of all unique covariates in transitions datasets
### =========================================================================

Pre_selection_data <- unlist(readRDS(list.files(Pre_FS_folder, full.names = TRUE, pattern = dataset_regex)), recursive = FALSE)
  
Unique_covs <- lapply(Pre_selection_data, function(x){

#Identify the covariate data
cov_data <- x[, .SD, .SDcols = Covariate_table$Covariate_ID]

#remove cols which only have 1 unique value
cov_data <- Filter(function(x)(length(unique(x))>2), cov_data)
covariate_names <- colnames(cov_data)

return(covariate_names)
})

#remove unnecessary objects from memory
rm(Pre_selection_data)

### =========================================================================
### C- Produce summaries from Feature selection results
### =========================================================================

#loop over summary levels 
Feature_selection_summaries <- lapply(summary_levels, function(summarize_by){
  
FS_summary <- lulcc.analysecovselection(All_cov_names = Unique_covs, cov_selection_results = FS_results, summary_level = summarize_by)
if(summarize_by != "across_trans"){
FS_summary <- lulcc.summarisecovselection(nested_list_of_trans = FS_summary, split_by = summarize_by)  
}
return(FS_summary)  
})

#rename
names(Feature_selection_summaries) <- lapply(summary_levels, function(x) paste0("FS_summary_by_", x))

#save results
saveRDS(Feature_selection_summaries, file = paste0("Results/Model_tuning/Covariate_selection/Cov_selection_summaries/", Data_period_name, "_", Dataset_scale, "_feature_selection_summary"))

#final bar chart from summary
Collin_results <- Feature_selection_summaries[["FS_summary_by_Bioregion"]][["Cov_occurence_summary"]][["Cov_occurence_tables"]][["Cov_occurence_after_collinearity_selection"]][,c("Covariate", "total_occurences")]
embedded_results <- Feature_selection_summaries[["FS_summary_by_Bioregion"]][["Cov_occurence_summary"]][["Cov_occurence_tables"]][["Cov_occurence_after_embedded_selection"]][,c("Covariate", "total_occurences")]

Combined_results <- merge(Collin_results, embedded_results, by= "Covariate", all.x = TRUE)
Combined_results[is.na(Combined_results)] <- 0
colnames(Combined_results) <- c("covariate", "Collinearity", "Embedded")
Filtered_results <- Combined_results[(Combined_results$Collinearity > 10 | Combined_results$Embedded >10),]

Long_results <- pivot_longer(Filtered_results, cols = c('Collinearity', 'Embedded'), names_to = "FS_stage", values_to = "num_covs")
Long_results$Clean_cov_name <- str_remove_all(Covariate_table$Covariate_ID[match(Long_results$covariate, Covariate_table$Layer_name)], paste(c("_mean_100m", "_100m", "_nhood"), collapse = "|"))
Long_results$cov_group <- Covariate_table$CA_category[match(Long_results$covariate, Covariate_table$Layer_name)]

FS_predictor_frequency_bar_chart <- 
    Long_results %>% ggplot(aes(y = fct_reorder(Clean_cov_name, num_covs, .desc = FALSE), x = num_covs, fill= FS_stage), alpha = 0.5)+
    geom_bar(position = position_dodge(), stat = "identity")+
 scale_fill_discrete(type = ghibli_palettes$TotoroMedium[5:6],
                        name = "Feature selection stage:",
                        labels = c("Collinearity selection","Embedded selection"))+
    labs(y = "Predictor",
       x= "Frequency of inclusion")+
    theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, size =12),
        axis.text.y = element_text(size =12),
        legend.title = element_text(size=12, face = "bold"), #change legend title font size
        legend.text = element_text(size=12), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position = c(x=0.1, y = 0.85))+coord_flip()

dir.create("Results/Figures/Covariate_selection", recursive = TRUE)

ggsave(plot = FS_predictor_frequency_bar_chart, filename = paste0("Results/Figures/Covariate_selection/", Data_period_name, "_bar_plot_frq_cov_occurence"), device='tiff', dpi=300,  width = 28, height = 20, units = "cm")

} #close wrapper function
