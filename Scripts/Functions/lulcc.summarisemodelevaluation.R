### =========================================================================
### lulcc.summarisemodelevaluation: Aggregate model evaluation metrics across transitions/bioregions
### =========================================================================
#'
#'
#' @param model_eval_results 
#' @param summary_metrics Vector, names of evaluation metrics to summarize by
#' can include: AUC, AUC.S, RMSE, Boyce, Score, threshold, Sensitivity,
#' Specificity, Accuracy, PPV, NPV, Jaccard, TSS Kappa, SEDI. 
#' @param plots Logical, whether to produce summary plots (TRUE or not (FALSE))
#' @returns either a table summarising performance across models or
#' if plots = TRUE a list of the performance table and plots
#' @author Ben Black
#' @export

lulcc.summarisemodelevaluation <- function(model_eval_results, summary_metrics, plots) {

#1. Transform list of model eval results to single data frame      
  #remove any empty results
  complete_results <- model_eval_results[lapply(model_eval_results, length) > 0]
  
  browser()
  #list names of eval metrics
  eval_metrics_names <- rownames(model_eval_results[[1]])
  
  lulc_classes <- c("Alp_Past", "Closed_Forest", "Grassland", "Int_AG", "Open_Forest", "Perm_crops", "Shrubland", "Urban", "Glacier", "Static") #create vector of region names
  
  Region_names <- c("Jura.", "Plateau.", "Northern_Prealps.", "Southern_Prealps.", "Western_Central_Alps.", "Eastern_Central_Alps.") 
  
  Region_names_regex <- regex(paste(paste(Region_names, sep = "_"), collapse = '|'))
    
  #get list of transition names
  transition_names <- unique(lapply(names(complete_results), function(x) {
  name_minus_region <- str_remove_all(str_replace_all(x, Region_names_regex, ''), "_glm")}))
  
  All_trans_models_unlisted <- unlist(complete_results, recursive = FALSE)
  
  list_trans_evals_data_frame <- lapply(complete_results, function(x) as.data.frame(t(data.frame(x))))
  

  list_trans_evals_data_frame_numeric <- lapply(list_trans_evals_data_frame, function(x) sapply(x, function(y) round(as.numeric(y),digits = 3), simplify = FALSE))
  
  All_evals_data_frame <- rbindlist(list_trans_evals_data_frame_numeric, use.names = TRUE)
  
  rownames(All_evals_data_frame) <- c(names(complete_results))
  All_evals_data_frame <- rownames_to_column(All_evals_data_frame, var= "trans_name")

  model_numbers <- regmatches(All_evals_data_frame$trans_name, gregexpr("[[:digit:]]+", All_evals_data_frame$trans_name))
  

  #add columns for region, transistion and model number
  All_evals_data_frame$Region <- str_match(All_evals_data_frame$trans_name, Region_names_regex) 
  All_evals_data_frame$transition <- str_match(All_evals_data_frame$trans_name, regex(paste(transition_names, collapse = '|')))
  All_evals_data_frame$model <- factor(as.numeric(regmatches(All_evals_data_frame$trans_name, gregexpr("[[:digit:]]+", All_evals_data_frame$trans_name))))
  All_evals_data_frame$initial_lulc <- str_match(All_evals_data_frame$transition, paste(lulc_classes, collapse = '|'))
  All_evals_data_frame$final_lulc <- lapply(str_match_all(All_evals_data_frame$transition, paste(lulc_classes, collapse = '|')), function(x) x[2,])
  #All_evals_data_frame <- All_evals_data_frame %>% rowwise() %>% mutate(model_score = mean(AUC.S, Boyce))
  All_evals_data_frame$Model_score <- rowMeans(All_evals_data_frame[,c('AUC.S', 'Boyce')], na.rm=TRUE)

  if(plots== TRUE){  
  #2. Summarise by Bioregion

  Data_by_region <- split(All_evals_data_frame, All_evals_data_frame$Region)
  
  
  regional.model.eval.plot <- function(regional_data, region_name, eval_metric){
    # create plot
    regional_plot <- ggplot(regional_data, aes_string(color= "model", y=eval_metric, x= "transition", group= "model")) + #note the use aes_string which allows the vector of eval_metric to be called 
    geom_point()+
    theme(axis.text.x = element_text(size = 9,angle = 90))+
    scale_fill_brewer(palette = "Dark2")+
    labs(title = region_name)+
    theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"))
    return(regional_plot)
    }
  
  
  # Produce plots for each bioregional summary table under each summary metric
    Bioregion_plots_list <- list() 
    for(i in summary_metrics){
      
    Bioregion_summary_plots <- mapply(regional.model.eval.plot, regional_data = Data_by_region, region_name = names(Data_by_region), eval_metric = i, SIMPLIFY = FALSE)
    
    Bioregion_combined_plot <- grid.arrange(grobs = Bioregion_summary_plots)

    Bioregion_plots_list[[i]]<- Bioregion_combined_plot
    }


#2. Aggregating by transitions
  
  #create scatter plot for each eval metric supplied in 'summary metric' argument against transitions with coloured by model number and shape by region
  Transitions_plots_list <- list() 
  
  for(i in summary_metrics){
  transition_plot <- ggplot(All_evals_data_frame, aes_string(y= i, x= "transition", color = "model")) + #note the use aes_string which allows the vector of eval_metric to be called 
  geom_point()+
  theme(axis.text.x = element_text(size = 9,angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
      theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
panel.border = element_blank(), 
       panel.background = element_blank(), 
       panel.grid = element_blank(), 
       panel.spacing.x = unit(0,"line"),
        axis.text = element_text(colour = "black"))
  transition_plot_facet <- transition_plot + facet_grid(. ~ initial_lulc, scales='free_x')
  Transitions_plots_list[[paste(i)]] <- transition_plot_facet
  }

#3. aggregate by initial LULC

  Initial_lulc_plots_list <- list()   
  
  for(i in summary_metrics){
  initial_lulc_plot <- ggplot(All_evals_data_frame, aes_string(y= i, x= "initial_lulc", shape = "model" , color = "Region")) + #note the use aes_string which allows the vector of eval_metric to be called 
  geom_point()+
  theme(axis.text.x = element_text(size = 9,angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
        theme(text = element_text(family = "Times New Roman"),
   plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"))
  Initial_lulc_plots_list[[paste(i)]] <- initial_lulc_plot
  }
  
#Violin
#  for(i in summary_metrics){
# initial_lulc_plot <- All_evals_data_frame %>% 
#  group_by(initial_lulc) %>% 
#  ggplot(aes_string(x = "initial_lulc", y = i, shape = "model" , color = "Region")) +
#  geom_violin(aes(fill = "initial_lulc"), alpha = .1, show.legend = FALSE) +
#  geom_jitter(alpha = .9, width = .08) +
#  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "black", width = .5) + 
#  stat_summary(fun = mean, geom = "point", colour = "black", size = 2) +
#  guides(colour = FALSE) +
#  theme(text = element_text(family = "Times New Roman"),
#        plot.title = element_text(size = rel(1.1), hjust = 0.5),
#        axis.line = element_line(1),
#        panel.background = element_blank(),
#        axis.text = element_text(colour = "black"))
#  Initial_lulc_plots_list[[paste(i)]] <- initial_lulc_plot
#  }
  

#4. aggregate by final LULC  
  
    Final_lulc_plots_list <- list()
    
  for(i in summary_metrics){
  final_lulc_plot <- ggplot(All_evals_data_frame, aes_string(y= i, x= "final_lulc", shape = "model" , color = "Region")) + #note the use aes_string which allows the vector of eval_metric to be called 
  geom_point()+
  theme(axis.text.x = element_text(size = 9,angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
    theme(text = element_text(family = "Times New Roman"),
   plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"))
  Final_lulc_plots_list[[paste(i)]] <- final_lulc_plot
  }
    
#Violin
#  for(i in summary_metrics){
#  final_lulc_plot <- All_evals_data_frame %>% 
#  group_by(final_lulc) %>% 
#  ggplot(aes_string(x = "final_lulc", y = i, shape = "model" , color = "Region", size = "class_imbalance")) +
#  geom_violin(aes(fill = "final_lulc"), alpha = .1, show.legend = FALSE) +
#  guides(colour = FALSE) +
#  labs(x = "Outcome Period", y = "ATT") +
#  theme(text = element_text(family = "Times New Roman"),
#        plot.title = element_text(size = rel(1.1), hjust = 0.5),
#        axis.line = element_line(1),
#        panel.background = element_blank(),
#        axis.text = element_text(colour = "black"))
#  Final_lulc_plots_list[[paste(i)]] <- final_lulc_plot
#  }    
    
# Class imbalance vs summary metric
    
  Class_imbalance_plots_list <- list()
    
  for(i in summary_metrics){
  class_imbalance_plot <- ggplot(All_evals_data_frame, aes_string(y= i, x= "class_imbalance", shape = "model" , color = "num_units")) + #note the use aes_string which allows the vector of eval_metric to be called 
  geom_point()+
    scale_color_gradient()+
    xlim(NA, 1200)+
  theme(axis.text.x = element_text(size = 9,angle = 90))+
  scale_fill_brewer(palette = "Dark2")+
    theme(text = element_text(family = "Times New Roman"),
   plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"))
  Class_imbalance_plots_list[[paste(i)]] <- class_imbalance_plot
  }
        
#5. format output     
    
  output <- list(Bioregion_plots_list, Transitions_plots_list, All_evals_data_frame, Initial_lulc_plots_list, Final_lulc_plots_list, Class_imbalance_plots_list)
  names(output) <- c("Bioregion_plots_list", "Transitions_plots_list", "tabular_output", "Initial_lulc_plots_list", "Final_lulc_plots_list", "Class_imbalance_plots_list")
  return(output)
}else {
  tabular_output <- All_evals_data_frame
  return(tabular_output)
} # close loop over plots 

} #close function







