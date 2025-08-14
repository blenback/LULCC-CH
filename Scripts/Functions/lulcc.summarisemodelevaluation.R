### =========================================================================
### lulcc.summarisemodelevaluation: Aggregate model evaluation metrics across transitions/bioregions
### =========================================================================
#'
#' Function to summarise results of transition modelling for a given specification under the different model hyper-parameters
#' saves a rds file of the evaluation results across the different transitions and an .rds object of various plots if specified.
#'
#' @param eval_results_folder Character, path to folder containing model evaluation results
#' @param data_period Character, string for the modelling period
#' @param model_tag Character, string for the model tag
#' @param plots Logical, whether to produce summary plots (TRUE or not (FALSE))
#' @param summary_metrics Vector, names of evaluation metrics to produce plots by if plots == TRUE
#' can include: AUC, AUC.S, RMSE, Boyce, Score, threshold, Sensitivity,
#' Specificity, Accuracy, PPV, NPV, Jaccard, TSS Kappa, SEDI. 
#' @author Ben Black
#' @export

lulcc.summarisemodelevaluation <- function(eval_results_folder,
                                           summary_metrics,
                                           plots,
                                           data_period,
                                           model_tag){
  
  # list files in eval results folder
  eval_results_files <- list.files(paste0(eval_results_folder, data_period), full.names = TRUE)
  
  # read in
  model_eval_results <- lapply(eval_results_files, function(x) readRDS(x))
  names(model_eval_results) <- str_remove_all(basename(eval_results_files), '.rds')
  
  # check if eval_results_folder contains string 'regionalized'
  if(grepl("regionalized", eval_results_folder) == TRUE){
    regionalized <- TRUE
    
    # get list of region names by splitting on first perion in names(model_eval_results)
    region_names <- unique(strsplit(names(model_eval_results), "\\.") %>% lapply(function(x) x[1]) %>% unlist())
    Region_names_regex <- regex(paste(paste(paste0(region_names, "."), sep = "_"), collapse = '|'))
    }

  #remove any empty results
  complete_results <- model_eval_results[lapply(model_eval_results, length) > 0]
  
  #list names of eval metrics
  eval_metrics_names <- rownames(complete_results[[1]])
  
  #get list of transition names
  transition_names <- unique(lapply(names(complete_results), function(x) {
  
    # remove the region tag, if regionalized == TRUE then split on _regionalized
    if(regionalized == TRUE){
      
      # split on _regionalized taking the first element
      transition_name <- strsplit(x, "_regionalized")[[1]][1]
      
      # remove the region name
      transition_name <- str_replace_all(transition_name, Region_names_regex, '')
    } else if(regionalized == FALSE){
      transition_name <- strsplit(x, "_full")[[1]][1]
    }
    return(transition_name)
  }))
  

  # loop over list entries converting to dataframe
  list_trans_evals_data_frame <- lapply(complete_results, function(x) {
    
    # transpose and convert to df
    eval_df <- as.data.frame(t(data.frame(x)))
  
    # round all columns to 3 decimal places
    eval_df_numeric <- sapply(eval_df, function(y) round(as.numeric(y),digits = 3), simplify = FALSE)
    
    return(eval_df_numeric)
  })
  
  # Bind to a single df
  All_evals_data_frame <- rbindlist(list_trans_evals_data_frame, use.names = TRUE)
  
  # Add column for model name
  All_evals_data_frame$model_name <- c(names(complete_results))

  #add columns for region, transistion and model number
  All_evals_data_frame$Region <- str_match(All_evals_data_frame$model_name, Region_names_regex) 
  All_evals_data_frame$transition <- str_match(All_evals_data_frame$model_name, regex(paste(transition_names, collapse = '|')))
  All_evals_data_frame$model <- factor(as.numeric(regmatches(All_evals_data_frame$model_name, gregexpr("[[:digit:]]+", All_evals_data_frame$model_name))))
  All_evals_data_frame$initial_lulc <- str_split(All_evals_data_frame$transition, "\\.")[[1]][1]
  All_evals_data_frame$final_lulc <- str_split(All_evals_data_frame$transition, "\\.")[[1]][2]
  
  # save the table using the model_tag
  saveRDS(All_evals_data_frame, paste0(eval_results_folder, model_tag, "_modelling_eval_summary.rds"))
  
  # Produce plots if plots == TRUE
  if(plots== TRUE){  
    
    # Summarise by Bioregion
    # Split data by region
    Data_by_region <- split(All_evals_data_frame, All_evals_data_frame$Region)
    
    # function to produce plot
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
    
    
    # Plots by transitions
    
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
    
    # Plots by initial LULC
    
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
    
    #Plots by final LULC  
    
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
    
    
    # Plots of Class imbalance vs summary metric
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
    
    # combine plot lists as single list     
    Plots_list <- list(Bioregion_plots_list, Transitions_plots_list, Initial_lulc_plots_list, Final_lulc_plots_list, Class_imbalance_plots_list)
    names(Plots_list) <- c("Bioregion_plots_list", "Transitions_plots_list", "Initial_lulc_plots_list", "Final_lulc_plots_list", "Class_imbalance_plots_list")
    
    # Save
    saveRDS(Plots_list, paste0(eval_results_folder, model_tag, "_modelling_eval_plots.rds"))
  }

  
} #close function







