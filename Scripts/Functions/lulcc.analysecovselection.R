### =========================================================================
### lulcc.analysecovselection: Produce summary information on results of
### 2 stage covariate selection procedure
### =========================================================================
#' 
#' @param All_pred_names list of unique covariate names 
#'   produced by function lulcc.evalfeatureselection 
#' @param Filtered_predictors list object containing results of 
#'   covariate selection produced by function lulcc.evalfeatureselection
#' @param summary_level Characters describing aspect of transition
#'   datasets to summarize by accepts:
#'   "across_trans", "Bioregion", "Initial_lulc", "Final_lulc".
#'
#' @author Ben Black
#' @export


lulcc.analysecovselection <- function(All_pred_names, Filtered_predictors, summary_level) {
  
#Instantiate small functions

MOCT <- function(x) {
 list(Mean= mean(x),
  Median = median(x),
  Mode = getmode(x),
  Standard_Deviation = sd(x),
  Min = min(x),
  Max = max(x))
}

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}  


if(summary_level=="across_trans"){
#create vector of all covariates selected by each selection process
# collinearity based selection
All_collinearity_selected_covs <- unlist(lapply(Filtered_predictors, function(x) x[["collinearity_preds"]]))

#Embedded GRRF selection
All_embedded_selected_covs <- unlist(lapply(Filtered_predictors, function(x) x[["GRRF_preds"]])) 

#Result 1: How many of the full selection of covariates remain after each stage of selection?
#for this we need to reduce the full vectors of covariates to just the unique values they contain
Covs_remaining_after_collinearity_selection <- length(unique(All_collinearity_selected_covs))
Covs_remaining_after_embedded_selection <- length(unique(All_embedded_selected_covs))

#Result 2: How many times are each of the covariates used across all transitions?
Collinearity_selected_variable_occurence <- data.frame(table(All_collinearity_selected_covs)) %>% arrange(desc(Freq))
Embedded_selected_variable_occurence <- data.frame(table(All_embedded_selected_covs)) %>% arrange(desc(Freq))

#Result 3: Measures of central tendency for number of covariates per transition at each stage of filtering

#First count the number of covariates selected under each process for each transition
Number_of_Covariates_selected_both <- lapply(Filtered_predictors, function(x) {
    collinearity_selected_covs <- length(x[["collinearity_preds"]])
  GRRF_preds <- length(x[["GRRF_preds"]])
  output <- list(collinearity_selected_covs, GRRF_preds)
  names(output) <- c("collinearity_selected_covs", "GRRF_preds")
  return(output)
})


MOCT_collinearity_selection <- MOCT(unlist(lapply(Number_of_Covariates_selected_both, function(x) x[["collinearity_selected_covs"]])))
MOCT_embedded_selection <- MOCT(unlist(lapply(Number_of_Covariates_selected_both, function(x) x[["GRRF_preds"]])))

#create dot plots of differences in covariate numbers across transitions

#bind together unique covs by transitions with results of feature selection
#get number of covariates 
Pre_FS_cov_nums <- rbindlist(lapply(All_pred_names, as.data.frame(length)), id = "Transition")
names(Pre_FS_cov_nums) <- c("Transition", "Pre_FS")

Post_FS_cov_nums <- rbindlist(lapply(Filtered_predictors, function(x){
collinearity <- length(x[["collinearity_preds"]])
embedded <- length(x[["GRRF_preds"]])
output <- as.data.frame(cbind(collinearity, embedded))
}), use.names = TRUE, idcol = "Transition")

#combine con numbers, Pre FS and after each step
cov_nums_combined <- merge(Pre_FS_cov_nums, Post_FS_cov_nums)
cov_nums_combined$Dummy <- 24
cov_nums_combined$Trans_redact <- factor(sapply(cov_nums_combined$Transition, function(x) str_replace_all(paste(str_split(x, "\\.")[[1]][2:3], collapse = " - "), "_", " ")))

#convert to long format
long_df <- pivot_longer(cov_nums_combined, cols = c("Pre_FS", "collinearity", "embedded"), names_to = "FS_stage", values_to = "num_covs")
long_df$Pre_FS <- long_df$num_covs
long_df$num_covs[long_df$FS_stage == "Pre_FS"] <- 24
long_df$FS_stage <- factor(long_df$FS_stage, levels = c("Pre_FS", "collinearity", "embedded"))
long_df$region <- sapply(long_df$Transition, function(x) str_split(x, "\\.")[[1]][1])


#Dot plot of changes in cov number faceted by regions

#create tidy facet labels
region.labs <- str_replace_all(unique(long_df$region), "_", " ") 
names(region.labs) <- unique(long_df$region)

Dot_plot_num_cov_changes  <- ggplot()+ geom_point(data = long_df,aes(x = num_covs, y = Trans_redact, colour = FS_stage), alpha = 0.7)+
    geom_line(data = long_df, aes(x = num_covs, y = Trans_redact, group = Trans_redact), linetype = 1, alpha = 0.1)+
    geom_text(data = long_df[long_df$FS_stage == "Pre_FS",], aes(label= Pre_FS, x = Dummy, y = Trans_redact), hjust = 0, nudge_x = 0.5, size = 2)+
    scale_x_continuous(limits = c(2, 26), breaks = seq(from = 2, to = 22, by = 2))+
    scale_colour_discrete(type = ghibli_palettes$MononokeMedium,
                        name = "Feature selection stage:",
                        labels = c("Pre-feature selection", "Collinearity selection","Embedded selection"))+
    labs(y = "Transition",
       x= "Number of predictors")+
    facet_wrap(~region, nrow = 3, ncol =2, labeller = labeller(region = region.labs))+
    theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size=8, face = "bold"), #change legend title font size
        legend.text = element_text(size=8), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="top")



#creating an aggregated plot by transitions across regions
Aggregated_cov_nums <- cov_nums_combined %>%
  group_by(Trans_redact) %>% 
  dplyr::summarise(mean_Pre_FS = signif(mean(Pre_FS), digits =2),
                   mean_collinearity = signif(mean(collinearity), digits =2),
                   mean_embedded = signif(mean(embedded), digits =2),
                   sd_embedded = sd(embedded))

long_df_agg <- pivot_longer(Aggregated_cov_nums, cols = c("mean_Pre_FS", "mean_collinearity", "mean_embedded"), names_to = "FS_stage", values_to = "num_covs")
long_df_agg$org_covs <- long_df_agg$num_covs
long_df_agg$num_covs[long_df_agg$FS_stage == "mean_Pre_FS"] <- 24
long_df_agg$Dummy <- 24
long_df_agg$FS_stage <- factor(long_df_agg$FS_stage, levels = c("mean_Pre_FS", "mean_collinearity", "mean_embedded"))


Agg_Dot_plot_num_cov_changes  <- ggplot(long_df_agg)+ 
    geom_point(aes(x = num_covs, y = Trans_redact, colour = FS_stage), alpha = 0.7)+
    geom_line(aes(x = num_covs, y = Trans_redact, group = Trans_redact), linetype = 2, alpha = 0.3)+
    geom_text(data = long_df_agg[long_df_agg$FS_stage == "mean_Pre_FS",], aes(label= org_covs, x = Dummy, y = Trans_redact), hjust = 0, nudge_x = 0.5, size = 2)+
    geom_errorbar(data = long_df_agg[long_df_agg$FS_stage == "mean_embedded",], aes(x = num_covs, y = Trans_redact, xmin = num_covs-sd_embedded, xmax = num_covs+sd_embedded),
                  width=.3,
                  position=position_dodge(0.05), 
                  colour = ghibli_palettes$MononokeMedium[3])+
    scale_x_continuous(limits = c(4, 25), breaks = seq(from = 4, to = 22, by = 2))+
    scale_colour_discrete(type = ghibli_palettes$MononokeMedium,
                        name = "Feature selection stage:",
                        labels = c("Pre-feature selection", "Collinearity selection","Embedded selection"))+
    labs(y = "Transition",
       x= "Number of predictors")+
    theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size=8, face = "bold"), #change legend title font size
        legend.text = element_text(size=8), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="top")


#Bind items as list to return
Output <-list(Covs_remaining_after_collinearity_selection, Covs_remaining_after_embedded_selection, Collinearity_selected_variable_occurence, Embedded_selected_variable_occurence, MOCT_collinearity_selection, MOCT_embedded_selection, Dot_plot_num_cov_changes, Agg_Dot_plot_num_cov_changes)  
names(Output) <- c("Covs_remaining_after_collinearity_selection", "Covs_remaining_after_embedded_selection", "Collinearity_selected_variable_occurence", "Embedded_selected_variable_occurence", "MOCT_collinearity_selection", "MOCT_embedded_selection", "Dot_plot_num_cov_changes", "Agg_Dot_plot_num_cov_changes")
return(Output)  
}

if(summary_level=="Bioregion"){
  
#nest data to bioregion
Bioregion_Covariate_selection_variable_names_both <- lulcc.listbybioregion(Filtered_predictors)

#Get vectors of all covariates under each selection process in each Bioregion
All_covariates_by_Bioregion <- lapply(Bioregion_Covariate_selection_variable_names_both, function(x) {
  list(Collinearity_selected_covs = unlist(lapply(x, function(y) y[["collinearity_preds"]])),
  Embedded_selected_covs =unlist(lapply(x, function(y) y[["GRRF_preds"]]))
    )
})

#Result 1: How many of the full selection of covariates remain after each stage of selection
cov_selection_Bioregion_results <- lapply(All_covariates_by_Bioregion, function(x) {
Covs_remaining_after_collinearity_selection <- length(unique(x[["Collinearity_selected_covs"]]))
Covs_remaining_after_embedded_selection <- length(unique(x[["Embedded_selected_covs"]]))

#Result 2: How many times are each of the covariates used across all transitions?
Collinearity_selected_variable_occurence <- data.frame(table(x[["Collinearity_selected_covs"]])) %>% arrange(desc(Freq))
Embedded_selected_variable_occurence <- data.frame(table(x[["Embedded_selected_covs"]])) %>% arrange(desc(Freq))

#Bind items as list to return
Output <-list(Covs_remaining_after_collinearity_selection, Covs_remaining_after_embedded_selection, Collinearity_selected_variable_occurence, Embedded_selected_variable_occurence)  
names(Output) <- c("Covs_remaining_after_collinearity_selection", "Covs_remaining_after_embedded_selection", "Collinearity_selected_variable_occurence", "Embedded_selected_variable_occurence")
return(Output)  
})

#create visualisation i.e bar plot number of occurrence each covariate

#Result 3: Measures of central tendency for number of covariates per transition at each stage of filtering

#First count the number of covariates selected under each process for each transition
Number_of_Covariates_selected_by_Bioregion <- lapply(Bioregion_Covariate_selection_variable_names_both, function(x) lapply(x, function(y) {
    list(Num_collinearity_selected_covs = length(y[["collinearity_preds"]]),
  Num_GRRF_preds = length(y[["GRRF_preds"]]))   
}))

MOCT_results <- lapply(Number_of_Covariates_selected_by_Bioregion, function(x) {
list(MOCT_collinearity_selection = MOCT(unlist(lapply(x, function(y) y[["Num_collinearity_selected_covs"]]))),
MOCT_embedded_selection = MOCT(unlist(lapply(x, function(z) z[["Num_GRRF_preds"]])))
)
})


return(list.merge(cov_selection_Bioregion_results, MOCT_results))

}


if(summary_level=="Initial_lulc"){
  
#nest data to initial_lulc
Covariate_selection_variable_names_both_LULC <- lulcc.listbylulc(Filtered_predictors, initial_or_final = "Initial")

#Get vectors of all covariates under each selection process grouped by initial lulc class
All_covariates_by_LULC <- lapply(Covariate_selection_variable_names_both_LULC, function(x) {
  list(Collinearity_selected_covs = unlist(lapply(x, function(y) y[["collinearity_preds"]])),
  Embedded_selected_covs =unlist(lapply(x, function(y) y[["GRRF_preds"]]))
    )
})


#Result 1: How many of the full selection of covariates remain after each stage of selection
cov_selection_lulc_results <- lapply(All_covariates_by_LULC, function(x) {
Covs_remaining_after_collinearity_selection <- length(unique(x[["Collinearity_selected_covs"]]))
Covs_remaining_after_embedded_selection <- length(unique(x[["Embedded_selected_covs"]]))

#Result 2: How many times are each of the covariates used across all transitions?
Collinearity_selected_variable_occurence <- data.frame(table(x[["Collinearity_selected_covs"]])) %>% arrange(desc(Freq))
Embedded_selected_variable_occurence <- data.frame(table(x[["Embedded_selected_covs"]])) %>% arrange(desc(Freq))

#Bind items as list to return
Output <-list(Covs_remaining_after_collinearity_selection, Covs_remaining_after_embedded_selection, Collinearity_selected_variable_occurence, Embedded_selected_variable_occurence)  
names(Output) <- c("Covs_remaining_after_collinearity_selection", "Covs_remaining_after_embedded_selection", "Collinearity_selected_variable_occurence", "Embedded_selected_variable_occurence")
return(Output)  
})

#create visualisation i.e bar plot number of occurrence each covariate

#Result 3: Measures of central tendency for number of covariates per transition at each stage of filtering

#First count the number of covariates selected under each process for each transition
Number_of_Covariates_selected_by_lulc <- lapply(Covariate_selection_variable_names_both_LULC, function(x) lapply(x, function(y) {
    list(Num_collinearity_selected_covs = length(y[["collinearity_preds"]]),
  Num_GRRF_preds = length(y[["GRRF_preds"]]))   
}))

MOCT_results <- lapply(Number_of_Covariates_selected_by_lulc, function(x) {
list(MOCT_collinearity_selection = MOCT(unlist(lapply(x, function(y) y[["Num_collinearity_selected_covs"]]))),
MOCT_embedded_selection = MOCT(unlist(lapply(x, function(z) z[["Num_GRRF_preds"]])))
)
})


return(list.merge(cov_selection_lulc_results, MOCT_results))

}

if(summary_level=="Final_lulc"){
  
#nest data to initial_lulc
Covariate_selection_variable_names_both_LULC <- lulcc.listbylulc(Filtered_predictors, initial_or_final = "Final")

#Get vectors of all covariates under each selection process grouped by final lulc class
All_covariates_by_LULC <- lapply(Covariate_selection_variable_names_both_LULC, function(x) {
  list(Collinearity_selected_covs = unlist(lapply(x, function(y) y[["collinearity_preds"]])),
  Embedded_selected_covs =unlist(lapply(x, function(y) y[["GRRF_preds"]]))
    )
})


#Result 1: How many of the full selection of covariates remain after each stage of selection
cov_selection_lulc_results <- lapply(All_covariates_by_LULC, function(x) {
Covs_remaining_after_collinearity_selection <- length(unique(x[["Collinearity_selected_covs"]]))
Covs_remaining_after_embedded_selection <- length(unique(x[["Embedded_selected_covs"]]))

#Result 2: How many times are each of the covariates used across all transitions?
Collinearity_selected_variable_occurence <- data.frame(table(x[["Collinearity_selected_covs"]])) %>% arrange(desc(Freq))
Embedded_selected_variable_occurence <- data.frame(table(x[["Embedded_selected_covs"]])) %>% arrange(desc(Freq))

#Bind items as list to return
Output <-list(Covs_remaining_after_collinearity_selection, Covs_remaining_after_embedded_selection, Collinearity_selected_variable_occurence, Embedded_selected_variable_occurence)  
names(Output) <- c("Covs_remaining_after_collinearity_selection", "Covs_remaining_after_embedded_selection", "Collinearity_selected_variable_occurence", "Embedded_selected_variable_occurence")
return(Output)  
})

#create visualisation i.e bar plot number of occurrence each covariate

#Result 3: Measures of central tendency for number of covariates per transition at each stage of filtering

#First count the number of covariates selected under each process for each transition
Number_of_Covariates_selected_by_lulc <- lapply(Covariate_selection_variable_names_both_LULC, function(x) lapply(x, function(y) {
    list(Num_collinearity_selected_covs = length(y[["collinearity_preds"]]),
  Num_GRRF_preds = length(y[["GRRF_preds"]]))   
}))

MOCT_results <- lapply(Number_of_Covariates_selected_by_lulc, function(x) {
list(MOCT_collinearity_selection = MOCT(unlist(lapply(x, function(y) y[["Num_collinearity_selected_covs"]]))),
MOCT_embedded_selection = MOCT(unlist(lapply(x, function(z) z[["Num_GRRF_preds"]])))
)
})


return(list.merge(cov_selection_lulc_results, MOCT_results))

}

}