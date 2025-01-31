### =========================================================================
### lulcc.plotBoyceandROCcurves: Produce Boyce and AUC curves for a
### specified LULC transition under specific models 
### =========================================================================
#'
#'
#' @param RF_specs Character specifying which group RF models to plot curves for  
#' @param GLM_specs Character specifying which GLM models to plot curves for 
#' @param RF_model_num Numeric to identify which RF model according to number of trees 
#' @param transition_name Character to identify which LULC transition to produce curves for
#' @returns List containing seperate ROC and Boyce plots as well as a combined plot
#'  and data for ROC plot 
#' @author Ben Black
#' @export


lulcc.plotBoyceandROCcurves <- function(RF_specs, GLM_specs, RF_model_num, transition_name){

### =========================================================================
### A- Load fitted models
### =========================================================================

#name model specs
names(RF_specs) <- lapply(RF_specs, function(x) paste0("RF_", x))
names(GLM_specs) <- lapply(GLM_specs, function(x) paste0("GLM_", x))

#finalise folder paths for fitted models
RF_model_folder_paths <- lapply(RF_specs, function(x) paste0("Results/Fitted_models/Fitted_rf_models/", x, "/", Data_period_name, "_rf_models"))
GLM_model_folder_paths <- lapply(GLM_specs, function(x) paste0("Results/Fitted_models/Fitted_glm_models/", x, "/", Data_period_name, "_glm_models"))  

#append folder paths for all models
Fitted_model_folder_paths <- append(RF_model_folder_paths, GLM_model_folder_paths)

#load fitted models 
All_models <- unlist(lapply(Fitted_model_folder_paths, function(model_folder, RF_model_num){

  # list all files
  file_list <- list.files(model_folder, pattern = ".rds", full.names = TRUE, recursive=TRUE)
  names(file_list) <- sapply(list.files(model_folder, pattern = ".rds", full.names = FALSE, recursive=TRUE), function(x) str_split(x, "/")[[1]][2])
  
  #subset to just one transition
  single_trans_models <- file_list[grep(transition_name, names(file_list))]
  
  #for the RF models subset to just the specific numtrees model to be compared
  if(grepl("RF", model_folder, ignore.case = TRUE)){
  single_trans_models <- single_trans_models[c(grep(RF_model_num, names(single_trans_models)))]}
  
  #Load models
  Fitted_models <- lapply(single_trans_models, function(single_model_path) {
   Fitted_model <- readRDS(single_model_path) #load single model
  })
  
  #basic naming
  #single_trans_names <- list.files(model_folder ,pattern = ".rds", full.names = FALSE, recursive=TRUE)[grep(transition_name, list.files(model_folder ,pattern = ".rds", full.names = FALSE, recursive=TRUE))]
  
  #names(Fitted_models) <- c(lapply(strsplit(single_trans_names, split = "/"), function(x) x[[2]]))
  
  #remove model type from names
  names(Fitted_models) <- lapply(names(Fitted_models), function(x) str_remove_all(x , paste(c(paste0("_rf-", RF_model_num, ".rds"), "_glm.rds"), collapse = "|")))

  return(Fitted_models)},
  RF_model_num = RF_model_num), recursive = FALSE)

### =========================================================================
### B- Re-run evaluation to produce objects needed for Boyce and ROC curves 
### =========================================================================

AUC_Boyce_results <- lapply(All_models, function(x) {
  
  #subset to just model object
  mod <- x[["model"]]
  
  #subset test datasets and fits to just a single replicates results chosen at random
  rep_select <- sample(1:length(mod@tesdat), 1)
  
  mod@tesdat <- mod@tesdat[rep_select]
  mod@fits <- mod@fits[rep_select]
  
  #Get Boyce and AUC objects
  eval_result <- lulcc.evalcurves(mod, crit="maxTSS")
  
  #subset to model performance results 
  single_rep <- unlist(eval_result@performance[[1]], recursive = FALSE)
  })

#shorten name to just model spec
names(AUC_Boyce_results) <- lapply(names(All_models), function(x) str_replace_all(str_remove(x, paste0("\\.", transition_name)), c(unfiltered="full",
                            filtered="reduced",
                            GLM= "LR",
                            LR_full_CH_full = "LR_full\\.full_CH")))

rm(All_models)


# ### =========================================================================
# ### D- plotting Boyce and ROC curves 
# ### =========================================================================

#Boyce curve
Boyce_results_df <- rbindlist(lapply(AUC_Boyce_results, function(x){
  browser()
  Boyce_res <- as.data.frame(x[[2]][c(1,3)])
}), idcol = "model_tag")

#add columns
Boyce_results_df$model <- sapply(Boyce_results_df$model_tag, function(x) str_split(str_split(x, "\\.")[[1]][1], "_")[[1]][1])

Boyce_plot <- Boyce_results_df %>% group_by(model) %>%
ggplot(aes(x = HS, y = F.ratio, colour=model), alpha = 0.7)+
  geom_line(alpha = 0.7)+
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)+
  labs(y = "Predicted/expected ratio", x= "Predicted probability", colour = "Model type:")+
  #scale_colour_ghibli_d("PonyoMedium")+
  scale_colour_manual(values = c("indianred3", "darkolivegreen4"))+
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(0.1, colour = "black"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        legend.position="none",
        legend.key = element_blank(),
        legend.title = element_text(size = 12)
        )+
  stat_cor(aes(color = model, label = ..r.label..),
           method = "spearman",
           cor.coef.name = "rho",
           show.legend = FALSE,
           p.accuracy = 0.001,
           r.accuracy = 0.01,
           size = 4)


#seperate AUC and ROC results and tidy to list of dataframes
ROC_results <- rbindlist(lapply(AUC_Boyce_results, function(x){
  ROC_df <- data.frame(FalsePositive = x[[3]]@x.values,#extract ROCR details to df
                 TruePositive =x[[3]]@y.values,
                 cutoff = x[[3]]@alpha.values,
                 AUC = x[[1]]@y.values[[1]])
  colnames(ROC_df) <- c("FalsePositive","TruePositive", "Cutoff", "AUC")
  return(ROC_df)
}), idcol = "model_tag")

ROC_results$model <- sapply(ROC_results$model_tag, function(x) str_split(str_split(x, "\\.")[[1]][1], "_")[[1]][1])
ROC_results$label <- paste0(ROC_results$model, " ", "(AUC =", ROC_results$AUC, ")")
ROC_results$diff <- ROC_results$TruePositive - ROC_results$FalsePositive

LR_results <- ROC_results[ROC_results$model == "LR" & ROC_results$TruePositive > 0.5]
Max_LR_diff <-LR_results[LR_results$diff == max(LR_results$diff)]

#ROC curves faceted by model type (LR vs. RF)
ROC_plot <- ggplot(alpha = 0.7)+
  geom_line(data = ROC_results, aes(x = FalsePositive, y = TruePositive,  colour=model))+
  #scale_colour_ghibli_d("MononokeMedium")+
      scale_colour_manual(values = c("indianred3", "darkolivegreen4"))+
geom_abline(intercept = 0, slope = 1,
                color = "darkgrey", linetype = "dashed", show.legend = TRUE)+
  geom_point(data = Max_LR_diff, aes(x = FalsePositive, y = TruePositive))+
  geom_text(data = ROC_results %>% filter(FalsePositive == max(FalsePositive)) %>% group_by(model),
    size =4,
    show.legend = FALSE,
    aes(x = 0.05, y = c(0.75, 0.8), label = paste0("AUC= ", signif(AUC, 3)), colour = model))+
  geom_label(data = Max_LR_diff,
    size =3,
    show.legend = FALSE,
    aes(x = FalsePositive +0.05, y = TruePositive - 0.05, label = round(Cutoff,2)))+
  #facet_wrap(~model, ncol = 2)+
  labs(y = "True positive rate", x= "False positive rate", colour = "Model")+
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(0.1, colour = "black"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        legend.position= "bottom",
        legend.key = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.title.align = 0.9,
        legend.direction="horizontal",
        legend.text.align = 0.6,
        legend.background = element_blank())

# Lay out plots
ROC_boyce_plot <- try(ggarrange(plotlist = list(Boyce_plot, ROC_plot),
                    labels = c("a.", "b."),
                    label.x = 0.00,
                    label.y =0.97,
                    ncol = 1,
                    font.label = list(size = 12, color = "black", face = "bold", family = "Times New Roman", position = "top")))

# #save portait sized plot
ggsave(plot = ROC_boyce_plot, filename = paste0("Results/Figures/Model_evaluation/Boyce_ROC_curves/", str_replace_all(transition_name, "\\.", "_"), "_boyce_ROC_curve"), device='tiff', dpi=300,  width = 18, height = 22, units = "cm")

return(list(Boyce_plot = Boyce_plot,
             ROC_Boyce_results = AUC_Boyce_results,
             ROC_plot = ROC_plot,
            ROC_boyce_plot = ROC_boyce_plot))
 } #close function