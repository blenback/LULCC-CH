### =========================================================================
### lulcc.analysedownsampling: Produce plots and perform statistical analysis to compare downsampling vs. regular RF 
### =========================================================================
#' @author Ben Black
#' @export

lulcc.analysedownsampling <- function(Comparative_table, summary_metrics){  

comparison_output <- list()
  
for(i in 1:length(eval_metrics)){#loop over model eval metrics  
  
#calculate changes in AUC and Score under downsampling
Change_in_eval_metric <- Comparative_table[, .SD, .SDcols = c("trans_name", "Model_name", eval_metrics[i], "imbalance_ratio", "num_units", "model")] %>%
 pivot_wider(names_from = Model_name, values_from = c(eval_metrics[i])) 
Change_in_eval_metric <- Change_in_eval_metric %>% mutate(model_diff = .[[5]] - .[[6]])

num_increase_performance <- sum(Change_in_eval_metric$model_diff > 0)
num_decrease_performance <- sum(Change_in_eval_metric$model_diff < 0)

avg_increase <- format(round(mean(Change_in_eval_metric$model_diff[Change_in_eval_metric$model_diff > 0]), 3), nsmall =3) 
avg_decrease <- format(round(mean(Change_in_eval_metric$model_diff[Change_in_eval_metric$model_diff < 0]), 3), nsmall =3)  


Change_in_eval_metric <- Change_in_eval_metric[Change_in_eval_metric$model_diff != 0,] %>% 
  mutate(Color = ifelse(model_diff > 0, paste("Positive", " n=", num_increase_performance, "(avg. increase =", avg_increase, ")"), paste("Negative", " n=", num_decrease_performance, "(avg. decrease =", avg_decrease, ")")), Color = factor(Color))

  #create a plot from change in eval metric values 
  Change_eval_metric_plot <-  ggplot(data = Change_in_eval_metric, aes(y= model_diff, x= imbalance_ratio, colour = model_diff > 0)) + 
  geom_point(aes(y= model_diff, x= imbalance_ratio, colour = Color), alpha = 0.7)+
  scale_colour_manual(name = 'Valency', values = setNames(c('darkolivegreen4','darkred'), c("Positive", "Negative")), labels = c(paste("Negative", " n=", num_decrease_performance), paste("Positive", " n=", num_increase_performance)))+
  labs(y = paste0("Change in ", str_replace(eval_metrics[i], "_", " ")), x= "Class imbalance (ratio minority:majority)", colour = "Valency") +
  scale_x_continuous(limits = c(0, 500), breaks = seq(from = 0, to = 500, by = 50))+
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
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
  
Change_eval_metric_plot_3d <- plot_ly(data = Change_in_eval_metric, x= ~imbalance_ratio,
                                      y= ~num_units,
                                      z= ~model_diff,
                                      type="scatter3d",
                                      mode="markers",
                                      color= ~Color,
                                      alpha = 0.8,
                                      colors = c('darkred','darkolivegreen4'))%>%
  layout(scene = list(xaxis = list(
               title= list(text = 'Class imbalance (ratio minority:majority)'),
                            nticks = 10,
                            range = c(0,1000)),
        yaxis = list(
               title= list(text = 'Number of units')),
        zaxis = list(
               title= list(text = paste0("Change in ", str_replace(eval_metrics[i], "_", " "))),
                           nticks = 15,
                           range = c(-0.1,0.2))),
        legend = list(title= 'Valency', x = 0.1, y = 0.9),
        font= list(size=12, family='Times New Roman')) 
  
  #summary statistics 
  general_stat_summary <- Comparative_table %>%
  group_by(Model_name) %>%
  get_summary_stats(eval_metrics[i], type = "common") 

#vector general model formula   
model_formula <- as.formula(paste0(eval_metrics[i], "~Model_name"))   

#vector friedman formula
friedman_formula <- as.formula(paste0(eval_metrics[i], " ~ Model_name|trans_name"))

"Shapiro-wilks test for normality of residuals"
Shapiro_test <- shapiro.test(residuals(lm(model_formula, data= Comparative_table)))

#Bartlett test for homoscedasticity of variance
Bartlett_test <- bartlett.test(model_formula, data= Comparative_table)

#plot results of anova
invisible(Anova_plot <- aov(model_formula, data= Comparative_table))

#Friedman test
Comparative_table$trans_name <-factor(Comparative_table$trans_name) #convert trans_id to a factor
Friedman_output <- try(friedman_test(friedman_formula, data= Comparative_table[order(trans_name),]))

#Pairwise comparisons
pwc_eval_metric <- Comparative_table %>%
  wilcox_test(model_formula, paired = TRUE, p.adjust.method = "bonferroni")

Eval_metric_violin <- ggwithinstats(
  data = Comparative_table,
    plot.type = "boxviolin",
  x = Model_name,
  y = !!eval_metrics[i],
  xlab = "Class balance adjustment",
  ylab = str_replace(eval_metrics[i], "_", " "),
  k= 3,
  type = "nonparametric",
  pairwise.comparisons = TRUE, ## display pairwise comparisons
  bf.message = FALSE,
  centrality.label.args = list(size = 3, label.size = 0, nudge_y = -0.02, force = 3),
  results.subtitle = TRUE,
  sample.size.label = F,
  ggplot.component = list(
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 8))))

  
outputlist <- list(Change_in_eval_metric, avg_increase, avg_decrease, Change_eval_metric_plot,Change_eval_metric_plot_3d,  general_stat_summary, Shapiro_test, Bartlett_test, Anova_plot, Friedman_output, pwc_eval_metric, Eval_metric_violin)
names(outputlist) <- c("Changes_in_model_performance", "avg_increase", "avg_decrease", "Change_performance_plot", "Change_performance_plot_3d",  "Summary_stats", "Shapiro_test", "Bartlett_test", "Anova_plot", "Friedman_output", "Pairwise_comparions", "Eval_metric_violin")

comparison_output[[eval_metrics[i]]] <- outputlist
} #close for loop over eval_metrics
return(comparison_output)
} #close function
