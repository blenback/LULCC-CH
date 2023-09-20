### =========================================================================
### lulcc.fitevalsave: Wrapper function to perform model fitting, evaluation and saving
### with either RF or GLM
### =========================================================================
#' 
#' @param Transition_dataset 
#' @param Transition_name Character
#' @param replicatetype Character, (how) should replicates be generated? may be 'none', 'splitsample',
#' 'cv' 'block-cv'.
#' @param reps Numeric number of replicates
#' @param balance_class Logical, should downsampling be implemented in RF? (TRUE/FALSE) 
#' @param Downsampling_bounds Numeric vector, 
#' c(lower bound of class imbalance: Upper bound of class imbalance), 
#' if balance_class = TRUE use to determine which datasets are modelled with downsampling
#' @param Data_period Character
#' @param model_folder Character
#' @param eval_results_folder Character
#' @param Model_type Character
#'
#' @author Antoine Adde adapted by Ben Black
#' @export

lulcc.fitevalsave <- function(Transition_dataset, 
                              Transition_name,
                              replicatetype,
                              reps, 
                              balance_class,
                              Downsampling_bounds,
                              Data_period,
                              model_folder,
                              eval_results_folder,
                              Model_type) {


#separate list of model settings (i.e. different specifications of models)
modinp <- Transition_dataset$model_settings

#create empty lists for evaluation results of models on both testing and training data

eval_list<-list()
model_result_list <- list()
model_save_results <- list()
eval_list_training <-list()


# Loop over different model specifications
for(i in 1:length(modinp)){

# i<-1 (seperate first model for fitting)
modinp_i<- modinp[i]


## 1. Fit model
cat(paste0('fitting ',modinp_i[[1]]@tag, ' for transition:', Transition_name,  '...\n'))
mod<-NULL #Ensure model object is empty
ptm <- proc.time() #timer for model fitting
mod <-try(lulcc.fitmodel(trans_result = Transition_dataset$trans_result, #transitions data
                        cov_data = Transition_dataset$cov_data, #covariate data
                        replicatetype= replicatetype, # cross-validation strategy
                        reps= reps, # Number of replicates 
                        mod_args=modinp_i,
                        path = NA,
                        Downsampling = if (balance_class == TRUE) {TRUE} else {FALSE}
                                      #& Transition_dataset$imbalance_ratio <= Downsampling_bounds$lower | Transition_dataset$imbalance_ratio >= Downsampling_bounds$upper) {TRUE} else {FALSE} # utilise downsampling based on imbalance ratio 
                     ), TRUE)  #Supply model arguments
timer<-c(proc.time() - ptm) #record timer 

#add result of fitting to list to highlight failures
if(class(mod)=="try-error"){
model_result_list[[names(modinp_i)]] <- mod  
} else if(class(mod) !="try-error"){
  model_result_list[[names(modinp_i)]] <- "Success"}

## 2. Save model
 #if the model fitting has not resulted in an error then save the model
    if(class(mod)!="try-error"){
cat(paste0('saving ',modinp_i[[1]]@tag, ' for transition:', Transition_name,  '...\n'))
save_model_result <- try(lulcc.savethis(object=list(model = mod, parameters = modinp_i, time = timer),
                                        transition_name = Transition_name,
                                        tag = modinp_i[[1]]@tag,
                                        save_path = paste0(model_folder, "/", Data_period)),
                         TRUE)} else{save_model_result <- list()} 	

#add result of model_saving to list to highlight failures in saving
if(class(save_model_result)=="try-error"){
model_save_results[[names(modinp_i)]] <- save_model_result} else{
  model_save_results[[names(modinp_i)]] <-"Success"} 


## 3. Evaluate model on testing data
evals<-NULL #empty object for model evaluation results

evals <- try(lulcc.evaluate(mod, crit="maxTSS", train_test = "test"),TRUE) # criteria for threshold metrics
 if(class(evals)!= "try-error"){ #If model eval has been successful, summarise across replicates
      smev <- try(pipe.summary(evals))
      t <- rbind(t.user=timer[1], t.elapsed=timer[3], imbalance_ratio = Transition_dataset$imbalance_ratio , num_units= Transition_dataset$num_units, num_covs = ncol(Transition_dataset$cov_data))
      smev <- rbind(smev,t) #combine with timer results
      eval_list[[names(modinp_i)]] <- "Success" #add to overall model evals list
      lulcc.savethis(object= smev, #save summary of evaluation results
                   transition_name= Transition_name,
                   tag=modinp_i[[1]]@tag,
                   save_path = paste0(eval_results_folder, "/", Data_period))
      } 
else if(class(evals)== "try-error"){
  eval_list[[names(modinp_i)]]<- paste0("eval_failed: ", evals)
}

## 5. Evaluate model on training data
#   evals_training<-NULL #empty object for model evaluation results
#   eval_result_training <- NULL
#   try(evals_training<-lulcc.evaluate(mod, crit="maxTSS", train_test="train"),TRUE) # criteria for threshold metrics
#   if(class(evals_training)!="NULL"){ #If model eval has been successful, summarise across replicates
#       smev<-pipe.summary(evals_training)
#       t<-rbind(t.user=timer[1],t.elapsed=timer[3], imbalance_ratio=Transition_dataset$imbalance_ratio , num_units= Transition_dataset$num_units, num_covs = ncol(Transition_dataset$cov_data))
#       smev<-rbind(smev,t) #combine with timer results
#       eval_list_training[[names(modinp_i)]]<-smev
#       eval_result_training <- smev
#       } #add to overall model evals list
#   else if(class(evals_training)=="NULL"){
#   eval_list_training[[names(modinp_i)]]<-"eval_failed"
# }
# 
# ## 6. Save training evaluation table
# if(class(eval_result_training)!="NULL"){
# try(lulcc.savethis(object= eval_result_training, transition_name= Transition_name,
#                tag=modinp_i[[1]]@tag,
#               save_path = paste0(eval_results_folder, Data_period, "_", Model_type, "_model_eval_training")), TRUE)}

} #Close loop over model specifications

return(list(model_fitting_result = model_result_list,
            model_save_result = model_save_results,
            #model_eval_results_training = eval_list_training,
            model_eval_result = eval_list))

} #Close function