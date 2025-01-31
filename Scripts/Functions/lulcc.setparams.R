### =========================================================================
### lulcc.setparams: set hyperparameters for fitting statistical models
### =========================================================================
#' 
#'
#' @param model_name Character, abbreviation for model type accepts: "glm", "rf" 
#' @param transition_result Vector of transition result
#'  used as response variable in models 
#' @param param_grid Character, file path for grid of hyperparameters
#' @param covariate_names Vector of covariates names to be used in model formulas
#' @param weights Vector of weights to be used in modelling
#' @returns List object of model settings 
#' @author Antoine Adde (main) edited by Ben Black
#' @export

lulcc.setparams <- function(model_name, transition_result, param_grid, covariate_names, weights=1){

# Import parameter table
param.table <- read_excel(param_grid)

# Weighting?
if(length(weights)==1){
weighting=FALSE
} else {
weighting=TRUE}

modinp<-list() # list where all model settings will be stored

## GLM
if(model_name=="glm"){
form_glm<- axe_env(as.formula(paste("transitions_result~", paste(paste0(covariate_names), collapse="+")))) #create GLM formula (axe_env ensures that the result does not link to the globalenv when saving)
multi_glm<-pipe.multi("glm", list(formula=form_glm, family="binomial"), tag=paste0("glm"), weight=weighting) #append formula to other model specs
modinp <-append(modinp, multi_glm)
} #close GLM statement

## RF
if(model_name=="rf"){
#Check that both cols in the param_table have values in each row 
if(nrow(param.table)>1){
  params_rf<-na.omit(expand.grid(data.frame(param.table)))
} else {
  params_rf<-data.frame(param.table)
}

for(p in 1:nrow(params_rf)){
param_rf<-params_rf[p,]
multi_rf<-pipe.multi("randomForest",list(formula=axe_env(transitions_result~.), #axe_env ensures that the result does not link to the globalenv when saving
                                         ntree=as.numeric(param_rf$num.trees),
                                         mtry=floor(sqrt(length(covariate_names))),
                                         nodesize = as.numeric(param_rf$min.node.size),
                                         classwt=c("0"= ifelse(sum(transition_result == 1) > sum(transition_result == 0), max(weights), min(weights)), "1"= ifelse(sum(transition_result == 1) < sum(transition_result == 0), max(weights), min(weights)))),
                                        tag=paste0("rf-",p),
                                        weight=weighting) 


modinp<-append(modinp, multi_rf)
} #close loop over model params
} #close RF statement

names(modinp)<- unlist(lapply(modinp, function(x){x@tag})) #name models by tag

return(modinp)
}