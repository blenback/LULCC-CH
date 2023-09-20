### =========================================================================
### define wsl.glm function
### =========================================================================
#' Re-sample data and fit RF models
#'
#'
#' @param trans_result vector with presence/absence values
#' @param cov_data data.frame with environmental predictors
#' @param replicatetype (how) should replicates be generated? may be 'none', 'splitsample',
#' 'cv' 'block-cv'
#' @param reps number of replicates
#' @param strata a numeric vector of the same length as observations with integers separating
#' cross validation replicates (used when replicatetype='block-cv')
#' @param project character indicating the name of the project within which the models are run
#' (later used to define saving directories)
#' @param path where to save? (not implemented yet)
#' @param step (for glms and gams only) should the models be updated with the step function?
#' @param mod_tag (not in wsl.flex) label for the current model
#' @param mod_args list with elements of class 'multi.input' which specify models to be fitted
#' in lulcc.fitrf
#' @Param Downsampling Logical, should downsampling be implemented in RF? (TRUE/FALSE)  
#' @return Object of class wsl.fit including slots for meta info, testing data for
#' evaluation, and model objects
#' @author Philipp Brun (main) with edits by Antoine Adde and Ben Black
#'
#' @rdname fitdoc
#' @export

#instantiate small function to reduce size of saved GLM/RF objects  
  stripGlm = function(cm) {
  cm$y = c()
  cm$model = c()
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  cm
  }
  
stripRF = function(cm) {  
cm$predicted <- NULL
cm$y <- NULL
cm$err.rate <- NULL
cm$test <- NULL
cm$proximity <- NULL
cm$confusion <- NULL
cm$localImportance <- NULL
cm$importanceSD <- NULL
cm$inbag <- NULL
cm$votes <- NULL
cm$oob.times <- NULL
attr(cm$terms,".Environment") = c()
attr(cm$formula,".Environment") = c()
cm
}

lulcc.fitmodel <-function(
                   trans_result=numeric(),
                   cov_data=data.frame(),
                   replicatetype=character(),
                   reps,
                   strata=NA,
                   project=NA,
                   path=NA,
                   mod_args=list(),
                   Downsampling){

 # Check supplied model types
  for(i in 1:length(mod_args)){
    if(!(mod_args[[i]]@mod%in%c("randomForest", "glm"))){
      warning(paste(mod_args[[i]]@mod,"not valid specifications for Random Forest model, please check and repeat."))
    }
  }

  # check and resample data and prepare object for output
  lis=lulcc.preps(call=match.call())

  # loop over the number of specificed replicates
  fits=list()
  for(i in 1:reps){

    modi=list()
    # loop over models
    for(j in 1:length(mod_args)){

    #append data to model settings object  
    mod_args[[j]]@args$data=lis$train[[i]]

    if(mod_args[[j]]@mod=="glm"){# Open GLM chunk
  
    Full_glm <-do.call(mod_args[[j]]@mod,mod_args[[j]]@args) #run GLM model
    modi[[j]]= axe_env(stripGlm(Full_glm)) #strip unnecessary elements
    }else if(mod_args[[j]]@mod=="randomForest"){ #close GLM chunk open RF chunk
    
    #set transition result as factor
    mod_args[[j]]@args$data$transitions_result=as.factor(mod_args[[j]]@args$data$transitions_result)

    if(Downsampling == TRUE){
    #append strata to model settings object  
    mod_args[[j]]@args$strata = mod_args[[j]]@args$data$transitions_result #Strata uses the dependent variable column to perform stratified sampling according the frequencies given in sampsize
    
    #create sampsize argument
    nmin <- sum(mod_args[[j]]@args$data$transitions_result == names(which.min(table(mod_args[[j]]@args$data$transitions_result)))) #get the total number of the minority class values in the dataset for downsampling
    mod_args[[j]]@args$sampsize = c(nmin, nmin) #create vector for stratified sampling based on the number of minority classes sampling with replacement is default
}

    #call to randomForest function using the arguments specified, note that the vector for  
        Full_rf <-do.call(mod_args[[j]]@mod,mod_args[[j]]@args)
        modi[[j]]= axe_env(stripRF(Full_rf)) #strip unnecessary elements
}#close RF chunk
        
    #name fitted model
      names(modi)[j]=ifelse(mod_args[[j]]@tag=="",paste0("model_",j),mod_args[[j]]@tag)

    }

    fits[[i]]=modi

  }

  names(fits)=paste0("replicate_",sprintf("%02d",1:reps))

  # supply fitted objects
  lis$wslfi@fits=fits

  # Save
  #...

  return(lis$wslfi)

}
