### =========================================================================
### preparation function
### =========================================================================
#' Check input data, collect meta information, take care of data subsetting.
#' Called by model fitting functions.
#'
#' Not to be called directly by the user
#' @author Philipp Brun adapted by Ben Black
#' @export
lulcc.preps=function(env=parent.frame(),call){

  env=as.list(env)

  ### ----------------
  ### check input data
  ### ----------------

  if(env$replicatetype%in%c("none","cv","block-cv","splitsample")==FALSE){stop("Non-existing replicatetype!")}

  if(env$replicatetype=="block-cv" && is.na(env$strata)){stop("Stratum vector needed for block crossvalidation!")}

  if(env$replicatetype=="block-cv" && length(unique(env$strata))!=env$reps){stop("Stratum vector has wrong number of levels!")}

  if(env$replicatetype%in%c("cv","block-cv","splitsample")==TRUE && (is.na("reps") ==TRUE || env$reps<2)){stop("Give reasonalbe number of replicates!")}

  if(env$replicatetype=="none" && env$reps>1){env$reps=1;warning("Replicate type is 'none' but multiple replicates were chosen - replicates are set to 1")}


  #check for file path
  #if(env$save && is.na(env$project)){stop("supply project in which data should be saved!")}

  #if(env$save && !(env$project%in%list.dirs(env$path))){stop(paste("Project directory not existing in",env$path,
  #                                                                 "- please create manually"))}

  ### -------------------
  ### generate wsl.fit obj
  ### -------------------

  out<-wsl.fit()

  # store function call
  out@call<-call

  ### -------------------
  ### add meta.info
  ### -------------------

  m.i=list()

  m.i$author=Sys.info()[["user"]]
  m.i$date=Sys.time()
  m.i$replicatetype=env$replicatetype
  m.i$replicates=env$reps
  m.i$env_vars=paste(colnames(env$cov_data),collapse=", ")
  m.i$project=env$project
  m.i$model_tag=env$mod_tag

  # Add step info if exists
  if("step"%in%names(env)){
    m.i$step=env$step
  }

  # Add pseudoabsence info if exists
  if(class(env$x)=="wsl.pseudoabsences"){
    m.i$pseudoabsence_type=env$x@meta$type
  }

  out@meta=m.i

  ### ----------------------
  ### partition observations
  ### ----------------------
  # partition observations according to replicate type
  dat=cbind(data.frame(transitions_result=env$trans_result),env$cov_data) #dat is the full transition result and covariate data
  
  obschoice<-list()
  testing<-list()
  if(env$replicatetype=="none"){
    obschoice[[1]]<-dat #no replicates means use the full dataset (i.e. prediction)

## Original approach for split sampling however this causes problems as it doesn't force the inclusion of presences    
#  } else if (env$replicatetype=="splitsample"){
#    for (i in 1:env$reps){    #Loop over the numer of replicates supplied in the function call
#      chc=sample(1:nrow(dat),size=round(nrow(dat)*0.7),replace=FALSE)  #take a 70% sample without replacement 
#      obschoice[[i]]<-dat[chc,] #subset the rows in dat to the rows in the sample to give the training data 
#      testing[[i]]<-dat[-chc,]  # remove the sample rows from dat to give the test data
#    }

#Quick fix for split sampling
} else if (env$replicatetype=="splitsample"){
  for (i in 1:env$reps){
         dat$sid <- 1:nrow(dat) #add numeric ID 
     chc = dplyr::slice_sample(dplyr::group_by(dat, transitions_result), prop =  0.7, replace= FALSE)
     obschoice[[i]] <-dat[chc$sid,]  
     testing[[i]] <- dat[-chc$sid,]
     obschoice[[i]]$sid <- NULL
     testing[[i]]$sid <- NULL
  }
    
  } else if (grepl("cv",env$replicatetype)){

    if(env$replicatetype=="cv"){
      unistr=sample(1:5,size=nrow(dat),replace=TRUE)
    } else {
      unistr=env$strata
    }

    for (i in 1:env$reps){
      obschoice[[i]]<-dat[which(unistr!=unique(unistr)[i]),]
      testing[[i]]<-dat[which(unistr==unique(unistr)[i]),]
    }

  }

  # add testing data to wsl.fit obj
  out@tesdat=testing
  out@train = obschoice

  # return objects for model fitting
  return(list(wslfi=out,train=obschoice))
}
