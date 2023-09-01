#' Evaluate models
#'
#' Produce objects needed for Boyce and ROC curves of all models in a wsl.fit object. 
#'
#' @param x a wsl.fit object
#' @param tester data.frame with testing data (only mandatory if replicatetype='none'
#' was chosen when models were fitted)
#' @param threshold vector of the same length as number of models chosen with custom
#' thresholds for model evaluation. for wsl.flex outputs the thresholds have to be labelled
#' with the same names provided to models
#' @param crit which threshold criterion should be considered? Currently 'pp=op'
#' (predicted prevalence = observed prevalence), 'maxTSS' (threshold yielding maximum TSS),
#' and 'external' (thresholds manually supplied) are possible
#' @param prevalence_correction logical. Should imbalanced presence/absence data be upsampled to
#' prevalence 0.5 for model evaluation.
#' data. In a pseudoabsence correction the same amount of pseudoabsences as presences are
#' subsampled for model validation.
#' @return an obejct of class 'wsl.evaluation'
#' @author Philipp Brun (main) with edits by Antoine Adde and Ben Black
#' @export
lulcc.evalcurves<-function(x,tester=data.frame(),thres=numeric(),crit="pp=op", prevalence_correction=FALSE){

  ### ------------------------
  ### check thresholds
  ### ------------------------

  # thres has to be a vector with named elements (same names
  # as in evaluation matrix)
  if(length(thres)>0){
    if(length(x@fits[[1]])!=length(thres)){
      stop("Wrong number of thresholds supplied! Should be one threshold per model type...")
    }
    if(crit!="external"){
      warning("Assuming you want external tresholds to be used - setting crit='external'!")
      crit="external"
    }
  }

  if(!(crit%in%c("pp=op","maxTSS","external"))){
    stop("Invalid threshold criterion chosen!")
  }


  ### ------------------------
  ### Check testing data and prepare for evaluation
  ### ------------------------

  if(x@meta$replicatetype=="none" && nrow(tester)==0){
    stop("External testing data must be supplied for replicatetype 'none'")
  } else if(x@meta$replicatetype%in%c("cv","block-cv","splitsample")) {

    if(prevalence_correction){
      x@tesdat=lapply(x@tesdat,function(y){
        tdpres=y[which(y$transitions_result==1),]
        tdabs=y[which(y$transitions_result==0),]
        #If the number of absences (non-trans) is less than the number of presences (trans) then resample them
        #with replacement so that they are they same. Vice versa for if presences(trans) < absences (non-trans) 
        if(nrow(tdabs)<nrow(tdpres)){
          tdabs=tdabs[sample(1:nrow(tdabs),nrow(tdpres),replace=T),]
        } else if(nrow(tdpres)<nrow(tdabs)){
          tdpres=tdpres[sample(1:nrow(tdpres),nrow(tdabs),replace=T),]
        }
        return(rbind(tdpres,tdabs))
      })
    }

    outerloop<-length(x@tesdat)
    testa<-lapply(x@tesdat,function(x){
      y<-x[,-which(colnames(x)=="transitions_result"),drop=FALSE]
    })
    papa<-lapply(x@tesdat,function(x){
      y<-x[,"transitions_result"]
    })

  } else if(x@meta$replicatetype=="none"){

    outerloop<-1
    testa<-list(tester[,-which(colnames(tester)=="transitions_result"),drop=FALSE])
    papa<-list(tester[,"transitions_result"])

  }

  ### ------------------------
  ### generate wsl.evaluation and add meta info
  ### ------------------------

  out<-preva.meta(type="evaluation")



  ### -------------------------------------------
  ### Evaluate models
  ### -------------------------------------------

  lis<-list()
  # loop over replicates
  for(i in 1:length(x@fits)){

    lisa<-list()
    # Loop over model types
    for(j in 1:length(x@fits[[1]])){

      # Make prediction
      pred=pipe.prd(x@fits[[i]][[j]] ,testa[[i]])

      # Evaluate (with external threshold if available)
      if(length(thres)==0){

        scores<-lulcc.BoyceROCcurves(f=pred,
                      pa=papa[[i]],
                      tesdat=testa[[i]],
                      crit=crit)

      } else{

        scores<-lulcc.BoyceROCcurves(f=pred,
                      pa=papa[[i]],
                      tesdat=testa[[i]],
                      tre=thres[which(names(thres)==names(x@fits[[i]])[j])],
                      crit=crit)

      }
    
      
      lisa[[j]]<-scores

    }
    names(lisa)=names(x@fits[[i]])
    lis[[i]]<-lisa
  }
  names(lis)<-names(x@fits)
  out@performance<-lis

  return(out)
}
