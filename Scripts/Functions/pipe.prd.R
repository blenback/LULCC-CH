### =========================================================================
### prediction-evaluation prediction function
### =========================================================================
#' Correctly feed the predict functions depending on model type (glm, gbm, maxent...)
#'
#' Not to be called directly by the user
#' @author Philipp Brun (main) with edits by Antoine Adde
#' @export
pipe.prd=function(mod,tst){

  # Generate probabilistic precitions
  if("maxnet"%in%class(mod)){

    pred<-pipe.df_or_rast(mod,
                     nwdat=tst,
                     type="logistic")
    
  } else if(any(c("glm","ranger")%in%class(mod))){

    pred<-pipe.df_or_rast(mod=mod,
                     nwdat=tst,
                     type="response")

  } else if("gbm"%in%class(mod)){

    pred<-pipe.df_or_rast(mod,
                     nwdat=tst,
                     n.trees=mod$n.trees,
                     type="response")

  } else if("lgb.Booster"%in%class(mod)){
    
    pred<-pipe.df_or_rast(mod=mod,
                     nwdat=tst)
    
  } else if("randomForest"%in%class(mod)){

    pred<-pipe.df_or_rast(mod,
                     nwdat=tst,
                     type="prob")
  }

  # Convert to numeric
  if(class(tst)=="data.frame"){
    pred<-as.numeric(pred)
  }

  return(pred)

}
