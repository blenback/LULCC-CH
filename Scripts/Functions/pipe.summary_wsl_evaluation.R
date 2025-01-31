### =========================================================================
### define summary function for wsl.evaluation objects
### =========================================================================
#' @author Philipp Brun (main) with edits by Antoine Adde
#' @export
pipe.summary=function(object){

  #cat("\nMeta information: \n")
  df=data.frame(object@meta[c("author","date")],object@meta$wsl.fit[c("project","replicatetype","replicates")])

  rownames(df)=""
  #print(df)

  #cat("\nThreshold: \n")
  df=as.data.frame(object@meta[c("cutoff")])

  rownames(df)=""
  #print(df)

  #cat("\nMean skill: \n")

  mats=list()
  for(i in 1:length(object@performance)){
    mats[[i]]=do.call("cbind",object@performance[[i]])
  }
  
  mn=rowMeans(do.call(cbind, mats), na.rm = TRUE)
  mn=as.matrix(mn)
  colnames(mn)=colnames(mats[[1]])

  return(mn)

}
