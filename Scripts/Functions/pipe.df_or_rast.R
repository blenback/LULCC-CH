### =========================================================================
### predict df or raster
### =========================================================================
#' Predict to data.frame or raster
#'
#' Not to be called directly by the user
#'
#' @author Philipp Brun (main) with edits by Antoine Adde
#' @export
pipe.df_or_rast=function(mod,nwdat,...){

  if("randomForest"%in%class(mod)){
    index=2
  }else{
    index=1
  }

  if(class(nwdat)%in%c("RasterStack","RasterBrick","RasterLayer")){

    add.arg=list(...)
    beginCluster(5)
    cl=getCluster()
    parallel::clusterExport(cl,envir=environment(),varlist=list("nwdat","mod","add.arg","index"))
    out=clusterR(nwdat,predict,args=c(list(model=mod,index=index),add.arg))

    returnCluster()
    # out=raster::predict(nwdat,mod,...)
    endCluster()

  } else if(class(nwdat)%in%c("data.frame","matrix")){
    
    if("ranger"%in%class(mod)){
      out=predict(mod,data=nwdat,...)$predictions
    } else if("lgb.Booster"%in%class(mod)){

      out=predict(mod,data=as.matrix(nwdat))
    } else {
      out=predict(mod,newdata=nwdat,...)
    }


    if("randomForest"%in%class(mod)){
      out=out[,2]
    }
  }

  return(out)

}
