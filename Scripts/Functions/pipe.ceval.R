### =========================================================================
### evaluate core function (peval)
### =========================================================================
#' Do the actual model evaluations
#'
#' Not to be called directly by the user
#'
#' @param f Vector of model predicted probabilities
#' @param pa Vector of response variable values
#' @param tesdat Dataframe of test data
#' @param crit which threshold criterion should be considered? Currently 'pp=op'
#' (predicted prevalence = observed prevalence), 'maxTSS' (threshold yielding maximum TSS),
#' and 'external' (thresholds manually supplied) are possible
#' @param tre Optional numeric threshold for seperating predictions
#' @returns Matrix of model eval metric names and results
#' @author Philipp Brun (main) with edits by Antoine Adde and Ben Black
#' @export
pipe.ceval<-function(f,pa,tesdat,crit,tre=numeric()){

  # If there are any presences in the evaluation data
  if(any(pa==1)){

    if(crit=="maxTSS" && length(tre)==0){
      
      ordf=order(f,decreasing = T) #create a vector of positions to order prediction data 
      
      rdf=f[ordf] #create a duplicate vector of prediction data in decreasing order

      opa=pa[ordf] #create a duplicate vector of the presence/absence data ordered by the decreasing order of predictions

      cpa=cumsum(opa) #cumulative summation of vector of presence/absence records

      
      # Reduce to sensible range
      nsns=which(cpa==max(cpa)) #this is return the index positions in cpa which all have the max value in the vector

      nsns=nsns[-1] #dropping the first element of the vector 


      if(length(nsns)!=0){
      rdf=rdf[-nsns]
      opa=opa[-nsns]
      cpa=cpa[-nsns]
      }

      
      tsss=apply(cbind(1:length(cpa),cpa),1,function(x,y,z){
        out=tss(x[2],x[1]-x[2],z-x[2],y-x[1]-(z-x[2]))
        return(out)
      },y=length(pa),z=sum(pa))

      tre=rdf[which.max(tsss)]

    } else if (crit=="" && length(tre) == 0){
      tre = mean(pa)
    }

    # Calculate threshold-dependent metrics
    bina=ifelse(f<tre,0,1)
    tb = table(factor(bina,levels=c("0","1")),factor(pa,levels=c("0","1")))
    tdep = pipe.all.metrics(tb[2,2],tb[2,1],tb[1,2],tb[1,1])

    
    #In the Boyce chunk below, the $Spearman.cor is subsetting the output of the function
    #to just the boyce index correlation value, when the function normally returns a list 
    #which also includes vectors of the predicted/expected values (F) and predicted probability (HS)
    #So if I want to plut the Boyce curves I need to set it to return all of these items and then include them
    #in the functions overall output. 
  
   	# Boyce
	  boyce=try(ecospat.boyce(fit=f, obs=f[pa==1], PEplot = FALSE)$Spearman.cor, TRUE) 
	  if(!is.numeric(boyce)){                                                           
    boyce<-NA}

    # AUC
    z = prediction(f,pa)
    auc= ROCR::performance(z,measure="auc")@y.values[[1]]
    rmse= ROCR::performance(z,measure="rmse")@y.values[[1]]
	
	# Somer's AUC
	aucS=(2*auc)-1 # rescale AUC to -1 +1

	# # Consensus score
	score <- mean(aucS, boyce, tdep["TSS"], trim = 0, na.rm = TRUE)


    # Return results
    weg=c(auc, aucS, rmse, boyce, score, tre, tdep)
    names(weg)[1:6]=c("AUC", "AUC.S", "RMSE", "Boyce", "Score", "threshold")
    #names(weg)[16:17]=c("imbalance.ratio", "num_units")
    return(unlist(weg))
  }
}
