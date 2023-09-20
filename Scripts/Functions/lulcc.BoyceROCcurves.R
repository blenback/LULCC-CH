### =========================================================================
### lulcc.BoyceAUCcurves
### =========================================================================
#' Return objects for producing AUC and Boyce Curves
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
#' @returns List of model eval result objects: AUC, Boyce and the ROC value
#' @author Philipp Brun (main) with edits by Ben Black
#' @export

lulcc.BoyceROCcurves <-function(f,pa,tesdat,crit,tre=numeric()){

  # If there are any presences in the evaluation data
  if(any(pa==1)){

   	# Boyce
	  boyce = ecospat.boyce(fit=f, obs=f[pa==1], res = 1000, PEplot = FALSE) 

    # AUC
    z=prediction(f,pa)
    auc=performance(z,measure="auc")

    # ROC
    roc=performance(z, "tpr", "fpr")
    
    
    # Return results
    weg <- list(AUC = auc, Boyce = boyce, ROC = roc)
    return(weg)
  }
}
