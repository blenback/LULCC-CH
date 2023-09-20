### =========================================================================
### lulcc.filtersel: Apply the collinearity filtering on groups of predictors
### =========================================================================
#' 
#' @param transition_result a vector containing binary values of transition result (0/1)
#' @param cov_data a datatable containing column wise values of covariates 
#' @param categories a vector of strings of covariate categories
#' @param collin_weight_vector vector of weights used for collinearity filtering
#' @param embedded_weight_vector weights used for embedded (grrf) filtering
#' @param focals a character vector identifying focal covariates
#' @param method a character of the method used to rank covariates
#'              accepts: "IQR", "IQR.M", "COR.P", "COR.S", "WIL", "GLM", "MAD.M"  
#' @param corcut numeric value for correlation cut-off to remove covariates
#'
#' @author Antoine Adde (main) edited by Ben Black
#' @export

lulcc.filtersel <- function(transition_result, cov_data, categories, collin_weight_vector, embedded_weight_vector, focals=NULL, method, corcut=0){

# Split candidate covariates by categories
covdata.candidates <- split.default(data.frame(cov_data), categories)

## Level 0 for focals, focals is specificed in the function call as one of the categories identified in the covariate info table in my case the focal categoriy is Neighbourhood
if(length(focals>0)){   #check length of focals vector
for(f in 1:length(focals)){ #loop through each category of focal covariate (redundant in my case because there is only one)
foc<-focals[f]
ix_foc<-which(names(covdata.candidates)==foc)

#the intention of this line is to seperate the focal neighbourhood covariates into a nested list according to their type (active LULC) and within that the different focal window sizes (and decay rates)
#this originally split on the '_' however my column names have multiple of these so instead I split on the word 'cov'
covdata.focals<-split.default(covdata.candidates[[ix_foc]],sub("\\cov.*", "", names(covdata.candidates[[ix_foc]]))) 

#apply the filtering procedure to each group of LULC neighbourhood covariates
covdata.focals.filter<-lapply(covdata.focals, function(x) lulcc.covfilter(cov_data = x, method=method, trans_result= transition_result, weights=collin_weight_vector, corcut=0))
covdata.candidates[[foc]]<-do.call("cbind",covdata.focals.filter)

}
}

## Level 1 for each category
covdata.candidates.L1<-lapply(covdata.candidates, function(y) lulcc.covfilter(cov_data = y, method=method, trans_result= transition_result, weights=collin_weight_vector, corcut=corcut))

## Level 2 for all selected currently removed because I don't need to filter beyond the neighbourhood and suitability categories.
#covdata.candidates.L2 <- LULCC.binarizedcovfilter(cov_data=do.call("cbind",covdata.candidates.L1), method=method, trans_result= trans_result, weights=weights, corcut=corcut)

#but this means that we do need to bind the seperate lists of covariates by categories because this is ordinarily done within level 2
covdata.candidates.bound=do.call("cbind",covdata.candidates.L1)

# return results
names(covdata.candidates.bound)<-gsub("^.*\\.","", names(covdata.candidates.bound)) #reformatting names to match original
recompile_data <- list(transition_result, covdata.candidates.bound, collin_weight_vector, embedded_weight_vector)
names(recompile_data) <- c("transition_result", "covdata_collinearity_filtered", "collin_weight_vector", "embedded_weight_vector")
return(recompile_data) #return recombined data
}