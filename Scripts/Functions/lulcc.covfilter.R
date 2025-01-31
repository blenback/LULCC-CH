### =========================================================================
### lulcc.covfilter: Covariate collinearity testing
### =========================================================================
#'
#' @param cov_data 
#' @param trans_result 
#' @param method a character of the method used to rank covariates
#'               accepts: "IQR", "IQR.M", "COR.P", "COR.S", "WIL", "GLM", "MAD.M"   
#' @param weights vector of 'prior weights' to be used in the glm fitting
#'                process. Should be NULL or a numeric vector.
#' @param corcut  numeric value for correlation cut-off to remove covariates
#' @return a dataframe of the collinearity filtered covariates
#' @author Antoine Adde (main) edited by Ben Black
#' @export

lulcc.covfilter <- function(cov_data, trans_result, method, weights, corcut=0){
  
# If only one covariate in the candidate set, don't do anything 
  if(ncol(cov_data)==1){
  cov_data.filter<-cov_data
  return(cov_data.filter)}
  
# Bind covariates and trans_result data
  covdata<-cbind(cov_data, trans_result=factor(trans_result))

# Rank candidate covariates using selected method
  if(as.numeric(length(unique(trans_result))) <=2){ #approach for binary datasets
  if(method=="IQR"){
    covranked<-data.frame(iqr=sort(sapply(subset(covdata[covdata$trans_result==1,], select=-c(trans_result), IQR))))
  }
  
  if(method=="IQR.M"){
  covranked<-data.frame(iqr=sort(abs(sapply(subset(covdata[covdata$trans_result==1,], select=-c(trans_result)), IQR)/ sapply(subset(covdata[covdata$trans_result==1,], select=-c(trans_result)), median))))
  }
  
  if(method=="COR.P"){
  covranked<-data.frame(cor=abs(t(cor(trans_result, as.matrix(subset(covdata, select=-c(trans_result))), method="pearson", use="complete.obs"))))
  covranked<-covranked[order(-covranked$cor), , drop = FALSE]
  }
  
  if(method=="COR.S"){
    covranked<-data.frame(cor=abs(t(cor(trans_result, as.matrix(subset(covdata, select=-c(trans_result))), method="spearman", use="complete.obs"))))
    covranked<-covranked[order(-covranked$cor), , drop = FALSE]
  }
  
  if(method=="WIL"){
    res <- sort(apply(subset(covdata, select=-c(trans_result)), 2, function(x){
    wilcox.test(x ~ covdata$trans_result)$p.value
}))
   covranked<-data.frame(wil=res, row.names=names(res))
}

  if(method=="GLM"){
    res <- sort(apply(subset(covdata, select=-c(trans_result)), 2, function(x){
    min(summary(glm(covdata$trans_result ~ poly(x,2), family="binomial", weights=weights))$coefficients[2:3,4]) #This is subsetting too the p value of the intercept and the covariate 
}))
   covranked<-data.frame(pval=res, row.names=names(res))
}

   if(method=="MAD.M"){
    covranked<-data.frame(iqr=sort(abs(sapply(subset(covdata[covdata$trans_result==1,], select=-c(trans_result)), mad)/ sapply(subset(covdata[covdata$trans_result==1,], select=-c(trans_result)), median))))
   }
  }
  
if(as.numeric(length(unique(trans_result))) >2){ #This is for Multiclass datasets
  
  res <- sort(apply(subset(covdata, select=-c(trans_result)), 2, function(x){ #loop over covariates
    
    #set baseline category of dependent variable for multinomial model
    baseline_trans_ID <- tail(unique(covdata$trans_result),1) #identify last unique transition ID 
    trans_result_RL <- relevel(covdata$trans_result, ref = baseline_trans_ID) #relevel the trans-result vector
    
    #run multinomial model
    mn_model <- multinom(trans_result_RL ~ poly(x,2))
    
    #calculate p value
    z <- summary(mn_model)$coefficients/summary(mn_model)$standard.errors #calculate z value
    p <- ((1 - pnorm(abs(z), 0, 1)) * 2) #p values for cov and it's polynomial across all values of dependent variables 
    p_min <- min(c(mean(p[,2]), mean(p[,3]))) #take an average p value across the values of dependent variable for the covariate and it's polynomial seperately and then keep only the minimum value. 
    return(p_min)  
}))
   covranked<-data.frame(pval=res, row.names=names(res))
}  

# Reorder covdata accordingly and compute initial correlation matrix
  cov_names <- row.names(covranked)
  covdata.ranked <- data.frame(covdata[, cov_names])
  cor.mat<-abs(cor(covdata.ranked,use="pairwise.complete.obs")) #This is calculating the Pearsons correlation coefficient (the default for the function) of the ranked covariates  

# Thin candidate covariate set until no pairwise correlation > corcut
    sanctuaire<-NULL #sanctuaire receives the thinned covariate list and iteratively updated until the corellation cut off is reached. 
	
    if (all(cor.mat[cor.mat!=1] < corcut)) {
    sanctuaire <-c(sanctuaire,row.names(cor.mat))
    } else {
    while(length(cor.mat) > 1) {
    tmp_cm<-data.frame(cor.mat[,1])
    ix<-which(tmp_cm[,1]<=corcut)                      
    var.name<-row.names(cor.mat)[1]
    sanctuaire<-c(sanctuaire,var.name)
    cor.mat<-cor.mat[ix,ix]
    if (all(cor.mat[cor.mat!=1] < corcut)) { 
    sanctuaire <-c(sanctuaire, row.names(cor.mat))
    break
    }
    }
    }

# Return dataframe of selected covariates  
    cov_data.filter<-as.data.frame(covdata[,sanctuaire])
    if(length(sanctuaire)==1){
    colnames(cov_data.filter)<-sanctuaire}
    return(cov_data.filter)
}

   
 
