### =========================================================================
### lulcc.grrffeatSelect: Embedded feature selection with Guided Regularized Random Forests
### ===========================================================================
#' 
#'
#' @param transition_result vector of transitions result used as response vector for GRRF 
#' @param cov_data dataframe of predictors to be used in GRRF 
#' @param weight_vector vector of weights to be supplied to 'classwt' argument in RF
#' @param gamma numeric between 0-1) controls the weight
#'               of the normalized importance score and is called the importance coefficient. 
#'               Note that when Gamma = 0 we are essentially perform RRF (Regularized Random Forest) 
#'               i.e there is no 'Guided effect' vice versa when Gamma = 1 then we are applying
#'               the strongest guiding effect which will lead to the most penalisation of redundant
#'               features and hence the most concise feature sets
#' @returns dataframe of covariates selected by GRRF ranked according to importance (Mean decrease Gini)
#' @author Adapted from Deng (2013: https://arxiv.org/pdf/1306.0237.pdf)
#'         by Antoine Adde edited by Ben Black
#' @export


lulcc.grrffeatselect <- function(transition_result, cov_data, weight_vector, gamma){
    
# Do embeddded covariate selection with GRRF
rf <- RRF(cov_data, as.factor(transition_result), flagReg = 0) #run a non-regularized RF model 
impRF <- rf$importance[,"MeanDecreaseGini"]   #create named list list of importance score for each covariate
imp <- impRF/(max(impRF)) #normalize the importance score

coefReg <- (1-gamma)+gamma*imp #calculate weighted averages of importance scores (Penalty coefficients) under the importance coefficient (Gamma)

#info for how 'classwt' operates: "We assign a weight to each class, with the
#minority class given larger weight (i.e., higher misclassification cost). The class weights are incorporated
#into the RF algorithm in two places. In the tree induction procedure, class weights are used to weight
#the Gini criterion for finding splits. In the terminal nodes of each tree, class weights are again taken into
#consideration. The class prediction of each terminal node is determined by "weighted majority vote"; i.e.,
#the weighted vote of a class is the weight for that class times the number of cases for that class at the
#terminal node. The final class prediction for RF is then determined by aggregatting the weighted vote from
#each individual tree, where the weights are average weights in the terminal nodes (Chen et al. 2004). 

mdl.rf <- RRF(cov_data, as.factor(transition_result), classwt= weight_vector, coefReg=coefReg, flagReg=1) #Run guided regularized RF

# Extract results
rf.beta<-data.frame(var = row.names(mdl.rf$importance), mdl.rf$importance,  row.names=NULL)
rf.beta<-rf.beta[which(rf.beta$MeanDecreaseGini > 0),]
rf.beta<-data.frame(rf.beta[order(rf.beta$MeanDecreaseGini, decreasing = TRUE),], rank = 1:nrow(rf.beta))  
return(rf.beta)
}