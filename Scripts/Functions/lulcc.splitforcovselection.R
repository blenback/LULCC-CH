### =========================================================================
### lulcc.splitforcovselection: 
### =========================================================================
#' @author Ben Black
#' @export

lulcc.splitforcovselection <- function(trans_dataset, predictor_table){

#Identify the last col of the dataset as the 'transition result; and rename appropriately. 
#setnames(trans_dataset, ncol(trans_dataset), "trans_result")
names(trans_dataset)[ncol(trans_dataset)] <- "trans_result"

#seperate the trans result column        
trans_result <- trans_dataset[,"trans_result"]
  
#Identify the covariate data
cov_data <- trans_dataset[, c(predictor_table$Covariate_ID)]

#remove cols which only have 1 unique value
cov_data <- Filter(function(x)(length(unique(x))>2), cov_data)

#group the non-cov dat
#non_cov_data <- trans_dataset[, .SD, .SDcols = -predictor_table$Covariate_ID]
non_cov_data <- trans_dataset[, !(colnames(trans_dataset) %in% predictor_table$Covariate_ID)]

#get the number of unique values in the trans_result col
num_trans <- as.numeric(length(unique(trans_result)))

#Measure of class imbalance
imbalance_ratio <- sum(trans_result == min(trans_result))/sum(trans_result == max(trans_result)) #instances of minority class/majority class
	
#record dataset size 
num_units <- length(trans_result)

#Next step is to create weight vectors to be used for in both processes of covariate selection
#Random forests 'classwt' variable can accept class weights as a named vector 
#which is a different form to the weight vector required for GLMs (within collinearity based selection process)
#as such it makes sense to create a separate weight vector for each process

#If no. of unique values is <=2 then the data is binary
if(num_trans<=2){
#Create weight vector to be used in collinearity based covariate selection
transitions_vec <-which(trans_result==1) #identify which elements in the trans_result vector equal 1 (i.e. transition occurring)
nontransitions_vec <- which(trans_result==0) #identify which elements in the trans_result vector equal 0 (i.e. no transition occurring)
collin_weights <-rep(1,length(trans_result)) #create a vector the same length as trans_result and fill it all with 1's

trans_weighting <- round(length(nontransitions_vec)/length(transitions_vec)) #calculate the weighting value for the transitions 
non_trans_weighting <-  round(length(transitions_vec)/length(nontransitions_vec)) #calculate the weighting value for the no-transitions

collin_weights[transitions_vec] <- ifelse(length(transitions_vec) < length(nontransitions_vec), trans_weighting, 1) #insert the weighting value at the positions of transitions (if the number of transitions is more than the number of no-transitions then the weighting value will be rounded to zero which must be substituted for a 1 for it to work properly in GRRF)
collin_weights[nontransitions_vec] <- ifelse(length(transitions_vec) > length(nontransitions_vec), non_trans_weighting, 1)

#Create weight vector to be used in embedded (grrf) covariate selection
embed_weights <- c("0" = ifelse(length(transitions_vec) > length(nontransitions_vec), non_trans_weighting, 1),
                   "1" = ifelse(length(transitions_vec) < length(nontransitions_vec), trans_weighting, 1))

w_vectors <- list(collin_weights = collin_weights, 
                  embed_weights = embed_weights)

} else { #If no. of unique values is >2 then the data is multiclass

#create Collinearity covariate filtering weight vector
unique_trans_results <- unique(trans_result) #get vector of unique values in trans_result
collin_weights <-rep(1,length(trans_result)) #create a vector the same length as trans_result and fill it all with 1's
embed_weights <- c()

#loop over each unique transition ID 
for(i in unique_trans_results){
trans_id <- i  
Pos_vec_for_trans_ID <- which(trans_result==i) #position vector for transition ID i
nontransitions_vec <- which(trans_result!=i) #identify which elements in the trans_result vector 
trans_ID_weighting <- round(length(which(trans_result!=i))/length(Pos_vec_for_trans_ID)) #calculate the weighting value for the transitions
collin_weights[Pos_vec_for_trans_ID] <- ifelse(length(Pos_vec_for_trans_ID) < length(which(trans_result!=i)), trans_ID_weighting, 1) #insert the weighting value at the positions for trans ID i transitions (if the number of transitions is more than the number of no-transitions then the weighting value will be rounded to zero which must be substituted for a 1 for it to work properly
embed_weights <- c(embed_weights, (ifelse(length(Pos_vec_for_trans_ID) < length(which(trans_result!=i)), trans_ID_weighting, 1))) #insert the weighting value for trans_ID in vector using the ifelse statement to avoid 0 value for majoirty class 
}

names(embed_weights) <- c(unique_trans_results) #supply names to embed_weight vector

w_vectors <- list(collin_weights = collin_weights, 
                  embed_weights = embed_weights)
}

#group the outputs as a list
seperated_outputs <- list(trans_result, cov_data, non_cov_data, w_vectors[["collin_weights"]], w_vectors[["embed_weights"]], imbalance_ratio, num_units)
names(seperated_outputs) <- c("trans_result", "cov_data", "non_cov_data", "collin_weights", "embed_weights", "imbalance_ratio", "num_units")
return(seperated_outputs)
}


 