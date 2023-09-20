#############################################################################
## Trans_modelling: Fit and evaluate models of LULC under multiple model specifications 
##
##
## Date: 08-04-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set the working directory
wpath <- "E:/LULCC_CH"
setwd(wpath)

# navigate to the working directory in the files pane for easy viewing
rstudioapi::filesPaneNavigate(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","rlist", "randomForest", "RRF",
         "butcher", "ROCR","ecospat","caret", "foreach", "doMC", "data.table",
         "raster", "tidyverse","testthat", "sjmisc", "tictoc", "lulcc",
         "pbapply", "stringr", "readr", "openxlsx", "readxl", "future",
         "future.apply")

if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

### =========================================================================
### B- Creation of shannon entropy table for exemplar dataset
### =========================================================================

#To calculate the entropy measures on the ensemble we need 
#instance wise probablistic predictions across all trees. 
#This can be done in advance by predicting the training data using the fitted RF model 

#load exemplar data set
exp_data <- readRDS("E:/R_loop/Exemplar_data.rds")

#split into train and test
exp_data$sid <- 1:nrow(exp_data) #add numeric ID 
chc <-  dplyr::slice_sample(dplyr::group_by(exp_data, transitions_result), prop =  0.7, replace=FALSE)
train <- exp_data[chc$sid,]  
test <- exp_data[-chc$sid,]
train$sid <- NULL
test$sid <- NULL

#train an rf model
Mod <- randomForest(x = train[,-c("transitions_result")],
                    y = train$transitions_result,
                    ntree = 500,
                    keep.forest = TRUE)

### =========================================================================
### D- Test calculation of uncertainty using test data (as per simulation step)
### =========================================================================

#reload the entropy table for the exemplar data
Entropy_table <- readRDS("E:/R_loop/Entropy_table.rds")

#run prediction on the data and get the terminal node ID for each instance
#for each tree in the forest. 
Predict_nodes <- attr(predict(Mod, test, nodes=TRUE),'nodes')

#start timer
Start_time <- Sys.time()

#specify parallel processing with the future package
future::plan(multisession)

#current version
#Outer loop over rows of terminal node predictions
system.time({Uncertainties_calc <- future_lapply(1:nrow(Predict_nodes), function(instance){

  #calculate average predicted probability for each class across the models (trees)
  Instance_records <- rbindlist(lapply(1:ncol(Predict_nodes), function(tree){
  Entropy_table[Entropy_table$Node_ID == Predict_nodes[instance,tree] & Entropy_table$Tree_num == tree,]
  }))
  
  Average_pred_probs <-  list(Average_prob_0_over_trees = sum(Instance_records$Prob_0)/nrow(Instance_records),
                              Average_prob_1_over_trees =sum(Instance_records$Prob_1)/nrow(Instance_records))
  
  #calculate the total uncertainty as the negative sum of the shannon entropies
  #of the average predicted probabilities for each class
  Instance_total_unc <- -(sum(sapply(Average_pred_probs, function(p){
    Entropy = p*log10(p)
      Entropy = replace(Entropy, is.infinite(Entropy),0)
  })))

  #calculate aleatoric uncertainty as the average shannon entropy over all models(trees)
  Instance_aleatoric_unc <- sum(Instance_records$Entropy)/ncol(Predict_nodes)
  
  #Epistemic uncertainty = Total uncertainty - Aleatoric uncertainty
  Instance_epistemic_unc <- Instance_total_unc - Instance_aleatoric_unc
  
  return(list(Total_unc = Instance_total_unc,
              Aleatoric_unc = Instance_aleatoric_unc,
              Epistemic_unc = Instance_epistemic_unc))  
  
}) #close loop over instances
}) #close system.time
plan(sequential) #close parallel processing
stop_time <- Sys.time() #stop timer
stop_time - Start_time #calculate operation time


#using a for loop with pre-assignment of vector
system.time({Uncertainties_calc_new <- sapply(1, function(instance){

  #calculate average predicted probability for each class across the models (trees)
  Instance_records <- data.frame(matrix(nrow = ncol(Predict_nodes), ncol = 5))
  for(tree in 1:ncol(Predict_nodes)){
  Instance_records[tree,] <- Entropy_table[Entropy_table$Node_ID == Predict_nodes[instance,tree] & Entropy_table$Tree_num == tree,]  
  }
  colnames(Instance_records) <- colnames(Entropy_table)
  
  Average_pred_probs <-  list(Average_prob_0_over_trees = sum(Instance_records$Prob_0)/nrow(Instance_records),
                              Average_prob_1_over_trees =sum(Instance_records$Prob_1)/nrow(Instance_records))
  
  #calculate the total uncertainty as the negative sum of the shannon entropies
  #of the average predicted probabilities for each class
  Instance_total_unc <- -(sum(sapply(Average_pred_probs, function(p){
    Entropy <- p*log10(p)
      Entropy <- replace(Entropy, is.infinite(Entropy),0)
  })))

  #calculate aleatoric uncertainty as the average shannon entropy over all models(trees)
  Instance_aleatoric_unc <- sum(Instance_records$Entropy)/ncol(Predict_nodes)
  
  #Epistemic uncertainty = Total uncertainty - Aleatoric uncertainty
  Instance_epistemic_unc = Instance_total_unc - Instance_aleatoric_unc
  
  return(list(Total_unc = Instance_total_unc,
              Aleatoric_unc = Instance_aleatoric_unc,
              Epistemic_unc = Instance_epistemic_unc))  
  
}) #close loop over instances
})

#What is clear from this testing is that it takes ~4.5 seconds to calculate
#the uncertainty estimates for a single instance under a 500 tree RF model. Given that we have 
#~4,500,000 cells to calculate for 10 potential transitions (obviously not all
#cells can undergo all 10 transitions but the actual number is difficult to deduce)
#and the fact that we want to do this process in every simulation step means 
#that even with parallel computation it would be 1000's of hours of processing time 

#Options to deal with this: 
#Switch to a 100 tree model which reduces calculation time to ~1 sec
#but even then this does not reduce the scope of the problem. 

#Perform calculation for only a subset of the cells: 
# For validation: Only perform the uncertainty estimation for the cells that are
#selected to change and those that changed in the observed data but not change
#in the simulated data
#For simulation: Only the cells selected to change. 

#Either way this requires a map comparison step so the first check that needs to
#be made is how many cells are actually changing in an average simulation step  

### =========================================================================
### E- Determining number of transitions performed per time steps
### =========================================================================

#These figures were taken from the tabular outputs contained in the Dinamica run during one of the calibration simulation steps: 
Expander_total <- sum(c(809,40579,2496,3183,10435,7273,6755,1254,3878,18602,1100,4359,6969,7675,2119,13087,1571,1766,1189,4361, 11369))
Patcher_total <- sum(c(341,3091,302,321,8618, 3498,1055,192,919,5040,1118,2003,1698,1864,3053,2358,1882,287,177,479,824))
Total = Expander_total + Patcher_total

#calculating how many hours calculating the uncertainty for all of these
#transitioning cells would take = ~30 hours.... 
Hours_computation <- (Total*4.5)/8/60/60

#This is not even included the observed cellular transitions that were missed...

#calculate an example of how many simulated transitions were right and how many wrong 
#load observed LULC raster
Obs_LULC <- raster("E:/LULCC_CH/Data/Historic_LULC/LULC_2018_agg.grd") 
Sim_LULC <- raster("E:/LULCC_CH/Results/Dinamica_simulated_LULC/CALIBRATION/v1/simulated_LULC_scenario_CALIBRATION_simID_v1_year_2020.tif")
Sim_LULC_minus_1 <- raster("E:/LULCC_CH/Results/Dinamica_simulated_LULC/CALIBRATION/v1/simulated_LULC_scenario_CALIBRATION_simID_v1_year_2015.tif")

Trans <- Sim_LULC != Sim_LULC_minus_1
Correct_cells <- Obs_LULC == Sim_LULC
incorrect_cells <- Obs_LULC != Sim_LULC
Correct_trans <- Correct_cells*Trans
Incorrect_trans <- incorrect_cells*Trans
freq(Correct_trans)
freq(Incorrect_trans)
freq(Trans)

