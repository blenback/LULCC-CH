#############################################################################
## Shaker_unc_ranger: Testing Shaker and Hullermeier point wise uncertainty 
## approach using the Ranger RF implementation 
##
##
## Date: 22-11-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set the working directory
wpath <- "YOUR DIR"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","rlist",  # Data management and processing
         "randomForest", "RRF", "butcher", "ranger", # Core modelling
         "ROCR","ecospat","caret", "foreach", "doMC", "data.table", "raster", "tidyverse",
         "testthat", "sjmisc", "tictoc", "lulcc", "pbapply", "stringr", "readr", "openxlsx", "readxl") #Model evaluation

if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

### =========================================================================
### B- Creation of shannon entropy table for exemplar dataset
### =========================================================================

#To calculate the entropy measures on the ensemble we need 
#instance wise probablistic predictions across all trees. 
#This can be done in advance by predicting the training data using the fitted RF model 

#load an exemplar data set
exp_data <- readRDS("Data/Transition_datasets/Post_predictor_filtering/Period_2009_2018_filtered_predictors_regionalized")[[1]]

#bind independent and dependent variables for train/test split
exp_data <- cbind(exp_data$trans_result, exp_data$cov_data)
names(exp_data)[1] <- "transitions_result" 

#split into train and test
exp_data$sid <- 1:nrow(exp_data) #add numeric ID 
chc <-  dplyr::slice_sample(dplyr::group_by(exp_data, transitions_result), prop =  0.7, replace=FALSE)
train <- exp_data[chc$sid,]  
test <- exp_data[-chc$sid,]
train$sid <- NULL
test$sid <- NULL

#train a probabilistic ranger classification model
Mod <- ranger(x = train[,c(names(train) != "transitions_result")],
                    y = train$transitions_result,
                    num.tree = 500,
                    probability = TRUE)

#unlike the randomForests implementation of RF, ranger is able to return 
#tree-wise probablistic predictions for each instance according to the process
#of Malley et al. 2012 (dubbed probability machines). This is a shortcut compared
#to the process I was using because it means we don't to go through the probablistic
#predictions of all leaves in the trees.

#a big question remaining though: Are the Tree-wise probablistic predictions 
#calculated by ranger using only the out of bag samples of training data or are
#they using all instances i.e. both in bag and out of bag
#Interestingly it appears that in Shakers work they did not rely on the 
#probablistic predictions calculated from all training instances only the out
#of bag instances this is evident by their call to the randomforestsclassifier
#which sets the argument of Bootstrap = TRUE

test_probs <- predict(Mod, data = test, predict.all = TRUE)








