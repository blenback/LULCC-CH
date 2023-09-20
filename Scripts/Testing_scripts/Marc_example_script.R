

### =========================================================================
### A- Preparation
### =========================================================================

wpath <- "E:/LULCC_CH"
setwd(wpath)

rstudioapi::filesPaneNavigate(wpath)

#I've deliberately deleted the right hand side of the assignment below
#add a vector that could be used with lines 16-21 to load some common packages
#that would be needed for this analysis
packs <-

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
if (length(new.packs)) install.packages(new.packs)

#what is lapply()doing in this lines?
invisible(lapply(packs, require, character.only = TRUE))
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

### =========================================================================
### B- Creation of shannon entropy table for exemplar dataset
### =========================================================================

#To calculate the entropy measures on the ensemble we need 
#instance wise probablistic predictions across all trees. 
#This can be done in advance by predicting the training data using the fitted RF model 

#load an exemplar data set
exp_data <- readRDS("Data/Transition_datasets/Post_predictor_filtering/Period_2009_2018_filtered_predictors_regionalized")[[1]]

#?
exp_data <- cbind(exp_data$trans_result, exp_data$cov_data)
names(exp_data)[1] <- "transitions_result" 

#?
exp_data$sid <- 1:nrow(exp_data) 
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

#what is the function 'attr' doing here? what would be another base R alternative?
Train_nodes <- attr(predict(Mod, train, nodes=TRUE),'nodes')

#run prediction on test data and get the terminal node ID for each instance
#for each tree in the forest. 
Test_nodes <- attr(predict(Mod, test, nodes=TRUE),'nodes')

# loop over all the unique terminal node IDs in the training data
#and calculate the probablistic predictions. 

#Create a duplicate matrix to be filled with the results
# Prob_preds_training <- vector("list", ncol(Train_nodes))
# names(Prob_preds_training) <- colnames(Train_nodes)

#outer loop over columns (e.g RF trees)
future::plan(multisession)
system.time({Prob_preds_training <- future_lapply(1:ncol(Train_nodes), function(tree){
 
  #inner loop over unique terminal IDs in column
Tree_preds <- lapply(unique(Train_nodes[,tree]), function(ID){
    
    #Get indices of training instances with this terminal ID
    train_indices <-  which(Train_nodes[,tree] == ID) 
    
    #perform Laplace correction and then calculate shannon entropy
    #over both class values
    LPC_probs <- c((sum(train[train_indices,"transitions_result"] == 0) + 1)/(length(train_indices)+2),
                  (sum(train[train_indices,"transitions_result"] == 1) + 1)/(length(train_indices)+2))
    
    Shannon_Entropy <- sum(sapply(LPC_probs, function(p){
      Entropy <- -p*log10(p)
      Entropy <- replace(Entropy, is.infinite(Entropy),0)
  
    }))
  return(list(LPC_probs = LPC_probs,
                Shannon_Entropy = Shannon_Entropy))  
  })#close inner loop  
   names(Tree_preds) <- unique(Train_nodes[,tree])
return(Tree_preds)
})#close outer loop
names(Prob_preds_training) <- colnames(Train_nodes)

}) #close system.time
plan(sequential)

#flatten the nested list into a DF
Entropy_table <- rbindlist(lapply(Prob_preds_training, function(x) {
  df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE),stringsAsFactors=FALSE)
  names(df) <- c("Prob_0", "Prob_1", "Entropy")
  df$Node_ID <- names(x)
  return(df)
  }), idcol = "Tree_num")

#save
saveRDS(Entropy_table, "E:/LULCC_CH/Scripts/Testing_scripts/Exemplar_entropy_table.rds")

### =========================================================================
### C- Creation of shannon entropy tables for all transition models in all time periods
### =========================================================================

#vector model periods
Model_periods <- c("1985_1997", "1997_2009", "2009_2018")

lapply(Model_periods , function(Model_period){
#paste together path
Model_folder_path <- paste0("Data/Fitted_models/Fitted_RF_models/regionalized_filtered/Period_", Model_period, "_rf_models") 

#list model file paths
model_paths <- as.list(list.files(Model_folder_path, recursive = TRUE, full.names = TRUE))

#rename
names_w_dir <- sapply(as.list(list.files(Model_folder_path, recursive = TRUE, full.names = FALSE)),function(x) 
  str_remove_all(str_split(x, "/")[[1]][2], "_rf-1.rds")
                 )

#load viable trans_list for period
Initial_LULC_classes <-  unique(readRDS("E:/LULCC_CH/Tools/Viable_transitions_lists.rds")[[Model_period]][["Initial_class"]])
Initial_LULC_classes <- paste0(Initial_LULC_classes, "_")

#replacing the "_" between LULC classes with a '.'
names_w_dir <- sapply(names_w_dir, function(name){
Initial_class <- Initial_LULC_classes[sapply(Initial_LULC_classes, function(class) {grepl(class, name)}, simplify = TRUE)]
new_name <- gsub('^\\_|\\_$', '.', Initial_class)
renamed <- gsub(Initial_class, new_name, name)
})

names(model_paths) <- names_w_dir

Save_model_entropy_table <- function(model_file_path, model_name){
   #create a folder path based on time period
  folder_path <- paste0("Data/Uncertainty_tables/", Model_period) 
  dir.create(folder_path, recursive = TRUE)
  
  #expand to file path using model name
  file_path <- paste0(folder_path, "/", model_name, ".", Model_period, ".Entropy_table.rds")
  
  #load model object
  model_object <- readRDS(model_file_path)
  
  #extract the fitted model (RF object) 
  Mod <- model_object[["model"]]@fits[["replicate_01"]][["rf-1"]]
  
  Train_data <- model_object[["model"]]@train[[1]]
  
  #Get the terminal node IDs of the training data 
  Train_nodes <- attr(predict(Mod, Train_data , nodes=TRUE),'nodes')
 
  Prob_preds_training <- lapply(1:ncol(Train_nodes), function(tree){
 
    #inner loop over unique terminal IDs in column
    Tree_preds <- lapply(unique(Train_nodes[,tree]), function(ID){
    
      #Get indices of training instances with this terminal ID
      train_indices <-  which(Train_nodes[,tree] == ID) 
    
      #perform Laplace correction and then calculate shannon entropy
      #over both class values
      LPC_probs <- c((sum(Train_data[train_indices,"transitions_result"] == 0) + 1)/(length(train_indices)+2),
                  (sum(Train_data[train_indices,"transitions_result"] == 1) + 1)/(length(train_indices)+2))
    
      Shannon_Entropy <- sum(sapply(LPC_probs, function(p){
        Entropy <- -p*log10(p)
        Entropy <- replace(Entropy, is.infinite(Entropy),0)
        }))

    })#close inner loop  
    names(Tree_preds) <- unique(Train_nodes[,tree])
  return(Tree_preds)
  })#close outer loop
  names(Prob_preds_training) <- colnames(Train_nodes)

  #flatten the nested list into a DF
  Entropy_table <- rbindlist(lapply(Prob_preds_training, function(x) {
    df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE),stringsAsFactors=FALSE)
    names(df) <- "Entropy"
    df$Node_ID <- names(x) 
    return(df)
    }), idcol = "Tree_num")

  #save the entropy table
  saveRDS(Entropy_table, file = file_path)
  } #close function

#apply the function
mapply(Save_model_entropy_table, 
       model_file_path = model_paths,
       model_name = names(model_paths))

}) #close loop over periods


### =========================================================================
### D- Test calculation of uncertainty using test data (as per simulation step)
### =========================================================================
#copy some variables to the same names used in trans_potent_calc
Fitted_model <- Mod
pred_data <- test

#reload the entropy table for the exemplar data
Entropy_table <- readRDS("E:/LULCC_CH/Scripts/Testing_scripts/Exemplar_entropy_table.rds")

#run prediction on the data and get the terminal node ID for each instance
#for each tree in the forest. 
Predict_nodes <- attr(predict(Fitted_model, pred_data, nodes=TRUE),'nodes')

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
  
  #TO DO: don't create list from average
  Average_pred_probs <-  list(Average_prob_0_over_trees = sum(Instance_records$Prob_0)/nrow(Instance_records),
                              Average_prob_1_over_trees =sum(Instance_records$Prob_1)/nrow(Instance_records))
  
  #TO DO: No need to use a loop as the equations is already vectorized
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

