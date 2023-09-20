Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup_test.xlsx", sheetName = Period_tag)

Prediction_probs <- Trans_dataset_complete[, c("ID", "x", "y", unique(Model_lookup$Initial_LULC), "Static")]

#add cols to both the complete and NA data to capture predict probabilities to each class
Final_LULC_classes <- unique(Model_lookup$Final_LULC)
for(i in Final_LULC_classes){
Prediction_probs[[paste0("Prob_", i)]] <- 0
Trans_dataset_na[[paste0("Prob_", i)]] <- NA
}

#Non_parallel TPC calculation: 
Non_par_start <- Sys.time() 
#loop over transitions
for(i in 1:nrow(Model_lookup)){

#vector trans_name
Trans_name <- Model_lookup[i, "Trans_name"]
Trans_ID <- Model_lookup[i, "Trans_ID"]
Region <- Model_lookup[i,"Region"]
Final_LULC <- Model_lookup[i, "Final_LULC"]
Initial_LULC <- Model_lookup[i, "Initial_LULC"]

#print status message
cat(paste0("predicting probabilities for transitions from ", Initial_LULC, " to ", Final_LULC, " within region: ", Region, "\n"))

#load model
Fitted_model <- readRDS(Model_lookup[i, "File_path"])

#subset data for prediction
pred_data <- Trans_dataset_complete[Trans_dataset_complete[Initial_LULC] == 1 &
                                      Trans_dataset_complete$Bioregion_Class_Names == Region,] 

#predict using fitted model
prob_predicts <- as.data.frame(predict(Fitted_model, pred_data, type="prob"))
names(prob_predicts)[[2]] <- paste0("Prob_", Final_LULC)

#bind to ID
predict_ID <- cbind(ID = pred_data[, c("ID")], prob_predicts[paste0("Prob_", Final_LULC)])

#append the predictions at the correct rows in the results df
Prediction_probs[which(Prediction_probs$ID %in% predict_ID$ID), paste0("Prob_", Final_LULC)] <- predict_ID[paste0("Prob_", Final_LULC)]

} #close loop over Models
Non_par_end <- Sys.time()
Non_par_time <- Non_par_end - Non_par_start #sequential time = 2.937131 mins
  } #Close non-parallel TPC chunk 

