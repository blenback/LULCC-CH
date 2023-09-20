#subset model_lookup table to unique trans ID
Unique_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID), ]

Unique_initial_classes <- unique(Unique_trans$Initial_LULC)

Pred_prob_columns <- grep("Prob_", names(Prediction_probs), value = TRUE)

#Loop over unique trans using details to subset data and check prediction
test_pred_probs <- lapply(Unique_initial_classes, function(i){
                          
Final_LULC_classes <- Unique_trans[Unique_trans$Initial_LULC == i, "Final_LULC"]

#subset prediction probs to the initial class
Initial_LULC_dat <- Prediction_probs[Prediction_probs[i]==1,]

#check for non zero values in Final_class columns
col_sums <- colSums(Initial_LULC_dat[,Pred_prob_columns] != 0) 

#check for non zero values in columns other than the final classes
test <- list("other_classes_non_zero" = any(col_sums[!(names(col_sums) %in% paste0("Prob_", Final_LULC_classes))]>0),
#check for non zero values in the final classes
"Final_classes_non_zero" = any(col_sums[(names(col_sums) %in% paste0("Prob_", Final_LULC_classes))]>0))  

return(test) 
})


#testing the Raster_prob_values before the spatial interventions in the same way
test_raster_probs <- lapply(Unique_initial_classes, function(i){
                          
Final_LULC_classes <- Unique_trans[Unique_trans$Initial_LULC == i, "Final_LULC"]

#subset prediction probs to the initial class
Initial_LULC_dat <- na.omit(Raster_prob_values[Raster_prob_values[i]==1,])

#check for non zero values in Final_class columns
col_sums <- colSums(Initial_LULC_dat[,Pred_prob_columns] != 0) 

#check for non zero values in columns other than the final classes
test <- list("other_classes_non_zero" = any(col_sums[!(names(col_sums) %in% paste0("Prob_", Final_LULC_classes))]>0),
#check for non zero values in the final classes
"Final_classes_non_zero" = any(col_sums[(names(col_sums) %in% paste0("Prob_", Final_LULC_classes))]>0))  

return(test) 
})

#testing the values in the rasters by performing zonal stats

test_rast <- rast(current_LULC_path)

Pred_rast_paths <- list.files(prob_map_folder, full.names = TRUE)
names(Pred_rast_paths) <- sapply(Pred_rast_paths, function(x){
  class_name <- as.character(str_match(str_split_i(x, "_to_", 1),paste0(LULC_rat$Class_abbreviation, collapse = "|")))
  class_val <- unlist(LULC_rat[LULC_rat$Class_abbreviation == class_name, "Aggregated_ID"])
  })

test_raster_vals <- lapply(1:length(Pred_rast_paths), function(i){
  
  Pred_rast <- rast(Pred_rast_paths[i])
  zonal_result <- zonal(x = Pred_rast, z = test_rast, fun = "sum", na.rm=TRUE)
  
  #check if values for any layers other than the initial class are greater than zero
  #check for non zero values in columns other than the final classes
  test <- list("other_classes_non_zero" = any(zonal_result[zonal_result$layer != names(Pred_rast_paths)[i],2]>0),
  #check for non zero values in the final classes
  "Final_classes_non_zero" = any(zonal_result[zonal_result$layer == names(Pred_rast_paths)[i], 2]>0))
                            
  return(test)
})


