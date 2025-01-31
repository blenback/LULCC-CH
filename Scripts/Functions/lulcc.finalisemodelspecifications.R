### =========================================================================
### lulcc.finalisemodelspecifications: Enter optimal model specification
### and parameters for prediction 
### =========================================================================
#' 
#'
#' @param Model_specs_path Character, file path for table of model specifications
#' @param Param_grid_path Character, file path for grid of hyperparameters
#' @author Ben Black
#' @export

lulcc.finalisemodelspecifications <- function(Model_specs_path, Param_grid_path){

  #Load model specifications
  model_specs <- read_excel(Model_specs_path)

  #filter for completed model specifcations
  model_specs <- model_specs[model_specs$Modelling_completed == "Y",]
  
  #Use prompts to get specifications as input from user
  Model_type <- readline(prompt= paste0("Choose model type from: ", paste(unique(model_specs$Model_type), sep = "or"), ", Enter type: "))
  Model_scale <- readline(prompt= paste0("Choose model scale from: ", paste(unique(model_specs$Model_scale), sep = "or"), ", Enter scale: "))
  Feature_selection_employed <- readline(prompt= "Use transitions datasets that have undergone feature selection (TRUE or FALSE): ")
  Balance_adjustment <- if(Model_type == "rf"){readline(prompt= "Use Tree-wise downsampling to address class imbalance: ")} else{"FALSE"}
  
  #Subset model specifications by user inputs
  Predict_model_specs <- model_specs[model_specs$Model_type == Model_type &
                                     model_specs$Model_scale == Model_scale &
                                     model_specs$Feature_selection_employed == Feature_selection_employed &
                                     model_specs$Balance_adjustment == Balance_adjustment,]
  
  #change completion values to 'N'
  Predict_model_specs$Modelling_completed <- "N"

  #Print a warning that not all data periods are included in the predict model specs table
  if(all(Data_periods %in% unique(Predict_model_specs$Data_period_name)) == FALSE){
  cat(paste0("Warning the data period/s: ",
           paste(Data_periods[which(!(Data_periods %in% unique(Predict_model_specs$Data_period_name)))], sep = ","),
           " do not have models of the desired specification completed for them
           (i.e. Errors may have occured in model fitting or evaluation,
           please check before continuing"))
} 

#save prediction model specs
openxlsx::write.xlsx(Predict_model_specs, file = "Tools/Predict_model_specs.xlsx")

#Load parameter grid
param_grid <- read_excel(Param_grid_path, sheet = Model_type)

#create a duplicate parameter grid for prediction
Predict_param_grid <- data.frame(matrix(nrow = nrow(param_grid), ncol = ncol(param_grid))) 
colnames(Predict_param_grid) <- colnames(param_grid)

#fill grid 
for(i in colnames(param_grid)){
  Predict_param_grid[[i]] <- readline(prompt= paste0("Enter value for parameter ", i, ": "))
}

#save prediction parameter grid
openxlsx::write.xlsx(Predict_param_grid, file = "Tools/Predict_param-grid.xlsx", sheetName = Model_type)

}