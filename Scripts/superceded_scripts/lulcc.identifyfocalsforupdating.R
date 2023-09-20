### =========================================================================
### lulcc.identifyfocalsforupdating: Identifying focal layers in final covariate selection (after embedded selection)
### and creating a lookup table for the given modelling period to be used in dynamic updating of focal during simulation
### ===========================================================================
#' 
#'
#' @param Final_cov_selection List of results objects from feature colection. 
#' @param Data_period_name Character name of dataset period to be used in file saving.
#' @param Dataset_scale Character scale of datasets to be used in file saving.
#'
#' @author Ben Black
#' @export


lulcc.identifyfocalsforupdating <- 
  function(Final_cov_selection, Data_period_name, Dataset_scale){

#split the variable names by those that are focal (i.e. neighbourhood), by matching 'nhood' in name, and those that are not.
Focal_names <- lapply(Final_cov_selection, function(x) {
  All_variables <- x[["var"]]
  nhood_vars <- grep("nhood", All_variables, value = TRUE) 
  return(nhood_vars)
  })

#gather names of all focal variables required to be produced into a vector and filter for unique entries
Focal_layers_required <- unique(Reduce(c, unlist(Focal_names, recursive = FALSE)))

#load focal layer look up table
Focal_lookup <- readRDS("Data/Preds/Raw/Neighbourhood/Focal_layer_lookup")

#subset by the current data period
Focal_lookup <- Focal_lookup[Focal_lookup$period == Data_period_name,]

#subset the focal look up table by the list of focals required for the transition models for this period
Focal_subset <- Focal_lookup[Focal_lookup$layer_name %in% Focal_layers_required,]

#save the Focal layer details for this period 
saveRDS(Focal_subset, file = paste0("Data/Preds/Tools/Neighbourhood_details_for_dynamic_updating/", Data_period_name, "_", Dataset_scale, "_focals_for_updating"))
}