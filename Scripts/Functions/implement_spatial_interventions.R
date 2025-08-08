#' @title Implement Spatial Interventions on Raster Probability Values
#' @description
#' Implement all specified spatial interventions on raster probability values
#' @param interventions_dir Directory containing YAML files with intervention definitions.
#' @param scenario_ID Identifier for the scenario to apply interventions.
#' @param raster_prob_values Data frame containing raster probability values with columns for coordinates and probabilities.
#' @param simulation_time_step The time step at which to apply the interventions.
#' @param LULC_rat data frame containing LULC class abbreviations and their aggregated IDs, used for filtering based on LULC classes.
#' @return A data frame with updated raster probability values after applying interventions.
implement_spatial_interventions <- function(
  interventions_dir,
  scenario_ID,
  raster_prob_values,
  simulation_time_step,
  LULC_rat,
  Proj = ProjCH
){
  
  # Load interventions for scenario from YAML file
  Interventions <- yaml.load_file(file.path(interventions_dir, paste0(scenario_ID, "_interventions.yml")))
  
  # filter to Intervention_stage == Allocation
  Current_interventions <- Interventions[sapply(Interventions, function(x) x[["Intervention_stage"]] == "Allocation")]
  
  # Subset to only interventions for which simulation_time_step is in Time_steps_implemented
  Current_interventions <- Current_interventions[c(sapply(Current_interventions, function(x) simulation_time_step %in% x$Time_steps_implemented))]

  # If no interventions are found, return the raster_prob_values unchanged
  if (length(Current_interventions) == 0) {
    warning("No interventions found for the specified scenario and time step. Returning original raster_prob_values.")
    return(raster_prob_values)
  } else {
    
    cat(paste("Found", length(Current_interventions), "allocation stage interventions for scenario", scenario_ID, "at time step", simulation_time_step), "\n")
    
    # order interventions by Intervention_ranking putting NAs last
    Current_interventions <- Current_interventions[order(sapply(Current_interventions, function(x) {
      if (is.null(x$Intervention_ranking)) {
        return(NA)  # Handle cases where Intervention_ranking is NULL
      } else {
        return(as.numeric(x$Intervention_ranking))
      }
      }), na.last = TRUE)]
    
    #vector names of columns of probability predictions (matching on Prob_)
    Pred_prob_columns <- grep("Prob_", names(raster_prob_values), value = TRUE)

    #convert probability table to raster stack
    Prob_raster_stack <- c(lapply(Pred_prob_columns, function(x) {
      col_rast <- rast(raster_prob_values[, c("x", "y", x)])
      crs(col_rast) <- Proj  # Set the CRS for each raster layer)
      return(col_rast)
    }))
    names(Prob_raster_stack) <- Pred_prob_columns
    cat("Converted raster_prob_values to RasterStack", "\n")
    
    # if any of the interventions have a value of From_lulc_filter, that is not
    #NULL then create a raster of the current LULC
    if (any(sapply(Current_interventions, function(x) {
      val <- x[["From_lulc_filter"]]
      !is.null(val) && length(val) > 0 && any(val != "None")
    }))) {
      # create a raster of the current LULC
      Current_lulc_raster <- rast(raster_prob_values[, c("x", "y", "LULC")])
      names(Current_lulc_raster) <- "Current_LULC"
      
      # add the crs
      crs(Current_lulc_raster) <- Proj
      
    }

    
    # loop over interventions
    for(intervention in Current_interventions) {
      
      # Print intervention name
      cat(paste("Applying intervention:", intervention[["Intervention_ID"]], "\n"))
      
      # Adjust format of Transition_target_classes
      Target_classes <- paste0("Prob_", intervention[["Transition_target_classes"]])
      
      # Prepare the Intervention mask
      if(intervention$Mask_type == "Static"){
        
        # load mask using the Intervention_masks
        Intervention_mask <- rast(intervention[["Intervention_mask"]])
      } else if (intervention$Mask_type == "Dynamic") {

        # subset the list of intervention$Intervention_mask to the current simulation_time_step
        # This assumes that the Intervention_mask is a list with keys corresponding to time steps
        intervention_mask_list <- intervention[["Intervention_mask"]]
        intervention_mask_path <- unlist(intervention_mask_list[names(intervention_mask_list) == simulation_time_step])
        
        # load mask using the Intervention_mask_path appending simulation_time_step
        Intervention_mask <- rast(intervention_mask_path)
      } else {
        stop(paste("Unknown Mask_type:", intervention[["Mask_type"]]))
      }
      
      # If the Intervention requires filtering by LULC classes then adjust the mask
      if (!is.null(intervention$From_lulc_filter) && length(intervention$From_lulc_filter) > 0 && all(intervention$From_lulc_filter != "None")) {
        
        # Get the raster value of the From_lulc_filter class from LULC_rat
        LULC_filter_classes <- unlist(LULC_rat[LULC_rat$Class_abbreviation %in% intervention[["From_lulc_filter"]], "Aggregated_ID"])
        
        cat(paste("Filtering to only cells that are currently LULC classes:", paste(LULC_filter_classes, collapse = ", "), "\n"))
        
        # Filter the Current_lulc_raster to only include the LULC_filter_classes
        LULC_mask <- Current_lulc_raster
        
        # set all values of LULC_mask that are not in LULC_filter_classes to NA
        LULC_mask[!(values(LULC_mask) %in% LULC_filter_classes)] <- NA
        
        # Now set all values that are not NA to 1
        LULC_mask[!is.na(LULC_mask)] <- 1
        
        # Mask the Intervention_mask with the LULC_mask
        Intervention_mask <- terra::mask(Intervention_mask, LULC_mask == 1)
      }
      
      # if intervention$Intervention_ID is "Agri_maintenance" or "Agri_abandonment"
      # then we need to subset the mask to the most marginal pixels
      if(intervention[["Intervention_ID"]] %in% c("Agri_maintenance", "Agri_abandonment")){
          
        cat(paste("because the intervention is:", intervention[["Intervention_ID"]], ", subsetting the Intervention mask to pixels 
                    with values >= the upper quartile ofagricultural marginality", "\n"))
        # Get the most marginal pixels by calculating the upper quartile value of the Intervention_mask
        # and setting all values that are not equal or greater than to the upper quartile value to NA
        min_value <- quantile(values(Intervention_mask), probs = 0.75, na.rm = TRUE)
          
        # Set all values that are not equal to min_value to NA
        Intervention_mask[values(Intervention_mask) >= min_value] <- NA
          
        # Set all values that are not NA to 1
        Intervention_mask[!is.na(Intervention_mask)] <- 1
      }
      
      # Apply different functions based on whether the intervention specifies absolute or relative adjustments to probabilities
      # if intervention$Prob_adjust_type == Absolute then apply absolute adjustment function
      if(intervention$Prob_adjust_type == "Absolute"){
        
        cat(paste("Applying absolute probability adjustment to cells:", intervention[["Prob_adjust_zone"]], "the intervention area, adjusting probability values to:", intervention[["Prob_adjust_value"]],  "\n"))
        
        Prob_raster_stack <- absolute_prob_adjust(
          Prob_raster_stack = Prob_raster_stack,
          Prob_adjust_zone = intervention$Prob_adjust_zone,
          Prob_adjust_value = intervention$Prob_adjust_value,
          Target_classes = Target_classes,
          Intervention_mask = Intervention_mask
        )
      } else if(intervention$Prob_adjust_type == "Relative") {
        
        # convert percentile values to numeric and decimal
        intervention[["Prob_adjust_intervention_percentile"]] <- as.numeric(intervention[["Prob_adjust_intervention_percentile"]]) / 100
        intervention[["Prob_adjust_non_intervention_percentile"]] <- as.numeric(intervention[["Prob_adjust_non_intervention_percentile"]]) / 100
        
        # Apply relative adjustment function
        Prob_raster_stack <- relative_prob_adjust(
          Prob_adjust_valency = intervention[["Prob_adjust_valency"]],
          Prob_adjust_intervention_percentile = intervention[["Prob_adjust_intervention_percentile"]],
          Prob_adjust_non_intervention_percentile = intervention[["Prob_adjust_non_intervention_percentile"]],
          Prob_adjust_threshold = intervention[["Prob_adjust_threshold"]],
          Prob_adjust_zone = intervention[["Prob_adjust_zone"]],
          Target_classes = Target_classes,
          Intervention_mask = Intervention_mask,
          Prob_raster_stack = Prob_raster_stack
        )
      } else {
        stop(paste("Unknown Prob_adjust_type:", intervention[["Prob_adjust_type"]]))
      }
  } # end of intervention loop
    
  }
  
  #convert raster stack back to dataframe
  # because terra::as.data.frame() does not handle NA values well,
  # we will loop over the names and convert each layer to df and replace the corresponding column in raster_prob_values
  for(i in names(Prob_raster_stack)) {
    
    # convert each raster layer to a data frame
    layer_df <- terra::as.data.frame(Prob_raster_stack[[i]], na.rm = FALSE)
    
    # replace the corresponding column in raster_prob_values
    raster_prob_values[, i] <- layer_df[, i]  # assuming the third column is the values
  }

  #return the updated raster_prob_values
  return(raster_prob_values)
}


