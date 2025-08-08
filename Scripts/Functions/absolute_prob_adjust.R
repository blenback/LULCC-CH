#' @title Perform absolute adjustment of probabilities of change in target classes
#' @description
#' Perform absolute adjustment of probabiltieis of change in target classes
#' either inside or outside an intervention mask.
#' @param Prob_raster_stack A RasterStack containing layers of probability of change to certain land use classes, layers named Prob_*class*.
#' @param Prob_adjust_zone A string indicating the zone for adjustment, either "Inside" or "Outside".
#' @param Prob_adjust_value A numeric value to set the probabilities in the target area.
#' @param Target_classes A vector of land use classes to be targeted by the intervention.
#' @param Intervention_mask A RasterLayer or RasterStack representing the intervention mask.
#' @return A RasterStack with updated probabilities for the target land use classes.
absolute_prob_adjust <- function(
    Prob_raster_stack,
    Prob_adjust_zone = Outside,
    Prob_adjust_value = 0.1,
    Target_classes,
    Intervention_mask
){
  
  # loop over the target classes
  for(lulc_class in Target_classes){
    
    cat(paste("Adjusting pixels values of class:", lulc_class, ",", Prob_adjust_zone, "mask to:", Prob_adjust_value, "\n"))
    
    # Subset to target layer
    Target_layer <- Prob_raster_stack[[lulc_class]]
    layer_index <- which(names(Prob_raster_stack) == lulc_class)
  
    # If Prob_adjust_zone is Inside, then mask the target layer to inside the mask
    if(Prob_adjust_zone == "Inside"){
      Target_area <- terra::mask(Target_layer, Intervention_mask == 1)
      } else if(Prob_adjust_zone == "Outside"){
        # invert the mask to get the non-intersecting area
        Target_area <- terra::mask(Target_layer, Intervention_mask, inverse = TRUE)
      }

    # Adjust the probabilities in the target area
    Target_area[values(Target_area) > 0] <- Prob_adjust_value

    # Identify which cells need to have value updated
    ix <- cells(Target_area > 0)
    
    # Replace values in target raster
    Target_layer[ix] <- Target_area[ix]
    
    # Update the Prob_raster_stack with the modified Target_layer
    Prob_raster_stack[[layer_index]] <- Target_layer
  }
  
  return(Prob_raster_stack)
}