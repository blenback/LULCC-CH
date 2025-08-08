#' @title Perform relative probability adjustment for target land use classes
#' @description
#' Perform relative probability adjustment for target lulc classes based upon 
#' the % difference in average probabilities above specified percentiles for 
#' the intervention and non-intervention pixels with the option to specify
#' target pixels as those outside or inside the intervention mask areas
#' @param Prob_adjust_valency A string indicating the valency of the adjustment, either "Increase", "Decrease" or "Increase_inside_decrease_outside".
#' @param Prob_adjust_intervention_percentile A numeric value indicating the percentile for the intervention area.
#' @param Prob_adjust_non_intervention_percentile A numeric value indicating the percentile for the non-intervention area.
#' @param Prob_adjust_threshold A numeric value indicating the threshold for the percentage difference.
#'@param Prob_adjust_zone A string indicating the zone for adjustment, either "Inside" or "Outside".
#' @param Target_classes A vector of land use classes to be targeted by the intervention.
#' @return Prob_raster_stack A RasterStack with updated probabilities for the target land use classes.
relative_prob_adjust <- function(
    Prob_adjust_valency,
    Prob_adjust_intervention_percentile,
    Prob_adjust_non_intervention_percentile,
    Prob_adjust_threshold,
    Prob_adjust_zone,
    Target_classes,
    Intervention_mask,
    Prob_raster_stack
){
  
  # check that of Prob_adjust_valency == Increase_inside_decrease_outside that Prob_adjust_zone is "Inside"
  if(Prob_adjust_valency == "Increase_inside_decrease_outside" && Prob_adjust_zone != "Inside"){
    stop("If Prob_adjust_valency is 'Increase_inside_decrease_outside', then Prob_adjust_zone must be 'Inside'.")
  }

  # loop over the target classes
  for(lulc_class in Target_classes){
    
    # Subset to target layer
    Target_layer <- Prob_raster_stack[[lulc_class]]
    
    # Identify the layer index in the raster stack
    layer_index <- which(names(Prob_raster_stack) == lulc_class)
  
    # If Prob_adjust_zone is Inside, then the intervention area is inside the mask and non-intersecting area is outside the mask
    if(Prob_adjust_zone == "Inside"){
      Intervention_area <- terra::mask(Target_layer, Intervention_mask == 1)
      Non_intervention_area <- terra::mask(Target_layer, Intervention_mask == 1, inverse = TRUE)
      } else if(Prob_adjust_zone == "Outside"){
        # if the Prob_adjust_zone is Outside, then the intervention area is outside the mask and the non-intersecting area is inside the mask
      Intervention_area <- terra::mask(Target_layer, Intervention_mask, inverse = TRUE)
      Non_intervention_area <- terra::mask(Target_layer, Intervention_mask == 1)
    }
    
    # calculate percentile values of probability for pixels in the 
    # intervention area vs. non-intervention area (i.e. intervention - non_intervention)
    #outside the mask 
    
    # seperate raster values
    Intervention_vals <- values(Intervention_area)
    Non_Intervention_vals <- values(Non_intervention_area)
    
    # get percentile values      
    Intervention_ptile_val <- quantile(Intervention_vals[Intervention_vals > 0], probs =Prob_adjust_intervention_percentile, na.rm = TRUE)
    Non_intervention_ptile_val <- quantile(Non_Intervention_vals[Non_Intervention_vals >0], probs = Prob_adjust_non_intervention_percentile, na.rm = TRUE)

    #get the means of the values above the 90th percentile
    Intervention_ptile_mean <- mean(Intervention_vals[Intervention_vals >= Intervention_ptile_val], na.rm = TRUE)
    Non_intervention_ptile_mean <- mean(Non_Intervention_vals[Non_Intervention_vals >= Non_intervention_ptile_val], na.rm = TRUE)

    #mean difference
    Mean_diff <- Intervention_ptile_mean - Non_intervention_ptile_mean

    #Average of means
    Average_mean <- (Intervention_ptile_mean + Non_intervention_ptile_mean) / 2

    #calculate percentage difference
    Perc_diff <- (Mean_diff / Average_mean) * 100
    
    #print the percentage difference for debugging purposes
    cat(paste0("The Percentage difference in average probability above the ", Prob_adjust_intervention_percentile, " and ", Prob_adjust_non_intervention_percentile, " percentiles of the intervention & non-intervention areas respectively for ", lulc_class, " is : ", Perc_diff), "\n")
    
    # If Prob_adjust_valency == "Increase" then the goal of the intervention is
    # to increase the probability of change for the target land use class in the intervention area
    if(Prob_adjust_valency == "Increase"){
    
    # However if Perc_diff > 0 this implies that the average probability of change
    # above the specificed percentile in the intervention area is higher than in the 
    # non-intervention area and as such it we should increase the probabilities
    # in the intervention area by the % difference. 
    
    # Whereas if Perc_diff < 0 this implies that the average probability of change
    # above the percentile in the intervention area is lower than in the 
    # non-intervention area and as such we should decrease the probabilities
    # in the non- intervention area by the % difference.
    
      if(Perc_diff > 0){
        
        #check that Perc_diff is above the Prob_adjust_threshold
        if (abs(Perc_diff) < Prob_adjust_threshold) {
          cat(paste0("The Percentage difference is below the threshold for ", lulc_class, ", setting to threshold value: ", Prob_adjust_threshold), "\n")
          Perc_diff <- Prob_adjust_threshold
          }
        
        cat(paste("because the Prob_adjust_valency is", Prob_adjust_valency, "and the percentage difference is >0 then increasing the probability of the intervention pixels"), "\n")
        
        # Increase the probability of instances above the specified percentile
        Intervention_area[values(Intervention_area) > Intervention_ptile_val] <- Intervention_area[values(Intervention_area) > Intervention_ptile_val] + 
          (Intervention_area[values(Intervention_area) > Intervention_ptile_val] / 100) * Perc_diff
        
        # Identify which cells need to have value updated
        ix <- cells(Intervention_area > Intervention_ptile_val)
    
        # Replace values in target raster
        Target_layer[ix] <- Intervention_area[ix]
        
      } else if(Perc_diff < 0){
        
        #check that Perc_diff is below the Prob_adjust_threshold
        if (abs(Perc_diff) < Prob_adjust_threshold) { 
          cat(paste0("The Percentage difference is below the threshold for ", lulc_class, ", setting to threshold value: ", Perc_diff), "\n")
          Perc_diff <- -(Prob_adjust_threshold) 
          }
        
        cat(paste("because the Prob_adjust_valency is", Prob_adjust_valency, "and the percentage difference is <0 then decreasing the probability of the non-intervention pixels"), "\n")
        
        # Decrease the probability of instances above the specified percentile
        Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] <- 
          Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] + 
          (Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] / 100) * Perc_diff
        
        # Identify which cells need to have value updated
        ix <- cells(Non_intervention_area > Non_intervention_ptile_val)
    
        # Replace values in target raster
        Target_layer[ix] <- Non_intervention_area[ix]
    
      }
    } else if(Prob_adjust_valency == "Decrease"){
      
    # If Prob_adjust_valency == "Decrease" then the goal of the intervention is
    # to decrease the probability of change for the target land use class in the intervention area
    
    # If Perc_diff > 0 this implies that the average probability of change in the
    # intervention area is higher than in the non-intervention area and as such we should
    # decrease the probabilities in the intervention area by the % difference.
    # Whereas if Perc_diff < 0 this implies that the average probability of change in the
    # intervention area is lower than in the non-intervention area and as such we should
    # increase the probabilities in the non-intervention area by the % difference.
    
      if(Perc_diff > 0){
        
        #check that Perc_diff is above the Prob_adjust_threshold
        if (abs(Perc_diff) < Prob_adjust_threshold) {
          cat(paste0("The Percentage difference is below the threshold for ", lulc_class, ", setting to threshold value: ", Prob_adjust_threshold), "\n")
          Perc_diff <- Prob_adjust_threshold
        }
        
        cat(paste("because the Prob_adjust_valency is", Prob_adjust_valency, "and the percentage difference is >0 then decreasing the probability of the intervention pixels"), "\n")
        
        # Decrease the probability of instances above the specified percentile
        Intervention_area[values(Intervention_area) > Intervention_ptile_val] <- 
          Intervention_area[values(Intervention_area) > Intervention_ptile_val] + 
          (Intervention_area[values(Intervention_area) > Intervention_ptile_val] / 100) * -(Perc_diff)
        
        # Identify which cells need to have value updated
        ix <- cells(Intervention_area > Intervention_ptile_val)
    
        # Replace values in target raster
        Target_layer[ix] <- Intervention_area[ix]
        
      } else if(Perc_diff < 0){
        
        #check that Perc_diff is below the Prob_adjust_threshold
        if (abs(Perc_diff) < Prob_adjust_threshold) {
          cat(paste0("The Percentage difference is below the threshold for ", lulc_class, ", setting to threshold value: ", Prob_adjust_threshold), "\n")
          Perc_diff <- -(Prob_adjust_threshold) 
        }
        cat(paste("because the Prob_adjust_valency is", Prob_adjust_valency, "and the percentage difference is <0 then increasing the probability of the non-intervention pixels"), "\n")
        
        # Increase the probability of instances above the specified percentile
        Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] <- 
          Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] + 
          (Non_intervention_area[values(Non_intervention_area) > Non_intervention_ptile_val] / 100) * abs(Perc_diff)
        
        # Identify which cells need to have value updated
        ix <- cells(Non_intervention_area > Non_intervention_ptile_val)
    
        # Replace values in target raster
        Target_layer[ix] <- Non_intervention_area[ix]
      }
    } else if(Prob_adjust_valency == "Increase_inside_decrease_outside"){
      
      # If Prob_adjust_valency == "Increase_inside_decrease_outside" then the goal of the intervention is
      # to simulataneously increase the probability of change for the target land use class in the intervention area
      # and decrease the probability of change for the target land use class in the non-intervention area
      
      if(abs(Perc_diff) < Prob_adjust_threshold){
        cat(paste0("The Percentage difference is below the threshold for ", lulc_class, ", setting to threshold value: ", Prob_adjust_threshold), "\n")
        Perc_diff <- Prob_adjust_threshold
      }
      
      cat(paste("because the Prob_adjust_valency is", Prob_adjust_valency, "increasing the probability of the intervention pixels and decreasing the probability of the non-intervention pixels"), "\n")
      
      # Increase the probability of instances above the specified percentile in the intervention area
      Intervention_area[Intervention_area > Intervention_ptile_val] <- 
        Intervention_area[Intervention_area > Intervention_ptile_val] + 
        (Intervention_area[Intervention_area > Intervention_ptile_val] / 100) * abs(Perc_diff)
      
      # Identify which cells need to have value updated
      ix <- cells(Intervention_area > Intervention_ptile_val)
      # Replace values in target raster
      Target_layer[ix] <- Intervention_area[ix]
      
      # Decrease the probability of instances above the specified percentile in the non-intervention area
      Non_intervention_area[Non_intervention_area > Non_intervention_ptile_val] <- 
        Non_intervention_area[Non_intervention_area > Non_intervention_ptile_val] + 
        (Non_intervention_area[Non_intervention_area > Non_intervention_ptile_val] / 100) * -(abs(Perc_diff))
      
      # Identify which cells need to have value updated
      ix <- cells(Non_intervention_area > Non_intervention_ptile_val)
      # Replace values in target raster
      Target_layer[ix] <- Non_intervention_area[ix]
    }
    
    # set any values in Target_layer that are greater than 1 to 1
    Target_layer[Target_layer > 1] <- 1
    
    # Update the Prob_raster_stack with the modified Target_layer
    Prob_raster_stack[[layer_index]] <- Target_layer
    
  }
  
  # return the updated Prob_raster_stack
  return(Prob_raster_stack)
}
  
