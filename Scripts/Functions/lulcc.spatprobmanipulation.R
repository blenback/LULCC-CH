#############################################################################
## lulcc.spatprobmanipulation: function to manipulate spatial transition 
##probabilities to represent interventions/policies/management activities 
## Date: 17-03-2023
## Author: Ben Black
#############################################################################
#'
#' @param Scenario_ID Chr, abbreviation of current scenario
#' @param Raster_prob_values Dataframe, predicted cellular transitions 
#'                            probabilities with x,y and cell ID cols
#' @param Simulation_time_step Chr, current simultion time step
#'
#' @author Ben Black
#' @export
#'

lulcc.spatprobmanipulation <- function(Interventions,
                                       Scenario_ID,
                                       Raster_prob_values,
                                       Simulation_time_step){

  #vector names of columns of probability predictions (matching on Prob_)
  Pred_prob_columns <- grep("Prob_", names(Raster_prob_values), value = TRUE)

  #convert probability table to raster stack
  Prob_raster_stack <- stack(lapply(Pred_prob_columns, function(x) rasterFromXYZ(Raster_prob_values[,c("x", "y", x)])))
  names(Prob_raster_stack@layers) <- Pred_prob_columns

  #convert Time_step and Target_classes columns back to character vectors
  Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
    x <- str_remove_all(x, " ")
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)
  
  Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
    x <- str_remove_all(x, " ")
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)
  
  #subset interventions to scenario
  Scenario_interventions <- Interventions[Interventions$Scenario_ID == Scenario_ID,] 
  
  #subset to interventions for current time point
  Time_step_rows <- sapply(Scenario_interventions$Time_step, function(x) any(grepl(Simulation_time_step, x)))
  Current_interventions <- Scenario_interventions[Time_step_rows, ]
  
  #loop over rows
  if(nrow(Current_interventions) !=0){
  for(i in nrow(Current_interventions)){

    #vector intervention details for easy reference
    Intervention_ID <- Current_interventions[i, "Intervention_ID"]
    Target_classes <- paste0("Prob_", Current_interventions[[i, "Target_classes"]])
    Intervention_data <- Current_interventions[i, "Intervention_data"]
    
    #if Perc_diff is too small then the effect will likely not be achieved
    #to mitigate use a threshold of minimum probability perturbation
    Prob_perturb_thresh <- as.numeric(Current_interventions[i, "Prob_perturb_threshold"])
    
    #--------------------------------------------------------------------------
    # Urban_densification intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_densification"){

    #load building zone raster
    Intervention_rast <- raster(Intervention_data)
  
    #identify pixels inside of building zones 
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #increase probability to one
    Intersecting[Intersecting > 0] <- 1
    
    #index which cells need to have value updated
    ix <- Intersecting == 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
    
    }#close Urban_densification chunk
    
    
    #--------------------------------------------------------------------------
    #Urban_sprawl intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_sprawl"){

    #load building zone raster
    Intervention_rast <- raster(Intervention_data)
  
    #identify pixels inside of building zones 
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of building zones
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #calculate 90th percentile values of probability for pixels inside vs.outside
    #excluding those with a value of 0
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the 90th percentile
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean
    
    #Average of means
    Average_mean <- (Intersect_percentile_mean + Nonintersect_percentile_mean)/2
    
    #calculate percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #The intended effect of the intervention is to increase the probability of
    #urban development outside the building zone, however depending on the
    #valency of the Perc_diff values this needs to be implemented differently
    
    #If Perc_diff is >0 then increase the probability of instances above the
    #90th percentile for the outside pixels by the percentage difference
    #between the means
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}
      
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      positive_test <- Prob_raster_stack@layers[[Target_classes]]
    
      #else if Perc_diff is <0 then decrease the probability of instances above the
      #90th percentile for the inside pixels by the percentage difference
      #between the means
      }else if(Perc_diff <0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
    
      Intersecting[Intersecting > Intersect_percentile] <- Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting < 0] <- 0
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
      
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      negative_test <- Prob_raster_stack@layers[[Target_classes]]
      } #close else if statement
    
    }#close Urban_sprawl chunk
    
    #--------------------------------------------------------------------------
    # Urban_migration intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Urban_migration"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    #for this intervention: 325, 326, 327, 335, 338
    Leg <- Intervention_rast@data@attributes[[1]]
    Leg[Leg$ID %in% c(325, 326, 327, 335, 338), "type"] <- 1
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of remote rural municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  

    #identify pixels outside of remote rural municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to decrease the 
    #probability of urban development in the remote rural municipalities
    #calculate 90th percentile value of probability for pixels inside
    #and the 80th percentile value for pixels outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.80, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean
    
    #Average of means
    Average_mean <- (Intersect_percentile_mean + Nonintersect_percentile_mean)/2
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is < 0 then decrease the probability of instances above the
    #90th percentile for the pixels in remote rural municipalities by the percentage difference
    #between the means
    if(Perc_diff <0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
      
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
    
      #else if Perc_diff is >0 then increase the probability of instances above the
      #90th percentile for the outside pixels by the percentage difference
      #between the means
      }else if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
    
    }#close Urban_migration chunk
    
    #--------------------------------------------------------------------------
    #Mountain_development intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Mountain_development"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    Leg <- Intervention_rast@data@attributes[[1]]
    
    #For this intervention there are two different specs for scenarios
    #EI_NAT: 314
    #EI_SOC: 314,334
    if(Scenario_ID == "EI_NAT"){
    Leg[Leg$ID == 314, "type"] <- 1
    } else if(Scenario_ID == "EI_SOC"){
    Leg[Leg$ID %in% c(314, 334), "type"] <- 1  
    }
    
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of mountainous remote municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of mountainous remote municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to increase the 
    #probability of urban development in the mountainous municipalities
    
    #calculate 90th percentile value of probability for pixels inside and outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean 
    
    #Average of means
    Average_mean <- (Intersect_percentile_mean + Nonintersect_percentile_mean)/2
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is > 0 then increase the probability of instances above the
    #90th percentile for the pixels in mountainous municipalities by the
    #percentage difference between the means (or the threshold value)
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      #increase the values
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      
    
      #else if Perc_diff is >0 then increase the probability of instances above the
      #90th percentile for the outside pixels by the percentage difference
      #between the means
      }else if(Perc_diff <0){
        
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -c(Prob_perturb_thresh)} 
        
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
    
    }#close Mountain_development chunk

    #--------------------------------------------------------------------------
    # Rural_migration intervention
    #--------------------------------------------------------------------------
    if(Intervention_ID == "Rural_migration"){
    
    #load municipality typology raster
    Intervention_rast <- raster(Intervention_data)
    
    #seperate raster legend and recode values for remote rural municaplities
    Leg <- Intervention_rast@data@attributes[[1]]
    Leg[Leg$ID %in% c(325, 326, 327, 335, 338), "type"] <- 1
    Leg[Leg$type != 1, "type"] <- NA
    Leg$type <- as.numeric(Leg$type)
    
    #reclassify raster
    Intervention_rast <- reclassify(Intervention_rast, rcl = Leg)
    
    #identify pixels inside of remote rural municipalities
    Intersecting <- mask(Prob_raster_stack@layers[[Target_classes]], Intervention_rast == 1)  
    
    #identify pixels outside of remote rural municipalities
    non_intersecting <- overlay(Prob_raster_stack@layers[[Target_classes]],Intervention_rast,fun = function(x, y) {
      x[y==1] <- NA
      return(x)
    })
    
    #Because the intended effect of the intervention is to increase the 
    #probability of urban development in the remote rural municipalities
    
    #calculate 90th percentile value of probability for pixels inside and outside
    #excluding cells with a value of 0 in both cases
    Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
    #get the means of the values above the percentiles
    Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
    Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
    #mean difference
    Mean_diff <- Nonintersect_percentile_mean - Intersect_percentile_mean 
    
    #Average of means
    Average_mean <- (Intersect_percentile_mean + Nonintersect_percentile_mean)/2
    
    #percentage difference
    Perc_diff <- (Mean_diff/Average_mean)*100
    
    #If Perc_diff is > 0 then increase the probability of instances above the
    #90th percentile for the pixels in remote rural municipalities by the
    #percentage difference between the means (or the threshold value)
    if(Perc_diff >0){
      
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
      #increase the values
      Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      Intersecting[Intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- Intersecting > Intersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- Intersecting[ix]
      
    
      #else if Perc_diff is <0 then increase the probability of instances above the
      #90th percentile for the pixels outside the remote rural municipalities
      #by the percentage difference between the means
      }else if(Perc_diff <0){
        
      #check threshold
      if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -c(Prob_perturb_thresh)} 
        
      non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
      #replace any values greater than 1 with 1
      non_intersecting[non_intersecting > 1] <- 1
 
      #index which cells need to have value updated
      ix <- non_intersecting > Nonintersect_percentile
    
      #replace values in target raster
      Prob_raster_stack@layers[[Target_classes]][ix] <- non_intersecting[ix]
      } #close else if statement
  
    }#close Rural_migration chunk
    
    #--------------------------------------------------------------------------
    # Agri_abandonment intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Agri_abandonment"){
      
    #The predicted probability of cells to transition from agriculture to other
    #LULC classes already uses accessibility based predictors such as
    #distance to roads/slope however other variables e.g climaticor soil may be having
    #a larger effect hence we should apply a simple analysis based upon the 
    #model used by Gellrich et al. 2007 that considers distance to roads, 
    #slope and distance to building zones as a measure of 'marginality' and 
    #then select the 90th percentile of pixels according to this value
    
    #load the static predictor layers and re-scale between 0-1
    
    #function for rescaling:
    rescale <- function(x, x.min, x.max, new.min = 0, new.max = 1) {
    if(is.null(x.min)) {x.min = min(x)}
    if(is.null(x.max)) {x.max = max(x)}
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
    }
    
    #distance to roads
    Dist2rds <- raster("Data/Preds/Prepared/Layers/Transport/Distance_to_roads_mean_100m.tif")
    Dist2rds <- calc(Dist2rds, function(x) rescale(x,x.min= minValue(Dist2rds),
                                       x.max = maxValue(Dist2rds)))
    
    #Slope
    Slope <- raster("Data/Preds/Prepared/Layers/Topographic/Slope_mean_100m.tif")
    Slope <- calc(Slope, function(x) rescale(x, x.min= minValue(Slope),
                                       x.max = maxValue(Slope)))
    
    #elevation
    Elev <- raster("Data/Preds/Prepared/Layers/Topographic/Elevation_mean_100m.tif")
    Elev <- calc(Elev, function(x) rescale(x, x.min= minValue(Elev),
                                       x.max = maxValue(Elev)))
    
    # Distance to building zones
    #This layer needs to be inverted when re-scaling because
    #greater distance from building zones means lower land cost 
    #which means less likely to abandon hence x.min and x.max values swapped
    Dist2BZ <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif")
    Dist2BZ <- calc(Dist2BZ, function(x) rescale(x, x.min= maxValue(Dist2BZ),
                                       x.max = minValue(Dist2BZ)))
    
    #stack dist2rds, slope and forest_dist layers and sum values as raster
    Marginality_rast <- calc(stack(Dist2rds, Slope, Elev, Dist2BZ), mean)
    
    #subset the marginality raster to only the pixels of the agricultural
    #land types (Int_AG, Alp_Past)
    Agri_rast <- rasterFromXYZ(Raster_prob_values[,c("x", "y", "Alp_Past")])
    Agri_rast[Agri_rast == 0] <- NA
    Agri_marginality <- mask(Marginality_rast, Agri_rast)

    
    #calculate the upper quartile value of marginality for the agricultural cells
    Marginality_percentile <- quantile(Agri_marginality@data@values, probs = 0.75, na.rm=TRUE)

    #indexes of all cells above/below the upper quartile
    marginal_index <- Agri_marginality > Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    non_marginal_index <- Agri_marginality < Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    
    #loop over target classes gathering probability values for marginal vs. non-marginal cells
    for(class in Target_classes){
    
    #calculate the 90th percentile value of probability of transition to the
    #target class in the marginal agricultural cells vs. non-marginal
    marginal_cells <- Prob_raster_stack@layers[[class]][marginal_index]
    if(length(unique(marginal_cells)) >1){
    marginal_percentile <- quantile(marginal_cells[marginal_cells >0],probs = 0.9, na.rm=TRUE)
    marginal_cells_abv_percentile <- marginal_cells[marginal_cells > marginal_percentile]
    m_ix <- marginal_cells > marginal_percentile
    
    #increase the probability values above the 90th percentile of the marginal cells
    marginal_cells[m_ix] <- marginal_cells[m_ix] + (marginal_cells[m_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    marginal_cells[marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][marginal_index] <- marginal_cells
    } #close if statement
    
    non_marginal_cells <- Prob_raster_stack@layers[[class]][non_marginal_index]
    if(length(unique(non_marginal_cells)) >1){
    nonmarginal_percentile <- quantile(non_marginal_cells[non_marginal_cells >0], probs = 0.9, na.rm=TRUE)
    non_marginal_cells_abv_percentile <- non_marginal_cells[non_marginal_cells > nonmarginal_percentile] 
    nm_ix <- non_marginal_cells > nonmarginal_percentile 
    
    #decrease the probability values above the 90th percentile of the non_marginal cells
    non_marginal_cells[nm_ix] <- non_marginal_cells[nm_ix] - (non_marginal_cells[nm_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    non_marginal_cells[non_marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][non_marginal_index] <- non_marginal_cells
    } #close if statement
    
    } #close loop over target classes
    
    } #close Agri_abandonment chunk
    
    #--------------------------------------------------------------------------
    # Agri_maintenance intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Agri_maintenance"){
    
    #Use the same approach as the Agri_abandonment intervention to identify 
    #most marginal agricultural pixels and decrease their probability of 
    #transitioning to the target classes (i.e. away from agriculture)
    
    #load the static predictor layers and re-scale between 0-1
    
    #function for rescaling:
    rescale <- function(x, x.min, x.max, new.min = 0, new.max = 1) {
    if(is.null(x.min)) {x.min = min(x)}
    if(is.null(x.max)) {x.max = max(x)}
    new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
    }
    
    #distance to roads
    Dist2rds <- raster("Data/Preds/Prepared/Layers/Transport/Distance_to_roads_mean_100m.tif")
    Dist2rds <- calc(Dist2rds, function(x) rescale(x,x.min= minValue(Dist2rds),
                                       x.max = maxValue(Dist2rds)))
    
    #Slope
    Slope <- raster("Data/Preds/Prepared/Layers/Topographic/Slope_mean_100m.tif")
    Slope <- calc(Slope, function(x) rescale(x, x.min= minValue(Slope),
                                       x.max = maxValue(Slope)))
    
    #elevation
    Elev <- raster("Data/Preds/Prepared/Layers/Topographic/Elevation_mean_100m.tif")
    Elev <- calc(Elev, function(x) rescale(x, x.min= minValue(Elev),
                                       x.max = maxValue(Elev)))
    
    # Distance to building zones
    #This layer needs to be inverted when re-scaling because
    #greater distance from building zones means lower land cost 
    #which means less likely to abandon hence x.min and x.max values swapped
    Dist2BZ <- raster("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_distance.tif")
    Dist2BZ <- calc(Dist2BZ, function(x) rescale(x, x.min= maxValue(Dist2BZ),
                                       x.max = minValue(Dist2BZ)))
    
    #stack dist2rds, slope and forest_dist layers and sum values as raster
    Marginality_rast <- calc(stack(Dist2rds, Slope, Elev, Dist2BZ), mean)
    
    #subset the marginality raster to only the pixels of the agricultural
    #land types (Int_AG, Alp_Past)
    Agri_rast <- rasterFromXYZ(Raster_prob_values[,c("x", "y", "Alp_Past")])
    Agri_rast[Agri_rast == 0] <- NA
    Agri_marginality <- mask(Marginality_rast, Agri_rast)

    #calculate the upper quartile value of marginality for the agricultural cells
    Marginality_percentile <- quantile(Agri_marginality@data@values, probs = 0.75, na.rm=TRUE)

    #indexes of all cells above/below the upper quartile
    marginal_index <- Agri_marginality > Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    non_marginal_index <- Agri_marginality < Marginality_percentile
    marginal_index[marginal_index == 0] <- NA
    
    #loop over target classes gathering probability values for marginal vs. non-marginal cells
    for(class in Target_classes){
    
    #calculate the 90th percentile value of probability of transition to the
    #target class in the marginal agricultural cells vs. non-marginal
    marginal_cells <- Prob_raster_stack@layers[[class]][marginal_index]
    non_marginal_cells <- Prob_raster_stack@layers[[class]][non_marginal_index]
    if(length(unique(non_marginal_cells)) >1){
    nonmarginal_percentile <- quantile(non_marginal_cells[non_marginal_cells >0], probs = 0.95, na.rm=TRUE)
    m_ix <- marginal_cells > nonmarginal_percentile
    marginal_cells_abv_percentile <- marginal_cells[marginal_cells > nonmarginal_percentile]
    
    #decrease the probability values above the 90th percentile of the marginal cells
    marginal_cells[m_ix] <- marginal_cells[m_ix] - (marginal_cells[m_ix]/100)*Prob_perturb_thresh
    
    #replace any values greater than 1 with 1
    marginal_cells[marginal_cells > 1] <- 1
    
    #replace values in target raster
    Prob_raster_stack@layers[[class]][marginal_index] <- marginal_cells
    } #close if statement
    
    } #close loop over target classes
    
    } #close Agri_maintenance chunk
    
    #--------------------------------------------------------------------------
    #Protection intervention
    #--------------------------------------------------------------------------
    
    if(Intervention_ID == "Protection"){
    
    #Load intervention raster
    Intervention_rast <- raster(str_replace(Intervention_data, "X", Simulation_time_step))
    
    #loop over target classes
    for(class in Target_classes){
      
      #identify pixels of target class inside of protected areas
      Intersecting <- raster::mask(Prob_raster_stack@layers[[class]], Intervention_rast == 1)
      
      if(Scenario_ID == "EI_NAT" || Scenario_ID== "EI_SOC"){
        
        #decrease probability to zero
        Intersecting[Intersecting > 0] <- 0
    
        #index which cells need to have value updated
        ix <- Intersecting == 0
    
        #replace values in target raster
        Prob_raster_stack@layers[[class]][ix] <- Intersecting[ix]
        
      }else if(Scenario_ID == "EI_CUL"){
        
        #identify pixels outside of PAs
        non_intersecting <- overlay(Prob_raster_stack@layers[[class]],Intervention_rast,fun = function(x, y) {
          x[y==1] <- NA
        return(x)
        })
    
        #calculate 90th percentile values of probability for pixels inside vs.outside
        #excluding those with a value of 0
        
        #if statement for the condition that all pixels of the target class 
        #inside or outside the protected areas have prob values of 0 i.e not possible to calculate a percentile value. 
        if(length(unique(Intersecting))>1 & length(unique(non_intersecting)) >1){
          Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
          Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values >0], probs = 0.90, na.rm=TRUE)
    
          #get the means of the values above the 90th percentile
          Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
          Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)
    
          #mean difference
          Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean
    
          #Average of means
          Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)
    
          #calculate percentage difference
          Perc_diff <- (Mean_diff/Average_mean)*100
        
          #The goal is to reduce the likelihood of non-natural land transitions
          #inside protected areas. If Perc_diff is < 0 then this already
          #implies that the average probability of transition inside the PAs
          #is less than outside but we still want to decrease cellular probability
          #in general. Hence, decrease the probability of instances above the 
          #90th percentile for the pixels in PAs by the % difference
          #between the means unless the % diff between the means is less than 
          #the specificed threshold value
          if(Perc_diff <0){
      
            #check threshold
            if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- -(Prob_perturb_thresh)}
      
            Intersecting[Intersecting > Intersect_percentile] <-Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile]/100)*Perc_diff
    
            #replace any values greater than 1 with 1
            Intersecting[Intersecting > 1] <- 1
 
            #index which cells need to have value updated
            ix <- Intersecting > Intersect_percentile
    
            #replace values in target raster
            Prob_raster_stack@layers[[class]][ix] <- Intersecting[ix]
      
            #else if Perc_diff is >0 then increase the probability of instances 
            #above the 90th percentile for the pixels outside of the PAs by the
            #% difference between the means unless the % diff between the means
            #is less than the specificed threshold value
            }else if(Perc_diff >0){
      
            #check threshold
            if(abs(Perc_diff) < Prob_perturb_thresh){Perc_diff <- Prob_perturb_thresh}  
      
            non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile]/100)*Perc_diff
    
            #replace any values greater than 1 with 1
            non_intersecting[non_intersecting > 1] <- 1
 
            #index which cells need to have value updated
            ix <- non_intersecting > Nonintersect_percentile
    
            #replace values in target raster
            Prob_raster_stack@layers[[class]][ix] <- non_intersecting[ix]
            } #close if statement for Perc_diff >0
          } #close if statement for non-unique intersecting/non-intersecting pixel values
        } #close if statement fro EI_CUL scenario
    
      } #close loop over target classes
    } #close Protection chunk
    
  } #close loop over interventions
  
  #convert raster stack back to dataframe
  Prob_df <- raster::as.data.frame(Prob_raster_stack)
    
  #replace the original predictions with the manipulated values
  Raster_prob_values[,names(Prob_df)] <- Prob_df
  
  #subset to only the prediction and spatial info cols
  #Raster_prob_values <- Raster_prob_values[,c("ID", "x", "y", Pred_prob_columns)]
  } #close if statement
  
return(Raster_prob_values)
} # close function
