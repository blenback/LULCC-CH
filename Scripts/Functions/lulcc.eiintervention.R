#############################################################################
## lulcc.eiintervention: function to manipulate spatial transition 
##probabilities to represent interventions/policies/management activities 
## Date: 9-02-2024
## Author: Ben Black
#############################################################################
#'
#' @param Intervention_table_path Chr, path to the intervention table
#' @param EI_ID Chr, string to match with EI intervention details in intervention table
#' @param Raster_prob_values Dataframe, predicted cellular transitions 
#'                            probabilities with x,y and cell ID cols
#' @param Simulation_time_step Chr, current simultion time step
#'
#' @author Ben Black
#' @export

# #testing values
# Intervention_table_path <- "Tools/EI_interventions.csv"
# EI_ID <- "1"
# Simulation_time_step <- "2020"
# Raster_prob_values <- readRDS("Data/Exemplar_data/EXP_raster_prob_values.rds")
# Intervention_Effect <- "Restoration"


lulcc.eiintervention <- function(Intervention_table_path,
                                 EI_ID,
                                 Raster_prob_values,
                                 Simulation_time_step) {

  #load table of scenario interventions
  Interventions <- read.csv(Intervention_table_path)

  #subset to EI ID
  Intervention <- Interventions[Interventions$EI_ID == as.numeric(EI_ID),]

  #convert Time_step and Int_target_classes columns back to character vectors
  Int_time_steps <- strsplit(gsub(" ", "", Intervention$Time_step, fixed = TRUE), ",")[[1]]
  Int_target_classes <- strsplit(gsub(" ", "", Intervention$Target_classes, fixed = TRUE), ",")[[1]]

  #paste 'Prob_' to all classes to match column names in Raster_prob_values
  Int_target_classes <- paste0("Prob_", Int_target_classes)

  #if Simulation time step is valid for intervention perform manipulation of transition probabilities
  if (Simulation_time_step %in% Int_time_steps) {

    #vector names of columns of probability predictions (matching on Prob_)
    Pred_prob_columns <- grep("Prob_", names(Raster_prob_values), value = TRUE)

    #convert probability table to raster stack
    Prob_raster_stack <- stack(lapply(Pred_prob_columns, function(x) rasterFromXYZ(Raster_prob_values[, c("x", "y", x)])))
    names(Prob_raster_stack@layers) <- Pred_prob_columns

    #vector intervention details for easy reference
    Intervention_Effect <- Intervention$Effect

    #Vector scenario ID
    Scenario_ID <- Intervention$Scenario_ID

    #The threshold of minimum probability perturbation ensure that if the calculated percentage difference 
    #between cells inside/outside the EI area is too small then an effect will still take place using this value
    Prob_perturb_thresh <- as.numeric(Intervention$Prob_perturb_thresh)

    #create path for intverntion rast layer
    Intervention_data <- paste0("Data/Spat_prob_perturb_layers/Protected_areas/Future_PAs", Scenario_ID, "_PAs_", Simulation_time_step, ".tif")

    #Load intervention raster
    Intervention_rast <- raster(Intervention_data)

    #--------------------------------------------------------------------------
    # Protection effect: Reducing conversion to non-natural land uses inside EI areas
    #--------------------------------------------------------------------------

    if (Intervention_Effect == "Preservation") {

      #loop over target classes
      for (class in Int_target_classes) {

        #identify pixels of target class inside of EI areas
        Intersecting <- raster::mask(Prob_raster_stack@layers[[class]], Intervention_rast == 1)

        #for the EI Nature and EI society scenarios the management of protected areas is strict and
        #hence probability of target classes is reduced to 0 inside the EI areas
        if (Scenario_ID == "EINAT" || Scenario_ID == "EISOC") {

          #decrease probability to zero
          Intersecting[Intersecting > 0] <- 0

          #index which cells need to have value updated
          ix <- Intersecting == 0

          #replace values in target raster
          Prob_raster_stack@layers[[class]][ix] <- Intersecting[ix]


          #for the EI_CUL and BAU scenarios the management of protected areas is less strict and
          #probability is reduced relative the probability of the target class pixels outside the EI areas
        }else if (Scenario_ID == "EICUL" || Scenario_ID == "BAU") {

          #identify pixels outside of EI areas
          non_intersecting <- overlay(Prob_raster_stack@layers[[class]], Intervention_rast, fun = function(x, y) {
            x[y == 1] <- NA
            return(x)
          })

          #calculate 90th percentile values of probability for pixels inside vs.outside
          #excluding those with a value of 0

          #if statement for the condition that all pixels of the target class 
          #inside or outside the protected areas have prob values of 0 i.e not possible to calculate a percentile value. 
          if (length(unique(Intersecting)) > 1 & length(unique(non_intersecting)) > 1) {
            Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values > 0], probs = 0.90, na.rm = TRUE)
            Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values > 0], probs = 0.90, na.rm = TRUE)

            #get the means of the values above the 90th percentile
            Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
            Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)

            #mean difference
            Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean

            #Average of means
            Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)

            #calculate percentage difference
            Perc_diff <- (Mean_diff / Average_mean) * 100

            #The goal is to reduce the likelihood of non-natural land transitions
            #inside protected areas. If Perc_diff is < 0 then this already
            #implies that the average probability of transition inside the PAs
            #is less than outside but we still want to decrease cellular probability
            #in general. Hence, decrease the probability of instances above the 
            #90th percentile for the pixels in PAs by the % difference
            #between the means unless the % diff between the means is less than 
            #the specificed threshold value
            if (Perc_diff < 0) {

              #check threshold
              if (abs(Perc_diff) < Prob_perturb_thresh) { Perc_diff <- -(Prob_perturb_thresh) }

              Intersecting[Intersecting > Intersect_percentile] <- Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile] / 100) * Perc_diff

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
            }else if (Perc_diff > 0) {

              #check threshold
              if (abs(Perc_diff) < Prob_perturb_thresh) { Perc_diff <- Prob_perturb_thresh }

              non_intersecting[non_intersecting > Nonintersect_percentile] <- non_intersecting[non_intersecting > Nonintersect_percentile] + (non_intersecting[non_intersecting > Nonintersect_percentile] / 100) * Perc_diff

              #replace any values greater than 1 with 1
              non_intersecting[non_intersecting > 1] <- 1

              #index which cells need to have value updated
              ix <- non_intersecting > Nonintersect_percentile

              #replace values in target raster
              Prob_raster_stack@layers[[class]][ix] <- non_intersecting[ix]
            } #close if statement for Perc_diff >0
          } #close if statement for non-unique intersecting/non-intersecting pixel values
        } #close if statement for EI_CUL/BAU scenarios

      } #close loop over target classes
    } #close Preservation chunk

    #--------------------------------------------------------------------------
    # Resotration effect: Increase conversion to natural land uses inside EI areas
    #--------------------------------------------------------------------------

    if (Intervention_Effect == "Restoration") {

      #loop over target classes
      for (class in Int_target_classes) {

        #identify pixels of target class inside of EI areas
        Intersecting <- raster::mask(Prob_raster_stack@layers[[class]], Intervention_rast == 1)

        #identify pixels outside of EI areas
        non_intersecting <- overlay(Prob_raster_stack@layers[[class]], Intervention_rast, fun = function(x, y) {
          x[y == 1] <- NA
          return(x)
        })

        # The goal is to increase the probability of transition inside the EI 
        #and hence to do this we need to know how much to increase the
        #probability to ensure that cells are selected over those in non-EI areas
        #hence we need to calculate 90th percentile values of probability for
        #pixels inside vs.outside EI areas (excluding those with a value of 0)
        # and use these calculate the percentage difference between the
        # mean probability of instances above the 90th percentile inside and outside. 

        #if statement for the condition that all pixels of the target class 
        #inside or outside the protected areas have prob values of 0 i.e not possible to calculate a percentile value. 
        if (length(unique(Intersecting)) > 1 & length(unique(non_intersecting)) > 1) {
          Intersect_percentile <- quantile(Intersecting@data@values[Intersecting@data@values > 0], probs = 0.90, na.rm = TRUE)
          Nonintersect_percentile <- quantile(non_intersecting@data@values[non_intersecting@data@values > 0], probs = 0.90, na.rm = TRUE)

          #get the means of the values above the 90th percentile
          Intersect_percentile_mean <- mean(Intersecting@data@values[Intersecting@data@values >= Intersect_percentile], na.rm = TRUE)
          Nonintersect_percentile_mean <- mean(non_intersecting@data@values[non_intersecting@data@values >= Nonintersect_percentile], na.rm = TRUE)

          #mean difference
          Mean_diff <- Intersect_percentile_mean - Nonintersect_percentile_mean

          #Average of means
          Average_mean <- mean(Intersect_percentile_mean, Nonintersect_percentile_mean)

          #calculate percentage difference
          Perc_diff <- abs((Mean_diff / Average_mean) * 100)

          #The goal is to increase the likelihood of non-natural -> natural land transitions
          #inside protected areas, hence we need to do increase the probability of instances above the
          #90th percentile for the pixels inside the EI area by the percentage difference
          #between the means. If the the % diff between the means is less than
          #the specificed threshold value than we should use the threshold value
          #instead otherwise the effect likely will not result in any differences.

          #check threshold if Perc_diff is less than the threshold then set it to the threshold value
          if (Perc_diff < Prob_perturb_thresh) { Perc_diff <- Prob_perturb_thresh }

          #increase the probability of instances above the 90th percentile for the pixels inside EI areas by Perc_diff
          Intersecting[Intersecting > Intersect_percentile] <- Intersecting[Intersecting > Intersect_percentile] + (Intersecting[Intersecting > Intersect_percentile] / 100) * Perc_diff

          #replace any values greater than 1 with 1
          Intersecting[Intersecting > 1] <- 1

          #index which cells need to have value updated
          ix <- Intersecting > Intersect_percentile

          #replace values in target raster
          Prob_raster_stack@layers[[class]][ix] <- Intersecting[ix]
        } #close if statement for non-unique intersecting/non-intersecting pixel values

      } #close loop over target classes
    } #close Restoration chunk

    #convert raster stack back to dataframe
    Prob_df <- raster::as.data.frame(Prob_raster_stack)

    #replace the original predictions with the manipulated values
    Raster_prob_values[, names(Prob_df)] <- Prob_df

  } #close conditional statement performing spatial manipulation

  return(Raster_prob_values)

}

# close function
  