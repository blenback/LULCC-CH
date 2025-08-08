#' @title Perform Pre-allocation Interventions
#' @description This function applies pre-allocation interventions to simulation parameters based on specified scenarios.
#' @param scenario_IDs A vector of scenario IDs for which to apply pre-allocation interventions.
#' @param Simulation_param_dir The directory containing simulation parameters for each scenario.
#' @param LULC_aggregation_path The path to the LULC aggregation scheme file.
#' @param interventions_dir The directory containing intervention YAML files.
perform_pre_allocation_interventions <- function(
  scenario_IDs,
  Simulation_param_dir,
  LULC_aggregation_path,
  interventions_dir
){
  
# load the LULC aggregation scheme
LULC_agg <- openxlsx::read.xlsx(LULC_aggregation_path)
  
# Loop over scenario IDs
for(scenario_ID in scenario_IDs){
  
  # Print the scenario ID being processed
  cat(paste("Processing pre-allocation interventions for scenario:", scenario_ID, "\n"))
  # Load interventions for scenario from YAML file
  Interventions <- yaml.load_file(file.path(interventions_dir, paste0(scenario_ID, "_interventions.yml")))
    
  # filter to Intervention_stage == Allocation
  Pre_allocation_interventions <- Interventions[sapply(Interventions, function(x) x[["Intervention_stage"]] == "Pre-allocation")]
  
  
  # If no pre-allocation interventions are found, skip to the next scenario
  if (length(Pre_allocation_interventions) == 0) {
    warning(paste("No pre-allocation interventions found for scenario:", scenario_ID))
    next
  } else {
    
    cat(paste("Found", length(Pre_allocation_interventions), "pre-allocation interventions for scenario", scenario_ID, "\n"))
    
    # list all allocation_parameter files for this scenario
    scenario_param_files <- list.files(file.path(Simulation_param_dir, scenario_ID), full.names = TRUE)
    
    # loop over pre-allocation interventions
    for(intervention in Pre_allocation_interventions) {
      
      # get the LULC class values for the intervention using the LULC_agg table
      Target_classes <- sapply(intervention$Transition_target_classes, function(x) {
        # get the aggregation class for the LULC_to
        unique(LULC_agg[LULC_agg$Class_abbreviation == x, "Aggregated_ID"])
      })
      
      # subset the list of allocation param files for this intervention based on intervention$Time_steps_implemented
      Intervention_param_files <- scenario_param_files[grep(paste(intervention$Time_steps_implemented, collapse = "|"), scenario_param_files)]
      
      # loop over the files, modifying them and saving
      for(param_file in Intervention_param_files){
        
        # load the parameter table
        Param_table <- read.csv(param_file)
        
        # adjust column names
        colnames(Param_table) <- c("From*", "To*", "Mean_Patch_Size", "Patch_Size_Variance", "Patch_Isometry", "Perc_expander", "Perc_patcher")
        
        # alter rows for Target_classes
        
        # if intervention$Param_adjust_name is one of the two special cases
        # Perc_expander and Perc_patcher, then we need to handle them differently
          
        # if intervention$Param_adjust_name != "Perc_expander", "Perc_patcher"
        # then we cna simply replace the value according to it's name
        if(intervention$Param_adjust_name %in% c("Perc_expander", "Perc_patcher")){
          
          # get the opposite param based on name
          opposite_param <- ifelse(intervention$Param_adjust_name == "Perc_expander", "Perc_patcher", "Perc_expander")
          
          # if intervention$Param_adjust_type is "Absolute", then we set the value directly using the Param_adjust_value
          if(intervention$Param_adjust_type == "Absolute"){
            
            # calculate what the opposite value should be
            opposite_value <- 1 - intervention$Param_adjust_value
          
            # set the param value to intervention$Param_adjust_value for the Target_classes
            Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] <- intervention$Param_adjust_value
          
            # set the opposite param value for Transition_target_classes
            Param_table[Param_table$`To*` %in% Target_classes, opposite_param] <- opposite_value
            
          } else if(intervention$Param_adjust_type == "Relative") {
            # if intervention$Param_adjust_type is "Relative", then alter the value as a % of the current value
            Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] <- 
              Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] * 
              (1 + intervention$Param_adjust_value)
            
            # calculate what the opposite value should be
            opposite_value <- 1 - (Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name])
            
            # set the opposite param value for Transition_target_classes
            Param_table[Param_table$`To*` %in% Target_classes, opposite_param] <- opposite_value
          }
          
        } else if(intervention$Param_adjust_name %in% c("Mean_Patch_Size", "Patch_Size_Variance", "Patch_Isometry")){
          
             # if intervention$Param_adjust_type is "Absolute", then we set the value directly using the Param_adjust_value
             if(intervention$Param_adjust_type == "Absolute"){
              Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] <- intervention$Param_adjust_value
             } else if(intervention$Param_adjust_type == "Relative") {
               # if intervention$Param_adjust_type is "Relative", then alter the value as a % of the current value
               Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] <- 
                 Param_table[Param_table$`To*` %in% Target_classes, intervention$Param_adjust_name] * 
                 (1 + intervention$Param_adjust_value)
             }
          
        }

        
        # save the modified parameter table
        write_csv(Param_table, file = param_file)
        
      } # close loop over parameter files
      
    } # close loop over interventions
    

    
  } # close else statement
  
} # close loop over scenario IDs

cat("Pre-allocation interventions processing completed.\n")
} # close function

perform_pre_allocation_interventions(
  scenario_IDs = c("SSP0", "SSP1", "SSP3", "SSP4", "SSP5"), # Example scenario IDs
  Simulation_param_dir = "Data/Allocation_parameters/Simulation", # Path to simulation parameters directory
  LULC_aggregation_path = "Tools/LULC_class_aggregation.xlsx", # Path to LULC aggregation scheme
  interventions_dir = "Tools" # Path to interventions directory
)
