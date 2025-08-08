#' Identify Conservation Areas under Areal expansion Target and desired patch preference
#' @description
#' This function identifies conservation areas (CAs) based on a specified 
#' prioritization map, target for areal expansion and preference for patch configuration.
#' This is a wrapper function that calls other functions to identify priority patches,
#' generate patch solution sets, and select the optimal patch set based on the configuration.
#' @param Prio_rast_path Path to the raster file containing the prioritization map.
#' @param CA_rast_path Path to the raster file containing the protected area map.
#' @param expansion_masks List of SpatRaster objects to mask the prioritization map.
#' @param area_target Named numeric vector indicating the target area for conservation area expansion.
#' @param Ca_prioritization Character string indicating the prioritization method.
#' @param Ca_expansion_target Named numeric vector indicating the target area for conservation area expansion.
#' @param Ca_patch_preference Character string indicating the preference for patch configuration.
#' @param Save_results Logical indicating whether to save the results.
#' @param Recalc_results Logical indicating whether to recalculate the results.
#' @param Scratch_dir Character string indicating the directory for temporary files.
#' @param Use_parallel Logical indicating whether to use parallel processing.
#' @param n_cores Integer indicating the number of cores to use for parallel processing.
#' @param intervention_mask_dir Character string indicating the directory for intervention masks.
#' @param expansion_masks  
#' @return A list containing a raster of the optimal patches and a dataframe of their statistics.
identify_CAs_by_target_and_configuration <- function(
  Prio_rast_path = Prio_rast_path,
  CA_rast_path = CA_rast_path,
  expansion_masks,
  area_target = area_target,
  Ca_prioritization = Ca_prioritization,
  Ca_expansion_target = Ca_expansion_target,
  Ca_patch_preference = Ca_patch_preference,
  Save_results = TRUE,
  Recalc_results = FALSE,
  Scratch_dir = Scratch_dir,
  Use_parallel = Use_parallel,
  n_cores = n_cores, 
  Solution_quantile = 0.95
){
  
  # Apply function to identify patches according to their priority
  Priority_patches <- identify_priority_patches(
    Prio_rast_path = Prio_rast_path,
    CA_rast_path = CA_rast_path,
    expansion_masks = list(settlement, road, railway),
    area_target = area_target,
    Scratch_dir = file.path(Scratch_dir, "Priority_patches"),
    Ca_prioritization = Ca_prioritization,
    Save_results = Save_results,
    Recalc_results = Recalc_results
    )
  
  # Apply function to generate patch solution sets based on the identified priority patches
  # rank them according to the sum of median patch priority * patch size and subset to only those 
  # above an upper quantile value
  Ranked_solution_sets <- identify_patch_solution_sets(
    Ca_prioritization = Ca_prioritization,
    Ca_expansion_target = Ca_expansion_target,
    Scratch_dir = file.path(Scratch_dir, "Priority_solutions"),
    Recalc_results = Recalc_results,
    Prio_patch_stats = Prio_patch_stats,
    Use_parallel = Use_parallel,
    n_cores = n_cores,
    Solution_quantile = Solution_quantile
    )
  
  # Apply function to select optimal patch set based on patch configuration option
  Optimal_patches <- select_optimal_patch_set_for_configuration(
    Use_parallel = Use_parallel,
    n_cores = n_cores,
    CA_rast_path = CA_rast_path,
    Ca_expansion_target = Ca_expansion_target,
    Ca_prioritization = Ca_prioritization,
    Ca_patch_preference = Ca_patch_preference,
    Prio_patch_stats = Priority_patches$Prio_patch_stats,
    Prio_patches = Priority_patches$Prio_patches,
    Scratch_dir = file.path(Scratch_dir, "Optimal_solutions"),
    RecalConfig_results = FALSE,
    Save_results = TRUE,
    Solution_quantile = 0.95,
    Solutions = Ranked_solution_sets$Solutions_upperQ,
    Solution_stats = Ranked_solution_sets$Solutions_upperQ_stats
    )

  if(Save_results){
    #create dir
    Config_dir <- file.path(Scratch_dir, "Intervention_patches", Intervention_ID, Ca_prioritization, names(Ca_expansion_target), Ca_patch_preference)
    dir.create(Config_dir, showWarnings = FALSE, recursive = TRUE)
    
    #write raster
    writeRaster(Optimal_patches$Config_rast, file= file.path(Config_dir, paste0(Intervention_ID, "_", Ca_prioritization,"_", names(Ca_expansion_target), Ca_patch_preference ,".tif")), overwrite=TRUE)
    
    #save patch stats as rds
    saveRDS(Optimal_patches$Config_patch_stats, file.path(Config_dir, paste0(Intervention_ID, "_", Ca_prioritization,"_", names(Ca_expansion_target), Ca_patch_preference ,"_patch_stats.rds")))
  }

  #return the optimal patches
  return(Optimal_patches)
  
}