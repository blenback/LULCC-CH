#' Identify patch solution sets based on prioritization and expansion target,
#' rank them according to patch  median patch priority value and size, finally
#' subset to the specified upper quantile of solutions.
#' @param Ca_prioritization Character string indicating the prioritization method.
#' @param Ca_expansion_target Named numeric vector indicating the target area for conservation area expansion.
#' @param Patch_solutions_dir Character string indicating the directory for temporary files.
#' @param Recalc_results Logical indicating whether to recalculate results.
#' @param Prio_patch_stats Data frame containing statistics of priority patches.
#' @param Use_parallel Logical indicating whether to use parallel processing.
#' @param n_cores Integer indicating the number of cores to use for parallel processing.
#' @param Solution_quantile Numeric value indicating the quantile to subset solutions.
#' @return A list containing the paths of the objects containing the ranked solutions and their statistics.
#' 
identify_patch_solution_sets <- function(
  Ca_prioritization = Ca_prioritization,
  Ca_expansion_target = Ca_expansion_target,
  area_target_n_cells = area_target_n_cells,
  Patch_solutions_dir = file.path(Patch_solutions_dir, "Ranked_solutions"),
  Recalc_results = Recalc_results,
  Prio_patch_stats_path = Priority_patches_paths$Prio_patch_stats_path,
  Use_parallel = Use_parallel,
  n_cores = n_cores,
  Solution_quantile = Solution_quantile
){
  
  #Print start message
  cat(paste0("Identifying solutions under prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "\n"))
  
  # remove % from Ca_expansion_target and convert to numeric
  Ca_expansion_target <- as.numeric(gsub("%", "", Ca_expansion_target))
  
  #create dir
  Solutions_dir <- file.path(Patch_solutions_dir, Ca_prioritization, Ca_expansion_target)
  dir.create(Solutions_dir, showWarnings = FALSE, recursive = TRUE)
  
  #load Prio_patch_stats
  Prio_patch_stats <- readRDS(Prio_patch_stats_path)
  
  # create save path
  Solutions_path <- paste0(Solutions_dir, "/Solutions_", Ca_prioritization, "_", Ca_expansion_target, ".rds")
  Solutions_upperQ_path <- file.path(Solutions_dir, paste0("Solutions_upperQ_", Ca_prioritization, "_", Ca_expansion_target, ".rds"))
  Solutions_upperQ_stats_path <- file.path(Solutions_dir, paste0("Solutions_upperQ_stats_", Ca_prioritization, "_", Ca_expansion_target, ".rds"))
  
  
  # if recalc_results = FALSE then check for existing solutions file 
  if(!Recalc_results){
    if(file.exists(Solutions_path) && file.exists(Solutions_upperQ_path) && file.exists(Solutions_upperQ_stats_path)){
      # return the paths to the ranked solution objects
      return(list(
        Solutions_upperQ_path = Solutions_upperQ_path,
        Solutions_upperQ_stats_path = Solutions_upperQ_stats_path
        ))
      cat(paste0("Solutions for prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "% have been identified and loaded from file\n"))
    } else {
      message("Recalc_results == FALSE, however no existing results found, recalculating...")
    }
  }
  
  #run function to generate solutions that meet coverage target
  Solutions <- find_patch_sets_for_expansion_target(xy = Prio_patch_stats,
                                                    sfind = area_target_n_cells,,
                                                    nmax = 10000)
  
  
  #print message saying that solutions for this prioritization and coverage target have been identified
  cat(paste0("Solutions for prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "% have been identified\n"))
  
  #remove all elements from solutions except patch_ids
  Solutions <- list(patch_ids = Solutions$patch_ids)
  
  #save solutions
  saveRDS(Solutions, Solutions_path)
  cat(paste0("Solutions for prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "% have been saved to file\n"))
  
  #if Use_parallel is TRUE then use future_sapply to calculate the sum of median patch priority * patch size
  if(Use_parallel){
    plan(multisession, workers = n_cores)
  } else {
    plan(sequential)
  }
  
  cat(paste0("Calculating the sum of median patch priority * patch size for each solution under prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "%\n"))
  
  #Loop over solutions to calculate the sum of median patch priority * patch size
  Solution_prio <- future_sapply(1:length(Solutions$patch_ids), function(x){
    
    #subset patch_stats if the ids are in the solution
    idx <- which(Prio_patch_stats$patch_id %in% Solutions$patch_ids[[x]])
    
    #calculate sum of median patch priority * patch size for solution
    out <- sum(Prio_patch_stats$median[idx] * Prio_patch_stats$num_cells[idx])
  })
  names(Solution_prio) <- 1:length(Solutions$patch_ids)
  
  #sort results according to the sum of median patch priority * patch size
  Ranked_solution_prio <- sort(Solution_prio, decreasing = TRUE)
  
  #Calculate upper 5% quartile of solutions by patch_prio_sum
  upperQ <- quantile(Ranked_solution_prio, probs = Solution_quantile)
  
  #subset to upper 5% quartile of solutions by patch_prio_sum
  Solutions_prio_upperQ <- Ranked_solution_prio[Ranked_solution_prio >= upperQ]
  
  #convert to data.frame with names as 1st column and values in 2nd
  Solutions_prio_upperQ <- data.frame(Solution_num = as.numeric(names(Solutions_prio_upperQ)), patch_prio_sum = as.numeric(Solutions_prio_upperQ))
  
  #subset Solutions to the upper 5% quantile
  Solutions_subset <- Solutions$patch_ids[Solutions_prio_upperQ$Solution_num]
  names(Solutions_subset) <- Solutions_prio_upperQ$Solution_num
  
  #save upper quartile solution info
  saveRDS(Solutions_prio_upperQ, Solutions_upperQ_stats_path)
    
  #save subset of solutions
  saveRDS(Solutions_subset, Solutions_upperQ_path)
  
  #print message to say that solutions for this prioritization approach and coverage
  #have been ranked and subset to the specified quantile
  cat(paste0("Solutions for prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "% have been ranked and subset to the solution quantile of:", Solution_quantile, "\n"))
  
  #return the solutions
  return(list(
    Solutions_upperQ_path = Solutions_upperQ_path,
    Solutions_upperQ_stats_path = Solutions_upperQ_stats_path
  ))
  
}
