
select_optimal_patch_set_for_configuration <- function(
  Use_parallel = Use_parallel,
  n_cores = n_cores,
  CA_rast_path = CA_rast_path,
  Ca_expansion_target,
  Ca_prioritization,
  Ca_patch_preference,
  Prio_patch_stats_path = Priority_patches_paths$Prio_patch_stats_path,
  Prio_patches_path = Priority_patches_paths$Prio_patches_path,
  Optimal_solutions_dir = file.path(Optimal_solutions_dir, "Optimal_solutions"),
  Recalc_results = FALSE,
  Solutions = Ranked_solution_paths$Solutions_upperQ_path,
  Solution_stats = Ranked_solution_paths$Solutions_upperQ_stats_path
  ){
  
  # if Use_parallel == TRUE set options for Use_parallel processing with future package.skeleton() 
  if(Use_parallel == TRUE){
  
    #detect non-exportable objects
    options(future.globals.onReference = "warning")

    #set max allowed size of globals
    options(future.globals.maxSize = 20000 * 1024^2)
    
    #use Use_parallel for larger solution sets
    plan(multisession, workers = n_cores)
  } else {
    #use sequential for smaller solution sets
    plan(sequential)
  }

  #Print start message
  cat(paste0("Identifying optimal solutions under prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "%, for the configuration options: ", paste(Ca_patch_preference, collapse = ", "), "\n"))
  
  #load the ranked solutions
  Solutions <- readRDS(Solutions)
  Solution_stats <- readRDS(Solution_stats)
  
  # load the priority patch stats
  Prio_patch_stats <- readRDS(Prio_patch_stats_path)
  
  # create save dir and save path
  Solution_eval_dir <- file.path(Optimal_solutions_dir, Ca_prioritization, Ca_expansion_target)
  dir.create(Solution_eval_dir, showWarnings = FALSE, recursive = TRUE)
  Solution_eval_path <- file.path(Solution_eval_dir, paste0("Solutions_eval_", Ca_prioritization, "_", Ca_expansion_target, "_", Ca_patch_preference, ".rds"))
  
  # if Recalc_results = FALSE then check for existing solutions file
  if(!Recalc_results & file.exists(Solution_eval_path)){
    #if it exists, read it in and return
    Solutions_eval <- readRDS(Solution_eval_path)
    cat(paste0("Solutions for prioritization: ", Ca_prioritization, ", under the target area: ", Ca_expansion_target, "%, for the configuration options: ", paste(Ca_patch_preference, collapse = ", "), " have been identified and loaded from file\n"))
  } else if (Recalc_results == TRUE | !(file.exists(Solution_eval_path))){
      
    #loop over ranked solutions and identify the best according to the different configuration options
    Solution_eval <- future_lapply(Solution_stats$Solution_num, function(k){
    
      # reload the patches 
      Prio_patches <- rast(Prio_patches_path)
    
      #reload the initial PAs for this scenario
      CA_map <- rast(CA_rast_path)
    
      #replace NA values in current PA layer for 0
      CA_map <- ifel(is.na(CA_map), 0, 1)
    
      #empty list for output
      output <- list()
    
      #get patch_IDs
      Patch_IDs <- Solutions[[as.character(k)]]
    
      #create spatial layer of patches
      Solution_patches <-  ifel(Prio_patches %in% Patch_IDs, Prio_patches, NaN)
    
      #set NAs to 0
      Solution_patches <- ifel(is.na(Solution_patches), 0, Solution_patches)
    
      #set all values greater than 0 to 1
      Solution_patches <- ifel(Solution_patches > 0, 1, 0)
    
      #combine with existing PAs
      PAs_plus_patches <- Solution_patches + CA_map
    
      #set 0's to NA
      PAs_plus_patches <- ifel(PAs_plus_patches == 0, NA, 1)
    
      #Chunk for Large/small patches
      if("Large_patches" %in% Ca_patch_preference || "Small_patches" %in% Ca_patch_preference){
      
        #calculate average patch size with landscapemetrics::lsm_l_patch_size
        average_patch_size <- landscapemetrics::lsm_l_area_mn(PAs_plus_patches, directions = 8)[["value"]]
      
        #calculate number of patches with landscapemetrics::lsm_l_np
        num_patches <- landscapemetrics::lsm_l_np(PAs_plus_patches, directions = 8)[["value"]]
      
        #add to output
        output$Average_patch_size <- average_patch_size
        output$Num_patches <- num_patches
      }
    
      #Chunk for connectivity
      if("Connectivity" %in% Ca_patch_preference){
      
        #calculate cohesion index with landscapemetrics::lsm_l_cohesion
        Connectivity <- landscapemetrics::lsm_l_cohesion(landscape = PAs_plus_patches, directions = 8)[["value"]]
      
        #add to output
        output$Connectivity <- Connectivity
      }
      return(output)
    
    }) #close loop over ranked solutions
  
    #name according to solution number
    names(Solution_eval) <- Solution_stats$Solution_num
  
    #rbindlist
    Solutions_eval <- as.data.frame(rbindlist(Solution_eval, idcol = "Solution_num"))
  
    # save result
    saveRDS(Solutions_eval, Solution_eval_path)

  }
  
  
  #Identify the best solution under each config option
  
  # Load Priority patches
  Prio_patches <- rast(Prio_patches_path)
  
  #Large patch solution: highest average patch size and lowest number of patches
  if(Ca_patch_preference == "Large_patches"){
    
    # scale Solutions_eval$num_patches to be between 0 and 1 with order reversed
    Solutions_eval$Num_patches_Desc <- 1- (Solutions_eval$Num_patches - min(Solutions_eval$Num_patches)) / (max(Solutions_eval$Num_patches) - min(Solutions_eval$Num_patches))
    
    #scale Solutions_eval$Average_patch_size to be between 0 and 1
    Solutions_eval$Average_patch_size_Asc <- (Solutions_eval$Average_patch_size - min(Solutions_eval$Average_patch_size)) / (max(Solutions_eval$Average_patch_size) - min(Solutions_eval$Average_patch_size))
    
    #combine the rescaled values
    Solutions_eval$Large_patch_score <- (Solutions_eval$Num_patches_Desc + Solutions_eval$Average_patch_size_Asc) / 2
    
    #get solution number with largest average patch size
    Config_solution <- Solutions_eval[which.max(Solutions_eval$Large_patch_score), "Solution_num"]
    
    #get patch_IDs
    Config_patch_IDs <- Solutions[[Config_solution]]
  }
  
  #small patch solution: Smallest average patch size and highest number of patches
  if(Ca_patch_preference == "Small_patches"){
    
    #scale Solutions_eval$num_patches to be between 0 and 1
    Solutions_eval$Num_patches_Asc <- (Solutions_eval$Num_patches - min(Solutions_eval$Num_patches)) / (max(Solutions_eval$Num_patches) - min(Solutions_eval$Num_patches))
    
    #scale Solutions_eval$Average_patch_size to be between 0 and 1 but reversed
    Solutions_eval$Average_patch_size_Desc <- 1 - (Solutions_eval$Average_patch_size - min(Solutions_eval$Average_patch_size)) / (max(Solutions_eval$Average_patch_size) - min(Solutions_eval$Average_patch_size))
    
    #combine the rescaled values
    Solutions_eval$Small_patch_score <- (Solutions_eval$Num_patches_Asc + Solutions_eval$Average_patch_size_Desc) / 2
    
    #get solution number with highest Small_patch_score
    Config_solution <- Solutions_eval[which.max(Solutions_eval$Small_patch_score), "Solution_num"]
    
    #get patch_IDs
    Config_patch_IDs <- Solutions[[Config_solution]]
    
    #get patch stats
    Config_patch_stats <- Prio_patch_stats[Prio_patch_stats$patch_id %in% Config_patch_IDs,]
    
    #seperate best patches to raster
    Config_rast <-  ifel(Prio_patches %in% Config_patch_IDs, Prio_patches, NaN)
  }
  
  # Connectivity solution: Highest value of effective mesh size
  if(Ca_patch_preference == "Connectivity"){
    
    #get solution number with highest Effective_mesh_size        
    Config_solution <- Solutions_eval[which.max(Solutions_eval$Connectivity), "Solution_num"]
    
    #get patch_IDs
    Config_patch_IDs <- Solutions[[Config_solution]]
  }
  
  #get patch stats
  Config_patch_stats <- Prio_patch_stats[Prio_patch_stats$patch_id %in% Config_patch_IDs,]
    
  #seperate best patches to raster
  Config_rast <-  ifel(Prio_patches %in% Config_patch_IDs, Prio_patches, NaN)

  # Print message
  cat(paste0("Optimal solutions under prioritization:", Ca_prioritization, ", under the % target area: ", Ca_expansion_target, ", for the configuration options: ", paste(Ca_patch_preference, collapse = ", "), " have been identified \n"))

  return(list(
    Config_rast = Config_rast,
    Config_patch_stats = Config_patch_stats
  ))
} # close function
