#' @title Prepare conservation intervention masks
#' @description
#' This function prepares conservation intervention masks for specified scenarios.
#' It loads intervention definitions from YAML files, extracts conservation interventions,
#' and creates masks based on the types of protected areas specified in the interventions
#' it also idnetifies hypothetical new conservation areas to meet specificed targets if included.
#' @param Scenario_names A vector of scenario names for which to prepare masks.
#' @param interventions_dir Directory containing YAML files with intervention definitions.
#' @param CA_vect A SpatVector containing the protected areas.
#' @param Proj The projection to use for the rasterization of protected areas.
#' @param CA_mask_path Path to the raster mask of protected areas.
#' @param intervention_mask_dir Directory to save the intervention masks.
#' @return None. The function saves the intervention masks to the specified directory and updates the YAML files.
prepare_conservation_intervention_masks <- function(
    Scenario_names,
    interventions_dir = "Tools",
    CA_vect = CA_vect,
    Proj = ProjCH,
    CA_mask_path = "Data/Spat_prob_perturb_layers/Protected_areas/raw_data/Bio_prio.tif",
    intervention_mask_dir = "Data/Spat_prob_perturb_layers/Protected_areas/Simulation_PAs",
    Ref_grid = Ref_grid,
    expansion_mask_paths = expansion_mask_paths,
    Use_parallel = TRUE,
    n_cores = 4,
    Save_dir = "Data/Spat_prob_perturb_layers/Protected_areas",
    Solution_quantile = 0.95,
    Recalc_results = FALSE
    ){
  
  # Check parallel processing options
  if(Use_parallel){
    
    # check if n_cores is a positive integer
    if(!is.numeric(n_cores) || n_cores <= 0 || n_cores != round(n_cores)){
      stop("n_cores must be a positive integer.")
    }
  
    # check if n_cores is possible
    if(Use_parallel == TRUE & n_cores > parallel::detectCores()){
      stop("n_cores cannot be greater than the number of available cores.")
    }
  }

  # load the CA_mask raster
  CA_mask <- rast(CA_mask_path)
  crs(CA_mask) <- Proj
  
  # Loop over Scenario names, loading the relevant intervention.yml files to get details of
  # the types of PAs to include and whether expansion is necessary and create a
  # mask of conservation areas to be used for each intervention
  for(Scenario in Scenario_names){
  
    # load the yaml file using read lines and extract the comments in the header
    yaml_lines <- readLines(file.path(interventions_dir, paste0(Scenario, "_interventions.yml")))
  
    # extract the header comments
    header <- yaml_lines[grepl("^\\s*#", yaml_lines)]
  
    # Load the intervention.yml file for the scenario
    Scenario_interv <- yaml.load_file(file.path(interventions_dir, paste0(Scenario, "_interventions.yml")))
  
    # subset to the interventions where Intervention_ID contains "conservation"
    conservation_interv <- Scenario_interv[sapply(Scenario_interv, function(x) grepl("conservation", x$Intervention_ID, ignore.case = TRUE))]
  
    Scenario_mask_dir <- file.path(intervention_mask_dir, Scenario)
    # create the directory for the scenario masks if it does not exist
    dir.create(Scenario_mask_dir, showWarnings = FALSE, recursive = TRUE)
    
    # loop over the conservation interventions
    for(intervention in conservation_interv){
    
      # print the intervention ID and types of PAs
      cat(paste("Processing intervention:", intervention$Intervention_ID, "using conservation areas:", paste(intervention$Ca_types, collapse = ", "), "\n"))
    
      # subset the CA_vect to the types of PAs in the conservation interventions
      Intervention_CA_subset <- CA_vect[CA_vect$Res_Type %in% intervention$Ca_types, ]
    
      # rasterize the subset of PAs by masking the Biodiversity prioritization raster
      CA_subset_raster <- mask(CA_mask, Intervention_CA_subset, inverse = FALSE)
  
      # change non-NA values to 1
      CA_subset_raster <- ifel(!is.na(CA_subset_raster), 1, NA)

      # If intervention$Ca_expansion_target does not exist then save the raster and update the path in the yaml block
      if(is.null(intervention$Ca_expansion_target)){
      
        # create a file name for the raster
        intervention_file_name <- paste0(Scenario, "_", intervention$Intervention_ID, "_mask.tif")
      
        # save the raster to the intermediate directory
        terra::writeRaster(CA_subset_raster, filename = file.path(Scenario_mask_dir, intervention_file_name), overwrite = TRUE)
      
        # update the path in the intervention yaml block
        intervention$Intervention_mask <- file.path(Scenario_mask_dir, intervention_file_name)
    
        # replace the intervention in the scenario interventions list
        # get which intervention in the list matches the current intervention
        interv_ix <- sapply(Scenario_interv, function(x) x$Intervention_ID == intervention$Intervention_ID)
      
        # replace the intervention in the list
        Scenario_interv[[which(interv_ix)]] <- intervention
      
        # print the path of the saved raster
        cat(paste("Saved raster to:", intervention$Intervention_mask, "\n"))
      } else if(!(is.null(intervention$Ca_expansion_target))){
    
        # check if the masks have already been prepared for this intervention by
        # checking if the intervention$Intervention_mask exists
        if(!is.null(intervention$Intervention_mask) && all(file.exists(intervention$Intervention_mask))){
          # if the mask already exists then skip to the next intervention
          cat(paste("Mask already exists for intervention:", intervention$Intervention_ID, "\n"))
          next
        }
      
        # first calculate the additional area required to meet the conservation area expansion target
        area_simulation_region <- 41285*1000000 # area of Switzerland in m2
      
        # calculate the areal increase required to meet the conservation area expansion target
        CA_coverage <- calculate_ca_coverage_and_expansion_area(
          CA_vect = Intervention_CA_subset,
          area_simulation_region = area_simulation_region,
          coverage_target = intervention$Ca_expansion_target
        )
      
        # convert the % areal increase required to number of cells
        area_target_n_cells <- ceiling(CA_coverage$areal_increase_required*area_simulation_region/prod(res(Ref_grid)))
        
        # convert the current CA coverage to number of cells
        Current_ca_coverage_n_cells <- ceiling(CA_coverage$current_CA_coverage*area_simulation_region/prod(res(Ref_grid)))
        
        # save the current CA_map as a raster to be reloaded in subsequent functions
        # (makes parrallel processing easier)
        Scratch_dir <- file.path(Save_dir, "Scratch", Scenario, intervention$Intervention_ID)
        dir.create(Scratch_dir, showWarnings = FALSE, recursive = TRUE)
        CA_rast_path <- file.path(Scratch_dir, paste0(Scenario, "_", intervention$Intervention_ID, "_initial_PAs.tif"))
        writeRaster(CA_subset_raster, filename = CA_rast_path, overwrite = TRUE)

        # Apply function to identify patches according to their priority
        Priority_patch_paths <- identify_priority_patches(
          Prio_rast_path = "Data/Spat_prob_perturb_layers/Protected_areas/raw_data/Bio_prio.tif",
          CA_rast_path = CA_rast_path,
          expansion_mask_paths = expansion_mask_paths,
          area_target = area_target_n_cells,
          Priority_patch_dir = file.path(Scratch_dir, "Priority_patches"),
          Ca_prioritization = intervention$Ca_prioritization,
          Recalc_results = Recalc_results
          )
  
        # Apply function to generate patch solution sets based on the identified priority patches
        # rank them according to the sum of median patch priority * patch size and subset to only those 
        # above an upper quantile value
        Ranked_solution_paths <- identify_patch_solution_sets(
          Ca_prioritization = intervention$Ca_prioritization,
          area_target_n_cells = area_target_n_cells,
          Ca_expansion_target = intervention$Ca_expansion_target,
          Patch_solutions_dir = file.path(Scratch_dir, "Priority_solutions"),
          Recalc_results = Recalc_results,
          Prio_patch_stats_path = Priority_patch_paths$Prio_patch_stats_path,
          Use_parallel = Use_parallel,
          n_cores = n_cores,
          Solution_quantile = 0.95
          )
  
        # Apply function to select optimal patch set based on patch configuration option
        Optimal_patches <- select_optimal_patch_set_for_configuration(
          Use_parallel = TRUE,
          n_cores = n_cores,
          CA_rast_path = CA_rast_path,
          Ca_expansion_target = intervention$Ca_expansion_target,
          Ca_prioritization = intervention$Ca_prioritization,
          Ca_patch_preference = intervention$Ca_patch_preference,
          Prio_patch_stats_path = Priority_patch_paths$Prio_patch_stats_path,
          Prio_patches_path = Priority_patch_paths$Prio_patches_path,
          Optimal_solutions_dir = file.path(Scratch_dir, "Optimal_solutions"),
          Recalc_results = Recalc_results,
          Solutions = Ranked_solution_paths$Solutions_upperQ_path,
          Solution_stats = Ranked_solution_paths$Solutions_upperQ_stats_path
          )
        
        # Apply function to save layers for the time steps of the intervention
        Dynamic_ca_paths <- create_dynamic_ca_masks(
          intervention_time_steps = intervention$Time_steps_implemented,
          Ca_expansion_end_year = intervention$Ca_expansion_end_year, 
          Ca_expansion_start_year = intervention$Ca_expansion_start_year,
          Ca_expansion_rate = intervention$Ca_expansion_rate,
          Ca_prioritization = intervention$Ca_prioritization,
          Ca_expansion_target = intervention$Ca_expansion_target,
          Ca_patch_preference = intervention$Ca_patch_preference,
          area_target_n_cells = area_target_n_cells,
          Current_CA_rast = CA_subset_raster,
          Current_ca_coverage_n_cells = Current_ca_coverage_n_cells,
          intervention_mask_dir = Scenario_mask_dir,
          intervention_ID = intervention$Intervention_ID,
          intervention_patches = Optimal_patches$Config_rast,
          intervention_patch_stats = Optimal_patches$Config_patch_stats,
          Scenario = Scenario
          )
        
        # add the Dynamicca_paths to the intervention data
        # update the path in the intervention yaml block
        intervention$Intervention_mask <- Dynamic_ca_paths
    
        # replace the intervention in the scenario interventions list
        # get which intervention in the list matches the current intervention
        interv_ix <- sapply(Scenario_interv, function(x) x$Intervention_ID == intervention$Intervention_ID)
      
        # replace the intervention in the list
        Scenario_interv[[which(interv_ix)]] <- intervention
    } # close section for interventions requiring CA expansion
  
  # Save the updated scenario interventions to the same yaml file
  # convert the Scenario_interv list to yaml format
  yaml_text <- yaml::as.yaml(Scenario_interv, indent.mapping.sequence = TRUE)
  
  # combine with the header
  final_text <- c(header, "", yaml_text)
  
  # write the final text to the yaml file
  writeLines(final_text, file.path(interventions_dir, paste0(Scenario, "_interventions.yml")))
  
    } # close loop over conservation interventions
} # close loop over Scenario names

  # Print completion message
  cat("Conservation intervention masks prepared and saved successfully.\n")
  
} # close function





