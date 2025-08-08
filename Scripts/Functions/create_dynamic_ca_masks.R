create_dynamic_ca_masks <- function(
  intervention_time_steps,
  Ca_expansion_end_year, 
  Ca_expansion_start_year,
  Ca_expansion_rate,
  Ca_prioritization = Ca_prioritization,
  Ca_expansion_target = Ca_expansion_target,
  Ca_patch_preference = Ca_patch_preference,
  area_target_n_cells = area_target_n_cells,
  Current_CA_rast,
  Current_ca_coverage_n_cells,
  intervention_mask_dir = intervention_mask_dir,
  intervention_ID,
  intervention_patches = Intervention_patches$Config_rast,
  intervention_patch_stats = Intervention_patches$Config_patch_stats,
  Scenario = Scenario
){
  
  # replace NA values in Current_CA_rast with 0 and all other values with 1
  Current_CA_rast <- ifel(is.na(Current_CA_rast), 0, 1)
  
  #create an empty df to store values EI area in each time step
  CA_area_chg <- vector(mode="numeric", length = length(intervention_time_steps))
  names(CA_area_chg) <- as.character(intervention_time_steps)
  
  # Total expand CA area (cells)
  CA_expanded_area <- Current_ca_coverage_n_cells + area_target_n_cells
  
  # Intervention save dir
  intervention_dir <- file.path(intervention_mask_dir, intervention_ID , Ca_prioritization, Ca_expansion_target, Ca_patch_preference)
  dir.create(intervention_dir, showWarnings = FALSE, recursive = TRUE)
  
  #remove row numbers from intervention_patch_stats
  rownames(intervention_patch_stats) <- NULL
  
  #sort by decreasing value of median patch priority
  intervention_patch_stats <- intervention_patch_stats[order(intervention_patch_stats$median, decreasing = TRUE),]
  
  # use Ca_expansion_start_year and Ca_expansion_end_year to create a sequence of expansion time steps
  step_length <- intervention_time_steps[2] - intervention_time_steps[1]
  expansion_time_steps <- seq(Ca_expansion_start_year, Ca_expansion_end_year, by = step_length)
  num_expansion_steps <- length(expansion_time_steps)
  
  #get total number of cells in patches
  cell_total <- freq(intervention_patches > 0)[["count"]]
  
  #chunk for rate == "Linear"
  if(Ca_expansion_rate == "Steady"){
    
    #divide the cell total by the number of time steps
    cell_per_step <- cell_total/num_expansion_steps
    
    #cumulatively sum the cell_per_step
    Time_step_ncells <- ceiling(cumsum(rep(cell_per_step,num_expansion_steps)))
    
  } else if(Ca_expansion_rate == "Rapid"){

    #vector value to modify cell total
    mod <- 10^nchar(cell_total)

    #calculate mu (maximum growth rate in the exponential phase)
    # relative to cell_total and number of EI changing steps
    mu <- (log10(mod-cell_total)-log10(1))/num_expansion_steps
    
    #model for rapid establishment
    Rapid_model <- list(model = "Baranyi",
                         N0 = 1,
                         Nmax = cell_total,
                         mu = 2*mu,
                         lambda = -(2.5-mu))
    
    #vector intermediate time points
    intermed_points <- seq(1, (num_expansion_steps), by=1)
    
    Rapid_pred <- predict_growth(environment = "constant",
                                     times= intermed_points,
                                     primary_model= Rapid_model)
    
    #get values for time steps converting back from log10
    Time_step_ncells <- ceiling(10^Rapid_pred$simulation$logN)
    
  } else if(Ca_expansion_rate == "Lagged"){

    #vector value to modify cell total
    mod <- 10^nchar(cell_total)

    #calculate mu (maximum growth rate in the exponential phase)
    # relative to cell_total and number of EI changing steps
    mu <- (log10(mod-cell_total)-log10(1))/num_expansion_steps

    Lagged_model <- list(model = "Baranyi",
                        N0 = 1,
                        Nmax = cell_total,
                        mu = 1.5*mu,
                        lambda = 2.5-mu)
    
    #vector intermediate time points
    intermed_points <- seq(1, num_expansion_steps, by=1)
    
    Lagged_pred <- predict_growth(environment = "constant",
                                 times= intermed_points,
                                 primary_model= Lagged_model)
    
    #get values for time steps converting back from log10
    Time_step_ncells <- ceiling(10^Lagged_pred$simulation$logN)
    
  } #close else if for 'Lagged' rate
    
    #name with time steps
    names(Time_step_ncells) <- expansion_time_steps
    
    #calculate cumulative sum of patch area
    intervention_patch_stats$cumsum <- cumsum(intervention_patch_stats$num_cells)
    
    #insert values of Time_step_ncells into sim_steps matching by name
    Sim_step_ncells <- sapply(intervention_time_steps, function(x) ifelse(x %in% names(Time_step_ncells), Time_step_ncells[names(Time_step_ncells) == x], 0))
    names(Sim_step_ncells) <- as.character(intervention_time_steps)
    
    #insert final value of cell_total
    #Sim_step_ncells[length(Sim_step_ncells)] <- cell_total
    
    #loop over Sim_step_ncells using the names to identify the year and the 
    #values to the cumsum value by which to identify patches which ensures that
    #each subsequent step contains the patches of the previous step
    Time_step_paths <- list()
    for(j in 1:length(Sim_step_ncells)){
    
      #add Sim_time_steps to CA_area_chg
      CA_area_chg[names(CA_area_chg) == names(Sim_step_ncells[j])] <- Sim_step_ncells[j]
      
      #get patch IDs
      patch_IDs <- intervention_patch_stats$patch_id[intervention_patch_stats$cumsum <= Sim_step_ncells[j]]
      
      #if the length of patch IDs is 0 then no new EI patches are being added
      #so the map only includes the Current CAs
      if(length(patch_IDs) == 0){
        intervention_rast <- Current_CA_rast
        
        # set 0's to NA
        intervention_rast <- ifel(intervention_rast == 0, NA, 1)
      }else if(length(patch_IDs) >0){
      
        #seperate best patches to raster
        intervention_rast <-  ifel(intervention_patches %in% patch_IDs, intervention_patches, 0)
      
        #set all values greater than 0 to 1
        intervention_rast <- ifel(intervention_rast > 0, 1, 0)
        
        #combine with existing EI
        intervention_rast <- intervention_rast + Current_CA_rast

        #set 0's to NA
        intervention_rast <- ifel(intervention_rast == 0, NA, 1)
        
      } 
      
      #create path to save map
      intervention_map_path <- file.path(intervention_dir, paste0(Scenario, "_", intervention_ID, "_", names(Sim_step_ncells[j]), ".tif"))
      
      #write raster
      writeRaster(intervention_rast, file= intervention_map_path, overwrite=TRUE)
      
      # add the path to the Time_step_paths list
      Time_step_paths[[names(Sim_step_ncells[j])]] <- intervention_map_path
      
      #print message
      cat("\t Created future layer for", names(Sim_step_ncells[j]), "under scenario:", Scenario, "and intervention:", intervention_ID,  "\n")
    } #close loop over Sim_step_ncells
    
      cat("Finished creating future layers for scenario:", Scenario, "under intervention:", intervention_ID, "\n")
      cat ("The area of CAs has changed in the following time steps:", CA_area_chg, "\n")
      
      
    #return the Time_step_paths and CA_area_chg
    return(Time_step_paths = Time_step_paths)
}
