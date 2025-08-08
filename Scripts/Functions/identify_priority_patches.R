#############################################################################
## identify_priority_patches: function to identify patches according to a prioritization map and subset
## Date: 03-01-2024
## Author: Ben Black
#############################################################################
#'
#' @param Prio_rast_path SpatRaster of prioritization map
#' @param CA_rast_path Path to SpatRaster of conservation area map
#' @param expansion_masks List of SpatRaster to mask the prioritization map
#' @param area_target Numeric value of the area to be covered by the patches
#' @param Priority_patch_dir Character string of the directory to save the output
#' @param Ca_prioritization Character string of the prioritization approach
#' @param Recalc_results Logical indicating whether to recalculate results
#' @returns list containing the paths to the saved rast of patches and the patch stats .rds object 
#'
#' @author Ben Black
#' @export
#'

identify_priority_patches <- function(
  Prio_rast_path = Prio_rast_path,
  CA_rast_path = CA_rast_path,
  expansion_mask_paths = expansion_mask_paths,
  area_target,
  Recalc_results = FALSE,
  Priority_patch_dir = file.path(Priority_patch_dir, "Priority_patches"),
  Ca_prioritization
  ){
  
  # create the scratch directory if it does not exist
  dir.create(Priority_patch_dir, showWarnings = FALSE, recursive = TRUE)
  
  # create file paths for saving results
  Prio_patches_path <- file.path(Priority_patch_dir, paste0(Ca_prioritization, "_patches.tif"))
  Prio_patch_stats_path <- file.path(Priority_patch_dir, paste0(Ca_prioritization, "_patch_stats.rds"))
  
  # if recalc_results = FALSE then check for existing results and return
  if(!Recalc_results){
    if(file.exists(Prio_patches_path) && file.exists(Prio_patch_stats_path)){
      return(list(
        Prio_patches_path = Prio_patches_path,
        Prio_patch_stats_path = Prio_patch_stats_path
      ))
    } else {
      message("Recalc_results == FALSE, however no existing results found, recalculating...")
    }
  }
  
  # load the protected area map
  CA_map <- rast(CA_rast_path)
  
  # load the prioritization map
  Prio_map <- rast(Prio_rast_path)
  
  #mask Prio_map map so that values inside PAs are 0
  Prio_wo_pa <- mask(Prio_map, CA_map, maskvalue=1, updatevalue=NA)
  
  # load the expansion masks
  expansion_masks <- lapply(expansion_mask_paths, function(x) {
    rast(x)
  })
  
  #mask by expansion_masks
  for(i in 1:length(expansion_masks)){
    Prio_wo_pa <- mask(Prio_wo_pa, expansion_masks[[i]], inverse=TRUE)
  }

  #reclassify raster to discrete
  Prio_hist <- terra::hist(Prio_wo_pa, maxcell = ncell(Prio_wo_pa))

  #create patches using only the highest priority breaks that give sufficent
  #cells counts to cover the required amount

  #take the counts and iteratively sum 
  Prio_running_sum <- cumsum(rev(Prio_hist$counts))

  # identify the first cumsum that exceeds the required n_cells and add 1 to
  #include the lower bound of the break points
  Prio_min_ind <- min(which(Prio_running_sum > (area_target*1.25)))

  #subset only the high prioirty breaks that satisfy the desired n_cells
  Prio_cuts <- c(0, Prio_hist$breaks[(length(Prio_hist$breaks)-Prio_min_ind):length(Prio_hist$breaks)])

  #reclassify using the break values
  Prio_discrete <- classify(Prio_wo_pa, Prio_cuts)

  #create patches
  Prio_patches <- get_patches(Prio_discrete,
                              directions = 8,
                              return_raster = TRUE)

  #convert patch rasters to terra:rast and sum values excluding the first layer
  # (i.e. excluding the values of 0-min break)
  Prio_patch_sum <- sum(rast(lapply(Prio_patches$layer_1[2:length(Prio_patches$layer_1)], function(x){
  class_rast <- rast(x)
  class_rast <- ifel(!is.na(class_rast), 1, 0)
  })))

  #convert 0 to NA for patch identification
  Prio_patch_sum[Prio_patch_sum == 0] <- NA

  # Now we have patches based on high priority values now delineate them spatially
  #using terra::patches
  Prio_patches <- patches(Prio_patch_sum)

  #calculate stats for patches
  Prio_patch_stats <- calc_patch_stats(Patch_raster = Prio_patches,
                                       Val_raster = Prio_wo_pa)

  #count number of single cell patches
  Prio_num_SC <- nrow(Prio_patch_stats[Prio_patch_stats$num_cells <2,])
  
  #remove patches that are 2 cells or less
  Prio_patch_stats <- Prio_patch_stats[Prio_patch_stats$num_cells > 2,]
  row.names(Prio_patch_stats) <- 1:nrow(Prio_patch_stats)

  #subset the patches raster
  Prio_patches_subset <- ifel(Prio_patches %in% Prio_patch_stats$patch_id, Prio_patches, NaN)
  
  #save outputs
  writeRaster(Prio_patches_subset, file= Prio_patches_path, overwrite=TRUE)
  saveRDS(Prio_patch_stats,file= Prio_patch_stats_path)
  message(paste0("Saved prioritization patches and stats to: ", Priority_patch_dir))

  # return the patches and patch stats
  return(list(
    Prio_patches_path = Prio_patches_path,
    Prio_patch_stats_path = Prio_patch_stats_path
  ))

  }
