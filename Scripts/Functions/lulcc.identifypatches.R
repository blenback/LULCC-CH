#############################################################################
## lulcc.identifypatches: to identify patches according to a prioritization map and subset
## Date: 03-01-2024
## Author: Ben Black
#############################################################################
#'
#' @param Prio_map SpatRaster of prioritization map
#' @param PA_map SpatRaster of protected area map
#' @param mask_maps List of SpatRaster to mask the prioritization map
#' @param area_target Numeric value of the area to be covered by the patches
#' @param Save_dir Character string of the directory to save the output
#' @param Prio_string Character string of the prioritization approach
#'
#' @author Ben Black
#' @export
#'


lulcc.identifypatches <- function(Prio_map,
                                  PA_map,
                                  mask_maps,
                                  area_target,
                                  Save_dir,
                                  Prio_string) {

  #mask Prio_map map so that values inside PAs are 0
  Prio_wo_pa <- mask(Prio_map, PA_map, maskvalue = 1, updatevalue = NA)

  #mask by mask_maps
  for (i in 1:length(mask_maps)) {
    Prio_wo_pa <- mask(Prio_wo_pa, mask_maps[[i]], inverse = TRUE)
  }

  #reclassify raster to discrete
  Prio_hist <- terra::hist(Prio_wo_pa, maxcell = ncell(Prio_wo_pa))

  #create patches using only the highest priority breaks that give sufficent
  #cells counts to cover the required amount

  #take the counts and iteratively sum
  Prio_running_sum <- cumsum(rev(Prio_hist$counts))

  # identify the first cumsum that exceeds the required n_cells and add 1 to
  #include the lower bound of the break points
  Prio_min_ind <- min(which(Prio_running_sum > (area_target * 1.25)))

  #subset only the high prioirty breaks that satisfy the desired n_cells
  Prio_cuts <- c(0, Prio_hist$breaks[(length(Prio_hist$breaks) - Prio_min_ind):length(Prio_hist$breaks)])

  #reclassify using the break values
  Prio_discrete <- classify(Prio_wo_pa, Prio_cuts)

  #create patches
  Prio_patches <- get_patches(Prio_discrete,
                              directions = 8,
                              return_raster = TRUE)

  #convert patch rasters to terra:rast and sum values excluding the first layer
  # (i.e. excluding the values of 0-min break)
  Prio_patch_sum <- sum(rast(lapply(Prio_patches$layer_1[2:length(Prio_patches$layer_1)], function(x) {
    class_rast <- rast(x)
    class_rast <- ifel(!is.na(class_rast), 1, 0)
  })))

  #convert 0 to NA for patch identification
  Prio_patch_sum[Prio_patch_sum == 0] <- NA

  # Now we have patches based on high priority values now delineate them spatially
  #using terra::patches
  Prio_patches <- patches(Prio_patch_sum)
  #writeRaster(Prio_patches,file= paste0(Save_dir, "/AgriUnprod_patches.tif"), overwrite=TRUE)
  #Prio_patches <- rast(paste0(Save_dir, "/Prio_patches.tif"))

  #calculate stats for Agri patches
  Prio_patch_stats <- Patch_stats(Patch_raster = Prio_patches, Val_raster = Prio_wo_pa)

  #count number of single cell patches
  Prio_num_SC <- nrow(Prio_patch_stats[Prio_patch_stats$num_cells < 2,])

  #remove patches that are 2 cells or less
  Prio_patch_stats <- Prio_patch_stats[Prio_patch_stats$num_cells > 2,]
  row.names(Prio_patch_stats) <- 1:nrow(Prio_patch_stats)

  #subset the patches raster and save
  Prio_patches_subset <- ifel(Prio_patches %in% Prio_patch_stats$patch_id, Prio_patches, NaN)

  #create save dir
  Prio_dir <- paste0(Save_dir, "/", Prio_string)
  dir.create(Prio_dir, showWarnings = FALSE, recursive = TRUE)

  #save outputs
  writeRaster(Prio_patches_subset, file = paste0(Prio_dir, "/", Prio_string, "_patches.tif"), overwrite = TRUE)
  saveRDS(Prio_patch_stats, file = paste0(Prio_dir, "/", Prio_string, "_patch_stats.rds"))

}

#helper function calculating stats on patches
Patch_stats <- function(Patch_raster, Val_raster) {

  #calculate the area of all patches
  area_df <- as.data.frame(freq(Patch_raster))

  #subset cols and rename
  area_df <- area_df[c('value', 'count')]
  colnames(area_df) <- c("patch_id", "num_cells")

  # stack with the prioritization map
  r_stack <- c(Patch_raster, Val_raster)

  # Use terra's zonal function to compute the median for each unique value in Patch_rast
  zonal_median <- terra::zonal(r_stack, Patch_raster, fun = function(x) { median(x, na.rm = TRUE) })
  zonal_median <- zonal_median[c(2, 3)]

  # Rename the columns of zonal_median
  colnames(zonal_median) <- c("patch_id", "median")

  #merge dfs with median of prio and area of each patch
  df_merge <- merge(area_df, zonal_median, by = "patch_id")
}
