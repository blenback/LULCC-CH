#' Calculate patch statistics
#' This function calculates the area and median priority value of each patch in a raster.
#' @param Patch_raster A raster object representing patches.
#' @param Val_raster A raster object representing the priority values.
#' @return A data frame containing the patch ID, area (in number of cells), and median priority value for each patch.
calc_patch_stats <- function(
    Patch_raster,
    Val_raster
    ){

  #calculate the area of all patches
  area_df <- as.data.frame(freq(Patch_raster))

  #subset cols and rename
  area_df <- area_df[c('value','count')]
  colnames(area_df) <- c("patch_id", "num_cells")

  # stack with the prioritization map
  r_stack <- c(Patch_raster, Val_raster)

  # Use terra's zonal function to compute the median for each unique value in Patch_rast
  zonal_median <- terra::zonal(r_stack, Patch_raster, fun = function(x) { median(x, na.rm = TRUE) })
  zonal_median <- zonal_median[c(2,3)]

  # Rename the columns of zonal_median
  colnames(zonal_median) <- c("patch_id", "median")

  #merge dfs with median of prio and area of each patch
  df_merge <- merge(area_df,zonal_median,by="patch_id")
  
  #return the final df
  return(df_merge)
}
