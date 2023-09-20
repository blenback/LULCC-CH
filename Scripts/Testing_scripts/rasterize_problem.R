### =========================================================================
### Method with raster::rasterize
### =========================================================================

#reference raster
ref_raster <- raster(ncols=3600, nrows=2300, xmn=2480000, xmx=2840000, ymn=1070000, ymx=1300000, crs='+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +units=m +no_defs')

#create spatial points from raster
sp_points <- rasterToPoints(ref_raster, spatial = TRUE)

#data to rasterize
test_df <- data.frame(test_var = runif(length(sp_points@coords)/2, min = 1, max = 6))

#create spatial points dataframe
data_points <- SpatialPointsDataFrame(sp_points, test_df)

#rasterize
r <- raster::rasterize(x= data_points, y= ref_raster)
plot(r)

### =========================================================================
### Alternative with terra::rasterize
### =========================================================================

current_LULC_path <- str_replace(File_path_simulated_LULC_maps, "<v1>", paste0(Simulation_time_step))

ref_raster <- terra::rast(current_LULC_path)

