#extract to dataframe

#test with raster::extract
system.time(Trans_dataset <- as.data.frame(raster::extract(x=Trans_data_stack, y=part[["all"]])))
?raster::extract
#test terra::extract (require conversion of spatial objects first)
test_rast <- rast(lapply(Trans_data_stack@layers, rast))
point_vect <- vect(part[["all"]])
system.time(Trans_dataset_terra <- terra::extract(test_rast,point_vect))

all.equal(Trans_dataset, Trans_dataset_terra)
raster_na <- sum(is.na(getValues(SA_pred_stack)))
data_na <- sum(is.na(SA_data))

#its clear that this method of raster extraction introduces NAs in both cases
#(raster and terra) i.e. the number of NAs is greater in the dataframes than in the rasters

#instead trial the method I used previously by converting to dataframe
system.time(df_conversion <- raster::as.data.frame(SA_pred_stack)) #Convert Rasterstack to dataframe, because the LULC and Region layers have attribute tables the function creates two columns for each: Pixel value and class name
xy_coordinates <- coordinates(SA_pred_stack) #Get XY coordinates of cells
df_with_xy <- na.omit(cbind(df_conversion, xy_coordinates)) #cbind XY coordinates to dataframe and remove NAs
alt_data_na <- sum(is.na(df_with_xy))

#however whhen NAs are removed from the dataframe produced by extraction the
#same number of instances (row number) are left
test_redact <- na.omit(SA_data)
