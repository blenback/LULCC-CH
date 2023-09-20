### =========================================================================
### lulcc.extractsplitrasterstack: Extracting values from raster stack to dataframes and splitting by Bioregion 
### =========================================================================
#' returns a list of dataframes named for each Bioregion containing dataframes for each LULC class including all predictor
#' variables and x y coordinates for each cell
#'
#' @author Ben Black
#' @export

  lulcc.extractsplitrasterstack <- function(Rasterstack_for_splitting, Split_by){
  
  #Convert Rasterstack to dataframe, because the LULC and Region layers have
  #attribute tables the function creates two columns for each: Pixel value and class name
  df_conversion <- raster::as.data.frame(Rasterstack_for_splitting) 
  
  #Get XY coordinates of cells
  xy_coordinates <- coordinates(Rasterstack_for_splitting) 
  
  #cbind XY coordinates to dataframe and remove NAs
  df_with_xy <- na.omit(cbind(df_conversion, xy_coordinates)) 
  
  if(Split_by == "NONE"){
    
    return(df_with_xy)
  
  }else if (Split_by != "NONE"){
    
    #Split DF using the Bioregion class names column
    #inherited from the raster attribute table
    DFs_by_bioregion <- split(df_with_xy, f = df_with_xy[[Split_by]]) 
     
    return(DFs_by_bioregion)
  }
  } #close function