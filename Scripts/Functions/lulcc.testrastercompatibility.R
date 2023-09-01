### =========================================================================
### Confirm Raster compatibility prior to stacking
### =========================================================================
#' 
#'
#' @author Ben Black
#' @export

lulcc.TestRasterCompatibility <- function(Rasterlist, ExemplarRaster){

#creating an empty list to store comparison results in
comparison_result <- list()

#for loop that compares all rasters in the list to the exemplar and return results of any discrepancies to the empty list
cat('Comparing Rasters to confirm uniform characteristics prior to stacking...\n')

for(i in 1:length(Rasterlist)){
  comparison_result[[i]] <- capture_warnings(compareRaster(ExemplarRaster,Rasterlist[[i]], res=T, orig=T, stopiffalse=F, showwarning=T))
}

if ((is_empty(comparison_result, first.only = FALSE, all.na.empty = TRUE)) == TRUE) {
  cat('Rasters ready to stack...\n') 
} else {
  cat('Differences in Rasters characteristics, opening comparison results...\n')
  
  print(comparison_result)}
return(comparison_result)
}
