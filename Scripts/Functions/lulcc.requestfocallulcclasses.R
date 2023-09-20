### =========================================================================
### lulcc.finalisemodelspecifications: Enter optimal model specification
### and parameters for prediction 
### =========================================================================
#' 
#'
#' @param LULC_aggregation_path Character, file path for table of LULC class aggregations
#' @author Ben Black
#' @export

lulcc.requestfocallulcclasses <- function(LULC_aggregation_path){
  
  #Vector unqiue LULC classes
  LULC_classes <- unique(read_excel(LULC_aggregation_path)[["Class_abbreviation"]])
  
  #Use prompt to get active class names as input from user
  print(paste0("The aggregated LULC classes are: ", paste(LULC_classes, collapse = ", ")))
  Active_class_names <- readline(prompt= paste0("The aggregated LULC classes are: ", c(paste(LULC_classes, collapse = ", ")), ", 
  Enter those to be considered as active classes for the creation of focal layers: "))
  return(Active_class_names)
  
  Active_class_names <- input_select(LULC_classes, multiple = TRUE)
}