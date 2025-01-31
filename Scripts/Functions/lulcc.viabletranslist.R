### =========================================================================
### lulcc.viabletranslist: Identify viable LULC transitions from a 
### transition matrix based on a threshold inclusion rate
### =========================================================================
#' 
#'
#' @author Ben Black
#' @export

lulcc.viabletranslist <- function(transmatrix, inclusion_threshold){
  
#add the rownames as a column
transmatrix <- rownames_to_column(transmatrix, var = "Initial_class")
#reformat matrix from wide to long and sort
transmatrix_long <- transmatrix %>% gather(Final_class, value, -c(Initial_class))
transmatrix_sorted <- transmatrix_long[order(transmatrix_long$Initial_class),] 

#remove rows corresponding to transitions that occurred for less 0.5% of the total number of cells of the Initial LULC class total 
transmatrix_refined <- subset(transmatrix_sorted, value >= inclusion_threshold)

#add a column: 'trans_F_T' that combines the initial and final class seperated by '_' to use for naming columns
transmatrix_refined$trans_F_T <- c(paste0(transmatrix_refined$Initial_class,sep = '.', transmatrix_refined$Final_class))

#add a column: 'trans_num_ID' that will give a numerical ID for each transition that they can be identified in the multiclass modelling. 
transmatrix_refined$trans_num_ID <- seq.int(nrow(transmatrix_refined))

return(transmatrix_refined) 
}