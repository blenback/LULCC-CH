### =========================================================================
### lulcc.binarizetransitiondatasets: Add transition columns to datasets according
### to list of viable LULC transitions identified from transition matrix and
### fill with results of transition (1 : positive, 0: negative, NA: not applicable)
### =========================================================================
#' 
#'
#' @author Ben Black
#' @export

lulcc.binarizetransitiondatasets <- function(data_set_for_splitting, viable_trans_list){

#convert DF to data.table
data_set_for_splitting <- data.table(data_set_for_splitting)
  
#use setkey to sort the data.table
setkey(data_set_for_splitting, Initial_class_lulc_name) 

#remove rows with static initial class using key.
data_set_for_splitting_subset <- data_set_for_splitting[!"Static"] 

#add a numeric ID for recombining later
data_set_for_splitting_subset$Num_ID <- seq.int(nrow(data_set_for_splitting_subset))

#strip unnecessary columns
LULC_cols <- c('Num_ID', 'Initial_class_lulc_name', 'Final_class_lulc_name')
Trans_rel_cols <- data_set_for_splitting_subset[, .SD, .SDcols = LULC_cols]

#create empty list for results
Trans_cols_list <- list()

for(row in 1:nrow(viable_trans_list)){

  print(paste0("Identifying data for transition from ", viable_trans_list[row, "Initial_class"], " ", "to ", viable_trans_list[row, "Final_class"]))
  
  # get the from and to classes of the current transition
  f <- viable_trans_list[row, "Initial_class"]
  t <- viable_trans_list[row, "Final_class"]
  Trans_ID <- viable_trans_list[row, "Trans_name"]
  
  
  Trans_DF <- data.frame(apply(Trans_rel_cols, 1, function(x) ifelse(x["Initial_class_lulc_name"] == f & x["Final_class_lulc_name"] == t,
                                                                     1, ifelse(x["Initial_class_lulc_name"] == f & x["Final_class_lulc_name"] !=t,
                                                                               0, NA))))
  colnames(Trans_DF) <- Trans_ID #make the column in the dataframe name the same as Trans_ID
  Trans_cols_list[[Trans_ID]] <- Trans_DF 
 
} #close inner loop

rm(Trans_rel_cols)

#cbind list of transition cols into a dataframe
Trans_cols_df = do.call(cbind,Trans_cols_list)

#Merge the output of the transition column creation with the other data columns
data_set_for_splitting_recombine <- cbind(data_set_for_splitting_subset, Trans_cols_df)

#remove cols where all values are NA and all cols which don't have more than 1 unique value
#leaving only the relevant transitions for each list
Data_minus_NA_cols <- Filter(function(y)!all(is.na(y)), data_set_for_splitting_recombine)
Data_unique_cols <- Filter(function(y)(length(unique(y))>2), Data_minus_NA_cols)
rm(data_set_for_splitting_recombine)

#create a vector of transition IDs that matches the transition column names in the data
Trans_ID_vector <- as.vector(viable_trans_list$Trans_name)
  
#subset the data.table by all the columns apart from those related to the transitions
Info_cols <- Data_unique_cols[, .SD, .SDcols = !patterns(paste(Trans_ID_vector, collapse="|"))]

#subset only the columns that are related to the transitions because the number of these varies for each table it is best to use the negative invocation
Trans_cols <- Data_unique_cols[, .SD, .SDcols = patterns(paste(Trans_ID_vector, collapse="|"))]
 
Trans_cols_list <- split.default(Trans_cols, names(Trans_cols)) #Split transitions cols into a list of dataframes one for each column.
 
Trans_dataframes_seperated <- sapply(Trans_cols_list, function(y) {
   combined_data <- na.omit(cbind(Info_cols, y))
   }, simplify=F) #bind each individual column with the info cols and then return the results as a list

return(Trans_dataframes_seperated)

}