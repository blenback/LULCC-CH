### =========================================================================
### lulcc.listbylulc: Seperate list of dataframes into a nested list 
### according to the initial LULC class of the transition
### =========================================================================
#' 
#'
#' @author Ben Black
#' @export

lulcc.listbylulc <- function(All_trans_list, initial_or_final){
  
if(initial_or_final=="Initial"){
#split name of transition dataset to drop final state
name_minus_final_state <- lapply(names(All_trans_list), function(x)
str_split(x, pattern = "[.]")[[1]][2]
)

#create vector of initial LULC classes
#Initial_lulc_classes <- (unique(name_minus_final_state))[! (unique(name_minus_final_state)) %in% c('Static')]
Initial_lulc_classes <- unique(name_minus_final_state)

#create empty list for output
Trans_by_initial_lulc <- list()
  
  #loop over region names
  for (i in Initial_lulc_classes){
    Current_lulc_class <- i #Get name of current lulc class being looped over
  Trans_for_lulc_class <- All_trans_list[grep(paste0(Current_lulc_class, "."), names(All_trans_list))]  #select only the list elements that contain this name 
 #create an object and assign these transitions to these names
  Trans_by_initial_lulc[[paste(Current_lulc_class)]] <- Trans_for_lulc_class
  }
  return(Trans_by_initial_lulc)
}
  
if(initial_or_final=="Final"){
  #split name of transition dataset to final state
final_state <- lapply(names(All_trans_list), function(x)
str_split(x, pattern = "[.]")[[1]][3]
)

#rename with new names
names(All_trans_list) <- final_state 

#create vector of initial LULC classes
Final_lulc_classes <- c("Alp_Past", "Closed_Forest", "Grassland", "Int_AG", "Open_Forest", "Perm_crops", "Shrubland", "Urban", "Glacier", "Static") #create vector of region names

#create empty list for output
Trans_by_final_lulc <- list()
  
  #loop over region names
  for (i in Final_lulc_classes){
    Current_lulc_class <- i #Get name of current lulc class being looped over
  Trans_for_lulc_class <- All_trans_list[grep(Current_lulc_class, names(All_trans_list))]  #select only the list elements that contain this name 
 #create an object and assign these transitions to these names
  Trans_by_final_lulc[[paste(Current_lulc_class)]] <- Trans_for_lulc_class
  }
  return(Trans_by_final_lulc)
}
}



