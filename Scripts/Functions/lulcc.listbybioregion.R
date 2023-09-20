### =========================================================================
### lulcc.listbybioregion: Seperate list of dataframes into a nested list according to Bioregion
### =========================================================================
#' 
#'
#' @author Ben Black
#' @export

lulcc.listbybioregion <- function(All_trans_list){
  Region_names <- c("Jura", "Plateau", "Northern_Prealps", "Southern_Prealps", "Western_Central_Alps", "Eastern_Central_Alps") #create vector of region names
  Trans_by_bioregion <- list()
  
  #loop over region names
  for (i in Region_names){
    Current_Reg_name <- i #Get name of current region
  Trans_for_Reg <- All_trans_list[grep(Current_Reg_name, names(All_trans_list))]  #select only the list elements that contain this name 
 #create an object and assign these transitions to these names
  Trans_by_bioregion[[paste(Current_Reg_name)]] <- Trans_for_Reg
  }
  return(Trans_by_bioregion)
}