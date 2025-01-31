### =========================================================================
### Save this
### =========================================================================
#'
#' @param object List, model object to be saved
#' @param model_name Character, model type 
#' @param transition_name Character, transition name to be included in save path
#' @param tag Character, identifier for model
#' @param save_path Character, folder location for saving
#'
#' @author Antoine Adde with edits by Ben Black
#' @export
lulcc.savethis <- function(object, model_name=NULL, transition_name=NULL, tag=NULL, save_path){

suppressWarnings(dir.create(save_path,  recursive = TRUE))

if(is.null(tag)){f <- paste0(save_path,"/", paste(transition_name, model_name, sep="_"),".rds")
}else{f <-paste0(save_path,"/", paste(transition_name, tag, sep="_"),".rds")}
saveRDS(object, file=f)
}