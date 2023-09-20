### =========================================================================
### Quarto_render_save_upload: Render a Quarto document locally, save it and
### then upload to Google drive using the trackdown package. Prior to using you must
### follow the instructions on the trackdown vignette for OAuth Client App Configuration:
### https://claudiozandonella.github.io/trackdown/articles/oauth-client-configuration.html 

### =========================================================================
#' 
#' @param Quarto_path chr file path of Quarto document (.qmd) that you wish to save
#' @param Trackdown_auth_path chr path to .json file for authentication for trackdown to upload to Google drive 
#' @param File_ext chr either "html" or "pdf" 
#'
#' @author Ben Black
#' @import rstudioapi
#' @import trackdown
#' @import tools
#' @import quarto
#' @export
#' 

Quarto_render_save_upload <- function(Quarto_path, File_ext, Trackdown_auth_path){
  
#vector output path
Output_path <- gsub(tools::file_ext(Quarto_path), File_ext, x = Quarto_path)

#save document
ID <- rstudioapi::documentId(rstudioapi::documentOpen(path= Quarto_path))
rstudioapi::documentSave(id=ID)
quarto::quarto_render(Quarto_path)

#authenticate access to Google drive
trackdown::trackdown_auth_configure(path = Trackdown_auth_path)

#Try and Upload file
file_exists <- try(trackdown::upload_file(file = Quarto_path, gpath = "trackdown", hide_code = TRUE))

#If file exists use update instead
if(grepl("Error : A file with this name already exists in GoogleDrive", file_exists)){
  trackdown::update_file(file = Quarto_path, gpath = "trackdown", hide_code = TRUE, path_output = Output_path)
}
}

#test
# Quarto_render_save_upload(Quarto_path = "publication/LULCC_CH_ms.qmd",
#                           File_ext = "pdf")

