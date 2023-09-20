remotes::install_github("claudiozandonella/trackdown")

uniSave_upload <- function(){}

#save document
rstudioapi::documentSave()
quarto::quarto_serve("document.qmd")

#authenticate access to Google drive
trackdown::trackdown_auth_configure(path = "Gdrive_api_auth.json")
trackdown::upload_file(file = "publication/LULCC_CH_ms.qmd", gpath = "trackdown", hide_code = TRUE)
trackdown::update_file(file = "publication/LULCC_CH_ms.qmd", gpath = "trackdown", hide_code = TRUE, path_output = "publication/LULCC_CH_ms.pdf")
