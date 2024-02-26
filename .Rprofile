source("renv/activate.R")
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession)
    rstudioapi::navigateToFile('scripts/LULCC_CH_master.R', line = -1L, column = -1L)
    #quarto::quarto_run("LULCC_CH_user.qmd")
}, action = "append")
