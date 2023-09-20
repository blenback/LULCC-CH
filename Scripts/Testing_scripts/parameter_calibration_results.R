### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl","rlist", "tidyverse",
         "rstatix", "Dinamica", "raster", "openxlsx")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#read in csv files of results
results_paths <- list.files("E:/LULCC_CH/Results/Validation", recursive = TRUE, full.names = TRUE, pattern = ".csv")
names(results_paths) <- list.files("E:/LULCC_CH/Results/Validation", recursive = TRUE, pattern = ".csv")

calibration_results <- rbindlist(lapply(results_paths, function(x) read.csv(x, header = FALSE)), use.names = TRUE)
calibration_results$Sim_ID <- sapply(names(results_paths), function(x) str_split(x, "/")[[1]][1])
