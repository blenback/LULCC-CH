#############################################################################
## Transition_table_prep: Creating tables of transition rates for future time points
## for use in LULCC allocation within Dinamica EGO
## Date: 22-09-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("data.table", "raster", "tidyverse",
         "lulcc", "stringr", "readr", "xlsx", "gdata")

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Dataframe of LULC labels and values
LULC_classes <- data.frame(label = c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier"),
                           value = c(10,11,12,13,14,15,16,17,18,19))

### =========================================================================
### B- Make Dinamica produced transition tables more readable
### =========================================================================

#Dinamica produces tables of net transition rates (i.e. percentage of land that will change from one state to another)
#These net rates of change are then used to calculate gross rates of change during the CA allocation process
#by first calculating areal change based on the current simulated landscape and then dividing to number of cells that must transition

#The net transition rate tables can be calculated for single steps i.e. the difference between two initial and final historical landscape maps
#Or as Multi-step rates by dividing the single step by a specificed number of time steps.
#given that we want to model 5 year time steps for the future I have produced multi-step transition rate tables
#for the 3 historical periods using 2 time steps for each period which is a simplifaction given that the first two periods
#(1985-1997; 1997-2009) are seperated by 12 years and the final period (2009-2018) 9 years.

#Transition tables produced by Dinamica contain all possible transitions so they need to be subset to only transitions we want to model 

#Importantly Dinamica only accepts net transition rate tables in a very specific format
#So in preparing tables for each scenarios future time points then we need to stick to this

#The order of the rows in the table is also crucial because the probability maps produced for each transition
#need to be named accroding to their row number so the value is associated correctly in allocation. 

#Load multi-step net transition tables produced for historic periods
Calibration_multistep_tables <- lapply(list.files("Data/Transition_tables/raw_trans_tables", full.names = TRUE, pattern = "multistep"), read.csv)
names(Calibration_multistep_tables) <- str_remove_all(list.files("Data/Transition_tables/raw_trans_tables", full.names = FALSE, pattern = "multistep"), paste(c("Calibration_", "_multistep_trans_matrix.csv"), collapse = "|"))

#Add columns to the tables with the LULC class names to make them easier to interpret
Calibration_multistep_tables <- lapply(Calibration_multistep_tables, function(x){
  x$Initial_class <- sapply(x$From., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Final_class <- sapply(x$To., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Trans_name <- paste(x$Initial_class, x$Final_class, sep = "_")
  return(x)
  })

#The model lookup table specifies which transitions are modelled and
#can be used to subset the transition ratestables

#Vector time periods for calibration
Periods <- c("1985_1997", "1997_2009", "2009_2018")

#Load Model lookup tables for each period
Model_lookups <- lapply(Periods, function(Period){read.xlsx("Tools/Model_lookup.xlsx", sheetIndex = Period)})
names(Model_lookups) <- Periods  

#exclude persistence models and return unique transition names
Unique_trans_by_period <- lapply(Model_lookups, function(x) {
  Minus_peristence <- x[!x$Initial_LULC == x$Final_LULC,]
  Unique_trans <- unique(Minus_peristence$Trans_name)
  })

#subset transition rates tables by unique trans and add a sequential ID
Trans_rates_redact <- mapply(function(trans_table, Unique_trans){
  trans_table_subset <- trans_table[trans_table$Trans_name %in% Unique_trans,]
  trans_table_subset$Trans_ID <- sprintf("%02d", 1:nrow(trans_table_subset))
  return(trans_table_subset)
}, trans_table = Calibration_multistep_tables,
Unique_trans = Unique_trans_by_period,
SIMPLIFY = FALSE)

#merge the trans_tables for possible extrapolation of rates over time
Trans_tables_bound <- rbindlist(Trans_rates_redact, idcol = "Period")
Trans_tables_bound$Trans_ID <- NULL
Trans_tables_bound$X <- NULL
Trans_table_time <- pivot_wider(data = Trans_tables_bound, names_from = "Period", values_from = "Rate")

#remove added columns and save as individual csv. files as exemplar trans tables
#to be loaded into dinamica 
mapply(function(trans_table, table_name){
  
  #remove columns
  trans_table[, c("Trans_ID", "Trans_name", "Initial_class", "Final_class", "X")] <- list(NULL)
  
  #save
  write_csv(trans_table, file= paste0("Data/Transition_tables/prepared_trans_tables/Calibration_", table_name, ".csv"))},
         trans_table = Trans_rates_redact,
         table_name = names(Trans_rates_redact)
  )


#save
write.csv(Trans_table_time, "Data/Transition_tables/trans_rates_table_calibration_periods.csv")

#add the trans_ID into the Model_lookup table
Model_lookups_with_ID <- mapply(function(Model_lookup, trans_table){
  Model_lookup$Trans_ID <- sapply(Model_lookup$Trans_name, function(x) trans_table[trans_table$Trans_name == x, "Trans_ID"])
return(Model_lookup)
  }, Model_lookup = Model_lookups,
trans_table = Trans_rates_redact,
SIMPLIFY = FALSE)

#save updated model look up table
#necessary to delete existing xlsx first because overwrite is not possible
unlink("Tools/Model_lookup.xlsx")
mapply(function(model_table, model_name) write.xlsx(model_table, file="Tools/Model_lookup.xlsx", sheetName= model_name, row.names=FALSE, append = TRUE),
         model_table = Model_lookups_with_ID,
         model_name = names(Model_lookups_with_ID)
  )


### =========================================================================
### C- Create folder structure for scenario specific trans tables
### =========================================================================

#vector abbreviations of scenario's for folder/file naming
Scenario_names <- c("BIOPRO", "DIV", "SHAD", "BAU", "FUTEI")

#base folder for creating scenario specifci folders
base_trans_table_folder <- "Data/Transition_tables/prepared_trans_tables/" 

#loop over scenario names creating folders for each in base folder
sapply(Scenario_names, function(x){
  dir.create(paste0(base_trans_table_folder,x), recursive = TRUE)
})

### =========================================================================
### D- Create time dependent naming structure for trans tables 
### =========================================================================

#earliest possible model start time is 1985 and end time is 2060
#we have initially agreed to use 5 year time steps
Scenario_start <- 1985
Scenario_end <- 2060 
Step_length <- 5

#vector sequence of time points and suffix
Time_steps <- paste0("trans_table_", seq(Scenario_start, Scenario_end, Step_length))

#These time points and suffixes can be used to name trans tables in a loop over scenarios

#instantiate function for saving transition tables across time steps
lulcc.savescenariotranstables <- function(Scenario_name, Time_steps, Base_folder, trans_table){
sapply(Time_steps, function(x){
  file_name <- paste0(Base_folder, "/", Scenario_name, "/", Scenario_name, "_", x, ".csv")
  write_csv(trans_table, file = file_name)
})
}

### =========================================================================
### E- Tables for calibration model mode
### =========================================================================




### =========================================================================
### F- Dummy trans tables for simulating BAU
### =========================================================================

#load back in the table of net transition rates for the different time periods
#in the calibration period
calibration_table <- read.csv("Data/Transition_tables/trans_rates_table_calibration_periods.csv")

#exclude any rows with NAs in the 2009_2018 columns as these are transitions
#that are not modelled in the future
calibration_table <- calibration_table[!is.na(calibration_table$X2009_2018),]

#calculate average net transition rate over historical period
calibration_table$Rate <- rowMeans(calibration_table[c("X1985_1997", "X1997_2009", "X2009_2018")], na.rm = TRUE)

#subset to just the columns necessary for Dinamica
Dummy_BAU_trans_table <- calibration_table[c("From.", "To.", "Rate")]

#save a copy of the dummy table for each time point in the sequence
lulcc.savescenariotranstables(Scenario_name = "BAU",
                              Time_steps = Time_steps,
                              trans_table = Dummy_BAU_trans_table,
                              Base_folder = base_trans_table_folder)

### =========================================================================
### G- Trans tables for BIOPRO
### =========================================================================
#identify current areal coverage of each LULC class to be modelled

#load percentage increases in each class according to scenario

#calculate cumalative percentage increase required to go from starting amount
#to desired end amount over prescribed number of time steps

#divide % increase per time point according to the transitions that
#contribute to increases in that class according to relative contributions
#of each i.e. % of total changes in calibration period due to particular transition

#Save results using function above either do it in wide DF format and
#loop across columns or do it in long format and subset DF to time point
