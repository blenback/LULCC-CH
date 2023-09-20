#############################################################################
## Transition_identification: Using land use data from calibration (historic) periods to identify
## LULC transitions to be modeled in future simulations 
## 
## Date: 28/09/2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# Install packages if they are not already installed
#packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat", "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", "rgeos", "sf", "tiff")

#new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

#if(length(new.packs)) install.packages(new.packs)

# Load required packages
#invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
#invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#Historic LULC data folder path
LULC_folder <- "Data/Historic_LULC"

#vector years of LULC data
LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files(LULC_folder, full.names = FALSE, pattern = ".gri"))

#Dataframe of LULC labels and values
Aggregation_scheme <- read_excel(LULC_aggregation_path)
LULC_classes <- data.frame(label = unique(Aggregation_scheme$Class_abbreviation))
LULC_classes$value <- sapply(LULC_classes$label, function(x){unique(Aggregation_scheme[Aggregation_scheme$Class_abbreviation == x, "Aggregated_ID" ])})

#vector directory
trans_rates_dir <- "Data/Transition_tables/raw_trans_tables"
dir.create(trans_rates_dir, recursive = TRUE)

#Vector duration of time steps to be used in modelling
#Provided from master script
#Step_length <- 5

### =========================================================================
### B- Calculate historic areal change for LULC classes
### =========================================================================

#load LULC rasters
LULC_rasters <- lapply(list.files(LULC_folder, full.names = TRUE, pattern = ".gri"), raster)
names(LULC_rasters) <- LULC_years

#create a table of raw areal coverage of each LULC class in each time point
LULC_areas <- lapply(LULC_rasters, function(x) freq(x))

#Reduce to single table
LULC_areal_change <- Reduce(function(x, y) merge(x, y, by="value"), LULC_areas)
names(LULC_areal_change)[2:5] <- names(LULC_areas)

#remove row summing lulc class values
LULC_areal_change <- LULC_areal_change[1:(nrow(LULC_areal_change)-1),]

#update values to LULC class names
RAT <- LULC_rasters[["1985"]]@data@attributes[[1]]
LULC_areal_change$LULC_class <- sapply(LULC_areal_change$value, function(x) RAT[RAT$Pixel_value == x,"lulc_name"]) 

#create workbook to save in
write.csv(LULC_areal_change, paste0(trans_rates_dir,"/LULC_historic_areal_change.csv"), row.names = FALSE)

### =========================================================================
### C- Preparing Transition tables for each calibration period
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
#need to be named according to their row number so the value is associated correctly in allocation. 

#create a list combining the correct LULC years for each transition period
LULC_change_periods <- c()
for (i in 1:(length(LULC_years)-1)) {
            LULC_change_periods[[i]] <- c(LULC_years[i],LULC_years[i+1])
        }
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))

#instantiate function to produce raw, single step and multistep 
#net percentage transition tables and save each to file 
lulcc.periodictransmatrices <- function(Raster_combo, Raster_stack, period_name, Step_length) {
  
#produce transition matrix of areal changes over whole period
trans_rates_for_period <- crosstab(Raster_stack[[grep(Raster_combo[1], names(Raster_stack))]], 
                      Raster_stack[[grep(Raster_combo[2], names(Raster_stack))]])

#converting the transition matrix values into percentage changes
sum01 <- apply(trans_rates_for_period, MARGIN=1, FUN=sum)
percchange <- sweep(trans_rates_for_period, MARGIN=1, STATS=sum01, FUN="/")
options(scipen = 999)
perchange_for_period <- as.data.frame(percchange)

#alter column names to match Dinamica trans tables
colnames(perchange_for_period) <- c("From*", "To*", "Rate")

#convert columns to numeric
perchange_for_period$'From*' <- as.numeric(as.character(perchange_for_period$'From*'))
perchange_for_period$'To*' <- as.numeric(as.character(perchange_for_period$'To*'))  

#exclude LULC persistences (i.e. non-transitions) 
perchange_for_period <- perchange_for_period[!perchange_for_period$From == perchange_for_period$To,]

#remove any rows with a value of zero
perchange_for_period <- perchange_for_period[!perchange_for_period$Rate == 0,]

#sort by initial class (From) value
perchange_for_period <- perchange_for_period[order(perchange_for_period$From),]

#save this 'single step' transition table
write_csv(perchange_for_period, paste0(trans_rates_dir, "/Calibration_", period_name, "_singlestep_trans_table.csv"))

#Convert to Multistep matrix
#calculate number of time steps in period
Period_length <- as.numeric(Raster_combo[2])- as.numeric(Raster_combo[1])
Num_steps <- ceiling(Period_length/Step_length)

perchange_for_period_multistep <- perchange_for_period
perchange_for_period_multistep$Rate <- perchange_for_period_multistep$Rate/Num_steps

#save multi step transition table
write_csv(perchange_for_period_multistep, paste0(trans_rates_dir,"/Calibration_", period_name, "_multistep_trans_table.csv"))
}

#Run function
Transition_matrices_by_period <- mapply(lulcc.periodictransmatrices, 
                                        Raster_combo = LULC_change_periods,
                                        period_name = names(LULC_change_periods),
                                        MoreArgs = list(Raster_stack = LULC_rasters,
                                                        Step_length = Step_length), 
                                        SIMPLIFY = FALSE)


### =========================================================================
### C- subsetting to viable transitions
### =========================================================================

#vector inclusion threshold (minimum % of total transitions deemed acceptable for modelling)
#Provided from master script
#Inclusion_thres <- 0.5

#Load single-step net transition tables produced for historic periods
Calibration_singlestep_tables <- lapply(list.files(trans_rates_dir, full.names = TRUE, pattern = "singlestep"), read.csv)
names(Calibration_singlestep_tables) <- str_remove_all(list.files(trans_rates_dir, full.names = FALSE, pattern = "singlestep"), paste(c("Calibration_", "_singlestep_trans_table.csv"), collapse = "|"))

#Add columns to the tables with the LULC class names to make them easier to interpret
Viable_transitions_by_period_SS <- lapply(Calibration_singlestep_tables, function(x){
  x$Initial_class <- sapply(x$From., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Final_class <- sapply(x$To., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Trans_name <- paste(x$Initial_class, x$Final_class, sep = "_")
  
  #subset by inclusion threshold
  x <- x[x$Rate*100 >= Inclusion_thres,]
  
  #subset by transitions from static class
  x <- x[x$Initial_class != "Static",]
  
  x$Trans_ID <- sprintf("%02d", 1:nrow(x))
  return(x)
  })

#save viable transitions lists
saveRDS(Viable_transitions_by_period_SS, "Tools/Viable_transitions_lists.rds")

### =========================================================================
### D- Combining transition rates tables for calibration periods to use for extrapolation
### =========================================================================

#perform process for single step trans table first
Trans_tables_bound_SS <- rbindlist(Viable_transitions_by_period_SS, idcol = "Period")
Trans_tables_bound_SS$Trans_ID <- NULL
Trans_table_time_SS <- pivot_wider(data = Trans_tables_bound_SS, names_from = "Period", values_from = "Rate")

#save
write.csv(Trans_table_time_SS, "Data/Transition_tables/trans_rates_table_calibration_periods_SS.csv")

#Repeat for the multi-step transition matrices

#Load multi-step net transition tables produced for historic periods
Calibration_multistep_tables <- lapply(list.files(trans_rates_dir, full.names = TRUE, pattern = "multistep"), read.csv)
names(Calibration_multistep_tables) <- str_remove_all(list.files(trans_rates_dir, full.names = FALSE, pattern = "multistep"), paste(c("Calibration_", "_multistep_trans_table.csv"), collapse = "|"))

#Add columns to the tables with the LULC class names to make them easier to interpret
Viable_transitions_by_period_MS <- lapply(Calibration_multistep_tables, function(x){
  x$Initial_class <- sapply(x$From., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Final_class <- sapply(x$To., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Trans_name <- paste(x$Initial_class, x$Final_class, sep = "_")
  
  #subset by transitions from static class
  x <- x[x$Initial_class != "Static",]
  
  x$Trans_ID <- sprintf("%02d", 1:nrow(x))
  return(x)
  })

#because the same inclusion threshold does not apply to the multi-step rates
#instead subset by the trans names in the single step equivalent table
single_step_table <- read.csv("Data/Transition_tables/trans_rates_table_calibration_periods_SS.csv")
trans_names <- single_step_table[!is.na(single_step_table$X2009_2018), "Trans_name"]
Viable_transitions_by_period_MS <- lapply(Viable_transitions_by_period_MS, function(x){
filtered_table <- x[x$Trans_name %in% trans_names,]   
})

#remove added columns and save as individual csv. files as exemplar trans tables
#to be loaded into dinamica 
mapply(function(trans_table, table_name){
  
  #remove columns
  trans_table[, c("Trans_ID", "Trans_name", "Initial_class", "Final_class")] <- list(NULL)
  
  #save
  write_csv(trans_table, file= paste0(trans_rates_dir,"/Calibration_", table_name, "_viable_trans.csv"))},
         trans_table = Viable_transitions_by_period_MS,
         table_name = names(Viable_transitions_by_period_MS)
  )

#merge the trans_tables for possible extrapolation of rates over time
Trans_tables_bound_MS <- rbindlist(Viable_transitions_by_period_MS, idcol = "Period")
Trans_tables_bound_MS$Trans_ID <- NULL
Trans_table_time_MS <- pivot_wider(data = Trans_tables_bound_MS, names_from = "Period", values_from = "Rate")

#save
write.csv(Trans_table_time_MS, "Data/Transition_tables/trans_rates_table_calibration_periods_MS.csv")









