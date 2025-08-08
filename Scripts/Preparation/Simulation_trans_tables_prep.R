#############################################################################
## Transition_table_prep: Creating tables of transition rates for future time points
## for use in LULCC allocation within Dinamica EGO
## Date: 22-09-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

Simulation_start <- 2020
Scenario_end <- 2100
Step_length <- 5
Period_years <- c("2060", "2100")

# Future extrapolations dir
Future_extrapolations_dir <- "Data/Transition_tables/Future_extrapolations"
# Create a directory for future extrapolations if it doesn't exist
if (!dir.exists(Future_extrapolations_dir)) {
  dir.create(Future_extrapolations_dir, recursive = TRUE)
}

### =========================================================================
### B- Updating scenario specification expectations of LULC coverage  
### =========================================================================

# Load inputs of scenario specific expectations of future LULC class areas 
lulc_exp_areas <- readxl::read_excel("Tools/NCCS_simulation_LULC_areas.xlsx")

# test changing all shapes to Constant change
lulc_exp_areas$chosen_shape <- "Constant change"

# currently the values of glacial area in lulc_exp_areas come from
# the expert evaluation process and need to be replaced with the modelled values from
# Zekollari et al. The years that need to be replaced are the init_area (Simulation_start) and the other years in Period_years 
Glacier_update_years <- c(Simulation_start, Period_years)

# for the purpose of updating the glacier areas change the name of init_area to also include the simulation start year
colnames(lulc_exp_areas)[colnames(lulc_exp_areas) == "init_area"] <- paste0("final_area_", Simulation_start)

# remove the string Final_area_
for(scenario in Scenario_names){
  
  scenario <- Scenario_names[1]
  
  #load glacier index
  Glacier_index <- readRDS(paste0("Data/Glacial_change/Scenario_indices/", scenario, "_glacial_change.rds"))
  
  # loop for the Glacier_update_years
  for(year in Glacier_update_years){
    
    #get the number of glacier cells in the index
    Glacier_ncells <- length(which(Glacier_index[[year]]==1))
    
    #get current glacier value
    Proj_glacier_ncell <-  lulc_exp_areas[lulc_exp_areas$Scenario == scenario & lulc_exp_areas$LULC == "Glacier", paste0("final_area_", year)]
  
    #calculate difference between expected and modelled glacier area
    ncells_diff <- Glacier_ncells-Proj_glacier_ncell
  
    #update glacier value in table
    lulc_exp_areas[lulc_exp_areas$Scenario == scenario & lulc_exp_areas$LULC == "Glacier", paste0("final_area_", year)] <- Glacier_ncells
  
    # Get area of static class to add the difference
    Proj_static <- lulc_exp_areas[lulc_exp_areas$Scenario == scenario & lulc_exp_areas$LULC == "Static", paste0("final_area_", year)]
    
    #if the difference between the modelled glacier amount and the project amount
    #is >0 then reduce the amount of static by the difference
    if(ncells_diff >0){
      lulc_exp_areas[lulc_exp_areas$Scenario == scenario & lulc_exp_areas$LULC == "Static", paste0("final_area_", year)] <- Proj_static - abs(ncells_diff)  
    }else if(ncells_diff <0){
      #if the difference between the modelled glacier amount and the project amount
      #is <0 then increase the amount of static by the difference 
      lulc_exp_areas[lulc_exp_areas$Scenario == scenario & lulc_exp_areas$LULC == "Static", paste0("final_area_", year)] <- Proj_static + abs(ncells_diff) 
    }
  }
}

# change the column name back to init_area
colnames(lulc_exp_areas)[colnames(lulc_exp_areas) == paste0("final_area_", Simulation_start)] <- "init_area"

# load table of multistep trans rates
trans_rates <- read.csv("Data/Transition_tables/trans_rates_table_calibration_periods_SS.csv")
trans_rates <- trans_rates[, 5:10]

# remove all X from column names
colnames(trans_rates) <- str_remove_all(colnames(trans_rates), "X")

# rename columns Initial_LULC to iLULC
trans_rates <- trans_rates |>  dplyr::rename("iLULC" = "Initial_class",
                                      "jLULC" = "Final_class")

# vector columns containing rates from different time periods
rate_cols <- c("1985_1997", "1997_2009", "2009_2018")

# For each transition pair (iLULC -> jLULC), determine the min and max observed/modelled rates
trans_rates <- trans_rates %>%
 dplyr:: rowwise() %>% # Process row by row
  dplyr::mutate(
    minRate = min(c_across(all_of(rate_cols)), na.rm=TRUE), # Minimum rate across different sources
    maxRate = max(c_across(all_of(rate_cols)), na.rm=TRUE)  # Maximum rate across different sources
  ) %>%
  ungroup() %>%
  select(iLULC, jLULC, minRate, maxRate) # Keep relevant columns

# Load Model lookup table to use as a list of viable transitions in each region
Model_lookup <- readxl::read_excel("Tools/Model_lookup.xlsx")

# Given that for this study we are not differentiating transition rates between regions
# Filter for unique values of Trans_ID
viable_trans <- Model_lookup[!duplicated(Model_lookup$Trans_ID), c("Initial_LULC", "Final_LULC")]
viable_trans$trans <- paste0(viable_trans$Initial_LULC, "_", viable_trans$Final_LULC)

# seperate the rows from trans_rates where the initial and final LULC are the same
persistence_rows <- trans_rates[trans_rates$iLULC == trans_rates$jLULC, ]

# now subset the remaining rows to only those that are viable
trans_rates$trans <- paste0(trans_rates$iLULC, "_", trans_rates$jLULC)
viable_trans_rates <- trans_rates[trans_rates$trans %in% viable_trans$trans, ]

# Remove the trans column
viable_trans_rates$trans <- NULL

# Add back the persistence rows
viable_trans_rates <- rbind(viable_trans_rates, persistence_rows)

### =========================================================================
### C- Apply solver function to interpolated LULC areas and transition rates
### =========================================================================

# run function to extrapolate lulc area, transitions areas and transition rates
#over multiple periods, scenarios and regions
lulcc.solvemultiplefutureperiods(
  lulc_exp_areas = lulc_exp_areas,
  Step_length = Step_length,
  Simulation_start = Simulation_start,
  Period_years = c("2060", "2100"),
  trans_rates = trans_rates,
  lambda_bounds = 0.1, 
  mu_shape = 15,
  margin = 0.01,
  use_temporal_smoothing_constraint = TRUE,
  mu_temporal_smoothness = 100,
  output_dir = Future_extrapolations_dir
)

### =========================================================================
### D- Finalising transition rate tables for use in simulation
### =========================================================================

# Load the future extrapolations of transition rates
Future_trans_rates <- read.csv(file.path(Future_extrapolations_dir, "Extrapolated_LULC_transition_rates.csv"))

# remove X from any column names
colnames(Future_trans_rates) <- str_remove_all(colnames(Future_trans_rates), "X")

# remove Region column
Future_trans_rates$Region <- NULL

# load the table of lulc_class_aggregations
Aggregation_scheme <- read_excel(LULC_aggregation_path)

# load the viable transitions table
Viable_transitions <- readRDS("Tools/Viable_transitions_lists.rds")

# subset to the last entry in the list
Viable_transitions <- Viable_transitions[[length(Viable_transitions)]]

# replace the LULC classes with the Class IDs
Future_trans_rates$'From*' <- sapply(Future_trans_rates$LULC_from, function(x) {
  # get the aggregation class for the LULC_to
  unique(Aggregation_scheme$Aggregated_ID[Aggregation_scheme$Class_abbreviation == x])
})
Future_trans_rates$'To*' <- sapply(Future_trans_rates$LULC_to, function(x) {
  # get the aggregation class for the LULC_to
  unique(Aggregation_scheme$Aggregated_ID[Aggregation_scheme$Class_abbreviation == x])
})

# subset Future_trans_rates to LULC_from == "Urban" and LULC_to == "Static"
#Future_trans_rates <- Future_trans_rates[Future_trans_rates$LULC_from == "Urban" & Future_trans_rates$LULC_to == "Static", ]


# add a Trans_ID column by matching on the From* and To* columns to Initial_class and Final_class in Viable_transitions
Future_trans_rates$Trans_ID <- sapply(1:nrow(Future_trans_rates), function(i) {
  # get the From* and To* values
  from_class <- Future_trans_rates$'From*'[i]
  to_class <- Future_trans_rates$'To*'[i]
  
  # find the matching transition in Viable_transitions
  Trans_ID <- Viable_transitions$Trans_ID[
    Viable_transitions$'From.' == from_class & 
    Viable_transitions$'To.' == to_class
  ]
  
  if(length(Trans_ID) == 0){
    return(NA)  # If no match found, return NA
  } else {
    return(Trans_ID)
  }
})

# identify which of the cols are for the time points
time_cols <- colnames(Future_trans_rates)[!colnames(Future_trans_rates) %in% c("From*", "To*", "Scenario", "LULC_to", "LULC_from", "Trans_ID")]

# loop over the scenarios and identify which of the viable transitions are
# missing for each and add rows for them with 0 values in time_cols

# create an empty data frame to store the missing transitions
consistent_trans_extraps <- data.frame(
  'From*' = character(),
  'To*' = character(),
  Scenario = character(),
  LULC_to = character(),
  LULC_from = character(),
  Trans_ID = character()
)

# add the time columns to the data frame
for(time_col in time_cols){
  consistent_trans_extraps[[time_col]] <- numeric()
}
 

names(consistent_trans_extraps) 
for(scenario in unique(Future_trans_rates$Scenario)){
  
  # get the subset of entries for the scenarios
  scenario_subset <- Future_trans_rates[Future_trans_rates$Scenario == scenario, ]
  
  # get the unique combinations of From* and To* for the scenario
  scenario_trans <- paste0(scenario_subset$LULC_from, "_", scenario_subset$LULC_to)
  
  missing_trans <- Viable_transitions[!Viable_transitions$Trans_name %in% scenario_trans, ]
  
  # create a new data frame with the missing transitions and 0 values for time
  # columns
  
  if(nrow(missing_trans) != 0){
    missing_trans_df <- data.frame(
      From = missing_trans$'From.',
      To = missing_trans$'To.',
      Scenario = scenario,
      LULC_to = missing_trans$Final_class,
      LULC_from = missing_trans$Initial_class,
      Trans_ID = missing_trans$Trans_ID
      )
  
    # rename the From and To columns to match
    names(missing_trans_df) <- c("From*", "To*", "Scenario", "LULC_to", "LULC_from", "Trans_ID")
    
    # loop over the time columns and add 0 values
    for(time_col in time_cols){
      missing_trans_df[[time_col]] <- 0  # Set all time columns to 0
    }
    
    # ensure the column order of missing_trans_df matches the scenario_subset
    missing_trans_df <- missing_trans_df[, names(consistent_trans_extraps)]
  
    
    # add the missing transitions to the consistent transitions
    consistent_trans_extraps <- rbind(consistent_trans_extraps, missing_trans_df)
  }
  
    # add the scenario_subset to the consistent transitions data
    consistent_trans_extraps <- rbind(consistent_trans_extraps, scenario_subset)

}
  
# remove any rows from consistent_trans_extraps where Trans_ID == NA
consistent_trans_extraps <- consistent_trans_extraps[!is.na(consistent_trans_extraps$Trans_ID), ]

#save transition rates tables for each time point for each scenario
for(scenario in Scenario_names){
  
  # create a directory for the scenario if it doesn't exist
  dir.create(paste0(Trans_rate_table_dir,"/", scenario), recursive = TRUE)
  
  # loop over time cols
  for(time_col in time_cols){
    
    #subset to scenario, Trans_id and time column)
    scenario_time_table <- consistent_trans_extraps[consistent_trans_extraps$Scenario == scenario, c("From*", "To*", "Trans_ID", time_col)]
    
    # sort by the transition ID
    scenario_time_table <- scenario_time_table[order(scenario_time_table$Trans_ID), ]
    
    # remove Trans_ID column
    scenario_time_table$Trans_ID <- NULL
    
    # rename time_col to Rate
    names(scenario_time_table)[3] <- "Rate"
    
    # take the first year from the time_col
    file_year <- str_extract(time_col, "\\d{4}")

    #vector file path
    file_name <- file.path(Trans_rate_table_dir, scenario, paste0(scenario, "_trans_table_", file_year, ".csv"))

    #save
    write_csv(scenario_time_table, file = file_name)
  }
} #close loop over scenarios


