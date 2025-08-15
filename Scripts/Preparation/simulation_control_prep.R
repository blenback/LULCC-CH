########################################################################
## Script name: 
## Purpose of script:
## Author: 
## Date Created: 2023-12-11
## Notes:
########################################################################

### =========================================================================
### Preparation
### =========================================================================

## Install and load packages

#vector other required packages
packs<-c("tidyverse")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#load table of uncertain factors
UF_table <- read.csv2("Tools/UF_grid.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

#remove rows where group == "Economic"
#UF_table <- UF_table[UF_table$group != "Economic",]

groups <- unique(UF_table$group)

#extract values of lever for each unique group to a list
UF_list <- lapply(groups, function(x) {
  UF_table$value[UF_table$group == x]
})
names(UF_list) <- groups

#expand grid
UF_grid <- expand.grid(UF_list)

# load simulation control file
simulation_control <- read.csv2("Tools/simulation_control.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# get columns of simulation control file
simulation_control_cols <- names(simulation_control)
print(simulation_control_cols)

# map the column names for the climate economic and population scenarios
col_map <- c("Population" = "Pop_scenario.string",
             "Climate" = "Climate_scenario.string",
             "Economic" = "Econ_scenario.string",
             "LULC demand" = "Scenario_ID.string"
             )

# Use the col_map to swap the names of columns in UF_grid
UF_grid <- UF_grid %>%
  rename_with(~ col_map[.], everything())

# get names of cols not in UF_grid
simulation_control_cols_not_in_UF_grid <- simulation_control_cols[!(simulation_control_cols %in% names(UF_grid))]

# add the missing columns to UF_grid
UF_grid[simulation_control_cols_not_in_UF_grid] <- NA

#for Simulatuion_num. and Simulation_ID.string and EI_ID.string fill columns with seq 1:nrow(UF_grid)
UF_grid$Simulation_num. <- seq(1, nrow(UF_grid))
UF_grid$Simulation_ID.string <- seq(1, nrow(UF_grid))
UF_grid$EI_ID.string <- seq(1, nrow(UF_grid))

# remove these cols from simulation_control_cols_not_in_UF_grid
simulation_control_cols_not_in_UF_grid <- simulation_control_cols_not_in_UF_grid[!(simulation_control_cols_not_in_UF_grid %in% c("Simulation_num.", "Simulation_ID.string", "EI_ID.string"))]

# for the remaining columns in simulation_control_cols_not_in_UF_grid get unique values from Simulation_control
UF_grid[simulation_control_cols_not_in_UF_grid] <- lapply(simulation_control_cols_not_in_UF_grid, function(x) {
  unique(simulation_control[[x]])
})

# set Spatial)interventions.string to "N"
UF_grid$Spatial_interventions.string <- "N"

# set EI_interventions.string to "N"
UF_grid$EI_interventions.string <- "N"

# reorder columns to match simulation_control
UF_grid <- UF_grid[, simulation_control_cols]

# check that the unique entries of UF_grid$Econ_scenario.string match those of the column in simulation_control
if (!all(unique(UF_grid$Econ_scenario.string) %in% unique(simulation_control$Econ_scenario.string))) {
  stop("The unique entries of UF_grid$Econ_scenario.string do not match those of simulation_control$Econ_scenario.string")
}
# print none matching entries
non_matching_econ <- setdiff(unique(UF_grid$Econ_scenario.string), unique(simulation_control$Econ_scenario.string))

# write UF_grid to csv called Simulation_control.csv in the project root
write.csv(UF_grid, "Simulation_control.csv", row.names = FALSE, na = "")

Control_table_path <- "Simulation_control.csv"

#load control table 
Control_table <- read.csv(Control_table_path)

#subset to non-completed simulations
Control_table <- Control_table[Control_table$Completed.string == "N",]





