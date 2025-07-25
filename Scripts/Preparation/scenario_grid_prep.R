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


### =========================================================================
### Prepare grid of simulation specs
### =========================================================================

#load table of levers
lever_table <- read.csv2("Tools/lever_grid.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)

#name columns 'group' and 'lever'
names(lever_table) <- c("group", "lever")

#get unique groups
groups <- unique(lever_table$group)

#extract values of lever for each unique group to a list
lever_list <- lapply(groups, function(x) {
  lever_table$lever[lever_table$group == x]
})
names(lever_list) <- groups

#expand list entries to grid
lever_grid <- expand.grid(lever_list)

### =========================================================================
### Add additional info required for simulation control table
### =========================================================================

# get column names from Tools/Simulation_control.csv
sim_control_colnames <- names(read.csv2("Tools/Simulation_control.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE))

# subset to those not in lever_grid
sim_control_colnames <- sim_control_colnames[!(sim_control_colnames %in% names(lever_grid))]
print(paste("The following columns are not in the lever grid:", paste(sim_control_colnames, collapse = ", ")))

sim_ctrl <- lever_grid
sim_ctrl$'Simulation_num.' <- seq(1, nrow(sim_ctrl), 1) # add simulation number
sim_ctrl$'Simulation_ID.string' <- seq(1, nrow(sim_ctrl), 1)
sim_ctrl$'Model_mode.string' <- "Simulation"
sim_ctrl$'Scenario_start.real' <- 2020
sim_ctrl$'Scenario_end.real' <- 2060
sim_ctrl$'Step_length.real' <- 5
sim_ctrl$'Parallel_TPC.string' <- "N"
sim_ctrl$'Spatial_interventions.string' <- "Y"
sim_ctrl$'EI_interventions.string' <- "N"
sim_ctrl$'Deterministic_trans.string' <- "Y"
sim_ctrl$'Completed.string' <- "N"
sim_ctrl$'EI_ID.string' <- seq(1, nrow(sim_ctrl), 1)

# check that all columns are present
missing_cols <- sim_control_colnames[!(sim_control_colnames %in% names(sim_ctrl))]
if (length(missing_cols) > 0) {
  stop(paste("The following columns are missing from the simulation control table:", paste(missing_cols, collapse = ", ")))
} else {
  print("All columns are present in the simulation control table.")
}

# save simulation control table
write.csv2(sim_ctrl, "Tools/Simulation_control.csv", row.names = FALSE, fileEncoding = "UTF-8")

