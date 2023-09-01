#############################################################################
## Dinamica_subset_control_table: Subset the control table to remove simulations
## that have already been completed
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

#receive control table 
Control_table <- read.csv(s1)

#subset to non-completed simulations
Control_table <- Control_table[Control_table$Completed.string == "N",]

#Send back subsetted table
outputTable("Remaining_simulations" ,Control_table)