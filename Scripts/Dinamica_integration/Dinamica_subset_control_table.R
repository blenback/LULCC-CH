#############################################################################
## Dinamica_subset_control_table: Subset the control table to remove simulations
## that have already been completed
## Date: 01-10-2022, 16-11-2023
## Author: Ben Black, Carlson Büth
#############################################################################


#load control table 
Control_table <- read.csv(Control_table_path)

#subset to non-completed simulations
Control_table <- Control_table[Control_table$Completed.string == "N",]


