#############################################################################
## Dinamica_modify_param_table: Modify the table of parameters used for the allocation algorithm  
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

#Receive parameter table
Param_table <- t1

#subset to columns required for %expander input
Expander_table <- Param_table[, c("From", "To", "Perc_expander")]

#subset to columns required for patch generation input
Patch_table <- Param_table[,!(colnames(Param_table) %in% c("Perc_expander", "Perc_patcher"))]

#Output % Expansion table
#(Note the ',2' parameter is necessary for Dinamica to register that the table has 2 key columns (From and To))
outputTable("Expander_table", Expander_table, 2)
outputTable("Patch_table", Patch_table, 2)
