#############################################################################
## Dinamica_modify_param_table: Modify the table of parameters used for the allocation algorithm  
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

#subset to columns required for %expander input
Expander_table <- Param_table[, c("From", "To", "Perc_expander")]

#subset to columns required for patch generation input
Patch_table <- Param_table[,!(colnames(Param_table) %in% c("Perc_expander", "Perc_patcher"))]


