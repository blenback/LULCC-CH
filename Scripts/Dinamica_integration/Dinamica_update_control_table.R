#############################################################################
## Dinamica_update_control_table: Update the control table after completion of simulation 
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

library(readr)

#receive file path for control table 
Control_table_path <- s1

#receive Boolean value of whether simulation was completed successfully
Success <- s2
cat(Success)

#receive row number of current simulation
Simulation_num <- v1
#Simulation_num <- 1

#load control and subset to simulation number
Control_table <- read.csv(Control_table_path)

#update value in completed column for current simulation
if(grepl("TRUE", Success, ignore.case = TRUE)){
Control_table[Control_table$Simulation_num. == Simulation_num,"Completed.string"] <- "Y"
cat("Simulation was completed successfully")
}else{
Control_table[Control_table$Simulation_num. == Simulation_num,"Completed.string"] <- "ERROR"	
cat("Simulation encountered an error")
	}

#save table
readr::write_csv(Control_table, Control_table_path)
