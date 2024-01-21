#############################################################################
## Dinamica_update_control_table: Update the control table after completion of simulation 
## Date: 01-10-2022
## Author: Ben Black
#############################################################################

#Load readr package
library(readr)

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
