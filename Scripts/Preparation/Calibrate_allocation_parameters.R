#############################################################################
## Calculate_allocation_parameters: Using land use data from calibration (historic) periods 
## to calculate parameters for Dinamica's Patcher and Expander algorithmns
## Mean patch size, patch size variance, patch isometry,
## % of transitons in new patches vs. expansion of existing patches
## Date: 28/09/2022
## Author: Ben Black 
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

#Packages are loaded in the master script, uncomment here for testing
# # Install packages if they are not already installed
# packs<-c("data.table", "raster", "tidyverse","SDMtools", "doParallel","sf",
#          "tiff", "igraph", "readr")
# 
# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# #SDMtools is depreciated and needs to be installed from source
# #packageurl <- "https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz"
# #install.packages(packageurl, repos=NULL, type="source")
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))
# 
# # Source custom functions
invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

#vector years of LULC data
LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#Historic LULC data folder path
LULC_folder <- "Data/Historic_LULC"

#Load list of historic lulc rasters
LULC_rasters <- lapply(list.files(LULC_folder, full.names = TRUE, pattern = ".gri"), raster)
names(LULC_rasters) <- LULC_years

### =========================================================================
### B- Calculating patch size parameters for each historic period
### =========================================================================

#create folders for results
dir.create("Data/Allocation_parameters/Simulation", recursive = TRUE)
dir.create("Data/Allocation_parameters/Calibration/Periodic", recursive = TRUE)

#because each period relies on a different combination of raster layers
#create a vector of these to run through
LULC_change_periods <- c()
for (i in 1:(length(LULC_years)-1)) {
            LULC_change_periods[[i]] <- c(LULC_years[i],LULC_years[i+1])
        }
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))

lulcc.periodicparametercalculation <- function(Raster_combo, Raster_stack, period_name) {
  
#seperate rasters required for time period
yr1 <- Raster_stack[[grep(Raster_combo[1], names(Raster_stack))]] 
yr2 <- Raster_stack[[grep(Raster_combo[2], names(Raster_stack))]]

#load list of transitions 
transitions <- read.csv(list.files("Data/Transition_tables/raw_trans_tables", full.names = TRUE, pattern = paste0(period_name, "_viable_trans")))

#set up cluster for parallel computation
no_cores <- detectCores()-1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

#Loop over transitions
results <- foreach(i = 1:nrow(transitions),.combine = rbind,.packages =c("raster","SDMTools", "igraph")) %dopar%{
  
  #Identify cells in the rasters according to the 'From' and 'To' values
  r1 <- Which(yr1 == transitions[i,c("From.")])
  r2 <- Which(yr2 == transitions[i,c("To.")])
  Final_class_in_yr1 <- Which(yr1 == transitions[i,c("To.")])
  
  #multiply rasters to identify transition cells
  r <- r1*r2

  #identify patches of the final land use class in the yr1 raster
  Final_yr1_patches <- clump(Final_class_in_yr1, directions = 8)
  #convert values (Patch IDs) above 0 to 1 (i.e. binary in patch (1) outside patch (0))
  Final_yr1_patches[Final_yr1_patches >0] <- 1
  Final_yr1_patches[is.na(Final_yr1_patches[])] <- 0

  #identify patches of the final land use class in the yr2 raster
  Final_yr2_patches <- clump(r2, directions = 8)
  #convert values (Patch IDs) above 0 to 2 (i.e. binary in patch (10) outside patch (0))
  Final_yr2_patches[Final_yr2_patches >0] <- 10
  Final_yr2_patches[is.na(Final_yr2_patches[])] <- 0
 
  #Add rasters together so that:
  #0= not in patch in either year
  #1 = in patch in year 1
  #10 = in patch in year 2 only (i.e. new patch)
  #11 = in patch in both years 
  yr1_yr2_patches <- Final_yr1_patches+Final_yr2_patches
  
  #Multiply this raster by the raster of transition cells to select only
  #the transitoon cells in year 2 patches
  Trans_cells_yr_patches <- r*yr1_yr2_patches
  
  #test to see which 10 valued cells are adjacent to patch cells in yr1
  #i.e. they represent expansion and not new patches. 

  #create a new raster to store results in
  expansion_or_new <- Trans_cells_yr_patches 

  #Get cell numbers for those in patches in yr2 only
  patchcells <- which(values(Trans_cells_yr_patches) == 10)

  #loop over cells in patchs 
  for (cell in patchcells){
    #get the cell numbers of adjacent cells
    ncells <- adjacent(Trans_cells_yr_patches, cell=cell, direction=8,include=F,pairs=F)
    
    #sum up the values in the adjacent cells in the year 1 patches,
    #a value of >=1 indicates that the new patch cells are directly adjacent to
    #old patch cells and hence represent expansion
    #a value of <1 indicates no adjacent old patch cells and hence new patch
    Y_N <- if(sum(Final_yr1_patches[ncells], na.rm=T) >= 1){10} else{5}

    expansion_or_new[cell] <- Y_N
  }

  #calculate the proportions of transition cells
  #that exist in new patches vs. expansion
  perc_expander <- freq(expansion_or_new, value = 10)/freq(r, value = 1)*100
  perc_patcher <- freq(expansion_or_new, value = 5)/freq(r, value = 1)*100
  
  #Calculate class statistics for patchs in rasters
  cl.data <- ClassStat(r, bkgd=0, cellsize= res(r)[1])
  
  #Mean patch area
  mpa <- cl.data$mean.patch.area/10000
  
  #Standard Deviation patch area
  sda <- cl.data$sd.patch.area/10000
  
  #Patch Isometry
  iso <- cl.data$aggregation.index/70
  
  #Combine results
  result <- c(transitions[i,1], transitions[i,2], mpa, sda, iso, perc_expander, perc_patcher)
return(result)
}
stopCluster(cl)  

#convert to DF
results <- as.data.frame(results)

#better to save seperate tables for the patch related parameters vs.
#the % expansion params to eliminate the need to seperate when loading into Dinamica

#Adjust col names
colnames(results) <- c("From*","To*"," Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry", "Perc_expander", "Perc_patcher")

#save
write_csv(results, file = paste0("Data/Allocation_parameters/Calibration/Periodic/Allocation_parameters_", period_name, ".csv"))

return(results)
}

#Apply function
Allocation_params_by_period <- mapply(lulcc.periodicparametercalculation, Raster_combo = LULC_change_periods,
                                        period_name = names(LULC_change_periods),
                                        MoreArgs = list(Raster_stack = LULC_rasters), 
                                        SIMPLIFY = FALSE)

### =========================================================================
### C- Creating patch size parameter tables for calibration 
### =========================================================================

#Whilst we have estimated values of percentage of transitions corresponding to
#expansion of existing patches vs. occurring in new patches, we don't know how
#accurate these are so it is desirable to perform calibration by monte-carlo
#simulation using random permutations of these values

#IMPORTANT
#For Dinamica the % expansion values must be expressed as decimals
#so they are converted in this loop

#First create a lookup table to control looping over the simulations
Calibration_control_table <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(Calibration_control_table) <- c("Simulation_num.",
                                          "Scenario_ID.string",
                                          "Simulation_ID.string",
                                          "Model_mode.string",
                                          "Scenario_start.real",
                                          "Scenario_end.real",
                                          "Step_length.real",
                                          "Parallel_TPC.string",
                                          "Spatial_interventions.string",
                                          "Deterministic_trans.string",
                                          "Completed.string")

#reload allocation parameter tables
Allocation_params_by_period <- lapply(list.files("Data/Allocation_parameters/Calibration/Periodic", full.names = TRUE), read.csv)
names(Allocation_params_by_period) <- Data_periods

#reloading also causes r to mess up the column names, readjust
#Adjust col names
Allocation_params_by_period <- lapply(Allocation_params_by_period, function(x) {
  colnames(x) <- c("From*","To*"," Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry", "Perc_expander", "Perc_patcher")
  x$Perc_expander <- x$Perc_expander/100
  x$Perc_patcher <- x$Perc_patcher/100
  return(x)
  })

#because we are most interested in calibrating the patch size values for the
#time period that will be used in simulations we will run the calibration using
#the params from the most recent data period
#we have initially agreed to use 5 year time steps
Scenario_start <- 2010
Scenario_end <- 2020 
Step_length <- 5

#vector sequence of time points and suffix
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

#seperate vector of time points into those relevant for each calibration period
Time_points_by_period <- lapply(Data_periods, function(period){
  dates <- as.numeric(str_split(period, "_")[[1]])
  period_years <- Time_steps[sapply(Time_steps, function(year){
    if(year > dates[1] & year <= dates[2]){TRUE}else if((Scenario_end-dates[2]) < Step_length)
      {TRUE}else{FALSE}
    })]
})
names(Time_points_by_period) <- Data_periods

#remove any time periods that are empty
Time_points_by_period <- Time_points_by_period[lapply(Time_points_by_period,length)>0] 

#subset the list of allocation params tables by the names of the time_periods
Allocation_params_by_period <- Allocation_params_by_period[names(Time_points_by_period)]

#create seperate files of the estimated allocation parameters for each time point
#under the ID: v1

#loop over the list of years for each time point saving a copy of the
#corresponding parameter table foreach one
sapply(1:length(Time_points_by_period), function(period_indices){
  sapply(Time_points_by_period[[period_indices]], function(x){
  file_name <- paste0(Calibration_param_dir, "/", "v1", "/Allocation_param_table_", x, ".csv")
  write_csv(Allocation_params_by_period[[period_indices]], file = file_name)
  })
})

#create a sequence of names for the number of monte-carlo simulations
mc_sims <- sapply(seq(2, 100,1), function(x) paste0("v", x))

#loop over the mc_sim names and perturb the allocation params
#saving a table for every time point in the calibration period
sapply(mc_sims, function(Sim_name){

#inner loop over time periods and parameter tables  
mapply(function(Time_steps, Calibration_param_dir, param_table){
  
#create folder for saving param tables for MC sim name
dir.create(paste0(Calibration_param_dir, "/", Sim_name), recursive = TRUE)
  
#random perturbation of % expander 
#(increase of decrease value by random amount in normal distribution with mean = 0 and sd = 0.05 effectively 5% bounding)  
param_table$Perc_expander <- param_table$Perc_expander + rnorm(length(param_table$Perc_expander), mean = 0, sd = 0.05)

#if any values are greater than 1 or less than 0 then set to these values
param_table$Perc_expander <- sapply(param_table$Perc_expander, function(x){
checked_val <- if(x > 1){x <- 1}else if(x < 0){0}else{x}
})

#recalculate % patcher so that total does not exceed 1 (100%)
param_table$Perc_patcher <- 1-param_table$Perc_expander 

#inner loop over individual time points  
sapply(Time_steps, function(x){
  file_name <- paste0(Calibration_param_dir, "/", Sim_name, "/Allocation_param_table_", x, ".csv")
  write_csv(param_table, file = file_name)
}) #close loop over time points
  }, Time_steps = Time_points_by_period,
     Calibration_param_dir = Calibration_param_dir,
     param_table = Allocation_params_by_period,
SIMPLIFY = FALSE) #close loop over time periods

}) #close loop over simulation IDs. 

#Now add entries for these MC simulations into the calibration control table
#add v1 to mc_sims
mc_sims <- c("v1",mc_sims)

#add rows for MC sim_names
for(i in 1:length(mc_sims)){
    Calibration_control_table[i, "Simulation_ID.string"] <- mc_sims[i]
    Calibration_control_table[i, "Simulation_num."] <- i
  }

#fill in remaining columns
Calibration_control_table$Scenario_ID.string <- "CALIBRATION"
Calibration_control_table$Scenario_start.real <- Scenario_start
Calibration_control_table$Scenario_end.real <- Scenario_end
Calibration_control_table$Step_length.real <- Step_length
Calibration_control_table$Model_mode.string <- "Calibration"
Calibration_control_table$Parallel_TPC.string <- "N"
Calibration_control_table$Completed.string <- "N"
Calibration_control_table$Spatial_interventions.string <- "N"
Calibration_control_table$Deterministic_trans.string <- "N"

#save table
readr::write_csv(Calibration_control_table, "Tools/Calibration_control.csv")

### =========================================================================
### D- Perform simulation for calibration 
### =========================================================================

#Perform pre-check to make sure that all element required for Dinamica modelling
#are prepared
Control_table_path <- paste0(getwd(),"/Tools/Calibration_control.csv")
Pre_check_result <- lulcc.modelprechecks(Control_table_path, Param_dir = Calibration_param_dir)

#run the dinamica model with the calibration table
if(Pre_check_result == TRUE){

#Read in Model.ego file
Model_text <- try(readLines("Model/Dinamica_models/LULCC_CH.ego"))

#Replace dummy string for working directory path path
Model_text <- str_replace(Model_text, "=====WORK_DIR=====", getwd())

#Replace dummy string for control table file path
Model_text <- str_replace(Model_text, "=====TABLE_PATH=====", Control_table_path)

print('Creating a copy of the Dinamica model using the current control table')
#save a temporary copy of the model.ego file to run
Temp_model_path <- gsub(".ego", paste0("_calibration_", Sys.Date(), ".ego"), "Model/Dinamica_models/LULCC_CH.ego")
writeLines(Model_text, Temp_model_path)

#vector a path for saving the output text of this simulation
#run which indicates any errors
output_path <- paste0(Sim_log_dir,"/calibration_output_", Sys.Date(), ".txt")

print('Starting to run model with Dinamica EGO')
#Use a system command to run the Dinamica model
system2(command = paste(DC_path),
        #args = c("-processors 10","-memory-allocation-policy 1", Temp_model_path),
        args = c("-disable-parallel-steps",Temp_model_path),
        wait = TRUE,
        stdout= output_path,
        stderr = output_path)

#because the simulations may fail without the system command returning an error 
  #(if the error occurs in Dinamica) then check the simulation control table to see
  #if/how many simulations have failed
  Updated_control_tbl <- read.csv(Control_table_path)
  
  if(any(Updated_control_tbl$Completed.string == "ERROR")){
    print(paste(length(which(Updated_control_tbl$Completed.string == "ERROR")), "of", nrow(Updated_control_tbl),
                 "simulations have failed to run till completion, check simulation output .txt file for details of errors"))
    #slackr_bot('Simulation has stopped because of error')
  }else{
    #Send completion message
    #slackr_bot('Simulation completed sucessfully')
    print('All simulations completed sucessfully')
  
    #Delete the temporary model file
    #unlink(Temp_model_path)
    
    #clean up log and debug files created by Dinamica as their output
    #is stored in the .txt file anyway
    unlink(list.files(pattern = paste0(c("log_","debug_"),collapse="|"), full.names = TRUE))
    
  }
}else{
  print("Some elements required for modelling are not present/incorrect,
        consult the pre-check results object")}

### =========================================================================
### E- Evaluate calibration, selecting best parameter set
### =========================================================================

#load the similarity values produced from the validation process inside Dinamica
#for each simulation
Calibration_results <- lapply(list.files("Results/Validation", full.names = TRUE, recursive = TRUE, pattern = ".csv"), function(x) read.csv(x, header = FALSE)) 
names(Calibration_results) <- sapply(list.files("Results/Validation", full.names = FALSE, recursive = TRUE, pattern = ".csv"), function(x) str_split(x, "_")[[1]][2])

#remove thhe list item that has summary in it's name
Calibration_results <- Calibration_results[!grepl("summary", names(Calibration_results))]

#bind the list of dataframes into a single dataframe
Calibration_results <- rbindlist(Calibration_results, idcol = "Sim_ID")

#rename the similarity score column
names(Calibration_results)[2] <- "Similarity_score"

#summary statistics
Calibration_summary <- data.frame(Mean = mean(Calibration_results$Similarity_score),
                                  SD = sd(Calibration_results$Similarity_score),
                                  Min = min(Calibration_results$Similarity_score),
                                  Max = max(Calibration_results$Similarity_score),
                                  n = length(Calibration_results$Similarity_score))

#clean column names of summary statistics
colnames(Calibration_summary) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")

#save summary statistics
readr::write_csv(Calibration_summary, "Results/Validation/Validation_summary.csv")

#save copy to publication dir
#readr::write_csv(Calibration_summary, "publication/figures_tables/Validation_summary.csv")

#select best performing simulation_ID
Best_sim_ID <- Calibration_results[which.max(Calibration_results$Similarity_score),]$Sim_ID

#Use this sim ID to create parameter tables for all simulation time points
#in the Simulation folder

#get exemplar table
param_table <- read.csv(list.files(paste0(Calibration_param_dir, "/", Best_sim_ID), full.names = TRUE, pattern = "2020"))
colnames(param_table) <- c("From*","To*"," Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry", "Perc_expander", "Perc_patcher")                   

#save a copy for the publication
#readr::write_csv(param_table, "publication/figures_tables/Optimal_param_table.csv")

#get simulation start and end times from simulation control table
Simulation_control <- read.csv(Sim_control_path)
Simulation_start <- min(Simulation_control$Scenario_start.real)
Simulation_end <- max(Simulation_control$Scenario_end.real)
Scenario_IDs <- unique(Simulation_control$Scenario_ID.string)

#loop over scenario IDs and simulation time points creating allocation param tables
sapply(Scenario_IDs, function(y){
sapply(seq(Simulation_start, Simulation_end, Step_length), function(x){
  save_dir <- paste0(Simulation_param_dir, "/", y)
  dir.create(save_dir, recursive = TRUE)
  file_name <- paste0(save_dir, "/Allocation_param_table_", x, ".csv")
  write_csv(param_table, file = file_name)
})  
})



