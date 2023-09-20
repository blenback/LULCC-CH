#############################################################################
## Generate_Expander_Patcher_Matrices: Using land use data from calibration (historic) periods 
## to calculate parameters for Dinamica's Patcher and Expander algorithmns
## Mean patch size, patch size variance, patch isometry
## Date: 28/09/2022
## Author: Jaime Burbano Girón edited by Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#navigate to the working directory in the files pane for easy viewing
rstudioapi::filesPaneNavigate(wpath)

# Install packages if they are not already installed
packs<-c("data.table", "raster", "tidyverse","SDMtools", "doParallel","sf", "tiff")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

#SDMtools needs to be installed from source
packageurl <- "https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#vector years of LULC data
LULC_years <- c("1985", "1997", "2009", "2018")

#Dataframe of LULC labels and values
LULC_classes <- data.frame(label = c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier"),
                           value = c(10,11,12,13,14,15,16,17,18,19))

#Historic LULC data folder path
LULC_folder <- "Data/Historic_LULC"

#Load list of historic lulc rasters
LULC_rasters <- lapply(list.files(LULC_folder, full.names = TRUE, pattern = ".gri"), raster)
names(LULC_rasters) <- LULC_years

### =========================================================================
### B- Calculating patch size parameters for each historic period
### =========================================================================

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
transitions <- read.csv(list.files("E:/LULCC_CH/Data/Transition_tables/raw_trans_tables", full.names = TRUE, pattern = paste0(period_name, "_viable_trans")))

#set up cluster for parallel computation
no_cores <- detectCores()-1  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

#Loop over transitions
results <- foreach(i = 1:nrow(transitions),.combine = rbind,.packages =c("raster","SDMTools")) %dopar%{
  
  #Identify cells in the rasters according to the 'From' and 'To' values
  r1 <- Which(yr1 == transitions[i,c("From.")])
  r2 <- Which(yr2 == transitions[i,c("To.")])
  
  #multiply rasters
  r <- r1*r2
  
  #Calculate class statistics for patchs in rasters
  cl.data <- ClassStat(r, bkgd=0, cellsize= res(r)[1])
  
  #Mean patch area
  mpa <- cl.data$mean.patch.area/10000
  
  #Standard Deviation patch area
  sda <- cl.data$sd.patch.area/10000
  
  #Patch Isometry
  iso <- cl.data$aggregation.index/70
  
  #Combine results
  result <- c(transitions[i,1], transitions[i,2], mpa, sda, iso)
  return(result)
}
stopCluster(cl)  

#convert to DF
results <- as.data.frame(results)

#Adjust col names
colnames(results) <- c("From*","To*"," Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry")

#save
write_csv(results, file = paste0("Data/Allocation_parameters/Periodic/Allocation_parameters_", period_name, ".csv"))
return(results)
}

#Apply function
Allocation_params_by_period <- mapply(lulcc.periodicparametercalculation, Raster_combo = LULC_change_periods,
                                        period_name = names(LULC_change_periods),
                                        MoreArgs = list(Raster_stack = LULC_rasters), 
                                        SIMPLIFY = FALSE)

### =========================================================================
### C- Creating patch size parameter tables for simulations
### =========================================================================

#earliest possible model start time is 1985 and end time is 2060
#we have initially agreed to use 5 year time steps
Scenario_start <- 1985
Scenario_end <- 2060 
Step_length <- 5

#vector sequence of time points and suffix
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

#seperate vector of time points into those relevant for each calibration period
Time_points_by_period <- list("1985_1997" = Time_steps[Time_steps <= 1997],
             "1997_2009" = Time_steps[Time_steps >= 1997 & Time_steps <= 2009],
             "2009_2018" = Time_steps[Time_steps >= 2009 & Time_steps <= 2060])

mapply(function(Time_steps, Base_folder, param_table){
sapply(Time_steps, function(x){
  file_name <- paste0(Base_folder, "/Allocation_param_table_", x, ".csv")
  write_csv(param_table, file = file_name)
})}, Time_steps = Time_points_by_period,
     Base_folder = "Data/Allocation_parameters/Simulation",
     param_table = Allocation_params_by_period,
SIMPLIFY = FALSE)
