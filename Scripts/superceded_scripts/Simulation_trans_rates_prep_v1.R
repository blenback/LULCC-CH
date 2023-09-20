#############################################################################
## Transition_table_prep: Creating tables of transition rates for future time points
## for use in LULCC allocation within Dinamica EGO
## Date: 22-09-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
#Vector packages for loading
# packs<-c("data.table", "raster", "tidyverse",
#          "lulcc", "stringr", "readr", "xlsx", "gdata")
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))

#Dataframe of LULC labels and values
LULC_classes <- data.frame(label = c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier"),
                           value = c(10,11,12,13,14,15,16,17,18,19))

LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#Vector time Data_periods for calibration
#Inherit from master
#Data_periods <- c("1985_1997", "1997_2009", "2009_2018")

#The model lookup table specifies which transitions are modelled and
#can be used to subset the transition rates tables

#Load Model lookup tables for each period
Model_lookups <- lapply(Data_periods, function(Period){read.xlsx("Tools/Model_lookup.xlsx", sheetIndex = Period)})
names(Model_lookups) <- Data_periods 

### =========================================================================
### C- Create folder structure for scenario specific trans tables
### =========================================================================

#use simulation control table to get names of Scenarios
Scenario_names <- unique(read.csv("Tools/Simulation_control.csv")[["Scenario_ID.string"]])

#base folder for creating scenario specific folders
base_trans_table_folder <- "Data/Transition_tables/prepared_trans_tables/" 

#loop over scenario names creating folders for each in base folder
sapply(Scenario_names, function(x){
  dir.create(paste0(base_trans_table_folder,x), recursive = TRUE)
})

### =========================================================================
### D- Create time dependent naming structure for trans tables 
### =========================================================================

#earliest possible model start time is 1985 and end time is 2060
#we have initially agreed to use 5 year time steps
Scenario_start <- 1985
Scenario_end <- 2060 
Step_length <- 5

#vector sequence of time points and suffix
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

#These time points and suffixes can be used to name trans tables in a loop over scenarios

#instantiate function for subsetting and saving transition tables across time steps
lulcc.savescenariotranstables <- function(Scenario_name, Time_steps, Base_folder, trans_table){
sapply(Time_steps, function(x){
  
  #subset table
  trans_table <- trans_table[,c("From.", "To.", x)]
  
  #clean column names
  names(trans_table) <- c("From*", "To*", "Rate")
  
  #vector file path
  file_name <- paste0(Base_folder, "/", Scenario_name, "/", Scenario_name, "_trans_table_", x, ".csv")
  
  #save
  write_csv(trans_table, file = file_name)
})
}

### =========================================================================
### F- interpolate/Extrapolate transition rates until 2060 as a reference for scenarios
### =========================================================================

#Extrapolation of transition rates should be done using the multistep transition
#rates tables for the calibration Data_periods because the step length matches
#that of the future time points (5 years). But in some cases extrapolating
#based on the single step may also be desirable. 

#instantiate function to do extrapolation of trans rates and plotting
lulcc.transratesextrapolation <- function(trans_rate_table_path, plot_dir, single_multi_step, base_dir){

save_dir <- paste0(base_dir, "/", single_multi_step)  
dir.create(save_dir, recursive = TRUE)

trans_rate_table <- read.csv(trans_rate_table_path)

#exclude any rows with NAs in the 2009_2018 columns as these are transitions
#that are not modelled in the future
trans_rate_table <- trans_rate_table[!is.na(trans_rate_table$X2009_2018),]

#calculate average net transition rate over historical period
trans_rate_table$Rate <- rowMeans(trans_rate_table[c("X1985_1997", "X1997_2009", "X2009_2018")], na.rm = TRUE)

#extrapolating rates for future time steps
#rename period columns to single date to create an interval variable
names(trans_rate_table)[7:9] <- c("1997", "2009", "2018")

#pivot to long format
trans_rates_long <- trans_rate_table %>% pivot_longer(cols = c("1997", "2009", "2018"),
                                                            names_to = "Year",
                                                            values_to = "Perc_rate")
trans_rates_long$Year <- as.numeric(trans_rates_long$Year)

#log transformation of rate to ensure zero bounded values
trans_rates_long$Perc_rate_log <- log(trans_rates_long$Perc_rate)

Simulation_steps <- Time_steps

#create a df from these used for predicting
Pred_df <- as.data.frame(Simulation_steps)
names(Pred_df) <- "Year"

#create a duplicate table for storing extrapolated values with a column of
#unique Trans_IDs and a column for each simulation step
Extrap_calibration_rates <- data.frame(matrix(nrow = length(unique(trans_rates_long$Trans_name)),
                                                                   ncol = length(Simulation_steps)+1))
colnames(Extrap_calibration_rates) <- c("Trans_name", Simulation_steps)
Extrap_calibration_rates$Trans_name <- unique(trans_rate_table$Trans_name)

#loop over unique trans_IDs, create a linear model for the perc_rate
#and use it to predict future time points

#upper loop over Trans_names
for(Name in unique(trans_rates_long$Trans_name)){

#create model
Mod <-   lm(formula = Perc_rate ~ Year,
     data = trans_rates_long[trans_rates_long$Trans_name == Name,])

#create model with log transformed response variable
Mod_log <-   lm(formula = Perc_rate_log ~ Year,
     data = trans_rates_long[trans_rates_long$Trans_name == Name,])

#use 
#predict
Pred <- predict(Mod, newdata = Pred_df)

Pred_glm <- exp(predict(Mod_log, newdata = Pred_df)) 

#append to results df
Extrap_calibration_rates[Extrap_calibration_rates$Trans_name == Name, c(2:ncol(Extrap_calibration_rates))] <- Pred

#create scatterplot with fitted regression line
Trans_plot <- ggplot(trans_rates_long[trans_rates_long$Trans_name == Name,], aes(x = Year, y = Perc_rate)) + 
  geom_point() +
  stat_smooth(method = "lm")

#save plot
ggsave(filename = paste0(Name, "_", single_multi_step, "_trans_rate_plot.jpg") ,
       plot = Trans_plot,
       device = "jpeg",
       path = save_dir)
}

#combine historical and extrapolated values
Combined_table <- cbind(trans_rate_table, Extrap_calibration_rates[,c(paste(Simulation_steps))])
Combined_table$Rate <- NULL

#save
write.xlsx(Combined_table, paste0(save_dir, "/Extrapolated_trans_rates_", single_multi_step, ".xlsx"))
return(Combined_table)
} #close function

#vector directory used by function
Extrap_dir <- "Data/Transition_tables/Extrapolations" 

#Apply function to multit-step tables
MS_extrap_trans_rates <- lulcc.transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_Data_periods_MS.csv",
                        single_multi_step = "multi_step",
                        base_dir = Extrap_dir)

#Replace any negative values with zero because negative rate is not possible
MS_extrap_trans_rates[MS_extrap_trans_rates < 0] <- 0  

#Apply to single step if desired
#SS_extrap_trans_rates <- lulcc.transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_Data_periods.csv",
#                        single_multi_step = "single_step",
#                       base_dir = Extrap_dir)


### =========================================================================
### E- Calibration trans tables 
### =========================================================================

#Use table of interpolated/extrapolated trans rates to save a trans table
#for each time point in the calibration period for loading into Dinamica

#seperate vector of time points into those relevant for each calibration period
# Time_points_by_period <- list(Period_1985_1997 = Time_steps[Time_steps <= 1997],
#              Period_1997_2009 = Time_steps[Time_steps >= 1997 & Time_steps <= 2009],
#              Period_2009_2018 = Time_steps[Time_steps >= 2009 & Time_steps <= 2020])

#Load calibration trans_tables as a list with the same names
# calibration_trans_tables <- lapply(list.files("Data/Transition_tables/raw_trans_tables", full.names = TRUE, pattern = "viable_trans"), read.csv)
# names(calibration_trans_tables) <- names(Time_points_by_period)

#vector time points for calibration period
Calibration_time_steps <- seq(Scenario_start, Scenario_end, Step_length)
 
#Apply function to save individual tables
lulcc.savescenariotranstables(Scenario_name = "CALIBRATION",
                              Time_steps = Calibration_time_steps,
                              trans_table = MS_extrap_trans_rates,
                              Base_folder = base_trans_table_folder)

### =========================================================================
### G- calculate expected LULC class areal change until 2060
### =========================================================================

#1. Use the extrapolated transition rates and areal coverage of LULC classes in
#final observed time point (2018) to calculate expected LULC areal coverage in 2060
#This can be considered as the reference scenario as even the BAU will be a
#modification from this

#load historic LULC areal change table and subset to final time point
Historic_LULC_area <- read.csv("Data/Transition_tables/raw_trans_tables/LULC_historic_areal_change.csv")

#vector future time step
Future_time_steps <- Time_steps[Time_steps >=2025]

#create table to capture cumulative transition areal changes
#same details as extrapolated rates but only for future time points
Trans_areal_increase <- MS_extrap_trans_rates[, !(colnames(MS_extrap_trans_rates) %in% c(paste(setdiff(Time_steps, Future_time_steps)), "1997", "2009", "2018"))]
Trans_areal_increase[,paste(Future_time_steps)] <- as.numeric(NA)

#create table to capture total area of lulc classes for future time points
LULC_proj_area <- Historic_LULC_area[,c("LULC_class", "X2018")]
names(LULC_proj_area)[2] <- "2020" 
LULC_proj_area[paste(Future_time_steps)] <- NA

#duplicate table to capture net changes in LULC classes
LULC_net_change <- LULC_proj_area[,colnames(LULC_proj_area) != "2020"]

loop_years <- c("2020", paste(Future_time_steps))

#outer loop over time steps
for(i in 1:length(loop_years)){
  calc_year <- loop_years[i+1]
  prev_year <- loop_years[i]

  #inner loop over rows calculating transition areal increases
  for(x in 1:nrow(MS_extrap_trans_rates)){
  Current_initial_class <- MS_extrap_trans_rates[x, "Initial_class"]
  Trans_areal_increase[[x,calc_year]] <- MS_extrap_trans_rates[x,calc_year]*LULC_proj_area[LULC_proj_area$LULC_class == Current_initial_class, prev_year]
  }#close transition areal change loop

  #loop over LULC classes calculating net areal changes and cumulative change in class area: 
  for(LULC_class in LULC_net_change$LULC_class){
  
    #calculate gains
    gain <- sum(Trans_areal_increase[Trans_areal_increase$Final_class == LULC_class, calc_year])
    
    #calculate losses
    loss <- sum(Trans_areal_increase[Trans_areal_increase$Initial_class == LULC_class, calc_year])
    
    #combine and fill table
    LULC_net_change[LULC_net_change$LULC_class == LULC_class, calc_year] <- gain-loss
  
    #calculating cumulative LULC areal change
    LULC_proj_area[LULC_proj_area$LULC_class == LULC_class, calc_year] <- LULC_proj_area[LULC_proj_area$LULC_class == LULC_class, prev_year]+LULC_net_change[LULC_net_change$LULC_class == LULC_class, calc_year]
    
    } #close LULC class loop
}

### =========================================================================
### H- Scenario modification of LULC coverage, transition areas and rates
### =========================================================================

#Load inputs of scenario specific modifications to LULC class areas in 2060
Scenario_area_mods <- readxl::read_excel("Tools/Dummy_scenario_trans_modification.xlsx")

#create a list of tables to capture modified LULC areas
Mod_LULC_areas <- LULC_proj_area
Mod_LULC_areas[paste(Future_time_steps)] <- NA
Mod_area_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_LULC_areas)
names(Mod_area_tables) <- unique(Scenario_area_mods$Scenario)

#Create tables to capture modified transition areal increases
Mod_trans_areas <- Trans_areal_increase
Mod_trans_areas[paste(Future_time_steps)] <- NA
Mod_trans_area_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_trans_areas)
names(Mod_trans_area_tables) <- unique(Scenario_area_mods$Scenario)

#Create tables to capture modified transition rates
Mod_trans_rates <- Trans_areal_increase
Mod_trans_rates[paste(Future_time_steps)] <- NA
Mod_trans_rate_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_trans_rates)
names(Mod_trans_rate_tables) <- unique(Scenario_area_mods$Scenario)

#loop over scenarios
for(i in Scenario_area_mods$Scenario){}
i <- "BAU"
  
  #Separate scenario % changes
  Mod_percs <- unlist(Scenario_area_mods[Scenario_area_mods$Scenario == i, colnames(Scenario_area_mods) != "Scenario"])
  Proj_area_2060 <- LULC_proj_area[, ncol(LULC_proj_area)]
  
  #multiply with 2060 projected areas from ref scenario
  #TO DO: make conditional on whether the class is shrinking or gaining
  #because multiplying a shrinking class that had a percentage increase
  #according to the scenario should not result in a greater degree of shrinkage 
  gross_cover_2060 <- Proj_area_2060+(Proj_area_2060*Mod_percs)
  
  #because the qualitiative modification of class areas under the scenarios
  #is not necessarily balanced it can lead to the sum of class areas exceeding
  #the size of the study area, hence the net increases need to be re-scaled
  #against the study area size based on their size relative to the 
  #total projected area 
  
  Mod_total_area <- sum(gross_cover_2060)
  Total_area <- sum(LULC_proj_area[, loop_years[1]])
  if(Mod_total_area > Total_area){
    #loop over gross cover values re-scaling according to their 
    #relative proportions of area with respect to the total area
    gross_cover_2060 <- sapply(gross_cover_2060, function(y) {
      value <- y*Total_area/sum(gross_cover_2060)
      value[is.na(value)] <- 0 #dividing by Zero introduces NA's so these must be converted back to zero
      return(value)
      })
  }

  #net areal increase (projected area 2060 - area in 2020)
  net_change <- gross_cover_2060-LULC_proj_area[, loop_years[1]]
  
  #Areal increase per timestep
  Step_change <- net_change/(length(loop_years)-1)
  
  #loop over time steps calculating cumulative increases in areal coverage
  for(x in 1:(length(loop_years)-1)){
    Mod_area_tables[[i]][,paste(loop_years[x+1])] <- Mod_area_tables[[i]][,paste(loop_years[x])] + Step_change
    }
    
  #calculate modified areal increases for transitions
  #this requires a 2 step calculation, first for the LULC classes whose areal
  #coverage is increasing we target transitions where this is the final class with
  #the transition area calculated as the relative contribution
  #of the transition to the overall amount of  area transitioning to the final
  #class using the historic transition areal amount as a % of the required
  #amount of areal change for the LULC class 
  
  #secondly for the LULC classes whose area is projected to shrink we target
  #transitions where this is the initial class with the transiton area
  #calculated in the same way above
  
    #duplicate tables for storing trans_areas for gains and losses
  Trans_gains_calc_year <- Mod_trans_area_tables[[i]]
  Trans_gains_calc_year[,paste(Future_time_steps)] <- 0
  Trans_losses_calc_year <- Mod_trans_area_tables[[i]]
  Trans_losses_calc_year[,paste(Future_time_steps)] <- 0
  
  #upper loop over time steps
  for(calc_year in paste(Future_time_steps)){
  
    #1st inner loop over the gaining lulc classes
    for(gain_class in names(net_change[net_change > 0])){
    
      #subset trans areal increases to gaining transitions
      Gain_trans <- Trans_areal_increase[Trans_areal_increase$Final_class == gain_class,]
    
      #inner loop over transitions for gaining class
      for(trans in 1:nrow(Gain_trans)){

        #calculate contribution of trans
        trans_contrib <- Gain_trans[trans,calc_year]/sum(Gain_trans[,calc_year])
      
        #calculate area of trans as proportion of step_change according to % contribution
        trans_area <- trans_contrib*Step_change[names(Step_change)== gain_class]
      
        #add to table
        Trans_gains_calc_year[Trans_gains_calc_year$Trans_name == Gain_trans[trans,"Trans_name"],calc_year] <- trans_area
        } #close loop over gaining trans
    
    } #close loop over gaining classes

    #2nd inner loop over the shrinking lulc classes
    for(loss_class in names(net_change[net_change < 0])){
    
      #subset trans areal increases to gaining transitions
      Loss_trans <- Trans_areal_increase[Trans_areal_increase$Initial_class == loss_class,]
    
      #inner loop over transitions for gaining class
      for(trans in 1:nrow(Loss_trans)){

        #calculate contribution of trans
        trans_contrib <- Loss_trans[trans,calc_year]/sum(Loss_trans[,calc_year])
      
        #calculate area of trans as proportion of step_change according to % contribution
        trans_area <- trans_contrib*Step_change[names(Step_change)== loss_class]
      
        #add to table
        Trans_losses_calc_year[Trans_losses_calc_year$Trans_name == Loss_trans[trans,"Trans_name"],calc_year] <- trans_area
      } #close loop over shrinking trans
    
    } #close loop over shrinking classes
  
    #subtract losses from gains to get final transition areas
    Final_trans_areas <- Trans_gains_calc_year[,calc_year]-Trans_losses_calc_year[,calc_year]
  
    #add to table ensuring no negative values
    Mod_trans_area_tables[[i]][,calc_year] <- sapply(Final_trans_areas, function(A) if(A >= 0){A}else{0})
  
    } #close loop over time steps
  
    #calculate Modified transition rates by looping over loop_years
    for(calc_year in 2:length(loop_years)){
      
      #inner loop over lulc_classes
      for(LULC_class in Mod_area_tables[[i]][["LULC_class"]]){
        
      #modified transition rate = modified transition area as
      #% modified area of initial class in previous timestep  
      Mod_trans_rate_tables[[i]][Mod_trans_rate_tables[[i]][["Initial_class"]] == LULC_class ,loop_years[calc_year]] <- Mod_trans_area_tables[[i]][Mod_trans_area_tables[[i]][["Initial_class"]] == LULC_class,loop_years[calc_year]]/Mod_area_tables[[i]][Mod_area_tables[[i]][["LULC_class"]] == LULC_class, loop_years[(calc_year-1)]] 
      }
    }
  
#add in the trans rates columns from the historic Data_periods
#tO DO CHANGE TO BE ALL OF THE TIME STEPS INSTEAD OF THE LULC YEARS
#SUBSET BY COLUMN NAME BE CAREFUL TO USE PASTE() FOR NUMERICS
Mod_trans_rate_tables[[i]] <- cbind(Mod_trans_rate_tables[[i]], MS_extrap_trans_rates[,c(LULC_years[!LULC_years == "1985"], "2020")])
  
#save transition rates table
sapply(Time_steps, function(x){
  file_name <- paste0(Base_folder, "/", Scenario_name, "/", Scenario_name, "_trans_table_", x, ".csv")
  write_csv(trans_table, file = file_name)
})

#} close loop over scenarios


#TO DO: potential removing any transitions with zero rates
  #because they may cause error with Dinamica




### OLD PLAN FOR PROCESS:
#1. identify current areal coverage of each LULC class to be modelled
#using raster from 2018

#2. load percentage increases in each class according to scenario
#create a csv table that specifies this

#3. calculate cumulative percentage increase required to go from starting amount
#to desired end amount over prescribed number of time steps

#4. use existing multi-step transition rates tables to calculate the relative percentages
#of total transitions to a given LULC class individual transitions are responsible for

#5. divide value of cumulative % increase per time point according to relative % contributions of transitions that
#contribute to increases in that class. 

#6. Some LULC classes we are not specifying increases in under scenarios but the cumulative increases in transitions
#should not exceed the amount of total land area available
#check total projected area of land under increases expressed in scenarios (use LULC raster)
#re-scale rates of transitions in other classes so that total land area is not exceeded. 

#Save results using function above either do it in wide DF format and
#loop across columns or do it in long format and subset DF to time point

### =========================================================================
### X- Create dummy tables using the rates provided by Sven-Erik
### =========================================================================

#ALTERNATIVE: Load in the BAU trans table produced by SR
Dummy_BAU_trans_table_SR <- read_csv("Data/Transition_tables/BAU_transition_SR.csv")
Dummy_BAU_trans_table_SR$ID <- paste(Dummy_BAU_trans_table_SR$`From*`, Dummy_BAU_trans_table_SR$`To*`, sep = "_")

#subset Sven-Eriks table
Output_table <- Dummy_BAU_trans_table_SR[which(Dummy_BAU_trans_table_SR$ID %in% Dummy_BAU_trans_table$ID),]
Output_table <- Output_table[order(Output_table$`From*`),]
Output_table$ID <- NULL

#adjust column names in trans tables to include the '*' necessary for Dinamica to recognise them as key columns
names(Dummy_BAU_trans_table)[names(Dummy_BAU_trans_table) == "From."] <- "From*"
names(Dummy_BAU_trans_table)[names(Dummy_BAU_trans_table) == "To."] <- "To*"

#save a copy of the dummy table for each time point in the sequence
lulcc.savescenariotranstables(Scenario_name = "BAU",
                              Time_steps = Time_steps,
                              trans_table = Output_table,
                              Base_folder = base_trans_table_folder)
















