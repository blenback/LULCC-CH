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

LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#Vector time Data_periods for calibration
#Inherit from master
#Data_periods <- c("1985_1997", "1997_2009", "2009_2018")

#base folder for creating scenario specific folders
#inherit from master
#Trans_rate_table_dir <- "Data/Transition_tables/prepared_trans_tables"

#The model lookup table specifies which transitions are modelled and
#should be used to subset the transition rates tables

#Load Model lookup tables for each period and subset to just transition names
Periodic_trans_names <- lapply(Data_periods, function(Period){
  full_table <- read.xlsx("Tools/Model_lookup.xlsx", sheetIndex = Period)
  trans_names <- unique(full_table[["Trans_name"]])
  })
names(Periodic_trans_names) <- Data_periods

#identify final year of calibration periods
Final_calib_year <- max(c(sapply(names(Periodic_trans_names), function(x)as.numeric(str_split(x, "_")[[1]]), simplify = TRUE)))

#load simulation control table
Sim_control_table <- read.csv(Sim_control_path)

#vector all time steps in calibration and simulation
All_time_steps <- seq(min(LULC_years), max(Sim_control_table$Scenario_end.real), Step_length)

### =========================================================================
### B- Create folder structure for scenario specific trans rates tables
### =========================================================================

#use simulation control table to get names of Scenarios
Scenario_names <- unique(Sim_control_table[["Scenario_ID.string"]])

#loop over scenario names creating folders for each in base folder
sapply(Scenario_names, function(x){
  dir.create(paste0(Trans_rate_table_dir,"/", x), recursive = TRUE)
})

### =========================================================================
### C- Interpolate/Extrapolate transition rates until 2060 as a reference
### for scenario modifications
### =========================================================================

#Extrapolation of transition rates should be done using the multistep transition
#rates tables for the calibration Data_periods because the step length matches
#that of the future time points (5 years). But in some cases extrapolating
#based on the single step may also be desirable. 

#instantiate function to do extrapolation of trans rates and plotting
lulcc.transratesextrapolation <- function(trans_rate_table_path,
                                          single_multi_step,
                                          base_dir,
                                          time_steps){

#vector save dir and create
save_dir <- paste0(base_dir, "/", single_multi_step)  
dir.create(save_dir, recursive = TRUE)

#read in trans rate table
trans_rate_table <- read.csv(trans_rate_table_path)

#Identify columns for all data periods and final period
Period_cols <- grep(paste(Data_periods, collapse ="|"), colnames(trans_rate_table), value = TRUE)
Final_period_col <- grep(Data_periods[length(Data_periods)], colnames(trans_rate_table), value = TRUE)


#exclude any rows with NAs in the 2009_2018 columns as these are transitions
#that are not modelled in the future
trans_rate_table <- trans_rate_table[!is.na(trans_rate_table[[Final_period_col]]),]

#calculate average net transition rate over historical period
trans_rate_table$Rate <- rowMeans(trans_rate_table[Period_cols], na.rm = TRUE)


#extrapolating rates for future time steps
#rename period columns to single date to create an interval variable
names(trans_rate_table)[names(trans_rate_table) %in% Period_cols] <- c(sapply(Data_periods, function(x)as.numeric(str_split(x, "_")[[1]][2]), simplify = TRUE))


#pivot to long format
trans_rates_long <- trans_rate_table %>% pivot_longer(cols = paste(sapply(Data_periods, function(x)as.numeric(str_split(x, "_")[[1]][2]), simplify = TRUE)),
                                                            names_to = "Year",
                                                            values_to = "Perc_rate")
trans_rates_long$Year <- as.numeric(trans_rates_long$Year)

#log transformation of rate to ensure zero bounded values
trans_rates_long$Perc_rate_log <- log(trans_rates_long$Perc_rate)

#create a df from these used for predicting
Pred_df <- as.data.frame(time_steps)
names(Pred_df) <- "Year"

#create a duplicate table for storing extrapolated values with a column of
#unique Trans_IDs and a column for each simulation step
Extrap_calibration_rates <- data.frame(matrix(nrow = length(unique(trans_rates_long$Trans_name)),
                                                                   ncol = length(time_steps)+1))
colnames(Extrap_calibration_rates) <- c("Trans_name", time_steps)
Extrap_calibration_rates$Trans_name <- unique(trans_rate_table$Trans_name)

#loop over unique trans_IDs, create a linear model for the perc_rate
#and use it to predict future time points

#upper loop over Trans_names
for(Name in unique(trans_rates_long$Trans_name)){

#create model
Mod <-   lm(formula = Perc_rate ~ Year,
     data = trans_rates_long[trans_rates_long$Trans_name == Name,])

#create model with log transformed response variable
#Mod_log <-   lm(formula = Perc_rate_log ~ Year,
#     data = trans_rates_long[trans_rates_long$Trans_name == Name,])

#use 
#predict
Pred <- predict(Mod, newdata = Pred_df)

#Pred_glm <- exp(predict(Mod_log, newdata = Pred_df)) 

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
Combined_table <- cbind(trans_rate_table, Extrap_calibration_rates[,c(paste(time_steps))])
Combined_table$Rate <- NULL

#save
write.xlsx(Combined_table, paste0(save_dir, "/Extrapolated_trans_rates_", single_multi_step, ".xlsx"))
return(Combined_table)
} #close function

#vector directory used by function
Extrap_dir <- "Data/Transition_tables/Extrapolations" 

#Apply function to multi-step tables
MS_extrap_trans_rates <- lulcc.transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_periods_MS.csv",
                        single_multi_step = "multi_step",
                        base_dir = Extrap_dir,
                        time_steps = All_time_steps)

#Replace any negative values with the last >0 rate
#because negative rate is not possible
MS_extrap_trans_rates[MS_extrap_trans_rates < 0] <- NA

fillInTheBlanks <- function(S) {
  L <- !is.na(S)
  c(S[L][1], S[L])[cumsum(L)+1]
}

MS_extrap_trans_rates[,paste(All_time_steps)] <- t(apply(MS_extrap_trans_rates[,paste(All_time_steps)],1,function(x) {
  return_vec <- unlist(fillInTheBlanks(x))
  }))
  

#Apply to single step if desired
#SS_extrap_trans_rates <- lulcc.transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_Data_periods.csv",
#                        single_multi_step = "single_step",
#                       base_dir = Extrap_dir)

### =========================================================================
### D- Saving trans rates tables for calibration periods
### =========================================================================

#instantiate function for subsetting and saving transition tables across time steps
lulcc.savescenariotranstables <- function(Scenario_name,
                                          Time_steps,
                                          Base_folder,
                                          trans_rate_table,
                                          Periodic_trans_names){
    #Loop over time steps
    sapply(Time_steps, function(year){
  
    #identify what time period the current year is from
      
    #if statement based on whether current year is less than the most
    #recent year contained in the data periods names
    
    #if yes then identify which historic period it belongs too
    #if no then use the most recent data period
    if(year < max(c(sapply(names(Periodic_trans_names), function(x)as.numeric(str_split(x, "_")[[1]]), simplify = TRUE)))){
    Time_period <- names(Periodic_trans_names)[sapply(names(Periodic_trans_names), function(x){
    Period_dates <- as.numeric(str_split(x, "_")[[1]])
    between(year, Period_dates[1], Period_dates[2])
    })]} else if (year >= max(c(sapply(names(Periodic_trans_names), function(x)as.numeric(str_split(x, "_")[[1]]), simplify = TRUE)))){
      Time_period <- names(Periodic_trans_names)[length(Periodic_trans_names)]
    }
  
    #subset table to only required transition names
    trans_rate_table <- trans_rate_table[trans_rate_table$Trans_name %in% Periodic_trans_names[[Time_period]],]
  
    #select required columns
    trans_rate_table <- trans_rate_table[,c("From.", "To.", year)]
  
    #clean column names
    names(trans_rate_table) <- c("From*", "To*", "Rate")
  
    #vector file path
    file_name <- paste0(Base_folder, "/", Scenario_name, "/", Scenario_name, "_trans_table_", year, ".csv")
  
    #save
    write_csv(trans_rate_table, file = file_name)
    
  }) #close loop over time_steps
  
  } #close function

#vector time points for calibration period
#for the end date find the multiple of the step length that 
#is closest to the most recent LULC year
Calibration_time_steps <- seq(min(LULC_years), round(as.numeric(max(LULC_years))/Step_length)*Step_length, Step_length)
 
#Apply function to save individual tables
lulcc.savescenariotranstables(Scenario_name = "CALIBRATION",
                              Time_steps = Calibration_time_steps,
                              trans_rate_table = MS_extrap_trans_rates,
                              Base_folder = Trans_rate_table_dir,
                              Periodic_trans_names = Periodic_trans_names)

### =========================================================================
### E- Calculate expected LULC class areal change until end of simulation
### =========================================================================

#1. Use the extrapolated transition rates and areal coverage of LULC classes in
#final observed time point (2018) to calculate expected LULC areal coverage in 2060
#This can be considered as the reference scenario as even the BAU will be a
#modification from this

#load historic LULC areal change table 
Historic_LULC_area <- read.csv("Data/Transition_tables/raw_trans_tables/LULC_historic_areal_change.csv")

#vector simulation time steps 
#(i.e. only the time steps until the end, omitting the start year)
Sim_time_steps <- All_time_steps[between(All_time_steps,(min(Sim_control_table$Scenario_start.real)+Step_length), max(Sim_control_table$Scenario_end.real))]

#vector all simulation years (i.e. including initial year)
Sim_years <- c(round(as.numeric(max(LULC_years))/Step_length)*Step_length, paste(Sim_time_steps))

#create table to capture cumulative transition areal changes
#same details as extrapolated rates but only for future time points
Trans_areal_increase <- MS_extrap_trans_rates[, !(colnames(MS_extrap_trans_rates) %in% c(paste(setdiff(All_time_steps, Sim_time_steps)), paste(sapply(Data_periods, function(x)as.numeric(str_split(x, "_")[[1]][2]), simplify = TRUE))))]
Trans_areal_increase[,paste(Sim_time_steps)] <- as.numeric(NA)

#create table to capture total area of lulc classes for future time points
LULC_proj_area <- Historic_LULC_area[,c("LULC_class", grep(Final_calib_year, names(Historic_LULC_area), value = TRUE))]
names(LULC_proj_area)[2] <- round(as.numeric(max(LULC_years))/Step_length)*Step_length 
LULC_proj_area[paste(Sim_time_steps)] <- NA

#duplicate table to capture net changes in LULC classes
LULC_net_change <- LULC_proj_area[,colnames(LULC_proj_area) != round(as.numeric(max(LULC_years))/Step_length)*Step_length ]

#outer loop over simulation years
for(i in 1:(length(Sim_years)-1)){
  calc_year <- Sim_years[i+1]
  prev_year <- Sim_years[i]

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

#export LULC_proj_area and LULC_net_change to excel


### =========================================================================
### F- Scenario modification of LULC coverage and transition areas 
### =========================================================================

#This process involves taking the LULC class areas in 2060 for each scenario that
#were derived by experts and disaggregating these areas across the relevant
#class gaining (to) and class shrinking (from) transitions across the simulation
#time steps before converting the transition areas into tables of rates.

#This process is complicated because it is an optimisation problem without 
#a single solution given that it is possible to either increase or decrease
#the rates of transitions and this must be conducted across all LULC classes
#sequentially which can result in negative transition area values which are
#of course impossible.

#The best approach we can devise is to programmatically try to approximate the
#result based on histroical proportions of class gains/vs.losses and the 
#relative contributions of the different transitions to this and then manually
#make appropriate challenges to improve the relation between the projected area
#values and the proscribed area values

#Load inputs of scenario specific modifications to LULC class areas in 2060
Scenario_area_mods <- readxl::read_excel("Tools/simulation_LULC_areas_2060.xlsx")

#Load table of glacial change area per time step under different scenarios
Glacial_area_change <- readxl::read_excel("Tools/Glacial_area_change.xlsx")

#the values of glacial area in the scenario area_mods currently come from
#the expert evaluation process and need to be replaced with the modelled values
for(scenario in Scenario_names){
  
  #load glacier index
  Glacier_index <- readRDS(paste0("Data/Glacial_change/Scenario_indices/", scenario, "_glacial_change.rds"))
  Glacier_ncells <- length(which(Glacier_index[[paste(Scenario_end)]]==1))
  
  #get current glacier value
  Proj_glacier_ncell <-  Scenario_area_mods[Scenario_area_mods$Scenario == scenario , "Glacier"]
  
  #calculate difference
  ncells_diff <- Glacier_ncells-Proj_glacier_ncell
  
  #update glacier value in table
  Scenario_area_mods[Scenario_area_mods$Scenario == scenario , "Glacier"] <- Glacier_ncells
  
  #update static value with difference
  Proj_static <- Scenario_area_mods[Scenario_area_mods$Scenario == scenario , "Static"]
  
  #if the difference between the modelled glacier amount and the project amount
  #is >0 then reduce the amount of static by the difference
  if(ncells_diff >0){
  Scenario_area_mods[Scenario_area_mods$Scenario == scenario , "Static"] <- Proj_static - abs(ncells_diff)  
  }else if(ncells_diff <0){
   #if the difference between the modelled glacier amount and the project amount
  #is <0 then increase the amount of static by the difference 
   Scenario_area_mods[Scenario_area_mods$Scenario == scenario , "Static"] <- Proj_static + abs(ncells_diff) 
  }
  
}

#create a list of tables to capture modified LULC areas under each scenario
Mod_LULC_areas <- LULC_proj_area
Mod_LULC_areas[paste(Sim_time_steps)] <- NA
Mod_area_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_LULC_areas)
names(Mod_area_tables) <- unique(Scenario_area_mods$Scenario)

#the glacier area from 2020 currently comes from the AS data replace this
#with the value from the glacier index and add the difference to the static class

Mod_area_tables <- lapply(names(Mod_area_tables), function(scenario){
  
  #seperate scenario table from list
  Scenario_table <- Mod_area_tables[[scenario]]
  
  #load glacier index
  Glacier_index <- readRDS(paste0("Data/Glacial_change/Scenario_indices/", scenario, "_glacial_change.rds"))
  Glacier_ncells_2020 <- length(which(Glacier_index[["2020"]]==1))
  
  #get current glacier value
  AS_glacier_ncell <-  Scenario_table[Scenario_table$LULC_class == "Glacier", "2020"]
  
  #calculate difference
  ncells_diff <- Glacier_ncells_2020-AS_glacier_ncell
  
  #update glacier value in table
  Scenario_table[Scenario_table$LULC_class == "Glacier", "2020"] <- Glacier_ncells_2020
  
  #count number of static cells
  AS_static <- Scenario_table[Scenario_table$LULC_class == "Static", "2020"]
  
  #if the difference between the modelled glacier amount and the project amount
  #is >0 then reduce the amount of static by the difference
  if(ncells_diff >0){
  Scenario_table[Scenario_table$LULC_class == "Static", "2020"] <- AS_static - abs(ncells_diff)  
  }else if(ncells_diff <0){
  #if the difference between the modelled glacier amount and the project amount
  #is <0 then increase the amount of static by the difference 
  Scenario_table[Scenario_table$LULC_class == "Static", "2020"] <- AS_static + abs(ncells_diff) 
  }
  
  return(Scenario_table)
})
names(Mod_area_tables) <- unique(Scenario_area_mods$Scenario)
rm(Mod_LULC_areas)

#Create tables to capture modified transition areal increases
Mod_trans_areas <- Trans_areal_increase
Mod_trans_areas[paste(Sim_time_steps)] <- NA
Mod_trans_area_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_trans_areas)
names(Mod_trans_area_tables) <- unique(Scenario_area_mods$Scenario)
rm(Mod_trans_areas)

#Create tables to capture modified transition rates
Mod_trans_rates <- MS_extrap_trans_rates[, !(colnames(MS_extrap_trans_rates) %in% c(setdiff(All_time_steps, Sim_years), sapply(Data_periods, function(x)as.numeric(str_split(x, "_")[[1]][2]), simplify = TRUE)))]
Mod_trans_rates[paste(Sim_time_steps)] <- NA
Mod_trans_rate_tables <- lapply(Scenario_area_mods$Scenario, function(x) Mod_trans_rates)
names(Mod_trans_rate_tables) <- unique(Scenario_area_mods$Scenario)
rm(Mod_trans_rates)

Perc_gain_losses <- list()
Final_trans_areas_check <- list()

#loop over scenarios and approximate areas of transitions for each time step
for(Scenario in Scenario_area_mods$Scenario){
  
  #----------------------------------------------------------
  #Step 1: Calculate net and stepwise LULC class areal changes
  #-----------------------------------------------------------

  #Separate scenario % changes
  gross_cover_2060 <- unlist(Scenario_area_mods[Scenario_area_mods$Scenario == Scenario, colnames(Scenario_area_mods) != "Scenario"])

  #because the qualitiative modification of class areas under the scenarios
  #is not necessarily balanced it can lead to the sum of class areas exceeding
  #the size of the study area, hence the net increases need to be re-scaled
  #against the study area size based on their size relative to the 
  #total projected area 
  
  #calculate the total expected area
  Mod_total_area <- sum(gross_cover_2060)
  
  #calculate total available area
  Total_area <- sum(LULC_proj_area[, Sim_years[1]])
  
  #If expected area exceeds the available area then re-scale expected area 
  #values according to class contributions
  if(Mod_total_area > Total_area){
    
    #loop over gross cover values re-scaling according to their 
    #relative proportions of area with respect to the total area
    gross_cover_2060 <- sapply(gross_cover_2060, function(y) {
      value <- y*Total_area/sum(gross_cover_2060)
      
      #dividing by Zero introduces NA's so these must be converted back to zero
      value[is.na(value)] <- 0 
      return(value)
      })
  }

  #net areal increase across classes 
  #(projected class area 2060 - class area in last year of calibration period)
  net_change <- gross_cover_2060-Mod_area_tables[[scenario]][, Sim_years[1]]
  
  #Linear areal increase per timestep to meet expectation
  Step_change <- net_change/(length(Sim_years)-1)
  
  #loop over time steps calculating cumulative increases in areal coverage
  for(x in 1:(length(Sim_years)-1)){
    
    #replace the glacier value in Step_change with the deterministic value
    #for the time step and scenario
    Step_change["Glacier"] <- -(as.numeric(Glacial_area_change[Glacial_area_change$Scenario == Scenario,paste(Sim_years[x+1])]))
    
    Mod_area_tables[[Scenario]][,paste(Sim_years[x+1])] <- Mod_area_tables[[Scenario]][,paste(Sim_years[x])] + Step_change
    }
    
  #-------------------------------------------------------------
  # Step 2 calculate modified areal increases for each transition
  #-------------------------------------------------------------
  
  #this requires a 2-step calculation as it is possible to adjust areal changes
  #by altering the area of class gaining transitions and by altering the area of
  #class shrinking (loss) transitions. 
  
  #This process is different for LULC classes that are increasing in size
  #over the simulation vs. those that are shrinking.
  
  #For expanding LULC classes:
  #Gain transition areas should be increased
  #Loss transition areas should be decreased
  
  #For shrinking LULC classes:
  #Gain transition areas should be decreased
  #loss transition areas should be increased
  
  #In both cases the balance between altering gain vs. loss transitions
  #is based upon the proportional areas of each in the historic data.
  #In addition because there may be multiple gain and loss
  #transitions for the same LULC class and as such the transition area for each
  #is calculated as the relative contribution of the transition to the
  #overall amount of  area transitioning to/from the LULC class using the
  #historic transition areal amount as a % of the required
  #amount of areal change for the LULC class 
  
  #Create duplicate tables for storing trans_areas for gains and losses
  Trans_gains_calc_year <- Mod_trans_area_tables[[Scenario]]
  Trans_gains_calc_year[,paste(Sim_time_steps)] <- 0
  Trans_losses_calc_year <- Mod_trans_area_tables[[Scenario]]
  Trans_losses_calc_year[,paste(Sim_time_steps)] <- 0
  
  #Perform nested loop operation to fill scenario table of Mod_trans_area_tables
  
  #upper loop over time steps
  for(calc_year in paste(Sim_time_steps)){

    #replace the glacier value in Step_change with the deterministic value
    #for the time step and scenario
    Step_change["Glacier"] <- -(as.numeric(Glacial_area_change[Glacial_area_change$Scenario == Scenario,calc_year]))
    
    #Inner loop over the LULC classes
    for(lulc_class in names(net_change)){
    
      #calculate the % balance between gains and losses for this class
      #using the expected transition areas of the reference scenario
      Ref_gain_area <- sum(Trans_areal_increase[Trans_areal_increase$Final_class == lulc_class, calc_year])
      Ref_loss_area <- sum(Trans_areal_increase[Trans_areal_increase$Initial_class == lulc_class, calc_year])
      Perc_gain <- Ref_gain_area/(Ref_gain_area+Ref_loss_area)*100
      Perc_loss <- Ref_loss_area/(Ref_gain_area+Ref_loss_area)*100
      
      #outputs to list for testing
      Perc_gain_losses[[Scenario]][[calc_year]][[lulc_class]][["Perc_gain"]] <- Perc_gain
      Perc_gain_losses[[Scenario]][[calc_year]][[lulc_class]][["Perc_loss"]] <- Perc_loss
    
      #if statement for lulc classes that exhibit positive net change
      #(expanding) in the scenario
      if(net_change[names(net_change) == lulc_class] > 0){
      
        #amount of Step_change to be allocated as gains
        Gain_area <- abs(Step_change[names(Step_change) == lulc_class])/100*Perc_gain
      
        #amount of step change to be allocated as losses
        #loss value needs to be negative 
        Loss_area <- -(abs(Step_change[names(Step_change) == lulc_class])/100*Perc_loss)
        
        #else if the lulc class exhibits net negative change then we need to
        #increase the losses and decrease the gains
        }else if(net_change[names(net_change) == lulc_class] < 0){
      
        #amount of Step_change to reduce gains by
        Gain_area <- -(abs(Step_change[names(Step_change) == lulc_class])/100*Perc_gain)
      
        #amount of step change to be allocated as losses
        Loss_area <- abs(Step_change[names(Step_change) == lulc_class])/100*Perc_loss 
        }
        
        #Alter areal gains across transitions to distribute total required Gain_area
        #subset trans areal increases to gaining transitions
        Gain_trans <- Trans_areal_increase[Trans_areal_increase$Final_class == lulc_class,]
      
        #EXCEPTION: because the amount of Glacial loss is deterministic then
        #any gains to the static class should not be attributed to the transition
        #Glacier -> Static
        
        if(lulc_class == "Static"){
          Gain_trans <- Gain_trans[Gain_trans$Initial_class != "Glacier",]
          
          #update Gain area to remove amount of change due to glacier -> static required 
          Gain_area <- Gain_area-abs(Step_change[names(Step_change) == "Static"])
        }
        
        if(nrow(Gain_trans) >0 & Gain_area != 0){
          
          #inner loop over transitions for gaining class
          for(trans in 1:nrow(Gain_trans)){

          #calculate relative contribution of transition to overall gains
          trans_contrib <- Gain_trans[trans,calc_year]/sum(Gain_trans[,calc_year])
          
          #calculate area of transition as proportion of required Gain_area according to % contribution
          trans_area <- trans_contrib*Gain_area
      
          #add this value to what else might have been calculated previously in the loop
          updated_gain_area <- Trans_gains_calc_year[Trans_gains_calc_year$Trans_name == Gain_trans[trans,"Trans_name"],calc_year] + trans_area
          
          #add to table
          Trans_gains_calc_year[Trans_gains_calc_year$Trans_name == Gain_trans[trans,"Trans_name"],calc_year] <- updated_gain_area 
          
            } #close loop over gains
          } #close if statement for the case of no transitions      
        
        #decrease areal losses across transitions to distribute total required Loss_area
        #subset trans areal increases to shrinking transitions
        Loss_trans <- Trans_areal_increase[Trans_areal_increase$Initial_class == lulc_class,]
    
        if(nrow(Loss_trans) > 0 && Loss_area != 0){
        
          #inner loop over transitions for shrinking class
          for(trans in 1:nrow(Loss_trans)){

            #calculate contribution of trans
            trans_contrib <- Loss_trans[trans,calc_year]/sum(Loss_trans[,calc_year])
      
            #calculate area of trans as proportion of step_change according to % contribution
            trans_area <- trans_contrib*Loss_area
        
            #add this value to what else might have been calculated previously in the loop
            updated_loss_area <- Trans_losses_calc_year[Trans_losses_calc_year$Trans_name == Loss_trans[trans,"Trans_name"],calc_year] + trans_area
      
            #add to the absolute value to the table
            Trans_losses_calc_year[Trans_losses_calc_year$Trans_name == Loss_trans[trans,"Trans_name"],calc_year] <- updated_loss_area
            
            } #close loop over losses
          } #close if statement for the case of no transitions
      
    } #close loop over lulc classes
      
    #Add the gains to the 'losses' (absolute values) to get final transition areas
    Final_trans_areas <- Trans_gains_calc_year[,calc_year] + Trans_losses_calc_year[,calc_year]
  
    Final_trans_areas_check[[Scenario]] <- Final_trans_areas
    
    #replace NAs with 0
    Final_trans_areas[is.na(Final_trans_areas)] <- 0 
    
    #At this point there maybe still some classes that have a negative
    #transition rate in this case we need to do a second pass to offset
    #these losses by increasing the gaining transitions.
    
    #create a duplicate vector to modify
    # Updated_trans_areas <- Final_trans_areas
    # 
    # #loop over the negative trans
    # for(x in which(Final_trans_areas <0)){
    #   
    #   #get area to increase gains by
    #   Gain_offset <- abs(Final_trans_areas[x])
    #   
    #   #identify gaining trans according to their final class
    #   #equaling the initial class of the negative transition
    #   Gain_offset_trans_indices <- which(Trans_areal_increase$Final_class == Trans_areal_increase[x,"Initial_class"])
    #   
    #     #loop over the gaining transitions and calculate what are of the offset
    #     #they should receive according to their relative gain contribution
    #     for(trans in Gain_offset_trans_indices){
    # 
    #         #calculate contribution of trans
    #         trans_contrib <- Trans_areal_increase[trans,calc_year]/sum(Trans_areal_increase[Gain_offset_trans_indices,calc_year])
    #   
    #         #calculate area of trans as proportion of step_change according to % contribution
    #         offset_area <- trans_contrib*Gain_offset
    #         
    #         updated_trans_area <- Final_trans_areas[trans] + offset_area
    # 
    #         Updated_trans_areas[trans] <- updated_trans_area
    #     } #close loop over gaining transitions to offset
    #   }#close loop over negative trans areal values

    #add to table ensuring no negative values
    Mod_trans_area_tables[[Scenario]][,calc_year] <- sapply(Final_trans_areas, function(A) if(A >= 0){A}else{0})
    
  } #close loop over time steps
} # Close loop over scenarios

### =========================================================================
### G- Check transition areas against desired amounts of LULC classes and manually improve
### =========================================================================

#first check of mismatches in areas, identifying synergistic transitions
#that currently have a rate of zero (i.e they have been cancelled out in the 1st approximation)
Mismatch_check <- lulcc.projectedclassareasmismatch(Scenario_names = Scenario_names,
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = Mod_trans_area_tables,
                                                         Scenario_class_area_tables = Mod_area_tables,
                                                          mismatch_thres = 1)

#Loop function using multiple thresholds to see if focusing on the synergistic transitions first helps
Threshold_iterations <- list()
num_reps = 50

for(t in seq(0.1, 5, 0.1)){
Iter_result <- lulcc.improvetransareadistrib(Scenario_class_area_tables = Mod_area_tables,
                                           old_trans_areas = Mod_trans_area_tables,
                                           Sim_time_steps = Sim_time_steps,
                                           reps = num_reps,
                                           mismatch_thres = t)

Threshold_iterations[[paste(t)]] <- Iter_result
}

#reduce results to only total mismatch perc
Thres_results <- sapply(Threshold_iterations, function(x) {
  sapply(x$Mismatch_checks[[num_reps]], function(y) y[["Total_mismatch_perc"]])
})

#collate values of total mismatch perc across all threshold values, repetitions and scenarios
#outer loop over threshold values
Improvement_results <- rbindlist(lapply(Threshold_iterations, function(x) {
  
  #inner loop over repetitions
  rep_percs <- data.frame(sapply(seq(1,num_reps,1), function(i){
    
    #inner loop over scenarios
    sapply(x$Mismatch_checks[[i]], function(y) y[["Total_mismatch_perc"]])
  })) #close reps
  colnames(rep_percs) <- paste(seq(1,num_reps,1))
  
  #identify reps with minimum mismatch perc for each scenario
  Scenario_results <- data.frame(matrix(nrow = nrow(rep_percs),ncol=0))
  Scenario_results$Scenario <- rownames(rep_percs)
  Scenario_results$min_mismatch_perc <- apply(rep_percs, MARGIN = 1, function(x) min(x, na.rm = TRUE))
  Scenario_results$rep_num <- colnames(rep_percs)[apply(rep_percs, MARGIN = 1, FUN = which.min)]
    
return(Scenario_results)
}), idcol = "Threshold") #close thresholds 

#identify best results for each scenario
Scenario_best_results <- rbindlist(lapply(unique(Improvement_results$Scenario), function(x){
  
  #seperate results for scenario
  Scenario_results <- Improvement_results[Improvement_results$Scenario ==x,]
  
  #identify result with minimum value of mismatch %
  Best_result <- Scenario_results[which.min(Scenario_results$min_mismatch_perc),]
}))

#use best results to separate trans_area tables accordingly
Best_trans_area_tables <- lapply(unique(Scenario_best_results$Scenario), function(x){
  Sres <- Scenario_best_results[Scenario_best_results$Scenario == x,]
  Scenario_table <- Threshold_iterations[[Sres$Threshold]][["Area_tables"]][[as.numeric(Sres$rep_num)]][[x]]
})
names(Best_trans_area_tables) <- Scenario_best_results$Scenario

#use best results to separate trans_area tables accordingly
Best_results_mismatch_checks <- lapply(unique(Scenario_best_results$Scenario), function(x){
  Sres <- Scenario_best_results[Scenario_best_results$Scenario == x,]
  Scenario_table <- Threshold_iterations[[Sres$Threshold]][["Mismatch_checks"]][[as.numeric(Sres$rep_num)]][[x]]
})
names(Best_results_mismatch_checks) <- Scenario_best_results$Scenario


#Final check
Final_mismatch_check <- lulcc.projectedclassareasmismatch(Scenario_names = Scenario_names,
                                                         Sim_time_steps = paste(Sim_time_steps),
                                                         Scenario_trans_area_tables = Best_trans_area_tables,
                                                         Scenario_class_area_tables = Mod_area_tables,
                                                         mismatch_thres = 1)
print(sapply(Final_mismatch_check, function(x){x[["Total_mismatch_perc"]]}))

#compare to the ref values for 2060

#save Final_trans_area_tables
saveRDS(Best_trans_area_tables, "E:/LULCC_CH/Data/Transition_tables/CURRENT_BEST_AREA_TABLES.rds")
Best_trans_area_tables <- readRDS("E:/LULCC_CH/Data/Transition_tables/CURRENT_BEST_AREA_TABLES.rds")

### =========================================================================
### H- Use transition areas to calculate Modified transition rates
### =========================================================================

#recalculate table of final class areas for each time step 
#duplicate table to capture net changes in LULC classes
Proj_class_area_tables <- Mod_area_tables

#loop over scenarios
for(Scenario in names(Best_trans_area_tables)){

  Scenario_class_areas <- Proj_class_area_tables[[Scenario]]
  Scenario_trans_areas <- Best_trans_area_tables[[Scenario]]
  
  #loop over simulation years
  for(i in 1:(length(Sim_years)-1)){
    calc_year <- Sim_years[i+1]
    prev_year <- Sim_years[i]

    #loop over LULC classes calculating net areal changes and cumulative change in class area: 
    for(LULC_class in Scenario_class_areas$LULC_class){
  
      #calculate gains
      gain <- sum(Scenario_trans_areas[Scenario_trans_areas$Final_class == LULC_class, calc_year])
    
      #calculate losses
      loss <- sum(Scenario_trans_areas[Scenario_trans_areas$Initial_class == LULC_class, calc_year])
    
      #calculating cumulative LULC areal change
      Scenario_class_areas[Scenario_class_areas$LULC_class == LULC_class, calc_year] <- Scenario_class_areas[Scenario_class_areas$LULC_class == LULC_class, prev_year]+(gain-loss)
      } #close LULC class loop
    } #close loop over time steps
  
  #replace result in list
  Proj_class_area_tables[[Scenario]] <- Scenario_class_areas
} #close loop over scenarios


#check against desired areas
# test <- sapply(names(Proj_class_area_tables), function(x){
#   Proj_area_2060 <- Proj_class_area_tables[[x]][["2060"]]
#   desired_area_2060 <- Mod_area_tables[[x]][["2060"]]
#   Area_mismatch <- desired_area_2060-Proj_area_2060
#   Perc_mismatch <- (Area_mismatch/desired_area_2060)*100
# })
# colSums(abs(test))

Final_trans_rate_tables <- Mod_trans_rate_tables

#loop over scenarios again to calculate transition rates
for(Scenario in names(Best_trans_area_tables)){

    #outer loop over simulation years
    for(Sim_year in 1:(length(Sim_years)-1)){
      
      calc_year <- paste(Sim_years[Sim_year+1])
      prev_year <- paste(Sim_years[Sim_year])
      
      #inner loop over transitions
      for(trans in 1:nrow(Best_trans_area_tables[[Scenario]])){
      
      #vector Initial class of transitions
      Initial_class <- Best_trans_area_tables[[Scenario]][trans, "Initial_class"]
        
      #seperate modified class area table
      Class_areas <- Proj_class_area_tables[[Scenario]][Proj_class_area_tables[[Scenario]][["LULC_class"]] == Initial_class,]
      
      #calculate modified transition rate as: modified transition area as
      #% of modified area of initial class in previous time step
      Trans_rate <- Best_trans_area_tables[[Scenario]][trans, calc_year]/Class_areas[,prev_year]
      
      #Add to table
      Final_trans_rate_tables[[Scenario]][trans, calc_year] <- Trans_rate
      } #close loop over transitions
    } #close loop over simulation years
  
}#close loop over scenarios


#save transition rates tables for each time point for each scenario
for(scenario in 1:length(Final_trans_rate_tables)){

  #seperate table for scenario
  trans_rate_table <- Final_trans_rate_tables[[scenario]]

  #vector scenario name
  Scenario_name <- names(Final_trans_rate_tables)[scenario]

  #loop over Simulation_years
  for(i in paste(Sim_years)){

  #select required columns
  trans_rate_table_subset <- trans_rate_table[,c("From.", "To.", i)]

  #clean column names
  names(trans_rate_table_subset) <- c("From*", "To*", "Rate")

  #vector file path
  file_name <- paste0(Trans_rate_table_dir, "/", Scenario_name, "/", Scenario_name, "_trans_table_", i, ".csv")

  #save
  write_csv(trans_rate_table_subset, file = file_name)
    } #close loop over simulation years

  }#close lapply over scenarios

