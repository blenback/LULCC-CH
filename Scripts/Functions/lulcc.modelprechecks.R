#############################################################################
## LULCC_model_pre_checks: function to check that all elements required
## for simulation with Dinamica are prepared  
## Date: 03-11-2022
## Author: Ben Black
#############################################################################
#'
#' @param Control_table_path Chr, file path of calibration/simulation control table
#'
#' @author Ben Black
#' @export
#' 

lulcc.modelprechecks <- function(Control_table_path, Param_dir){

#For testing purposes
#Control_table_path <- "Tools/Simulation_control.csv"
#Param_dir <- "Data/Allocation_parameters/Simulation"

#load table
Simulation_table <- read.csv(Control_table_path)

#Get model mode
Model_mode <- unique(Simulation_table$Model_mode.string)
  
#Get earliest simulation start date and latest end date
Scenario_start <- min(Simulation_table$Scenario_start.real)
Scenario_end <- max(Simulation_table$Scenario_end.real) 

#Enter duration of time step for modelling
Step_length <- unique(Simulation_table$Step_length.real)
  
#use start and end time to generate a lookup table of dates seperated by 5 years 
model_time_steps <- list( Keys = c(seq(Scenario_start,
                                       Scenario_end,
                                       Step_length)),
                          Values = c(seq((Scenario_start +5),
                                         (Scenario_end+5),
                                         Step_length)))
#Unique scenario names 
Scenario_IDs <- unique(Simulation_table$Scenario_ID.string)

#create list to catch errors 
Model_pre_checks <- list()

### =========================================================================
### A- Check that all simulation details are present (i.e complete rows)
### =========================================================================

if(all(complete.cases(Simulation_table)) == FALSE){
  Model_pre_checks <- list.append(Model_pre_checks, 
                                list(Message ="Some rows in the simulation table
                                     are missing entries in columns",
                                     Result = Simulation_table[!complete.cases(Simulation_table),]))}


### =========================================================================
### B- Check that not all simulations are already completed
### =========================================================================

if(nrow(Simulation_table[Simulation_table$Completed.string == "N",]) == 0){
  Model_pre_checks <- list.append(Model_pre_checks, 
                                list(Message ="All simulations are indicated as 
                                     already completed please revise table",
                                     Result = Simulation_table[Simulation_table$Completed.string == "Y",]))}

### =========================================================================
### C- Check that simulation start and end dates are appropriate for model mode
### =========================================================================

#Only need to check for calibration because simulation doesn't matter
#what time start or end is 
Model_mode_correct <- apply(Simulation_table, 1, function(x){
  if(grepl("calibration", x[["Scenario_ID.string"]], ignore.case = TRUE) &
     x[["Scenario_start.real"]] >2018 &
     x[["Scenario_end.real"]] >2020){FALSE} else{TRUE}
})
if(all(Model_mode_correct) == FALSE){
  Model_pre_checks <- list.append(Model_pre_checks, 
                                list(Message ="Some simulations have an 
                                     inappropriate model mode choice given their
                                     start and end dates, see result for details",
                                     Result = Model_mode_correct))}

### =========================================================================
### D- Transition matrix checks 
### =========================================================================

#Two checks need to be made:
#1. That there are sufficient transition matrices for the time steps specified
#2. That the matrices contain all of the required transitions
#if not abort.

#Check 1: 
#Loop over unique scenario IDs and time steps to create file paths of scenario specific transition tables
Scenario_transition_matrix_files <- unlist(lapply(Scenario_IDs, function(ID){
 generic_path <- paste0("Data/Transition_tables/prepared_trans_tables/", ID, "/", ID, "_", "trans_table", "_") 
 Time_step_paths <- sapply(model_time_steps$Keys, function(y) paste0(generic_path, y, ".csv"))
})) 

#run through list and check that all files exist return a vector of TRUE/FALSE
#and evaluate if all are TRUE
All_trans_matrixs_exist <- sapply(Scenario_transition_matrix_files, function(x) file.exists(x))
  
if (all(All_trans_matrixs_exist) == FALSE) {
Model_pre_checks <- list.append(Model_pre_checks, 
                                list(Message ="Transition tables are missing, 
                                     see result for details",
                                     Result = All_trans_matrixs_exist))}

#Check 2:
#loop over the files checking that they include the same From/To class values
#as the viable trans list
Trans_tables_correct <- sapply(Scenario_transition_matrix_files,
                               function(trans_table_path){

#extract numeric
Table_year <- as.numeric(gsub(".*?([0-9]+).*", "\\1", trans_table_path)) 

#vector time period
Period <- if (Table_year <= 1997) {"1985_1997"} else if (Table_year > 1997 & Table_year <= 2009) {
  "1997_2009"} else if (Table_year > 2009) {
      "2009_2018"}

#load the list of viable transitions 
viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[[Period]]

#add a from_to column
viable_trans_list$From_To <- paste(viable_trans_list$From., viable_trans_list$To., sep = "_")

#load the transition table    
trans_table <- read.csv(trans_table_path)
trans_table_from_to <- paste(trans_table$From., trans_table$To., sep = "_")

#check that from_to column is the same 
identical(viable_trans_list$From_To, trans_table_from_to)
})
if (all(Trans_tables_correct == FALSE)) {
Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Transition tables do not match the 
                                transitions to be modelled, see result for details",
                                Result = Trans_tables_correct))}

### =========================================================================
### E- Historic LULC data check
### =========================================================================

#vector historic lulc file paths
Obs_LULC_paths <- list.files("Data/Historic_LULC", full.names = TRUE, pattern = ".gri")

#extract numerics
Obs_LULC_years <- unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", Obs_LULC_paths)))

#loop over simulation start times and identify closest start years
Start_years <- sapply(Simulation_table$Scenario_start.real, function(x){
Min_diff <- abs(Obs_LULC_years[base::which.min(abs(Obs_LULC_years - x))]-x)
  })
names(Start_years) <- Simulation_table$Simulation_ID.string

if(any(between(Start_years, 0,5)) == FALSE){
Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Warning: Some simulations have start 
                                dates which are greater than 5 years from the 
                                closest observed LULC data",
                                Result = Start_years
                                ))}

### =========================================================================
### F- Allocation parameter checks
### =========================================================================

#check if all allocation parameter tables exist and
#contain the correct transitions, if not abort.

#gather all required allocation param table file paths
if(grepl("simulation", Model_mode, ignore.case = TRUE)){
Param_table_paths <- sapply(unique(Simulation_table$Scenario_ID.string), function(ID){
table_paths <- sapply(model_time_steps$Keys, function(x){
    str_replace(paste0(Param_dir, "/", ID, "/Allocation_param_table_<v1>.csv"), "<v1>", paste(x))
})
})
} else if(grepl("calibration", Model_mode, ignore.case = TRUE)){
Param_table_paths <- c(sapply(unique(Simulation_table$Simulation_ID.string), function(Sim_ID){
Params_path <- paste0(Param_dir, "/", Sim_ID, "/Allocation_param_table_<v1>.csv")

#loop over time steps
Time_step_paths <- sapply(model_time_steps$Keys, function(x){
str_replace(Params_path, "<v1>", paste(x))  
})
}))
}
  
#loop over the param table paths and check that all files exist return a vector
#of TRUE/FALSE and evaluate if all are TRUE
All_param_tables_exist <- sapply(Param_table_paths, function(Param_table_path)file.exists(Param_table_path))
  if (all(All_param_tables_exist) == FALSE) {
    Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Allocation parameter tables are 
                                missing, see result for details",
                                Result = All_param_tables_exist))}

#Check 2: loop over the file checking that they include the same From/To 
#class values as the viable trans list
Param_tables_correct <- sapply(Param_table_paths, function(Param_table_path){
  
  #extract numeric (because there simulation ID also contains
  #a numeric select the maximum value to capture the year)
  Table_year <- max(as.numeric(unlist(stringr::str_extract_all(Param_table_path, "\\d+"))))

  #vector time period
  Period <- if (Table_year <= 1997) {"1985_1997"} else if (Table_year > 1997 & Table_year <= 2009) {
  "1997_2009"} else if (Table_year > 2009) {
      "2009_2018"}

  #load the list of viable transitions 
  viable_trans_list <- readRDS("Tools/Viable_transitions_lists.rds")[[Period]]
  
  #add from_to column
  viable_trans_list$From_To <- paste(viable_trans_list$From., viable_trans_list$To., sep = "_")
  
  #Load param table
  Param_table <- read.csv(Param_table_path)
  
  #extract from_to values and compare to transitions list
  Param_table_from_to <- paste(Param_table$From., Param_table$To., sep = "_")
  identical(viable_trans_list$From_To, Param_table_from_to)
  })

  if (all(Param_tables_correct) == FALSE) {
        Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Allocation parameter tables do not 
                                match the transitions to be modelled, 
                                see result for details",
                                Result = Param_tables_correct))}

### =========================================================================
### G- Check model look up table  
### =========================================================================

#Check 1: Model look up table contains all of the transition IDs present
#in the viable trans lists and that all models in the table exist if not abort.

#get names of transition lists (the same as model lookup tables)
viable_trans_lists <- readRDS("Tools/Viable_transitions_lists.rds")
Period_names <- names(viable_trans_lists)
names(Period_names) <- Period_names

#loop over all model periods
All_trans_have_models <- sapply(Period_names, function(Period){

#subset to transitions list for period
viable_trans_list <- viable_trans_lists[[Period]]  

#load model look up table
Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period)  

#vector unique trans IDs
unique_trans_IDs <- sort(unique(Model_lookup$Trans_ID))

#test
if(identical(sort(viable_trans_list$Trans_ID), unique_trans_IDs) == FALSE){
result <- setdiff(viable_trans_list$Trans_ID, unique_trans_IDs)
}else{result <- TRUE}
return(result)
})

if (all(All_trans_have_models) == FALSE) {
        Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some transitions do not have 
                                corresponding models in the model lookup table,
                                see results for which transition IDs are missing",
                                Result = All_trans_have_models))}


#Check 2: that all model files contained in the look up table exist
All_models_exist <- unlist(sapply(Period_names, function(Period){
  
#load model look up table
Model_lookup <- xlsx::read.xlsx("Tools/Model_lookup.xlsx", sheetName = Period)

#loop over model file paths                                
sapply(Model_lookup$File_path, function(x) file.exists(x))
}))  

if (all(All_models_exist) == FALSE) {
        Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some models in lookup table do not 
                                have existing files, see results for which",
                                Result = All_models_exist))}  

### =========================================================================
### H- Check predictor data preparation   
### =========================================================================

#Three checks:
#1. That predictor tables contain all of the predictors for the models
#2. That predictors in each table exist and there are no duplicates
#3. That all SA predictor stacks exist to be loaded

###Check 1. ###
#Use results from the end of feature selection
#to get a list of unique predictors across all models 
SA_preds <- lapply(list.files("Results/Model_tuning/Predictor_selection/GRRF_embedded_selection", full.names = TRUE, recursive = TRUE), function(x){
  
  #read in results
  Results_object <- readRDS(x)
  
  #extract unique predictors
  Unique_preds <- unique(Results_object[["var"]])
  #Unique_preds <- unique(Reduce(c, unlist(sapply(Results_object, function(y) y[["var"]]), recursive = FALSE)))
  
  #remove any focal preds because these are only created during prediction
  SA_vars <- grep("nhood", Unique_preds, invert = TRUE, value = TRUE)
})
names(SA_preds) <- sapply(list.files("Results/Model_tuning/Predictor_selection/GRRF_embedded_selection", recursive = TRUE), function(x) str_split_i(x, "/",1))

Periodic_SA_preds <- sapply(unique(names(SA_preds)), function(period){
  unique(unlist(SA_preds[names(SA_preds) == period]))
})

#loop over sheets of predictor table ensuring that all preds are present
#for the calibration period sheets (1:3) this needs to be done with the
#corresponding list of SA preds for all simulation time steps this needs to
#be done with only the from the final period (2009-2018)

#get sheet names of predictor table
Pred_sheets <- getSheetNames(Pred_table_path)

#First check that the .xlsx file contains sheets for all of the simulation time steps
#all time points <2020 are covered by the calibration period so filter these out
Time_steps_subset <- model_time_steps$Keys[model_time_steps$Keys >= 2020]
Missing_sheets <- setdiff(Time_steps_subset, Pred_sheets)
if(length(Missing_sheets) > 0){
          Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some time points in the simulations 
                                do not have corresponding sheets in the 
                                predictor table, see results for which are missing",
                                Result = Missing_sheets))}

#Loop over calibration periods sheets ensuring that all SA vars are present
Calibration_preds_in_tables <- unlist(lapply(names(Periodic_SA_preds), function(Period){

  #subset to correct pred set
  Period_preds <- Periodic_SA_preds[[Period]]

  #load predictor table
  Period_sheet <- openxlsx::read.xlsx(Pred_table_path, sheet = grep(Period, Pred_sheets))

  #test
  output <- Period_preds %in% Period_sheet$Covariate_ID
}))
if (all(Calibration_preds_in_tables) == FALSE) {
  Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some predictors required for models in the calibration periods are not contained in the predictor tables used to produce stacks,see result for details",
                                Result = Calibration_preds_in_tables))}


#Seperate names of simulation predictor sheets
Simulation_sheets <- Pred_sheets[-grep(paste(names(Periodic_SA_preds), collapse="|"), Pred_sheets)]
  
#Loop over simulation period sheets
Simulation_preds_in_tables <- lapply(Simulation_sheets, function(Time_step){
  
  #subset to final period SA preds 
  Period_preds <- Periodic_SA_preds[[length(Periodic_SA_preds)]]

  #load predictor table
  Period_sheet <- openxlsx::read.xlsx(Pred_table_path, sheet = Time_step)

  #test
  output <- Period_preds %in% Period_sheet$Covariate_ID
})

if (any(unlist(Simulation_preds_in_tables)) == FALSE){
  Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some predictors required for models 
                                in the simulation time steps are not contained in
                                the predictor tables used to produce stacks,
                                see result for details",
                                Result = Simulation_preds_in_tables))}

### Check 2.###
#Get file path of all unique predictors in tables
Pred_raster_paths <- unique(unlist(sapply(Pred_sheets, function(Sheet){
  
#load predictor sheet
Predictor_table <- openxlsx::read.xlsx(Pred_table_path, sheet = Sheet)
Predictor_table$Prepared_data_path
}, simplify = TRUE)))


#check if they exist
All_pred_rasters_exist <- sapply(Pred_raster_paths, function(x) file.exists(x))
if(any(All_pred_rasters_exist == FALSE)) {
    Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some of the predictors required are
                                missing corresponding raster files,
                                see result for details",
                                Result = All_pred_rasters_exist))}


  
#check that there are no duplicates
No_duplicate_preds <- sapply(Pred_sheets, function(Sheet){

  #load predictor table
Predictor_table <- openxlsx::read.xlsx(Pred_table_path, sheet = Sheet)

any(duplicated(cbind(Predictor_table$Covariate_ID, Predictor_table$Scenario)))

}, simplify = TRUE)
if(any(No_duplicate_preds)== TRUE){
      Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some of the predictor stacks contain
                                          duplicate predictors,see result for details",
                                Result = No_duplicate_preds))}
  
### Check 3.###
#Create list of hypothetical predictor stacks required for the simulations
# Outer loop over scenario IDs 
#inner loop over time steps

#For calibration stacks, loop over sheet names from viable transitions lists 
#which equate to the calibration periods
if(grepl("calibration", Model_mode, ignore.case = TRUE)){
  Calibration_pred_stacks_exist <- unlist(lapply(Period_names, function(Period_name){
   File_path <- if(length(list.files("Data/Preds/Prepared/Stacks/Calibration",
                           full.names = TRUE,
                           pattern = Period_name)) == 1){TRUE}else{FALSE}}))
  if(all(Calibration_pred_stacks_exist) == FALSE) {
        Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some of the predictor stacks required
                           for the calibration periods do not exist,
                           see result for details",
                                Result = Calibration_pred_stacks_exist))}
}


#For simulation stacks, again all time point <2020 use the stacks from the calibration
#periods so use the same subset as above
if(grepl("simulation", Model_mode, ignore.case = TRUE)){
Simulation_pred_stacks_exist <- sapply(Scenario_IDs, function(Scenario_ID){
  
  #inner loop over time steps
  Time_step_paths <- sapply(Time_steps_subset, function(Time_step){
  paste0("Data/Preds/Prepared/Stacks/Simulation/SA_preds/SA_pred_", Scenario_ID, "_", Time_step, ".rds")})
  
  Paths_exist <- sapply(Time_step_paths, function(x) file.exists(x))
  }) 
if(all(Simulation_pred_stacks_exist) == FALSE) {
        Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some of the predictor stacks required
                           for simulation time steps do not exist,
                           see result for details",
                                Result = Simulation_pred_stacks_exist))}
}

### =========================================================================
### I- Check functionality of spatial interventions
### =========================================================================

if(grepl("simulation", Model_mode, ignore.case = TRUE)){
  
  #load table of scenario interventions
  Interventions <- openxlsx::read.xlsx(Scenario_specs_path, sheet = "Interventions")

  #convert Time_step and Target_classes columns back to character vectors
  Interventions$Time_step <- sapply(Interventions$Time_step, function(x) {
    x <- str_remove_all(x, " ")
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)
  
  Interventions$Target_classes <- sapply(Interventions$Target_classes, function(x) {
    x <- str_remove_all(x, " ")
    rep <- unlist(strsplit(x, ","))
    },simplify=FALSE)

  #remove parameter adjust interventions because they cannot be tested
  Interventions <- Interventions[Interventions$Intervention_type != "Param_adjust",]
  
  #Seperate interventions according to those which have temporally dynamic inputs
  #vs. those that have static inputs
  Dyn_int <- Interventions[Interventions$Dynamic_input == "Y",]
  Static_int <- Interventions[Interventions$Dynamic_input == "N",]
  
  #Load in exemplar dataset to test interventions on
  #Exemplar dataset is saved during calibration of allocation parameters
  Raster_prob_values <- readRDS("Data/Exemplar_data/EXP_raster_prob_values.rds")

  #sort by ID
  Raster_prob_values[order(Raster_prob_values$ID),]
  
  #loop spatial manipulation function over unique scenarios in static interventions
  Static_int_tests <- sapply(unique(Static_int$Scenario_ID), function(Scenario_ID){
    
    #Identify the first time step common across all interventions for the scenarios
    Common_step <- Reduce(intersect, Static_int[Static_int$Scenario_ID == Scenario_ID, "Time_step"])[1]
  
    #test interventions and log errors with 'try'
    test_interventions <- try(lulcc.spatprobmanipulation(Interventions = Static_int,
                                                         Scenario_ID = Scenario_ID,
                                                         Raster_prob_values = Raster_prob_values,
                                                         Simulation_time_step = Common_step),silent = TRUE)
    
    #check for any errors, if erros are present then return the message
    #otherwise return TRUE
    if(class(test_interventions) == "try-error"){return(test_interventions)}else{TRUE}
    })
  
  
  #loop function over unique scenarios in dynamic interventions
  Dynamic_int_tests <- sapply(unique(Dyn_int$Scenario_ID), function(Scenario_ID){
    
    #Loop over time steps for the Scenario
    #test interventions and log errors with 'try'
    Time_step_tests <- sapply(unlist(Dyn_int[Dyn_int$Scenario_ID == Scenario_ID, "Time_step"]), function(Tstep){
      
      test_interventions <- try(lulcc.spatprobmanipulation(Interventions = Dyn_int,
                                                        Scenario_ID = Scenario_ID,
                                                        Raster_prob_values = Raster_prob_values,
                                                        Simulation_time_step = Tstep),silent = TRUE)
      #check for any errors, if errors are present then return the message
      #otherwise return TRUE
      if(class(test_interventions) == "try-error"){FALSE}else{TRUE}  
    })
    names(Time_step_tests) <- unlist(Dyn_int[Dyn_int$Scenario_ID == Scenario_ID, "Time_step"])
    
    return(Time_step_tests)
    })
  
  Dynamic_results <- unlist(lapply(seq_len(ncol(Dynamic_int_tests)), function(i) Dynamic_int_tests[,i]))
  
  #Combine the results of the tests of static/dynamic interventions
  All_int_tests <- c(Static_int_tests, Dynamic_results)
  
  #add test result to list
  if(all(All_int_tests) == FALSE){
  Model_pre_checks <- list.append(Model_pre_checks,
                           list(Message = "Some spatial interventions are producing errors, check the names in the result",
                                Result = list("Static_interventions" = Static_int_tests,
                                              "Dynamic_interventions" = Dynamic_int_tests)))}


} #close if statement

### =========================================================================
### X- Close function
### =========================================================================


#if the list of model pre-check errors is empty then return TRUE else return the list
if(length(Model_pre_checks) == 0){
  return(TRUE)
}else{
  return(Model_pre_checks)
}

}

