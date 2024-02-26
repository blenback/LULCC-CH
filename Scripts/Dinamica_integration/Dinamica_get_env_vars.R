#############################################################################
## Dinamica_get_env_vars: Get environment variables required for simulation  
## Date: 18-01-2024
## Author: Ben Black
#############################################################################

#testing values for env vars
LULCC_CH_HPC_DIR <- "E:/LULCC_CH_Ensemble"
LULCC_M_CLASS_AGG <- "Tools/LULC_class_aggregation.xlsx"
LULCC_M_SPEC <- "Tools/Model_specs.csv"
LULCC_M_PARAM_GRID <- "Tools/param-grid.xlsx"
LULCC_M_PRED_TABLE <- "Tools/Predictor_table.xlsx"
LULCC_M_REF_GRID <- "Data/Ref_grid.gri"
LULCC_M_CAL_PARAM_DIR <- "Data/Allocation_parameters/Calibration"
LULCC_M_SIM_PARAM_DIR <- "Data/Allocation_parameters/Simulation"
LULCC_M_RATE_TABLE_DIR <- "Data/Transition_tables/prepared_trans_tables"
LULCC_M_SIM_CONTROL_TABLE <-  "Tools/Simulation_control.csv"
LULCC_M_SPAT_INTS_TABLE <- "Tools/Spatial_interventions.csv"
LULCC_M_EI_INTS_TABLE <- "Tools/EI_interventions.csv"
LULCC_M_SCENARIO_SPEC <- "Tools/Scenario_specs.csv"
LULCC_M_EI_LAYER_DIR <- "Data/EI_intervention_layers"
LULCC_CH_OUTPUT_BASE_DIR <- "lulcc_output"


# Created list of environment variables with names used in R scripts
Env_var_key <- list("LULCC_CH_HPC_DIR" = "wpath",# Working directory
                   "LULCC_M_CLASS_AGG"= "LULC_aggregation_path",#Path to LULC class aggregation table
                   "LULCC_M_SPEC"= "Model_specs_path", #Path to model specifications table
                   "LULCC_M_PARAM_GRID" = "Param_grid_path", #Path to model hyper parameter grids
                   "LULCC_M_PRED_TABLE"= "Pred_table_path", #Path to predictor table
                   "LULCC_M_REF_GRID" = "Ref_grid_path",#Path to reference raster
                   "LULCC_M_CAL_PARAM_DIR"= "Calibration_param_dir", #Path to calibration parameter directory
                   "LULCC_M_SIM_PARAM_DIR"= "Simulation_param_dir",  #Path to simulation parameter directory                      
                   "LULCC_M_RATE_TABLE_DIR" = "Trans_rate_table_dir",#Path to transition rate table directory
                   "LULCC_M_SIM_CONTROL_TABLE" = "Sim_control_path", #Path to simulation control table
                   "LULCC_M_SPAT_INTS_TABLE" = "Spat_ints_path", #Path to spatial interventions table
                   "LULCC_M_EI_INTS_TABLE" = "EI_ints_path", #Path to EI interventions table
                   "LULCC_M_SCENARIO_SPEC" = "Scenario_specs_path",#Path to Scenario specifications table
                   "LULCC_M_EI_LAYER_DIR" = "EI_layer_dir",#Path to EI intervention layers directory
                   "LULCC_CH_OUTPUT_BASE_DIR" = "LULCC_output_dir") #Path for map output directory 
                    

# Get environment variables set from SLURM
Sys.getenv(x = names(Env_var_key), names = TRUE)
                 
#list values of env_vars
Model_vars <- mget(names(Env_var_key))

#rename with model specific names
names(Model_vars) <- unlist(Env_var_key) 

# Send re-named variables to global environment
list2env(Model_vars, .GlobalEnv)

# Remove redundant variables from environment
rm(list = names(Env_var_key))
rm(Model_vars, Env_var_key)
