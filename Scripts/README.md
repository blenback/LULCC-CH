# Scripts

R code for LULCC-CH: data prep, modelling, validation.

## Subfolders

### Analysis

- [Landscape_pattern_analysis.R](Analysis/Landscape_pattern_analysis.R)
- [Simulated_map_summaries.R](Analysis/Simulated_map_summaries.R)
- [Results_analysis.R](Analysis/Results_analysis.R)
- [Simulated_map_finalisation.R](Analysis/Simulated_map_finalisation.R)


### Dinamica_integration

- [Dinamica_initialize.R](Dinamica_integration/Dinamica_initialize.R)
- [Dinamica_deterministic_trans.R](Dinamica_integration/Dinamica_deterministic_trans.R)
- [Dinamica_list_filenames.py](Dinamica_integration/Dinamica_list_filenames.py)
- [Dinamica_load_trans_matrix.R](Dinamica_load_trans_matrix.R) 
- [Dinamica_modify_lulc_save_path.R](Dinamica_modify_lulc_save_path.R)  
- [Dinamica_modify_param_table.R](Dinamica_modify_param_table.R) 
- [Dinamica_subset_control_table.R](Dinamica_subset_control_table.R) 
- [Dinamica_trans_potent_calc.R](Dinamica_trans_potent_calc.R)  
- [Dinamica_update_control_table.R](Dinamica_update_control_table.R)  
- [Dinamica_use_validation.R](Dinamica_use_validation.R)  

### Functions

- [lulcc.analysecovselection.R](Functions/lulcc.analysecovselection.R)  
- [lulcc.analysedownsampling.R](Functions/lulcc.analysedownsampling.R)  
- [lulcc.BoyceROCcurves.R](Functions/lulcc.BoyceROCcurves.R)  
- [lulcc.covfilter.R](Functions/lulcc.covfilter.R)  
- [lulcc.downloadunzip.R](Functions/lulcc.downloadunzip.R)  
- [lulcc.eiintervention.R](Functions/lulcc.eiintervention.R)  
- [lulcc.evalcurves.R](Functions/lulcc.evalcurves.R)  
- [lulcc.evalfeatureselection.R](Functions/lulcc.evalfeatureselection.R)  
- [lulcc.evaluate.R](Functions/lulcc.evaluate.R)  
- [lulcc.filtersel.R](Functions/lulcc.filtersel.R)  
- [lulcc.finalisemodelspecifications.R](Functions/lulcc.finalisemodelspecifications.R)  
- [lulcc.fitevalsave.R](Functions/lulcc.fitevalsave.R)  
- [lulcc.fitmodel.R](Functions/lulcc.fitmodel.R)  
- [lulcc.fitrf.R](Functions/lulcc.fitrf.R)  
- [lulcc.grrffeatselect.R](Functions/lulcc.grrffeatselect.R)  
- [lulcc.improvetransareadistrib.R](Functions/lulcc.improvetransareadistrib.R)  
- [lulcc.listbybioregion.R](Functions/lulcc.listbybioregion.R)  
- [lulcc.listbylulc.R](Functions/lulcc.listbylulc.R)  
- [lulcc.modelprechecks.R](Functions/lulcc.modelprechecks.R)  
- [lulcc.modelstatscomparison.R](Functions/lulcc.modelstatscomparison.R)  
- [lulcc.neighbourhoodlayercreation.R](Functions/lulcc.neighbourhoodlayercreation.R)  
- [lulcc.plotBoyceandROCcurves.R](Functions/lulcc.plotBoyceandROCcurves.R)  
- [lulcc.preps.R](Functions/lulcc.preps.R)  
- [lulcc.requestfocallulcclasses.R](Functions/lulcc.requestfocallulcclasses.R)  
- [lulcc.savethis.R](Functions/lulcc.savethis.R)  
- [lulcc.setparams.R](Functions/lulcc.setparams.R)  
- [lulcc.spatprobmanipulation.R](Functions/lulcc.spatprobmanipulation.R)  
- [lulcc.splitforcovselection.R](Functions/lulcc.splitforcovselection.R)  
- [lulcc.summarisecovselection.R](Functions/lulcc.summarisecovselection.R)  
- [lulcc.summarisemodelevaluation.R](Functions/lulcc.summarisemodelevaluation.R)  
- [lulcc.testrastercompatibility.R](Functions/lulcc.testrastercompatibility.R)  
- [lulcc.viabletranslist.R](Functions/lulcc.viabletranslist.R)  
- [pipe.all_metrics_function.R](Functions/pipe.all_metrics_function.R)  
- [pipe.ceval.R](Functions/pipe.ceval.R)  
- [pipe.df_or_rast.R](Functions/pipe.df_or_rast.R)  
- [pipe.multi_input_class.R](Functions/pipe.multi_input_class.R)  
- [pipe.multi.R](Functions/pipe.multi.R)  
- [pipe.prd.R](Functions/pipe.prd.R)  
- [pipe.summary_wsl_evaluation.R](Functions/pipe.summary_wsl_evaluation.R)  
- [preva_meta.R](Functions/preva_meta.R)  
- [Quarto_render_save_upload.R](Functions/Quarto_render_save_upload.R)  
- [wsl_evaluation_class.R](Functions/wsl_evaluation_class.R)  
- [wsl_fit_class.R](Functions/wsl_fit_class.R)  


### Preparation
- [Calibrate_allocation_parameters.R ](Preparation/Calibrate_allocation_parameters.R)
- [Calibration_predictor_prep.R](Preparation/Calibration_predictor_prep.R) 
- [Deterministic_trans_prep.R](Preparation/Deterministic_trans_prep.R)  
- [LULC_data_prep.R](Preparation/LULC_data_prep.R)  
- [Nhood_predictor_prep.R](Preparation/Nhood_predictor_prep.R)  
- [Region_prep.R](Preparation/Region_prep.R)  
- [Simulation_predictor_prep.R](Preparation/Simulation_predictor_prep.R)  
- [Simulation_trans_tables_prep.R](Preparation/Simulation_trans_tables_prep.R)  
- [Spatial_interventions_prep.R](Preparation/Spatial_interventions_prep.R)  
- [Trans_model_finalization.R](Preparation/Trans_model_finalization.R)  
- [Transition_dataset_prep.R](Preparation/Transition_dataset_prep.R)  
- [Transition_feature_selection.R](Preparation/Transition_feature_selection.R)  
- [Transition_identification.R](Preparation/Transition_identification.R)  
- [Transition_modelling.R](Preparation/Transition_modelling.R)


## Master Script

- [LULCC_CH_master.R](LULCC_CH_master.R)
