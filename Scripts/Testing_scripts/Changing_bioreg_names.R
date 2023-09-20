# change the nameof the Bioregion layer in the calibration pred stacks
Calib_pred_stacks <- lapply(list.files("E:/LULCC_CH/Data/Preds/Prepared/Calibration", recursive = TRUE, full.names = TRUE), readRDS)

names(Calib_pred_stacks[[1]])
