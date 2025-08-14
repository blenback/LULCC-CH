#############################################################################
## SA_var_prep: Prepare Suitability and Accessibility Predictor Layers at a 
## Uniform 100m Resolution, CRS, and Extent. Prepared layers are saved 
## separately and then combined into raster stacks for easy loading.
## Date: 01-08-2021
## Author: Ben Black (Modified version)
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================

# Assumes all packages and functions are loaded in the master script
# Assumes variables like Ref_grid_path and Pred_table_path are defined globally

# Load the reference grid using terra
Ref_grid <- terra::rast(Ref_grid_path)

# Extract LULC years from filenames
LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", 
                                                      full.names = FALSE, 
                                                      pattern = "\\.grd$"))

# Create a list of the data/modelling periods
LULC_change_periods <- vector("list", length = length(LULC_years) - 1)
for (i in 1:(length(LULC_years) - 1)) {
  LULC_change_periods[[i]] <- c(LULC_years[i], LULC_years[i + 1])
}
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))

# subset to only the 2009_2018 period
LULC_change_periods <- LULC_change_periods[names(LULC_change_periods) == "2009_2018"]

# Download basic map geometries for Switzerland
# Geoms_path <- "Data/Preds/Raw/CH_geoms"
# dir.create(Geoms_path, recursive = TRUE, showWarnings = FALSE)
# lulcc.downloadunzip(
#   url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master",
#   save_dir = Geoms_path
# )

### =========================================================================
### B- Gather Predictor Information
### =========================================================================

# Base directory for prepared predictor layers
Prepped_layers_dir <- "Data/Preds/Prepared/Layers"
dir.create(Prepped_layers_dir, recursive = TRUE, showWarnings = FALSE)

# Predictor table file path
# Assumes Pred_table_path is defined globally
sheets <- readxl::excel_sheets(Pred_table_path)

# subset to only sheets in LULC_change_periods
sheets <- sheets[sheets %in% names(LULC_change_periods)]

# Load all sheets as a list
Pred_tables <- lapply(sheets, function(x) openxlsx::read.xlsx(Pred_table_path, sheet = x))
names(Pred_tables) <- sheets

# Combine tables for all periods into a long table
Pred_table_long <- as.data.frame(rbindlist(Pred_tables, idcol = "Sheet"))

# Create directories for all predictor categories
unique_categories <- unique(Pred_table_long[["Predictor_category"]])
sapply(unique_categories, function(x) {
  dir.create(file.path(Prepped_layers_dir, x), recursive = TRUE, showWarnings = FALSE)
})

# Separate predictors that need to be prepared (Prepared != "Y")
Preds_to_prepare <- subset(Pred_table_long, Prepared != "Y")

### =========================================================================
### C- Predictors from Raw Data
### =========================================================================

# # Predictors that have a Raw_data_path
# Preds_raw <- subset(Preds_to_prepare, !is.na(Raw_data_path))
# 
# # Process Static Predictors
# Preds_static <- subset(Preds_raw, Static_or_dynamic == "static")
# 
# # Reduce to unique predictors
# Preds_static_unique <- Preds_static[!duplicated(Preds_static$Covariate_ID), 
#                                     c("Covariate_ID", "Predictor_category", 
#                                       "URL", "Raw_data_path", "Prepared_data_path")]  
# 
# # Loop over static predictors: load, aggregate, save, update table
# for(i in 1:nrow(Preds_static_unique)) {
#   
#   # Load raw data
#   Raw_dat <- readRDS(Preds_static_unique$Raw_data_path[i])
#   
#   # Aggregate to 100m resolution (assuming original resolution is 25m)
#   # terra::aggregate uses fact as list for x and y; here fact=4 to go from 25m to 100m
#   Agg_dat <- terra::aggregate(Raw_dat, fact = 4, fun = mean, na.rm = TRUE)
#   
#   # Define save path
#   layer_path <- file.path(Prepped_layers_dir, 
#                           Preds_static_unique$Predictor_category[i],
#                           paste0(Preds_static_unique$Covariate_ID[i], ".tif"))
#   
#   # Save aggregated raster
#   terra::writeRaster(Agg_dat, filename = layer_path, overwrite = TRUE)
#   
#   # Update the predictor table
#   Pred_table_long$Prepared_data_path[Pred_table_long$Covariate_ID == Preds_static_unique$Covariate_ID[i]] <- layer_path
#   Pred_table_long$Prepared[Pred_table_long$Covariate_ID == Preds_static_unique$Covariate_ID[i]] <- "Y"
#   
#   # Clean up
#   rm(Raw_dat, Agg_dat, layer_path)
# }
# 
# # Process Dynamic Predictors
# Preds_dynamic <- subset(Preds_raw, Static_or_dynamic == "dynamic")
# 
# # Loop over dynamic predictors: load rasters, calculate mean, aggregate, save, update table
# for(i in 1:nrow(Preds_dynamic)) {
#   
#   # List of raster files for the dynamic predictor
#   raster_files <- list.files(Preds_dynamic$Raw_data_path[i], 
#                              pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
#   
#   # Load and stack rasters using terra
#   raster_stack <- terra::rast(lapply(raster_files, readRDS))
#   
#   # Calculate mean across the stack
#   raster_mean <- terra::app(raster_stack, fun = mean, na.rm = TRUE)
#   
#   # Aggregate to 100m resolution
#   Agg_dat <- terra::aggregate(raster_mean, fact = 4, fun = mean, na.rm = TRUE)
#   
#   # Define save path with period
#   layer_name <- paste0(Preds_dynamic$Covariate_ID[i], "_", Preds_dynamic$period[i], ".tif")
#   layer_path <- file.path(Prepped_layers_dir, 
#                           Preds_dynamic$Predictor_category[i],
#                           layer_name)
#   
#   # Save aggregated raster
#   terra::writeRaster(Agg_dat, filename = layer_path, overwrite = TRUE)
#   
#   # Update the predictor table
#   condition <- (Pred_table_long$Covariate_ID == Preds_dynamic$Covariate_ID[i] & 
#                   Pred_table_long$period == Preds_dynamic$period[i])
#   Pred_table_long$Prepared_data_path[condition] <- layer_path
#   Pred_table_long$Prepared[condition] <- "Y"
#   
#   # Clean up
#   rm(raster_stack, raster_mean, Agg_dat, layer_path, layer_name)
# }

# Check new NCCS climate data and update predictor table
clim_var_dir <- "X:/CH_Kanton_Bern/03_Workspaces/07_Climate_data/chelsa_cmip6_processed/aggregated_reprojected/yearly"

# list files
clim_files <- list.files(clim_var_dir, pattern = "ssp245", full.names = TRUE)

# name with basename removing extension
names(clim_files) <- str_remove(basename(clim_files), "\\_ssp245.tif$")

# create a df to capture info
clim_var_info <- data.frame(
  
  # split on underscore and keep 2nd part
  sheet_name = str_split(names(clim_files), "_", simplify = TRUE)[, 2],
  period = paste0(as.numeric(str_split(names(clim_files), "_", simplify = TRUE)[, 2])-5, "_", str_split(names(clim_files), "_", simplify = TRUE)[, 2]),
  Covariate_ID = str_split(names(clim_files), "_", simplify = TRUE)[, 1],
  Unique_ID = NA,
  CA_category = "Suitability",
  Predictor_category = "Climatic",
  Variable_name = NA,
  Data_citation = "Project Internal",
  URL = NA,
  Original_resolution = "25m",
  Static_or_dynamic = "Dynamic",
  Temporal_coverage = paste0(as.numeric(str_split(names(clim_files), "_", simplify = TRUE)[, 2])-5, "_", str_split(names(clim_files), "_", simplify = TRUE)[, 2]),
  Temporal_resolution = NA,
  Prepared = "Y",
  Raw_data_path = clim_files,
  Prepared_data_path = file.path("Data/Preds/Prepared/Layers/Climatic", basename(clim_files)),
  Scenario_variant = "All"
)

# remove rownames
rownames(clim_var_info) <- NULL

# change specific values of periods and sheet names:
#'1991_1996' to '1985_1997',
#'2005_2010' to '1997_2009',
#'2015_2020' to '2009_2018'
clim_var_info$period <- gsub("1991_1996", "1985_1997", clim_var_info$period)
clim_var_info$period <- gsub("2005_2010", "1997_2009", clim_var_info$period)
clim_var_info$period <- gsub("2010_2015", "2009_2018", clim_var_info$period)
clim_var_info$sheet_name <- gsub("1996", "1985_1997", clim_var_info$sheet_name)
clim_var_info$sheet_name <- gsub("2010", "1997_2009", clim_var_info$sheet_name)
clim_var_info$sheet_name <- gsub("2015", "2009_2018", clim_var_info$sheet_name)

# remove the rows where period == "2000_2005" and "2010_2015"
clim_var_info <- clim_var_info[clim_var_info$period != "2000_2005", ]

# copy the files from the Raw_data_path to the Prepared_data_path
for(i in 1:nrow(clim_var_info)) {
  file.copy(from = clim_var_info$Raw_data_path[i], 
            to = clim_var_info$Prepared_data_path[i], 
            overwrite = TRUE)
}

# split the table into the historic layers and the future layers
clim_vars_historic <- clim_var_info[clim_var_info$period %in% c("1985_1997", "1997_2009", "2009_2018"), ]
clim_vars_future <- clim_var_info[!(clim_var_info$period %in% c("1985_1997", "1997_2009", "2009_2018")), ]

# save the future table to be used in Simulation_predictor_prep.R
saveRDS(clim_vars_future, 
        file = "Data/Preds/NCCS_future_climatic_vars.rds")

# # load a raster of the 1st file to check the resolution
# clim_test_raster <- terra::rast(clim_var_info$Prepared_data_path[1])
# terra::res(clim_test_raster)
# ext(clim_test_raster)
# crs(clim_test_raster)
# nlyr(clim_test_raster)
# 
# # check data type of layer
# datatype(clim_test_raster)

# open the predictor table as a workbook
Pred_table_update <- openxlsx::loadWorkbook(file = Pred_table_path)

# loop over the names of Pred_tables adding the rows for each period
sapply(names(Pred_tables), function(period) {
  
  # filter the climatic vars for the period
  period_vars <- clim_vars_historic[clim_vars_historic$sheet_name == period, ]
  
  # remove the sheet_name column
  period_vars <- period_vars[, -which(names(period_vars) == "sheet_name")]
  
  # append the new rows to the workbook after the existing rows
  openxlsx::writeData(Pred_table_update, sheet = period, x = period_vars, startRow = nrow(openxlsx::read.xlsx(Pred_table_update, sheet = period)) + 1)
})

# save the updated workbook
openxlsx::saveWorkbook(Pred_table_update, file = Pred_table_path, overwrite = TRUE)


### =========================================================================
### D- Predictors from Source
### =========================================================================

# #get urls for variable and convert to named list
# All_urls <- str_split(Preds_to_prepare[grep(Preds_to_prepare$Covariate_ID, pattern="Avg_chg_FTE"), "URL"][1], ",")[[1]]
# named_urls <- lapply(str_split(All_urls, pattern = " = "), function(x) trimws(x[2]))
# names(named_urls) <- lapply(str_split(All_urls, pattern = " = "), function(x) trimws(x[1]))
# 
# ### Statent data
# #seperate the Statent urls 
# Statent_urls <- named_urls[4:length(named_urls)]

#create dir for raw data
Statent_dir <- "Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Statent"

# #Download and unzip all datasets
# sapply(Statent_urls, function(x) lulcc.downloadunzip(url = x,
#                                              save_dir = Statent_dir))

#gather the relevant files
Statent_paths <- grep(list.files(Statent_dir, recursive = TRUE, full.names = TRUE, pattern = "csv"),
                        pattern = paste(c("GMDE", "NOLOC"), collapse = "|"),
                        invert=TRUE,
                        value=TRUE)

#name using the 4 digit numeric and the end of the path
names(Statent_paths) <- sapply(Statent_paths, function(x) {
  #split the file path on the last '_' and extract the 4 digit numeric in the 2nd half
  parts <- str_split(x, pattern = "_")[[1]]
  numeric_part <- parts[length(parts)]
  data_year <- str_extract(numeric_part, "\\d{4}")
  return(data_year)
})

#Get the variable IDs for the number of Full Time Equivalents in each sector 
#webpage for variable list: https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/arbeitsstaetten-beschaeftigung/statistik-unternehmensstruktur-statent-ab-2011.assetdetail.23264982.html
#download file from API URL
Statent_metadata <- openxlsx::read.xlsx("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23264982/master", startRow = 9, cols = c(1,3))
colnames(Statent_metadata) <- c("ID", "Name")

#subset using German variable names
Statent_var_names <- c("Vollzeitäquivalente Sektor 1", "Vollzeitäquivalente Sektor 2", "Vollzeitäquivalente Sektor 3")
Statent_var_IDs <- Statent_metadata[which(Statent_metadata$Name %in% Statent_var_names), "ID"]

#Provide clean names
names(Statent_var_IDs) <- c("Sec1", "Sec2", "Sec3")

#add the column names corresponding to year and spatial information
Statent_desc_vars <- c("E_KOORD", "N_KOORD", "RELI")

#loop over each file loading it in and seperating on the required variables including coords
future::plan(multisession(workers = availableCores()-2))
Statent_data_by_year <- future_mapply(function(annual_data_path, year){
  
  #load the xlsx file
  Annual_data <- read.csv2(annual_data_path)
  
  #subset to just the required variables
  Data_subset <- Annual_data[,c(Statent_desc_vars, Statent_var_IDs)]
  #rename the sectoral columns appending year
  names(Data_subset)[names(Data_subset) %in% Statent_var_IDs] <- paste(names(Statent_var_IDs), year, sep = "_")
  
  return(Data_subset)
}, annual_data_path = Statent_paths,
year = names(Statent_paths),
SIMPLIFY = FALSE)
plan(sequential)

#Merge based on Statent_desc_vars
Statent_merged <- Reduce(function(x, y) merge(x, y, by= Statent_desc_vars, all = TRUE), Statent_data_by_year)
#rm(Statent_data_by_year

# convert all columns to numeric
Statent_merged[, -which(names(Statent_merged) %in% Statent_desc_vars)] <- lapply(Statent_merged[, -which(names(Statent_merged) %in% Statent_desc_vars)], as.numeric)

# get colsums
col_sums <- colSums(Statent_merged[, -which(names(Statent_merged) %in% Statent_desc_vars)], na.rm = TRUE)
print(col_sums)
max(col_sums) # check that the max value is not too high

#rasterize
coordinates(Statent_merged) <- ~E_KOORD+N_KOORD
crs(Statent_merged) <- crs(Ref_grid)

# Convert to SpatVector, using same CRS as reference raster
points <- vect(Statent_merged, crs = crs(Ref_grid), geom = c("E_KOORD", "N_KOORD"))

# Rasterize using the reference raster as template
Statent_stack <- rasterize(points, Ref_grid, field = names(Statent_merged[, -which(names(Statent_merged) %in% Statent_desc_vars)]), fun = "mean")
Statent_stack <- r
rm(r)
#Statent_brick <- terra::rast(Statent_merged)

# get names of layers in the brick
layer_names <- names(Statent_stack)
sum_vals <- unlist(sapply(layer_names, function(x) global(Statent_stack[[x]], fun = "sum", na.rm = TRUE)))
min(sum_vals) # check that the sum of all values is not zero
max(sum_vals) # check that the max value is not too high

# ### Business census data
# Biz_census_urls <- named_urls[1:3]
# 
# #specify dir and download datasets
# Biz_census_dir <- c("Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Business_census")
# sapply(Biz_census_urls, function(x) lulcc.downloadunzip(url = x,
#                                              save_dir = Biz_census_dir))
# 
# #list all csv files. 
# Biz_census_paths <- grep(list.files(path = Biz_census_dir,
#                                     full.names = TRUE,
#                                     recursive = TRUE),
#                          pattern = "csv",
#                          value = TRUE,
#                          ignore.case = TRUE)
# 
# #manually name according to the year of each (based on abbreviations in file name e.g '05' == 2005)
# names(Biz_census_paths) <- c("2000", "2005", "1996", "2001", "2005", "2008", "1995", "1998")
# 
# #each dataset uses different IDs for the variable of Full Time Equivalents
# #detailed in seperate meta data files
# Biz_census_meta_paths <- grep(list.files(path = Biz_census_dir,
#                                     full.names = TRUE,
#                                     recursive = TRUE),
#                          pattern = "xls",
#                          value = TRUE,
#                          ignore.case = TRUE)
# 
# #Find the IDs used for the Full Time Equivalents variables in each metadata file
# Var_IDs_across_datasets <- unlist(sapply(Biz_census_meta_paths, function(x){
#   meta_df <- readxl::read_excel(x)
#   meta_df <- meta_df[26:nrow(meta_df),c(1,5)]
#   Var_IDs <- meta_df[which(meta_df[[2]] %in% Statent_var_names), 1]
# }))
# 
# #The IDs contain a common string across the datasets i.e 'VZAS' followed by
# #1,2 or 3 for the sector. Use to match columns across all datasets.
# BC_var_strings <- c("VZAS1", "VZAS2", "VZAS3")
# names(BC_var_strings) <- c("Sec1", "Sec2", "Sec3")
# 
# #The spatial variables are named differently for the Business census datasets
# BC_desc_vars <- c("X", "Y")
# names(BC_desc_vars) <- BC_desc_vars
# 
# #combine the variable ames vectors
# BC_vars <- c(BC_desc_vars, BC_var_strings)
# 
# #loop over Business census datasets
# BC_data_by_year <- mapply(function(annual_data_path, year){
#   
#   #load the file
#   Annual_data <- read.csv2(annual_data_path)
#   
#   #subset to just the required variables
#   Data_subset <- Annual_data[,grepl(pattern = paste(c(BC_vars), collapse = "|"), names(Annual_data))]
#   
#   #rename the sectoral columns appending year
#   names(Data_subset) <- lapply(names(Data_subset), function(y){
#   new_name <- names(BC_vars)[which(BC_vars %in% str_match(pattern = paste(c(BC_vars), collapse = "|"), y))]
#   if(grepl(new_name, pattern = "Sec")){paste0(new_name, "_", year)}else{new_name}
#   })
#   return(Data_subset)
# }, annual_data_path = Biz_census_paths,
# year = names(Biz_census_paths),
# SIMPLIFY = FALSE)
# 
# #merge
# BC_merged <- Reduce(function(x, y) merge(x, y, by= BC_desc_vars, all = TRUE), BC_data_by_year)
# 
# #rasterize
# coordinates(BC_merged) <- ~X+Y
# gridded(BC_merged) <- TRUE 
# crs(BC_merged) <- crs(Ref_grid)
# BC_brick <- brick(BC_merged)
# extent(BC_brick) <- extent(Ref_grid)
# BC_brick <- raster::resample(BC_brick, Ref_grid)
#  
# #Combine the two bricks together
# Data_stack <- stack(Statent_brick, BC_brick)

#intersect with the kantons

#load kanton shapefile
Canton_shp <- terra::vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

# merge polygons with the same canton names
Canton_shp <- terra::aggregate(Canton_shp, by = "NAME")

# set crs to the reference grid
Canton_shp <- terra::project(Canton_shp, crs(Ref_grid))

# # convert to dataframe with geometry
# Canton_df <- as.data.frame(Canton_shp)
# 
# # plot the cantons with names as labels
# ggplot2::ggplot() +
#   geom_spatvector(data = Canton_shp, fill = NA, color = "black", size = 0.1)+
#   geom_spatvector_label(data = Canton_shp, aes(label = NAME), size = 3, nudge_y = 0.1) 

##rasterize
Canton_shp$name <- as.factor(Canton_shp$NAME)
Canton_rast <- terra::rasterize(Canton_shp, Ref_grid, field = "name", background=NA)

#sum data in each canton
FTE_lab_market <- terra::extract(Statent_stack, Canton_shp, fun = sum, na.rm = TRUE)

# add canton names
FTE_lab_market$name <- Canton_shp$NAME

# add kantonsnum
FTE_lab_market$kantonsnum <- Canton_shp$mean_KANTONSNUM

# remove ID column
FTE_lab_market <- FTE_lab_market[, -which(names(FTE_lab_market) == "ID")]

# remove '_mean' from all column names except for 'kantonsnum' and 'name'
FTE_lab_market <- FTE_lab_market %>%
  rename_with(~ gsub("_mean", "", .), -c(kantonsnum, name))

# save the data for re-use in simulation_predictor_prep.R
saveRDS(FTE_lab_market, file = "Data/Preds/FTE_historic_cantonal_dat.rds")

# set a 5 year change period to investigate 
change_periods <- list("2009_2018" = c(2011,2015),
                      "2020_2025" = c(2016, 2020)
                      )

#split into sectors, vector sector numbers
sector_nums <- c(1,2,3)

#base dir
Prepared_FTE_dir <- "Data/Preds/Prepared/Layers/Socio_economic/Employment"
dir.create(Prepared_FTE_dir, recursive = TRUE)

#loop over sectors calculating difference for period years
Sector_extrapolations <- lapply(sector_nums, function(x){
  
  Sector_string <- paste0("Sec", x, "_")
  
  #seperate layers for sector
  Sector_data <- FTE_lab_market[,which(grepl(colnames(FTE_lab_market), pattern = paste(c(Sector_string, "kantonsnum", "name"), collapse = "|")))]
  
  #seperate the years by removing the prefix to the layer names
  year_cols <- colnames(Sector_data)[grepl(colnames(Sector_data), pattern = paste0(Sector_string, "\\d+"))]
  years <- as.numeric(str_remove_all(year_cols, pattern = Sector_string))
  names(years) <- year_cols
  years <- sort(years, decreasing = FALSE)
  
  #reorder columns on the basis of ascending years using the names of the years
  Sector_data <- Sector_data[,c("kantonsnum", "name", names(years))]
  
  # loop over the change periods calculated the difference in values between the two years
  for(i in seq_along(change_periods)) {
    
    period_name <- names(change_periods)[[i]]
    
    #get the change period dates
    year1 <- change_periods[[i]][1]
    year2 <- change_periods[[i]][2]
    
    #get the column names for the two years
    year1_col <- paste0(Sector_string, year1)
    year2_col <- paste0(Sector_string, year2)
    
    #calculate the difference in values between the two years
    Diff_col_name <- paste0("Diff_", year1, "_", year2)
    Sector_data[[Diff_col_name]] <- Sector_data[[year2_col]] - Sector_data[[year1_col]]
    
    # add the column to the shapefile
    Canton_shp[[Diff_col_name]] <- Sector_data[[Diff_col_name]]
  
    # rasterize
    Period_rast <- terra::rasterize(Canton_shp, Ref_grid, field = Diff_col_name, background=NA)
    
    # create a file path for the raster
    #Data/Preds/Prepared/Layers/Socio_economic/Employment/Avg_chg_FTE_2009_2018_Sec1.tif
    file_path <- paste0(Prepared_FTE_dir, "/chg_FTE_", period_name, "_sec", x, ".tif")
    
    # write the raster to file
    terra::writeRaster(
      Period_rast,
      file_path,
      overwrite = TRUE)
    
    }#close loop over change periods
    }) #close loop over sectors

# MANUALLY UPDATE THE PREDICTOR TABLE!!!!

# #Divide the changes in municipal employment estimated for the AS flying periods by
# #the number of years to get an average annual rate of change for each period
# #because this can also be calculated for the future projected data
# 
# #Outer loop over LULC_change_periods
# Period_sector_values <- rbindlist(lapply(LULC_change_periods, function(period_dates){
# 
# #calc period length
# Duration <- abs(diff(as.numeric(period_dates)))
#   
# #Inner loop over sector_extrapolations
# Sector_values <- rbindlist(mapply(function(Sector_data, Sector_name, period_dates){
# 
#   #subset data
#   dat <- Sector_data[, paste0(Sector_name, "_", period_dates)] 
#   Sector_data$Avg.diff <- (dat[,1]-dat[,2])/Duration
#   return(Sector_data[, c("ID", "name", "Avg.diff")])
#   }, Sector_data = Sector_extrapolations,
#   Sector_name = names(Sector_extrapolations),
#   MoreArgs = list(period_dates = period_dates),
# SIMPLIFY = FALSE), idcol = "Sector", fill = TRUE)
# 
# }), idcol = "Period") #close outer loop
# 
# #pivot to wide
# LMR_values <- pivot_wider(data = Period_sector_values,
#                           id_cols = c("ID"),
#                           names_from = c("Period", "Sector"),
#                           values_from = "Avg.diff",
#                           names_sep = "_")
# #rasterize
# LMR_shp$name <- as.factor(LMR_shp$name)
# LMR_rast <- rasterize(LMR_shp, Ref_grid, field = "name", fun='last', background=NA)
# 
# #use subs to match raster values based on ID and repeat across all columns
# #saving a seperate layer for each
# FTE_rasts <- subs(LMR_rast, 
#                   LMR_values, 
#                   by='ID',
#                   which=2:ncol(LMR_values))
# 
# 
# 
# #vector file names
# FTE_file_names <- paste0(Prepared_FTE_dir, "/", "Avg_chg_FTE_",
#                                     names(LMR_values)[2:length(LMR_values)],".tif") 
# 
# #save a seperate file for each layer
# writeRaster(FTE_rasts, 
#             filename = FTE_file_names,
#             bylayer=TRUE,
#             format="GTiff",
#             overwrite = TRUE)
# 
# #update the predictor table with the file paths
# Pred_table_long[grep(Pred_table_long$Covariate_ID, pattern="Avg_chg_FTE"), "Prepared_data_path"] <- FTE_file_names
# Pred_table_long[grep(Pred_table_long$Covariate_ID, pattern="Avg_chg_FTE"), "Prepared"] <- "Y"



#### D.1- Socio_economic: Employment

# # Check if there are predictors related to municipal population
# if(any(grepl("Muni_pop", Preds_to_prepare$Covariate_ID))) {
#   
#   # Load PX data from URL and convert to DataFrame
#   px_data <- read.px("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master")
#   px_df <- as.data.frame(px_data)
#   
#   # Subset to desired rows based on conditions
#   raw_mun_popdata <- subset(px_df, 
#                             Demografische.Komponente == "Bestand am 31. Dezember" &
#                               Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total" &
#                               Geschlecht == "Geschlecht - Total")
#   
#   # Identify municipality records by matching numeric in names
#   raw_mun_popdata <- raw_mun_popdata[grepl(".*?([0-9]+).*", raw_mun_popdata$Kanton.......Bezirk........Gemeinde.........), 
#                                      c("E_KOORD", "N_KOORD", "RELI")]
#   
#   # Rename columns
#   names(raw_mun_popdata) <- c("Name_Municipality", "Year", "Population")
#   
#   # Pivot wider to have years as columns
#   raw_mun_popdata <- tidyr::pivot_wider(raw_mun_popdata, 
#                                         names_from = "Year", 
#                                         values_from = "Population")
#   
#   # Remove periods from municipality names
#   raw_mun_popdata$Name_Municipality <- gsub("[.]","", as.character(raw_mun_popdata$Name_Municipality))
#   
#   # Separate BFS number from name
#   raw_mun_popdata$BFS_NUM <- as.numeric(gsub(".*?([0-9]+).*", "\\1", raw_mun_popdata$Name_Municipality)) 
#   
#   # Remove BFS number from name
#   raw_mun_popdata$Name_Municipality <- gsub("[[:digit:]]", "", raw_mun_popdata$Name_Municipality)
#   
#   # Subset to only municipalities existing in 2021
#   raw_mun_popdata <- subset(raw_mun_popdata, `2021` > 0)
#   
#   # Import municipality shapefile
#   lulcc.downloadunzip(
#     url = "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2021-07/swissboundaries3d_2021-07_2056_5728.shp.zip",
#     save_dir = "Data/Preds/Raw/CH_geoms"
#   )
#   
#   Muni_shp <- terra::vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
#   
#   # Filter out non-Swiss municipalities
#   Muni_shp <- Muni_shp[Muni_shp$ICC == "CH" & Muni_shp$OBJEKTART == "Gemeindegebiet", ]
#   
#   # Import data of municipality mutations from FSO web service
#   # Scrape content from HTML address
#   library(rvest) # Ensure rvest is loaded in master script
#   content <- read_html("https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True")
#   muni_mutations <- rvest::html_table(content, fill = TRUE)[[1]]
#   
#   # Remove the first row
#   muni_mutations <- muni_mutations[-1,]
#   
#   # Rename columns
#   colnames(muni_mutations) <- c("Mutation_Number", "Pre_canton_ID", 
#                                 "Pre_District_num", "Pre_BFS_num", 
#                                 "Pre_muni_name", "Post_canton_ID",
#                                 "Post_district_num", "Post_BFS_num", 
#                                 "Post_muni_name", "Change_date")
#   
#   # Identify municipalities with mutations
#   mutation_index <- match(raw_mun_popdata$BFS_NUM, muni_mutations$Pre_BFS_num)
#   
#   # Update BFS_NUM based on mutations
#   for (i in 1:nrow(raw_mun_popdata)) {
#     if (!is.na(mutation_index[i])) { 
#       raw_mun_popdata$BFS_NUM[i] <- muni_mutations$Post_BFS_num[mutation_index[i]]
#     }
#   }
#   
#   # Combine populations for merged municipalities
#   if(length(unique(raw_mun_popdata$BFS_NUM)) != nrow(raw_mun_popdata)){
#     
#     # Identify time points
#     Time_points <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(raw_mun_popdata))))
#     
#     # Create empty DataFrame for results
#     Muni_pop_final <- data.frame(matrix(ncol = length(Time_points), 
#                                         nrow = length(unique(raw_mun_popdata$BFS_NUM))))
#     colnames(Muni_pop_final) <- Time_points
#     
#     # Add BFS_NUM column
#     Muni_pop_final$BFS_NUM <- sort(unique(raw_mun_popdata$BFS_NUM))
#     
#     # Loop over each year and sum populations for merged municipalities
#     for (j in Time_points) {
#       for (i in 1:length(Muni_pop_final$BFS_NUM)) {
#         Muni_pop_final[i, as.character(j)] <- sum(raw_mun_popdata[raw_mun_popdata$BFS_NUM == 
#                                                                     Muni_pop_final$BFS_NUM[i], 
#                                                                   as.character(j)], na.rm = TRUE)
#       }
#     }
#     
#     # Replace raw data with the combined data
#     raw_mun_popdata <- Muni_pop_final
#   }
#   
#   # Add canton number
#   raw_mun_popdata$KANTONSNUM <- sapply(raw_mun_popdata$BFS_NUM, function(x){
#     unique(Muni_shp$KANTONSNUM[Muni_shp$BFS_NUMMER == x])
#   })
#   
#   # Save a copy for future population layers
#   saveRDS(raw_mun_popdata, "Data/Preds/Raw/Socio_economic/Population/raw_muni_pop_historic.rds")
#   
#   ### Create Historic Municipality Population Rasters
#   
#   # Separate population data for LULC years (excluding 2018 as only one layer is required)
#   pop_in_LULC_years <- raw_mun_popdata[, c("BFS_NUM", LULC_years[1:3])]
#   
#   Var_name <- "Muni_pop"
#   
#   # Link with spatial municipality data, rasterize, and save
#   Muni_save_paths <- sapply(LULC_years[1:3], function(i){
#     
#     # Define save path
#     save_path <- file.path(Prepped_layers_dir, 
#                            "Socio_economic/Employment/Population", 
#                            paste0(Var_name, "_", i, ".tif"))
#     
#     # Match population values with shapefile
#     Muni_shp$Pop_i <- sapply(Muni_shp$BFS_NUMMER, function(Muni_num){
#       pop_value <- pop_in_LULC_years$`i`[pop_in_LULC_years$BFS_NUM == Muni_num]
#       if(length(pop_value) == 0) { return(NA) } else { return(as.numeric(pop_value)) }
#     })
#     
#     # Rasterize using terra
#     pop_rast <- terra::rasterize(Muni_shp, Ref_grid, field = "Pop_i", background = NA)
#     
#     # Save raster
#     terra::writeRaster(pop_rast, filename = save_path, overwrite = TRUE)
#     
#     return(save_path)
#   })
#   
#   # Update the predictor table with the file paths
#   Pred_table_long$Prepared_data_path[Pred_table_long$Covariate_ID == Var_name] <- Muni_save_paths
#   Pred_table_long$Prepared[Pred_table_long$Covariate_ID == Var_name] <- "Y"
#   
#   # Clean up
#   rm(px_data, px_df, raw_mun_popdata, Muni_shp, muni_mutations, 
#      mutation_index, pop_in_LULC_years, Muni_pop_final, Muni_save_paths)
# }
# 
# #### D.2- Biophysical: Soil, Continentality, and Light (Descombes et al. 2020)
# 
# # Grab URL from predictor table
# Biophys_url <- unlist(strsplit(Preds_to_prepare$URL[grep("Descombes et al. 2020", Preds_to_prepare$Data_citation)], ","))
# 
# # Download and unpack using custom function
# Biophys_dir <- "Data/Preds/Raw/Biophysical"
# dir.create(Biophys_dir, recursive = TRUE, showWarnings = FALSE)
# lulcc.downloadunzip(url = Biophys_url, save_dir = Biophys_dir)
# 
# # Download metadata
# Biophys_meta <- openxlsx::read.xlsx("https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx")
# 
# # Clean required column names 
# colnames(Biophys_meta)[1:3] <- c("Layer_name", "Abbrev", "Desc_name") 
# 
# # Correct spelling mistake
# Biophys_meta$Desc_name[25] <- "Continentality"
# 
# # Get layer names using variable names
# Biophys_var_names <- unique(Preds_to_prepare$Variable_name[Preds_to_prepare$Data_citation == "Descombes et al. 2020"])
# Biophys_layer_names <- Biophys_meta$Layer_name[Biophys_meta$Desc_name %in% Biophys_var_names]
# 
# # Get descriptive names
# Biophys_desc_names <- Biophys_meta$Desc_name[Biophys_meta$Layer_name %in% Biophys_layer_names]
# 
# # Match descriptive names with the predictor table and return the covariate ID
# names(Biophys_layer_names) <- unique(Preds_to_prepare$Covariate_ID[Preds_to_prepare$Variable_name %in% Biophys_desc_names])
# 
# # Get layer paths
# Biophys_paths <- sapply(Biophys_layer_names, function(x) {
#   list.files(Biophys_dir, pattern = x, full.names = TRUE, recursive = TRUE)
# })
# 
# # Loop over paths and process layers
# for(i in 1:length(Biophys_paths)){
#   
#   Var_path <- Biophys_paths[i]
#   Var_name <- names(Biophys_paths)[i]
#   
#   # Load data using terra
#   Raw_dat <- terra::rast(Var_path)
#   
#   # Re-project and change resolution to match reference grid
#   Prepped_dat <- terra::project(Raw_dat, Ref_grid, method = 'near')
#   Prepped_dat_resamp <- terra::resample(Prepped_dat, Ref_grid, method = 'near')
#   
#   # Define save path
#   category <- unique(Preds_to_prepare$Predictor_category[Preds_to_prepare$Covariate_ID == Var_name])
#   layer_path <- file.path(Prepped_layers_dir, category, paste0(Var_name, ".tif"))
#   
#   # Save raster
#   terra::writeRaster(Prepped_dat_resamp, filename = layer_path, overwrite = TRUE)
#   
#   # Update the predictor table
#   Pred_table_long$Prepared_data_path[Pred_table_long$Covariate_ID == Var_name] <- layer_path
#   Pred_table_long$Prepared[Pred_table_long$Covariate_ID == Var_name] <- "Y"
#   
#   # Clean up
#   rm(Raw_dat, Prepped_dat, Prepped_dat_resamp, layer_path)
# }
# 
# #### D.3- Population
# 
# # Check if population predictors need to be prepared (Handled in D.1 above)
# 
# ### =========================================================================
# ### X- Update Predictor Table for SA Predictors
# ### =========================================================================    
# 
# # Load predictor table as workbook to add sheets
# Pred_table_update <- openxlsx::loadWorkbook(file = Pred_table_path)
# 
# # Split the table back into DataFrames for each period and save
# Periodic_pred_tables <- split(Pred_table_long, Pred_table_long$period)  
# 
# # Loop over period tables, adding sheets and adding the predictors to them
# for(i in names(Periodic_pred_tables)){
#   
#   # Attempt to add a new worksheet; ignore if it already exists
#   tryCatch({
#     openxlsx::addWorksheet(Pred_table_update, sheetName = i)
#   }, error = function(e) {
#     message(paste("Sheet", i, "already exists. Overwriting..."))
#   })
#   
#   # Write data to the sheet
#   openxlsx::writeData(Pred_table_update, sheet = i, x = Periodic_pred_tables[[i]])
# }
# 
# # Save the updated workbook
# openxlsx::saveWorkbook(Pred_table_update, Pred_table_path, overwrite = TRUE)    
# 
# cat('Preparation of Suitability and Accessibility predictor layers complete \n')
# 
# ### =========================================================================
# ### X- Create Neighbourhood Predictors
# ### =========================================================================
# 
# # This process is lengthy so is presented in a separate script, which includes
# # updating of the predictor table after layer creation
# 
# # Source the neighbourhood predictor preparation script
# source("Scripts/Preparation/Nhood_predictor_prep.R", echo = TRUE)
