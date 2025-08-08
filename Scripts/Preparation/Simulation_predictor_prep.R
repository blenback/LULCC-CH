#############################################################################
## Simulation_predictor_prep: Prepare predictor data lookup tables for simulation 
## time steps
##
## Date: 18-11-2021
## Author: Ben Black
#############################################################################

# All packages are sourced in the master document, uncomment here
#Vector packages for loading
# packs<-c("foreach", "data.table", "raster", "tidyverse",
#          "testthat", "sjmisc", "tictoc", "doParallel",
#          "lulcc", "pbapply", "stringr", "readr", "openxlsx", "bfsMaps", 
#          "jsonlite", "httr", "xlsx", "zen4R)
# 
# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))

#Supplied by master script only uncomment for testing
#Predictor table file path
#Pred_table_path <- "Tools/Predictor_table.xlsx"

#Load in the grid to use use for re-projecting the CRS and extent of predictor data
Ref_grid <- rast(Ref_grid_path)

#Fetch simulation start and end times from simulation control table
Simulation_control <- read.csv(Sim_control_path)
Simulation_start <- min(Simulation_control$Scenario_start.real)
Simulation_end <- max(Simulation_control$Scenario_end.real)
Step_length <- unique(Simulation_control$Step_length.real)

#vector time steps for future predictions
Time_steps <- seq(Simulation_start, Simulation_end, Step_length)

#base dir for saving
Prepared_layers_dir <- "Data/Preds/Prepared/Layers"

# load kanton shapefile
Canton_shp <- terra::vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

# merge polygons with the same canton names
Canton_shp <- terra::aggregate(Canton_shp, by = "NAME")

# set crs to the reference grid
Canton_shp <- terra::project(Canton_shp, crs(Ref_grid))

# rasterize
Canton_shp$name <- as.factor(Canton_shp$NAME)

# create a df from the shp
Canton_df <- as.data.frame(Canton_shp, xy = FALSE)

### =========================================================================
### A.1 Finalising layers of future FTE values for 2020-2025
### =========================================================================

# In calibration_predictor_prep.R I already prepared some layers for the 2020-2025 period
# but these need to be duplicated for each scenario

# list the existing files
Econ_data_paths <- list.files("Data/Preds/Prepared/Layers/Socio_economic/Employment", 
                              pattern = "2020_2025", full.names = TRUE)

# get names of unique scenarios from the simulation control table
Scenario_names <- unique(Simulation_control$Scenario_ID.string)

# rep Econ_data_paths to match the number of scenarios
Scenario_econ_paths <- rep(Econ_data_paths, length(Scenario_names))

# convert Scenario_econ_paths to df with a column named old_path
Scenario_econ_paths <- data.frame(old_path = Scenario_econ_paths)

# add a column of the Scenario names
Scenario_econ_paths$Scenario <- rep(Scenario_names, each = length(Econ_data_paths))

# add a column of the sectors e.g sec1
FTE_var_names <- c("chg_FTE_Sec1", "chg_FTE_Sec2", "chg_FTE_Sec3")
Scenario_econ_paths$sector <- rep( FTE_var_names, 
                                   length(Scenario_names))

# loop over the scenario names and paste these prior to the string '.tif' in the file paths
Scenario_econ_paths$new_path <- unlist(lapply(Scenario_names, function(x){
  # split path on .tif
  path_split <- strsplit(Econ_data_paths, ".tif")
  # paste the scenario name to the first part of the path
  new_paths <- paste0(path_split, "_", x, ".tif")
}))

# use the old_path to make copies of the files named using the new_path
for(i in 1:nrow(Scenario_econ_paths)){
  file.copy(from = Scenario_econ_paths$old_path[i], 
            to = Scenario_econ_paths$new_path[i], 
            overwrite = TRUE)
}

# remove the files of Econ_data_paths
file.remove(Econ_data_paths)

# load the predictor table
Predictor_table <- readxl::read_excel(Pred_table_path, sheet = "2020")

# loop over FTE_var_names and identify rows in Predictor_table$Covariate_ID
for(x in FTE_var_names) {
  
  # update the Prepared_data_path column in Predictor_table
  Predictor_table[Predictor_table$Covariate_ID == x, "Prepared_data_path"] <- Scenario_econ_paths$new_path[Scenario_econ_paths$sector == x]
  
  # update the Scenario_variant column in Predictor_table
  Predictor_table[Predictor_table$Covariate_ID == x, "Scenario_variant"] <- Scenario_econ_paths$Scenario[Scenario_econ_paths$sector == x]
  
  # change Prepared to Y
  Predictor_table[Predictor_table$Covariate_ID == x, "Prepared"] <- "Y"
}

#load predictor_table as workbook to add sheets
pred_workbook <- openxlsx::loadWorkbook(file = Pred_table_path)

# update the 2020 sheet in the workbook
openxlsx::writeData(pred_workbook, sheet = "2020", Predictor_table)

# save the workbook
openxlsx::saveWorkbook(pred_workbook, file = Pred_table_path, overwrite = TRUE)

### =========================================================================
### A.2 Preparing future FTE layers for all periods beyond 2020-2025
### =========================================================================

# load the rds file of the historic FTE data produced in calibration_predictor_prep.R
Historic_FTE <- readRDS(FTE_lab_market, file = "Data/Preds/FTE_historic_cantonal_dat.rds")

# rename 'name' column 'Canton' in Historic_FTE
Historic_FTE <- Historic_FTE %>%
  rename(Canton = name)

# only columns Canton and any columns containing '2020' in the name
Historic_FTE <- Historic_FTE %>%
  select(Canton, contains("2020"))

# for the columns that contain "2020" split the name on the "_" and extract the sector string
Historic_FTE <- Historic_FTE %>%
  pivot_longer(cols = -c(Canton), 
               names_to = c("Sector", "year"),
               values_to = "2020",
               names_sep = "_")%>%
  select(-year)

# load the table of future FTE values prepared for the project
Future_FTE_tbl <- read.csv("Data/Preds/NCCS_future_FTE.csv")

# remove the X in the colnames
Future_FTE_tbl <- Future_FTE_tbl %>%
  rename_with(~ gsub("^X", "", .), everything())

# In Sector column replace string 'Sector@ with 'Sec'
Future_FTE_tbl <- Future_FTE_tbl %>%
  mutate(Sector = gsub("Sector", "Sec", Sector))

# vector a list that links Canton names and abbreviations
canton_abbr <- list(
  "Aargau" = "AG",
  "Appenzell Ausserrhoden" = "AR",
  "Appenzell Innerrhoden" = "AI",
  "Basel-Landschaft" = "BL",
  "Basel-Stadt" = "BS",
  "Bern" = "BE",
  "Fribourg" = "FR",
  "Genève" = "GE",
  "Glarus" = "GL",
  "Graubünden" = "GR",
  "Jura" = "JU",
  "Luzern" = "LU",
  "Neuchâtel" = "NE",
  "Nidwalden" = "NW",
  "Obwalden" = "OW",
  "Schaffhausen" = "SH",
  "Schwyz" = "SZ",
  "Solothurn" = "SO",
  "St. Gallen" = "SG",
  "Thurgau" = "TG",
  "Ticino" = "TI",
  "Uri" = "UR",
  "Valais" = "VS",
  "Vaud" = "VD",
  "Zug" = "ZG",
  "Zürich" = "ZH"
)

# Replace the abbrevations of Cantons in Future_FTE_tbl with the names
Future_FTE_tbl$Canton <- sapply(Future_FTE_tbl$Canton, function(x) {
  if (x %in% canton_abbr) {
    return(names(canton_abbr)[canton_abbr == x])
  } else {
    return(x)
  }
})


# the numeric columns are 4:8 get the names
FTE_year_cols <- colnames(Future_FTE_tbl)[4:8]

# convert these columns to numeric and multiply values by 1000
Future_FTE_tbl[FTE_year_cols] <- Future_FTE_tbl[FTE_year_cols] %>%
  mutate(across(everything(), ~ as.numeric(.) * 1000))

# Loop over the unique scenarios in Future_FTE_tbl$Scenario
# adding the column 2020 from Historic_FTE to Future_FTE_tbl matching rows on Canton and Sector
Future_FTE_tbl$'2020' <- sapply(1:nrow(Future_FTE_tbl), 
                                 function(i) {
                                   row <- Future_FTE_tbl[i, ]
                                   Historic_row <- Historic_FTE %>%
                                     filter(Canton == row$Canton, Sector == row$Sector)
                                   if (nrow(Historic_row) > 0) {
                                     return(Historic_row$`2020`)
                                   } else {
                                     return(NA)
                                   }
                                 }
)

# add 2020 to FTE_year_cols
FTE_year_cols <- c("2020", FTE_year_cols)

# remove first year as it has already been prepared
FTE_steps <- Time_steps[-1]

# for each value apart from the 1st in Time_steps created a named list of the start and end
# points of the FTE years for each period
# the start year is 4 years before the Time_step value and the End year is the Time_step value
FTE_periods <- lapply(1:length(FTE_steps), function(x){
  Start_year <- FTE_steps[x] - 4
  End_year <- FTE_steps[x]
  return(c(Start_year, End_year))
})
names(FTE_periods) <- paste0(FTE_steps, "_", FTE_steps[-1])

# get all unique values than need to be prepared
years_to_interp <- unique(unlist(FTE_periods))

# remove any years that are already in the FTE_year_cols
years_to_interp <- years_to_interp[!years_to_interp %in% as.numeric(FTE_year_cols)]

# add columns of years_to_interpret to the Future_FTE_tbl
for (year in years_to_interp) {
  Future_FTE_tbl[[as.character(year)]] <- NA
}

# reorder the columns so that the years are sequentially increasing
Future_FTE_tbl <- Future_FTE_tbl[ , c("Scenario", "Sector", "Canton", sort(c(as.numeric(FTE_year_cols), years_to_interp)))]

#loop over the Kantons and scenarios in Future_FTE_tbl, fitting a linear model
# to each and using it to interpolate the required years and adding the data to a list
FTE_predicts <- as.data.frame(rbindlist(lapply(unique(Future_FTE_tbl$Canton), function(canton) {

  # filter the Future_FTE_tbl for the current kanton
  canton_data <- Future_FTE_tbl %>% filter(Canton == canton)
  
  # loop over the scenarios with an internal loop over the sectors
  scenario_predicts <- as.data.frame(rbindlist(lapply(unique(kanton_data$Scenario), function(scenario) {

    # filter for the current scenario
    scenario_data <- canton_data[canton_data$Scenario == scenario, ]
  
    # Apply interpolation to each row
    for(i in 1:nrow(scenario_data)) {
    
      scenario_row <- scenario_data[i, ]
      
      test <- scenario_row[FTE_year_cols]
      
      # Interpolate the FTE values for the years to interpolate
      interp_values <- stats::approx(y = scenario_row[FTE_year_cols], 
                                       xout = years_to_interp, 
                                       x = FTE_year_cols)
      
      # Store the interpolated values in the scenario_data
      scenario_data[i, as.character(interp_values$x)] <- interp_values$y
    }
    return(scenario_data)
    })))
  return(scenario_predicts)
  })))

#base dir
Prepared_FTE_dir <- "Data/Preds/Prepared/Layers/Socio_economic/Employment"
dir.create(Prepared_FTE_dir, recursive = TRUE)

# loop over the scenarios and sectors, creating a raster for each
FTE_paths <- lapply(unique(FTE_predicts$Scenario), function(scenario){
  
  # select the scenario data
  scenario_data <- FTE_predicts %>% filter(Scenario == scenario)
  
  #loop over sectors calculating difference for FTE_periods
  sector_paths <- lapply(unique(scenario_data$Sector), function(sector){
  
    #seperate data for sector
    sector_data <- scenario_data %>% 
      filter(Sector == sector) 
    
    # convert FTE_periods to a data.frame with the names as a column
    # 'period_name', the first value 'start_year and the 2nd value "end_year'
    FTE_periods_df <- data.frame(
      period_name = names(FTE_periods),
      start_year = sapply(FTE_periods, function(x) x[1]),
      end_year = sapply(FTE_periods, function(x) x[2]),
      stringsAsFactors = FALSE
    )
    
    # add column 'file_path' with NAs
    FTE_periods_df$file_path <- NA
    
    # loop over the change periods calculated the difference in values between the two years
    for(i in seq_along(FTE_periods)) {
    
    period_name <- names(FTE_periods)[[i]]
    
    #get the change period dates
    year1 <- as.character(FTE_periods[[i]][1])
    year2 <- as.character(FTE_periods[[i]][2])
    
    #calculate the difference in values between the two years
    Diff_col_name <- paste0("Diff_", year1, "_", year2)
    sector_data[[Diff_col_name]] <- sector_data[[year2]] - sector_data[[year1]]
    
    # add the column to the shapefile
    Canton_shp[[Diff_col_name]] <- sector_data[[Diff_col_name]]
  
    # rasterize
    #Period_rast <- terra::rasterize(Canton_shp, Ref_grid, field = Diff_col_name, background=NA)
    
    # create a file path for the raster
    #Data/Preds/Prepared/Layers/Socio_economic/Employment/Avg_chg_FTE_2009_2018_Sec1.tif
    file_path <- paste0(Prepared_FTE_dir, "/chg_FTE_", period_name, "_", sector, "_", scenario, ".tif")
    
    # add file path to the FTE_periods_df
    FTE_periods_df$file_path[i] <- file_path
    
    # # write the raster to file
    # terra::writeRaster(
    #   Period_rast,
    #   file_path,
    #   overwrite = TRUE)
  
    }#close loop over change periods
    
    return(FTE_periods_df)
    })
  names(sector_paths) <- unique(scenario_data$Sector)
  # create a data frame with the sector paths
  sector_paths_df <- as.data.frame(rbindlist(sector_paths, idcol = "Sector"))
  return(sector_paths_df)
  #close loop over sectors
})#close loop over scenarios
names(FTE_paths) <- unique(FTE_predicts$Scenario)
# create a data frame with the scenario paths
FTE_paths_df <- as.data.frame(rbindlist(FTE_paths, idcol = "Scenario"))

# add mising columns
FTE_paths_df$period <- FTE_paths_df$period_name
FTE_paths_df$period_name <- NULL
FTE_paths_df$Covariate_ID <- paste0("chg_FTE_", FTE_paths_df$Sector)
FTE_paths_df$Unique_ID <- NA
FTE_paths_df$CA_category <- "Suitability"
FTE_paths_df$Predictor_category <- "Socio_economic"
FTE_paths_df$Variable_name <- NA
FTE_paths_df$Data_citation <- "Project Internal"
FTE_paths_df$URL <- NA
FTE_paths_df$Original_resolution <- "Canton"
FTE_paths_df$Static_or_dynamic <- "Dynamic"
FTE_paths_df$Temporal_coverage <- paste0(FTE_paths_df$start_year, "_", FTE_paths_df$end_year)
FTE_paths_df$Temporal_resolution <- NA
FTE_paths_df$Prepared <- "Y"
FTE_paths_df$Raw_data_path <- NA
FTE_paths_df$Prepared_data_path <- FTE_paths_df$file_path
FTE_paths_df$file_path <- NULL
FTE_paths_df$Scenario_variant <- FTE_paths_df$Scenario
FTE_paths_df$Scenario <- NULL

#load predictor_table as workbook to add sheets
pred_workbook <- openxlsx::loadWorkbook(file = Pred_table_path)

# loop over the unique values of FTE_paths$period_name adding sheets to the workbook
sapply(unique(FTE_paths_df$period), function(period_name) {
  
  # select the rows for the period
  period_data <- FTE_paths_df %>% filter(period == period_name)
  
  period_end <- as.character(period_data$end_year[1])

  # if period_data$end_year is not already a sheet in the workbook, create a new sheet
  if (!period_end %in% openxlsx::getSheetNames(Pred_table_path)) {
      
      # add a new sheet with the name of the end_year
      openxlsx::addWorksheet(pred_workbook, sheet = period_end)
    
      # write the data except the columns 'Sector, end_year and start_year to the sheet
      openxlsx::writeData(pred_workbook, sheet = period_end, 
                      x = period_data %>% select(-Sector, -start_year, -end_year))
  } else {
    
    Predictor_data <- readxl::read_excel(Pred_table_path, sheet = period_end)
    
    # remove the columns 'Sector, end_year and start_year from the data
    period_data <- period_data %>% select(-Sector, -start_year, -end_year)
    
    # remove the rows with the same Covariate_ID from the Predictor_data
    Predictor_data <- Predictor_data %>% 
      filter(!Covariate_ID %in% period_data$Covariate_ID)
    
    # combine the Predictor_data with the period_data
    period_data <- rbind(Predictor_data, period_data)
    
    # write the data to the sheet
    openxlsx::writeData(pred_workbook, sheet = period_end, 
                        x = period_data)
  }
  


})

# save the workbook
openxlsx::saveWorkbook(pred_workbook, file = Pred_table_path, overwrite = TRUE)


### =========================================================================
### B- Preparing mechanism of future population projections
### =========================================================================

#This process does not result in the creation of spatial layers of
#future population, rather it involves modelling the relationship between 
#historic % of the cantonal population and the % of urban area per municipality
#These models are then used within the simulation step to predict % of cantonal
#population based on the current simulated % of urban area per municipality
#with population values then being distributed according the correct projection
#under the scenario being simulated. 

#------------------------------------------------------------------------------
#B.1- Prepare historic cantonal population data
#------------------------------------------------------------------------------

# #reload historic muni pop data produced in historic predictor prep
# raw_mun_popdata <- readRDS("Data/Preds/Raw/Socio_economic/Population/raw_muni_pop_historic.rds")
# 
# #load kanton shapefile
# Canton_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")
# 
# #read in population data from html and convert to DF
# px_data <- data.frame(read.px("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master"))
# 
# # subset px_data to historic cantonal population data
# raw_can_popdata <- px_data[px_data$Demografische.Komponente == "Bestand am 31. Dezember" &
#                      px_data$Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total" &
#                      px_data$Geschlecht == "Geschlecht - Total", c(4:6)]
# names(raw_can_popdata) <- c("Name_Canton", "Year", "Population")
# raw_can_popdata$Name_Canton <- as.character(raw_can_popdata$Name_Canton)
# 
# #remove municipalities records by inverse-matching on the numeric contained in their name
# raw_can_popdata <- raw_can_popdata[grep(".*?([0-9]+).*", raw_can_popdata$Name_Canton, invert = TRUE),]
# 
# #all cantons 
# raw_can_popdata <- raw_can_popdata[grep(">>", raw_can_popdata$Name_Canton, invert = TRUE),]
# raw_can_popdata <- raw_can_popdata[grep(paste0(unique(Canton_shp@data[["NAME"]]), collapse = "|"), raw_can_popdata$Name_Canton),]
# 
# #pivot to wide
# raw_can_popdata <- raw_can_popdata %>% pivot_wider(names_from = "Year",
#                                                 values_from = "Population")
# 
# #match Canton names to the shape file
# for(i in unique(Canton_shp@data[["NAME"]])){
# raw_can_popdata$Name_Canton[grep(i, raw_can_popdata$Name_Canton)] <- i  
# }
# 
# #add cantons numbers
# raw_can_popdata$KANTONSNUM <- sapply(raw_can_popdata$Name_Canton, function(x){
# unique(Canton_shp@data[Canton_shp@data$NAME == x, "KANTONSNUM"])
# }) 
# 
# #get the indices of columns that represent the years
# date_cols <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames(raw_can_popdata))))
# 
# #-------------------------------------------------------------------------------
# # B.2- Calculate % cantonal population per municipality
# #-------------------------------------------------------------------------------
# 
# pop_percentages <- do.call(cbind, sapply(date_cols, function(year){
#   
# #loop over canton numbers
# muni_percs <- rbindlist(sapply(unique(raw_can_popdata$KANTONSNUM), function(canton_num){
# 
# #subset cantonal data by year  
# can_data <- raw_can_popdata[raw_can_popdata$KANTONSNUM == canton_num, c(paste(year), "KANTONSNUM")]
# 
# #subset the municipality data by kanton name and year
# muni_data <- raw_mun_popdata[raw_mun_popdata$KANTONSNUM == can_data$KANTONSNUM, c(paste(year), "KANTONSNUM")]
# muni_data$KANTONSNUM <- NULL
# 
# #loop over municipalities
# muni_data[[paste0("Perc_", year)]]  <- as.numeric(sapply(muni_data[[paste(year)]], function(year_value){
# perc_value <- year_value/can_data[,paste(year)]*100
# },simplify = TRUE)) #close loop over municipalities
# 
# return(muni_data)
# }, simplify = FALSE))#close loop over kantons  
# 
# return(muni_percs)
# }, simplify = FALSE)) #close loop over years
# 
# #add back in BFS and Cantons numbers
# pop_percentages$BFS_NUM <- raw_mun_popdata$BFS_NUM
# pop_percentages$KANTONSNUM <- raw_mun_popdata$KANTONSNUM
# 
# #------------------------------------------------------------------------------
# #B.3- Calculate % urban area per municipality
# #------------------------------------------------------------------------------
# 
# #Load the most recent LULC map
# current_LULC <- raster("Data/Historic_LULC/LULC_2018_agg.gri")
# 
# #subset to just urban cell
# Urban_rast <- current_LULC == 10
# 
# #Zonal stats to get urban area per kanton
# Canton_urban_areas <- raster::extract(Urban_rast, Canton_shp, fun=sum, na.rm=TRUE, df=TRUE)
# 
# #append Canton ID 
# Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM
# 
# #combine areas for cantons with multiple polygons
# Canton_urban_areas <- Canton_urban_areas %>%
#   dplyr::group_by(Canton_num) %>%
#   dplyr::summarise(across(c(layer), sum))
# 
# #load the municipality shape file
# Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
# 
# #filter out non-swiss municipalities
# Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ] 
# 
# #Zonal stats to get number of Urban cells per Municipality polygon
# #sum is used as a function because urban cells = 1 all others = 0
# Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun=sum, na.rm=TRUE, df=TRUE)
# 
# #append Canton and Municipality IDs
# Muni_urban_areas$Canton_num <- Muni_shp@data[["KANTONSNUM"]]
# Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
# Muni_urban_areas$Perc_urban <- 0
# 
# #loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
# for(i in Canton_urban_areas$Canton_num){
# 
# #vector kanton urban area
# Kan_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "layer"])  
# 
# #subset municipalities to this canton number 
# munis_indices <- which(Muni_urban_areas$Canton_num == i)
# 
# #loop over municipalities in the Canton and calculate their urban areas as a % of the Canton's total  
# for(muni in munis_indices){
# Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "layer"]/Kan_urban_area)*100
#   } #close inner loop 
# } #close outer loop
# 
# #------------------------------------------------------------------------------
# #B.4- Model relationship between cantonal % population and % urban area
# #------------------------------------------------------------------------------
# 
# #subset pop percentages to 2018
# Muni_percs <- pop_percentages[, c("BFS_NUM", "KANTONSNUM", "Perc_2018")]
# colnames(Muni_percs) <-  c("BFS_NUM", "KANTONSNUM", "Perc_pop")
# 
# #combine with % urban values
# Muni_percs$Perc_urban <- sapply(Muni_percs$BFS_NUM, function(x){
# sum(Muni_urban_areas[Muni_urban_areas$Muni_num == x, "Perc_urban"])  
# })
# 
# #loop over kantons and model relationship
# Canton_models <- lapply(unique(Muni_percs$KANTONSNUM), function(canton_num){
# 
# #subset to data for this canton
# kanton_data <- Muni_percs[Muni_percs$KANTONSNUM == canton_num,]
# 
# #produce GLM
# Canton_model <- glm(data = kanton_data, formula = Perc_pop ~ Perc_urban, family = gaussian())  
#   
# return(Canton_model)
# })
# names(Canton_models) <- unique(Muni_percs$KANTONSNUM)
# 
# #save models
# saveRDS(Canton_models, "Data/Preds/Tools/Dynamic_pop_models.rds")
# 
# ### B.5- Prepare Cantonal projections of population development 
# 
# raw_data_path <- "Data/Preds/Raw/Socio_economic/Population/raw_pop_projections.xlsx"
# 
# #Download the required file 
# download.file(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/12107013/master", destfile = raw_data_path, mode="wb")
# 
# #Vector existing sheet names
# Sheet_names <- getSheetNames(raw_data_path)
# 
# #name the sheet names with the english names
# names(Sheet_names) <- c("Ref", "High", "Low")
# 
# #Create a workbook to add the ckleaned data too
# pop_proj_wb <- openxlsx::createWorkbook()
# 
# #The population projection table does not contain the canton numbers and because
# #the Canton names are in German they cannot be matched with the shapefile
# #to get the numbers (e.g Wallis - Valais etc.) instead load a different FSO
# #table to get the numbers 
# Canton_lookup <- openxlsx::read.xlsx("https://www.atlas.bfs.admin.ch/core/projects/13.40/xshared/xlsx/134_131.xlsx", rows = c(4:31), cols = c(2,3))
# Canton_lookup$Canton_num <- seq(1:nrow(Canton_lookup))
# 
# #loop over time steps adding sheets and adding the predictors to them
# #i=1
# for (i in 1:length(Sheet_names)){
# 
# org_sheet_name <- Sheet_names[i]
# new_sheet_name <- names(Sheet_names)[i]
#    
# #load correct sheet of raw data
# tempdf <- openxlsx::read.xlsx(raw_data_path, sheet = org_sheet_name, startRow = 2)
# colnames(tempdf)[1] = "Canton_name" #rename first column
# tempdf <- tempdf[tempdf$Canton_name != "Schweiz",] # remove the total for switzerland
# tempdf <- tempdf[complete.cases(tempdf),] #remove incomplete or empty rows
# 
# #vector the names of the year columns
# year_cols <- names(tempdf)[2:ncol(tempdf)]
# 
# #add the canton number
# tempdf$Canton_num <- sapply(tempdf$Canton_name, function(x) {
# Canton_lookup[grepl(x, Canton_lookup$X2), "Canton_num"]
# })
# 
# #replace values of non-matching names
# tempdf[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num), "Canton_num"] <- Canton_lookup$Canton_num[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num)]
# rownames(tempdf) <- 1:nrow(tempdf) #correct row names
# 
# #check if any time-points for scenarios are missing
# Missing_years <- Time_steps[!paste(Time_steps) %in% colnames(tempdf)]
# 
# if(length(Missing_years)>0){
# #extrapolate the missing years
# #add columns for missing years
# tempdf[paste(Missing_years)] <- NA
#   
# #loop over rows (cantons)
# for(r in 1:nrow(tempdf)){
# 
#   row_dat <-data.frame(t(tempdf[r, year_cols]))
#   names(row_dat) <- "Pred_pop"
#   row_dat$year <- as.numeric(year_cols) 
#  
#   #create linear model
#   mod <- lm(formula = Pred_pop~year, data = row_dat)
#   
#   #predict missing years
#   tempdf[r, paste(Missing_years)] <- round(predict(mod, data.frame(year = Missing_years)),0)
#   } #close loop over rows
# } #close if statement
# 
# #create sheet in workbook, the try() is necessary in case sheets already exist  
# try(addWorksheet(pop_proj_wb, sheetName = new_sheet_name))  
# 
# #write the data to the sheet
# writeData(pop_proj_wb, sheet = new_sheet_name, x = tempdf)
# 
# }
# #save workbook
# openxlsx::saveWorkbook(pop_proj_wb, "Data/Preds/Tools/Population_projections.xlsx", overwrite = TRUE)

# Preparing new population data for NCCS project
# The structure that is needed for the simulation is an excel file with individual sheets for each scenario
# within each sheet there are columns for the canton name, number, and for each year with population values

# load csv of population projections
Future_pop <- read.csv("Data/Preds/NCCS_future_population.csv")

# remove 'X' from colnames
colnames(Future_pop) <- str_replace_all(colnames(Future_pop), "X", "")

# add a column for canton numbers
Future_pop$Canton_num <- sapply(Future_pop$Canton, function(x) {
  Canton_df[Canton_df$NAME == x, "mean_KANTONSNUM"]
})


# get column names of years of population
pop_years <- colnames(Future_pop)[3:8]

# convert values to millions
Future_pop[pop_years] <- Future_pop[pop_years] * 1e6

# subset Time_steps by removing any years in pop_years
pop_interps <- as.character(Time_steps[!paste(Time_steps) %in% pop_years])

# add columns of pop_interps to the Future_pop
for (year in pop_interps) {
  Future_pop[[year]] <- NA
}

# reorder the columns so that the years are sequentially increasing
Future_pop <- Future_pop[, c("Canton", "Canton_num", "Scenario", sort(as.numeric(c(pop_years,pop_interps))))]

# create a workbook to store the interpolated population data
pop_proj_wb <- openxlsx::createWorkbook()

# loop over the scenarios
sapply(unique(Future_pop$Scenario), function(scenario){
  
  # seperate the data by scenario
  scenario_data <- Future_pop[Future_pop$Scenario == scenario, ]
  
  # create a new sheet in the workbook
  try(addWorksheet(pop_proj_wb, sheetName = scenario))
  
  # Apply interpolation to each row
  for(i in 1:nrow(scenario_data)) {
    
      scenario_row <- scenario_data[i, ]
      
      # Interpolate the FTE values for the years to interpolate
      interp_values <- stats::approx(y = scenario_row[pop_years], 
                                       xout = pop_interps, 
                                       x = pop_years)
      
      # Store the interpolated values in the scenario_data
      scenario_data[i, as.character(interp_values$x)] <- interp_values$y
  }
  
  # add data to workbook sheet
  writeData(pop_proj_wb, sheet = scenario, x = scenario_data)
    
  }) # close loop over scenarios

# save the workbook
openxlsx::saveWorkbook(pop_proj_wb, "Data/Preds/Tools/Population_projections.xlsx", overwrite = TRUE)

### =========================================================================
### C- Updating predictor table with future climatic data and static vars
### =========================================================================

# Load RDS file of info for future climatic layers prepared in Calibration_predictor_prep
Future_clim_info <- readRDS("Data/Preds/NCCS_future_climatic_vars.rds")

# sort by sheet name
Future_clim_info <- Future_clim_info[order(Future_clim_info$sheet_name), ]

# because the climate data only extends to 2085 but the simulations are until
# 2100 we need to use the 2085 data for the remaining periods.
Additional_years <- seq(2090, 2100, by = 5)

# get the rows for sheet_name "2085"
rows_2085 <- Future_clim_info[Future_clim_info$sheet_name == "2085", ]

# for i in Additional_years
for (year in Additional_years) {
  
  # create a new row with the year and the sheet_name
  new_rows <- rows_2085
  new_rows$sheet_name <- as.character(year)
  new_rows$period <- paste0(year-5, "_", year) 
  
  # append the new row to Future_clim_info
  Future_clim_info <- rbind(Future_clim_info, new_rows)
}

# now alter the values of Temporal_coverage for all rows to be 15 years before
# and 15 years after sheet_name
Future_clim_info$Temporal_coverage <- paste0(as.numeric(Future_clim_info$sheet_name) - 15, "_", 
                                               as.numeric(Future_clim_info$sheet_name) + 15)



# open the predictor table workbook
pred_workbook <- openxlsx::loadWorkbook(file = Pred_table_path)

# from the pred_workbook sheet = "2009_2018" get all rows where "Static_or_dynamic == 'static'
Static_vars <- read.xlsx(pred_workbook, sheet = "2009_2018", detectDates = TRUE)
Static_vars <- Static_vars[Static_vars$Static_or_dynamic == "static", ]
Static_vars$Scenario_variant <- "All"

# loop over the unique sheet_names in the Future_clim_info
for(sheet_name in unique(Future_clim_info$sheet_name)){
  
  # filter the Future_clim_info for the current sheet_name
  sheet_info <- Future_clim_info[Future_clim_info$sheet_name == sheet_name, ]
  
  # remove the column sheet_name
  sheet_info$sheet_name <- NULL
  
  # append sheet_info to the Static_vars
  to_add <- rbind(Static_vars, sheet_info)
  
  # first add the static_vars after any existing data in the sheet
  if(sheet_name %in% names(pred_workbook)) {
    # read the existing data from the sheet
    existing_data <- read.xlsx(pred_workbook, sheet = sheet_name)
    
    # change period value in to_add to match the existing data
    to_add$period <- existing_data$period[1]
    
    # check if there are existing rows
    if(nrow(existing_data) > 0) {
      # write the static variables after the existing rows
      writeData(pred_workbook,
                sheet = sheet_name,
                x = to_add,
                startRow = nrow(existing_data) + 1,
                colNames = FALSE)
    } else {
      # if no existing rows, just write the static variables
      writeData(pred_workbook, sheet = sheet_name, x = to_add, colNames = TRUE)
    }
  } else {
    # if the sheet does not exist, create it and write the static variables
    addWorksheet(pred_workbook, sheetName = sheet_name)
    writeData(pred_workbook, sheet = sheet_name, x = to_add, colNames = TRUE)
  }
  
} # close loop over sheets

# save the workbook
openxlsx::saveWorkbook(pred_workbook, file = Pred_table_path, overwrite = TRUE)





