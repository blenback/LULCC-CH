#############################################################################
## SA_var_prep: Use predictor table to prepare predictor layers at a uniform
##100m resolution, crs and extent prepared layers are saved seperately in a
## and then combined into raster stacks for easy loading
## Date: 01-08-2021
## Author: Ben Black
############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set your working directory
wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat",
         "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", "rgeos",
         "sf", "tiff", "bfsMaps", "rjstat", "future.apply", "future", "stringr")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#Load in the grid to use use for re-projecting the CRS and extent of predictor data
Ref_grid <- raster("Data/Ref_grid.gri")

#vector years of LULC data
LULC_years <- c("1985", "1997", "2009", "2018")

#create a list of the data/modelling periods
LULC_change_periods <- c()
for (i in 1:(length(LULC_years)-1)) {
            LULC_change_periods[[i]] <- c(LULC_years[i],LULC_years[i+1])}
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))

# #download basic map geometries for Switzerland
# Geoms_path <- "Data/Preds/Raw/CH_geoms"
# dir.create(Geoms_path)
# DownloadBfSMaps(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master",
# path = Geoms_path)

### =========================================================================
### X- Gather predictor information
### =========================================================================

#The predictor table contains details of the suitability and accessibility
#predictors in seperate sheets for the different time periods, some predictors
#are the same across periods (static) whereas other are temporally dynamic
#In terms of preparations some predictors are prepared by downloading the data
#direct from source and then processing with others being standardized
#from existing raw data

#create basic directories for prepared predictor layers
Prepped_stacks_dir<- "Data/Preds/Prepared/Stacks"
Prepped_layers_dir  <- "Data/Preds/Prepared/Layers" 
dir.create(Prepped_layers_dir, recursive = TRUE)
dir.create(Prepped_stacks_dir, recursive = TRUE)

#Predictor table file path
Pred_table_path <- "Data/Preds/Tools/Predictor_table_base.xlsx"

#get names of sheets to loop over
sheets <- excel_sheets(Pred_table_path)

#load all sheets as a list
Pred_tables <- lapply(sheets, function(x) read.xlsx(Pred_table_path, sheet = x))
names(Pred_tables) <- sheets

#combine tables for all periods
Pred_table_long <- as.data.frame(rbindlist(Pred_tables, idcol = "period"))

#filter for layers already completed
Pred_table_long <- Pred_table_long[Pred_table_long$Prepared != "Y",]

#create dirs for all predictor cateogries
sapply(unique(Pred_table_long[["Predictor_category"]]), function(x) {
  dir.create(paste0(Prepped_layers_dir, "/", x), recursive = TRUE)
  })
### =========================================================================
### X- Predictors from raw data
### =========================================================================

#The predictors with a raw data source are those already prepared in ValPar.CH
#at 25m resolution because they already use a grid of the same extent and CRS 
#processing is just aggregating to 100m

#seperate the preds that have a Raw path
Preds_source <- Pred_table_long[!is.na(Pred_table_long$Raw_data_path),]

#reduce to unique predictors
Preds_source_unique <- Preds_source[!duplicated(Preds_source$Covariate_ID), c("Covariate_ID", "Predictor_category", "URL", "Raw_data_path", "Prepared_data_path")]  

#loop over the preds loading the raw data, processing and saving, returning a prepared data_path
for(i in 1:nrow(Preds_source_unique)){
  
  #load data
  Raw_dat <- readRDS(unlist(Preds_source_unique[i,"Raw_data_path"]))

  #aggregate
  Agg_dat <- aggregate(Raw_dat, fact=4, fun=mean)
  
  #vector save path
  layer_path <- paste0(Prepped_layers_dir, "/", Preds_source_unique[i,"Predictor_category"] ,"/", Preds_source_unique[i,"Covariate_ID"], ".tif")

  #save
  writeRaster(Agg_dat, layer_path, overwrite=TRUE)
  
  #add the prepared path to the table
  Pred_table_long[Pred_table_long$Covariate_ID == Preds_source_unique[i,"Covariate_ID"], "Prepared_data_path"] <- layer_path
  Pred_table_long[Pred_table_long$Covariate_ID == Preds_source_unique[i,"Covariate_ID"], "Prepared"] <- "Y"
}

### =========================================================================
### X- Predictors from source
    
#The predictors that are prepared directly from source require different
#processing operations as such they are grouped into seperate chunks below    
    
### -------------------------------------------------------------------------
### X.1- Socio_economic: Employment
### -------------------------------------------------------------------------

#In order to have a common variable between the historic data and the future
#economic scenarios, the variables we will derive will be:
#1. Average annual change in number of full time equivalent employees in the primary sector
#2. Average annual change in number of full time equivalent employees in the secondary and tertiary sectors combined

#As the future scenarios of employment are expressed at the scale
#of labour market regions rather than municipalities then the historic data
#needs to be aggregated to this scale

#The historic employment data from the Federal statistical office
#comes from two sources:
#1. The business census (conducted from 1995-2008 for the timepoints: 1995, 2001, 2005, 2008)
#2. The business structure statistics (STATENT, conducted annual from 2011)

#The primary, secondary and tertiary sectors are themselves aggregations
#of all of the individual economic industries each of which as an official
#classification under the NOGA schematic. This schematic did change in 2008 with
#definitions/codes used for some industries being changed.
#It is difficult to match the old/new NOGA schemes exactly 
#however changes are minor in the scope of the number of industries them exactly  

### Wrangling Statent data
#get urls for variable and convert to named list
All_urls <- str_split(Pred_table_long[grep(Pred_table_long$Covariate_ID, pattern="Avg_chg_FTE"), "URL"][1], ",")[[1]]
named_urls <- lapply(str_split(All_urls, pattern = " = "), function(x) x[2])
names(named_urls) <- lapply(str_split(All_urls, pattern = " = "), function(x) x[1])


#seperate the Statent urls 
Statent_urls <- named_urls[4:length(named_urls)]
Statent_urls <- c("2011" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124093/master",
          "2012" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124098/master",
          "2013" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124099/master",
          "2014" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124091/master",
          "2015" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124090/master",
          "2016" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124092/master",
          "2017" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124095/master",
          "2018" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124094/master",
          "2019" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/20124116/master",
          "2020" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23245467/master")

Statent_dir <- "Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Statent"
sapply(Statent_urls, function(x) lulcc.downloadunzip(url = x,
                                             save_dir = Statent_dir))

#gather the relevant files
Statent_paths <- grep(list.files(Statent_dir, recursive = TRUE, full.names = TRUE, pattern = "csv"),
                        pattern = paste(c("GMDE", "NOLOC"), collapse = "|"),
                        invert=TRUE,
                        value=TRUE)

#name using numerics in paths
names(Statent_paths) <- sapply(Statent_paths, function(x){str_match(pattern = paste(names(Statent_urls), collapse = "|"), x)})

#Get the variable IDs for the number of Full Time Equivalents in each sector 
#webpage for variable list: https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/arbeitsstaetten-beschaeftigung/statistik-unternehmensstruktur-statent-ab-2011.assetdetail.23264982.html
#download file from API URL
Statent_metadata <- read.xlsx("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23264982/master", startRow = 9, cols = c(1,3))
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
Data_by_year <- future_mapply(function(annual_data_path, year){
  
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
Statent_merged <- Reduce(function(x, y) merge(x, y, by= Statent_desc_vars, all = TRUE), Data_by_year)
rm(Data_by_year)


### Wrangling Business census data
Biz_census_urls <- c("sec23_95_05" =  "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607233/master",
                     "sec1_96-05" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607232/master",
                     "sec123_08" = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/14607231/master")

Biz_census_dir <- c("Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Business_census")
#sapply(Biz_census_urls, function(x) lulcc.downloadunzip(url = x,
#                                             save_dir = Biz_census_dir))

#list all csv files. 
Biz_census_paths <- grep(list.files(path = Biz_census_dir,
                                    full.names = TRUE,
                                    recursive = TRUE),
                         pattern = "csv",
                         value = TRUE,
                         ignore.case = TRUE)

#manually name according to the year of each (based on abbreviations in file name e.g '05' == 2005)
names(Biz_census_paths) <- c("2000", "2005", "1996", "2001", "2005", "2008", "1995", "1998")

#each dataset uses different IDs for the variable of Full Time Equivalents
#detailed in seperate meta data files
Biz_census_meta_paths <- grep(list.files(path = Biz_census_dir,
                                    full.names = TRUE,
                                    recursive = TRUE),
                         pattern = "xls",
                         value = TRUE,
                         ignore.case = TRUE)

#Find the IDs used for the Full Time Equivalents variables in each metadata file
Var_IDs_across_datasets <- unlist(sapply(Biz_census_meta_paths, function(x){
  meta_df <- readxl::read_excel(x)
  meta_df <- meta_df[26:nrow(meta_df),c(1,5)]
  Var_IDs <- meta_df[which(meta_df[[2]] %in% Statent_var_names), 1]
}))

#The IDs contain a common string across the datasets i.e 'VZAS' followed by
#1,2 or 3 for the sector. Use to match columns across all datasets.
BC_var_strings <- c("VZAS1", "VZAS2", "VZAS3")
names(BC_var_strings) <- c("Sec1", "Sec2", "Sec3")

#The spatial variables are named differently for the Business census datasets
BC_desc_vars <- c("X", "Y")
names(BC_desc_vars) <- BC_desc_vars

#combine the variable ames vectors
BC_vars <- c(BC_desc_vars, BC_var_strings)

#loop over Business census datasets
BC_data_by_year <- mapply(function(annual_data_path, year){
  
  #load the file
  Annual_data <- read.csv2(annual_data_path)
  
  #subset to just the required variables
  Data_subset <- Annual_data[,grepl(pattern = paste(c(BC_vars), collapse = "|"), names(Annual_data))]
  
  #rename the sectoral columns appending year
  names(Data_subset) <- lapply(names(Data_subset), function(y){
  new_name <- names(BC_vars)[which(BC_vars %in% str_match(pattern = paste(c(BC_vars), collapse = "|"), y))]
  if(grepl(new_name, pattern = "Sec")){paste0(new_name, "_", year)}else{new_name}
  })
  return(Data_subset)
}, annual_data_path = Biz_census_paths,
year = names(Biz_census_paths),
SIMPLIFY = FALSE)

#merge
BC_merged <- Reduce(function(x, y) merge(x, y, by= BC_desc_vars, all = TRUE), BC_data_by_year)

#rasterize both data sets
coordinates(Statent_merged) <- ~E_KOORD+N_KOORD
gridded(Statent_merged) <- TRUE 
crs(Statent_merged) <- crs(Ref_grid)
Statent_brick <- brick(Statent_merged)
Statent_brick <- resample(Statent_brick, Ref_grid, method = 'ngb') # reproject to match extent

coordinates(BC_merged) <- ~X+Y
gridded(BC_merged) <- TRUE 
crs(BC_merged) <- crs(Ref_grid)
BC_brick <- brick(BC_merged)
extent(BC_brick) <- extent(Ref_grid)
BC_brick <- resample(BC_brick, Ref_grid)
 
#Combine the two bricks together
Data_stack <- stack(Statent_brick, BC_brick)

#intersect with labour market regions

#load shapefile of labour market regions
LMR_shp <- sf::st_read("E:/LULCC_CH/Data/Preds/Raw/CH_geoms/2022_GEOM_TK/03_ANAL/Gesamtfläche_gf/K4_amre_20180101_gf/K4amre_20180101gf_ch2007Poly.shp")


#sum data in each labour market region
FTE_lab_market <-  raster::extract(Data_stack, LMR_shp, fun=sum, na.rm=TRUE, df=TRUE)
FTE_lab_market$name <- LMR_shp$name

#now split into sectors
#vector sector numbers
sector_nums <-c(1,2,3)

#loop over numbers seperating data and perform linear model based interpolation
Sector_extrapolations <- lapply(sector_nums, function(x){
  
  Sector_string <- paste0("Sec", x, "_")
  
  #seperate layers for sector
  Sector_data <- FTE_lab_market[,which(grepl(colnames(FTE_lab_market), pattern = paste(c(Sector_string, "ID", "name"), collapse = "|")))]

  #seperate the years by removing the prefix to the layer names
  years <- as.numeric(str_remove_all(colnames(Sector_data), pattern = Sector_string))
  names(years) <- colnames(Sector_data)
  years <- sort(years, decreasing = FALSE)
  
  #reorder columns on the basis of ascending years using the names of the years
  Sector_data <- Sector_data[,c("ID", "name", names(years))]
  
  #vector years to interpolate
  Interpolate_years <- c(1985, 1997, 2009)
  
  #add columns for interpolation years
  Sector_data[paste0(Sector_string, Interpolate_years)] <- NA
  
  #loop over rows (regions)
  for(i in 1:nrow(Sector_data)){
    
  #create linear model
  mod <- lm(unlist(Sector_data[i, names(years)])~years)
  
  #loop over years to interpolate
  Sector_data[i, paste0(Sector_string, Interpolate_years)] <- sapply(Interpolate_years, function(y){
  round(coef(mod)[1]+coef(mod)[2]*y,0)  
  }) #close loop over interpolation years
  } #close loop over rows
return(Sector_data)
    }) #close loop over sectors
names(Sector_extrapolations) <- paste0("Sec", sector_nums)

#TO DO: delete when finished
#saveRDS(Sector_extrapolations, "Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Sector_extrapolations.rds")
#Sector_extrapolations <- readRDS("Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Sector_extrapolations.rds")

#Divide the changes in municipal employment estimated for the AS flying periods by
#the number of years to get an average annual rate of change for each period
#because this can also be calculated for the future projected data

#Outer loop over LULC_change_periods
Period_sector_values <- rbindlist(lapply(LULC_change_periods, function(period_dates){

#calc period length
Duration <- abs(diff(as.numeric(period_dates)))
    
#Inner loop over sector_extrapolations
Sector_values <- rbindlist(mapply(function(Sector_data, Sector_name){
  
  #subset data
  dat <- Sector_data[, paste0(Sector_name, "_", period_dates)] 
  Sector_data$Avg.diff <- (dat[,1]-dat[,2])/Duration
  return(Sector_data[, c("ID", "name", "Avg.diff")])
  }, Sector_data = Sector_extrapolations,
  Sector_name = names(Sector_extrapolations),
SIMPLIFY = FALSE), idcol = "Sector", fill = TRUE)

}), idcol = "Period") #close outer loop

#pivot to wide
LMR_values <- pivot_wider(data = Period_sector_values,
                          id_cols = c("ID"),
                          names_from = c("Period", "Sector"),
                          values_from = "Avg.diff",
                          names_sep = "_")
#rasterize
LMR_shp$name <- as.factor(LMR_shp$name)
LMR_rast <- rasterize(LMR_shp, Ref_grid, field = "name", fun='last', background=NA)

#use subs to match raster values based on ID and repeat across all columns
#saving a seperate layer for each
Prepared_FTE_dir <- "Data/Preds/Prepared/Layers/Socio_economic/Employment"
dir.create(Prepared_FTE_dir, recursive = TRUE)
FTE_rasts <- subs(LMR_rast, 
                  LMR_values, 
                  by='ID',
                  which=2:ncol(LMR_values))

#vector file names
FTE_file_names <- paste0(Prepared_FTE_dir, "/", "Avg_ann_chg_FTE_",
                                    names(LMR_values)[2:length(LMR_values)],".tif") 

#save a seperate file for each layer
writeRaster(FTE_rasts, 
            filename = paste0(Prepared_FTE_dir, "/", "Avg_ann_chg_FTE_",
                                    names(LMR_values)[2:length(LMR_values)],".tif"),
            bylayer=TRUE,
            format="GTiff",
            overwrite = TRUE)

#update the predictor table with the file paths

### -------------------------------------------------------------------------
### X.2- Biophysical: Soil, continentality and light (Descombes et al. 2020)
### -------------------------------------------------------------------------

#grab url from table

#use function to download and unpack
https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/e0faab13-0d1b-492a-8539-5370d48b9e35/download/predictors.zip

#download metadata
Biophys_meta <- https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx
        
#Soil variables: Descombes et al. 2020
    #Soil aeration 
    soil_aeration_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilD.tif")
    plot(soil_aeration_org)
    #re-project and change resolution
    Soil_aeration <- projectRaster(soil_aeration_org, crs= crs(Ref_grid), res = res(Ref_grid))
    plot(Soil_aeration)
    Soil_aeration <- resample(Soil_aeration, Ref_grid)
    plot(Soil_aeration)
    extent(Soil_aeration)
    writeRaster(Soil_aeration, "Covariates/Soil/Prepared/Soil_aeration_cov5.tif", overwrite=TRUE)
    
    #Soil moisture 
    soil_moisture_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilF.tif")
    #re-project and change resolution
    Soil_moisture <- projectRaster(soil_moisture_org, crs= crs(Ref_grid), res = res(Ref_grid))
    Soil_moisture <- resample(Soil_moisture, Ref_grid)
    extent(Soil_moisture)
    plot(Soil_moisture)
    res(Soil_moisture)
    writeRaster(Soil_moisture, "Covariates/Soil/Prepared/Soil_moisture_cov3.tif", overwrite=TRUE)
    
    #Soil humus 
    soil_humus_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilH.tif")
    #re-project and change resolution
    Soil_humus <- projectRaster(soil_humus_org, crs= crs(Ref_grid), res = res(Ref_grid))
    Soil_humus <- resample(Soil_humus, Ref_grid)
    writeRaster(Soil_humus, "Covariates/Soil/Prepared/Soil_humus_cov6.tif", overwrite=TRUE)
    
    #Soil nutrients 
    soil_nutrients_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilN.tif")
    #re-project and change resolution
    Soil_nutrients <- projectRaster(soil_nutrients_org, crs= crs(Ref_grid), res = res(Ref_grid))
    Soil_nutrients <-resample(Soil_nutrients, Ref_grid)
    plot(Soil_nutrients)
    writeRaster(Soil_nutrients, "Covariates/Soil/Prepared/Soil_nutrients_cov2.tif", overwrite=TRUE)
    
    #Soil PH 
    soil_PH_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilR.tif")
    plot(soil_PH_org)
    #re-project and change resolution
    Soil_PH <- projectRaster(soil_PH_org, crs= crs(Ref_grid), res = res(Ref_grid))
    Soil_PH <- resample(Soil_PH, Ref_grid)
    plot(Soil_PH)
    writeRaster(Soil_PH, "Covariates/Soil/Prepared/Soil_PH_cov1.tif", overwrite=TRUE)
    
    #Soil moisture_variability 
    soil_moisture_variability_org <- raster("Covariates/Soil/Original/SPEEDMIND_SoilW.tif")
    #re-project and change resolution
    Soil_moisture_variability <- projectRaster(soil_moisture_variability_org, crs= crs(Ref_grid), res = res(Ref_grid))
    Soil_moisture_variability <- resample(Soil_moisture_variability, Ref_grid)
    writeRaster(Soil_moisture_variability, "Covariates/Soil/Prepared/Soil_moisture_variability_cov4.tif", overwrite=TRUE)

        #Continentality
    Continentality <- raster("Data/Preds/Raw/Climatic/Continentality/Original/SPEEDMIND_SoilK.tif")
    #re-project and change resolution
    Continentality_100m <- projectRaster(Continentality, crs= crs(Ref_grid), res = res(Ref_grid))
    Continentality_100m <- resample(Continentality_100m, Ref_grid)
    res(Continentality_100m)
    extent(Continentality_100m)
    crs(Continentality_100m)
    writeRaster(Continentality_100m, "Covariates/Climatic/Continentality/Prepared/Continentality_100m_cov23.tif", overwrite=TRUE)
    
    #light
    light_mean <- raster("Covariates/Topographic/light/Original/SPEEDMIND_SoilL.tif")
    #re-project and change resolution
    light_100m <- projectRaster(light_mean, crs= crs(Ref_grid), res = res(Ref_grid))
    light_100m <- resample(light_100m, Ref_grid)
    writeRaster(light_100m, "Covariates/Topographic/light/Prepared/light_100m_cov18.tif", overwrite=TRUE)
    res(light_100m)
    extent(light_100m)
    crs(light_100m) 
    
### -------------------------------------------------------------------------
### X.3- Population
### -------------------------------------------------------------------------    
    

    
    
    
### =========================================================================    

### =========================================================================
### X- Updating predictor table
### =========================================================================    
    
#split the table back into Dfs for each period and save
# openxlsx::write.xlsx(Pred_tables_update, file = "Data/Preds/Predictor_table.xlsx", overwrite = TRUE)
    
cat(paste0(' Preparation of Suitability and accessibility predictor layers complete \n'))

    


### =========================================================================
### X- Climatic covariates
### =========================================================================

#periodic averages of each climatic variable for each period
    
        #first we need an efficient way to find and copy the files from the SwitchDrive 'CH-BMG' folder architecture
    
        #bio1 predictor 
        bio1_dirs <- list.files("C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel", pattern = "*bio1.rds$", recursive = TRUE)
    
        #split files into periods
        bio1_1985 <- bio1_dirs[c(1:5)]
        bio1_1997 <- bio1_dirs[c(6:11)]
        bio1_2009 <- bio1_dirs[c(12:17)]
        
        #copy the files for each period to folders in my file structure
        rawPath <- "C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel"
        bio1_directory_1985 <- "Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/Avg_ann_temp/" 
        bio1_directory_1997 <- "Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/Avg_ann_temp/"
        bio1_directory_2009 <- "Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/Avg_ann_temp/"
        
        file.copy(file.path(rawPath, bio1_1985), bio1_directory_1985 , overwrite = TRUE)
        file.copy(file.path(rawPath, bio1_1997), bio1_directory_1997 , overwrite = TRUE)
        file.copy(file.path(rawPath, bio1_2009), bio1_directory_2009 , overwrite = TRUE)
    
        
        #bio12 predictor
        bio12_dirs <- list.files("C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel", pattern = "*bio12.rds$", recursive = TRUE)
        #split files into periods
        bio12_1985 <- bio12_dirs[c(1:5)]
        bio12_1997 <- bio12_dirs[c(6:11)]
        bio12_2009 <- bio12_dirs[c(12:17)]
        
        #copy the files for each period to folders in my file structure
        bio12_directory_1985 <- "Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/Avg_precip/" 
        bio12_directory_1997 <- "Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/Avg_precip/"
        bio12_directory_2009 <- "Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/Avg_precip/"
        
        file.copy(file.path(rawPath, bio12_1985), bio12_directory_1985 , overwrite = TRUE)
        file.copy(file.path(rawPath, bio12_1997), bio12_directory_1997 , overwrite = TRUE)
        file.copy(file.path(rawPath, bio12_2009), bio12_directory_2009 , overwrite = TRUE)
        
        
        #gdd0 predictor
        gdd0_dirs <- list.files("C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel", pattern = "*gdd0.rds$", recursive = TRUE)
        #split files into periods
        gdd0_1985 <- gdd0_dirs[c(1:5)]
        gdd0_1997 <- gdd0_dirs[c(6:11)]
        gdd0_2009 <- gdd0_dirs[c(12:17)]
        
        #copy the files for each period to folders in my file structure
        gdd0_directory_1985 <- "Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/Sum_gdays_0deg/" 
        gdd0_directory_1997 <- "Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/Sum_gdays_0deg/"
        gdd0_directory_2009 <- "Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/Sum_gdays_0deg/"
        
        file.copy(file.path(rawPath, gdd0_1985), gdd0_directory_1985 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd0_1997), gdd0_directory_1997 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd0_2009), gdd0_directory_2009 , overwrite = TRUE)

        #gdd3 predictor
        gdd3_dirs <- list.files("C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel", pattern = "*gdd3.rds$", recursive = TRUE)
        #split files into periods
        gdd3_1985 <- gdd3_dirs[c(1:5)]
        gdd3_1997 <- gdd3_dirs[c(6:11)]
        gdd3_2009 <- gdd3_dirs[c(12:17)]
        
        #copy the files for each period to folders in my file structure
        gdd3_directory_1985 <- "Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/Sum_gdays_3deg/" 
        gdd3_directory_1997 <- "Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/Sum_gdays_3deg/"
        gdd3_directory_2009 <- "Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/Sum_gdays_3deg/"
        
        file.copy(file.path(rawPath, gdd3_1985), gdd3_directory_1985 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd3_1997), gdd3_directory_1997 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd3_2009), gdd3_directory_2009 , overwrite = TRUE)
        
        
        #gdd5 predictor
        gdd5_dirs <- list.files("C:/Users/bblack/switchdrive/CH-BMG/data/predictors/ch/bioclim/chclim25/present/pixel", pattern = "*gdd5.rds$", recursive = TRUE)
        #split files into periods
        gdd5_1985 <- gdd5_dirs[c(1:5)]
        gdd5_1997 <- gdd5_dirs[c(6:11)]
        gdd5_2009 <- gdd5_dirs[c(12:17)]
        
        #copy the files for each period to folders in my file structure
        gdd5_directory_1985 <- "Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/Sum_gdays_5deg/" 
        gdd5_directory_1997 <- "Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/Sum_gdays_5deg/"
        gdd5_directory_2009 <- "Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/Sum_gdays_5deg/"
        
        file.copy(file.path(rawPath, gdd5_1985), gdd5_directory_1985 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd5_1997), gdd5_directory_1997 , overwrite = TRUE)
        file.copy(file.path(rawPath, gdd5_2009), gdd5_directory_2009 , overwrite = TRUE)
    
    
        #Using a loop to calculate average values for each covariate in the years that make each up LULC flying period (i.e. lulc year and preceding 4 years)
        Covariates <- c("Avg_ann_temp", "Avg_precip", "Sum_gdays_0deg", "Sum_gdays_3deg" , "Sum_gdays_5deg")
        Covariates_names_1985 <- c("Avg_ann_temp_cov24", "Avg_precip_cov25", "Sum_gdays_0deg_cov26", "Sum_gdays_3deg_cov27" , "Sum_gdays_5deg_cov28")
        Covariates_names_1997 <- c("Avg_ann_temp_cov29", "Avg_precip_cov30", "Sum_gdays_0deg_cov31", "Sum_gdays_3deg_cov32" , "Sum_gdays_5deg_cov33")
        Covariates_names_2009 <- c("Avg_ann_temp_cov34", "Avg_precip_cov35", "Sum_gdays_0deg_cov36", "Sum_gdays_3deg_cov37" , "Sum_gdays_5deg_cov38")
        
        
        #initiate function for period1 (1981-1985) (passing covariates/folders argument)
        batch_rastMean_1985 <- function(Covariates){
        
        #loop through the different folders
        for (i in 1:length(Covariates)) {
            
            #get a list of the input rasters in each folder
            #pattern = "*.tif$" filters for main raster files only and skips any associated files (e.g. world files)
            grids <- list.files(paste0("Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/", Covariates[i], "/"), pattern = "*.rds$")
            x <- lapply(paste0("Covariates/Climatic/Periodic_averages/Period1_1981_1985/Original/", Covariates[i], "/", grids), readRDS)
        
            #create a raster stack from the input grids (in this example there are 12 tif files in each folder)
            s <-stack(x)
            
            #run the mean function on the raster stack - i.e. add (non-cumulatively) the rasters together
            r25 <- mean(s)
            
            #aggregate the raster from 25m to 100m
            r100 <- aggregate(r25, fact=4, fun=mean)
            
            #write the output raster to file
            r100 <- writeRaster(r100, filename = paste0("Covariates/Climatic/Periodic_averages/Period1_1981_1985/Prepared/", "Average_", Covariates_names_1985[i], ".tif"), overwrite=TRUE)
            
        }
    }
    
    #run the function 
    batch_rastMean_1985(Covariates)
    
    #initiate function for period 2 (1993-1997) (passing covariates/folders argument)
    batch_rastMean_1997 <- function(Covariates){
        
        #loop through the different folders
        for (i in 1:length(Covariates)) {
            
            #get a list of the input rasters in each folder
            #pattern = "*.tif$" filters for main raster files only and skips any associated files (e.g. world files)
            grids <- list.files(paste0("Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/", Covariates[i], "/"), pattern = "*.rds$")
            x <- lapply(paste0("Covariates/Climatic/Periodic_averages/Period2_1993_1997/Original/", Covariates[i], "/", grids), readRDS)
            
            #create a raster stack from the input grids (in this example there are 12 tif files in each folder)
            s <-stack(x)
            
            #run the mean function on the raster stack - i.e. add (non-cumulatively) the rasters together
            r25 <- mean(s)
            
            #aggregate the raster from 25m to 100m
            r100 <- aggregate(r25, fact=4, fun=mean)
            
            #write the output raster to file
            r100 <- writeRaster(r100, filename = paste0("Covariates/Climatic/Periodic_averages/Period2_1993_1997/Prepared/", "Average_", Covariates_names_1997[i], ".tif"), overwrite=TRUE)
            
        }
    }
            
    
    #run the function
    batch_rastMean_1997(Covariates)
    
    #initiate function for period 3 (2005-2009) (passing covariates/folders argument)
    batch_rastMean_2009 <- function(Covariates){
        
        #loop through the different folders
        for (i in 1:length(Covariates)) {
            
            #get a list of the input rasters in each folder
            #pattern = "*.tif$" filters for main raster files only and skips any associated files (e.g. world files)
            grids <- list.files(paste0("Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/", Covariates[i], "/"), pattern = "*.rds$")
            x <- lapply(paste0("Covariates/Climatic/Periodic_averages/Period3_2005_2009/Original/", Covariates[i], "/", grids), readRDS)
            
            #create a raster stack from the input grids (in this example there are 12 tif files in each folder)
            s <-stack(x)
            
            #run the mean function on the raster stack - i.e. add (non-cumulatively) the rasters together
            r25 <- mean(s)
            
            #aggregate the raster from 25m to 100m
            r100 <- aggregate(r25, fact=4, fun=mean)
            
            #write the output raster to file
            r100 <- writeRaster(r100, filename = paste0("Covariates/Climatic/Periodic_averages/Period3_2005_2009/Prepared/", "Average_", Covariates_names_2009[i], ".tif"), overwrite=TRUE)
            
        }
    }
    
    #run the function
    batch_rastMean_2009(Covariates) 
    

 
    
    
    
