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

# All packages are sourced in the master document, uncomment here
#if running the script in isolation
# Install packages if they are not already installed
# packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat",
#          "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", "rgeos",
#          "sf", "tiff", "bfsMaps", "rjstat", "future.apply", "future", "stringr",
#          "stringi" ,"readxl","rlist", "rstatix", "openxlsx", "pxR", "zen4R", "rvest")

# new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]
# 
# if(length(new.packs)) install.packages(new.packs)
# 
# # Load required packages
# invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
#invisible(sapply(list.files("Scripts/Functions", pattern = ".R", full.names = TRUE, recursive = TRUE), source))

#Load in the grid to use use for re-projecting the CRS and extent of predictor data
Ref_grid <- raster(Ref_grid_path)

#vector years of LULC data
LULC_years <- gsub(".*?([0-9]+).*", "\\1", list.files("Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#create a list of the data/modelling periods
LULC_change_periods <- c()
for (i in 1:(length(LULC_years)-1)) {
            LULC_change_periods[[i]] <- c(LULC_years[i],LULC_years[i+1])}
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))

# #download basic map geometries for Switzerland
Geoms_path <- "Data/Preds/Raw/CH_geoms"
dir.create(Geoms_path)
DownloadBfSMaps(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master",
path = Geoms_path)

### =========================================================================
### B- Gather predictor information
### =========================================================================

#The predictor table contains details of the suitability and accessibility
#predictors in seperate sheets for the different time periods, some predictors
#are the same across periods (static) whereas other are temporally dynamic
#In terms of preparations some predictors are prepared by downloading the data
#direct from raw and then processing with others being standardized
#from existing raw data

#create base directory for prepared predictor layers
Prepped_layers_dir  <- "Data/Preds/Prepared/Layers" 
dir.create(Prepped_layers_dir, recursive = TRUE)

#Predictor table file path (received from output_env only uncomment for testing)
#Pred_table_path <- "Tools/Predictor_table.xlsx"

#get names of sheets to loop over
sheets <- excel_sheets(Pred_table_path)

#load all sheets as a list
Pred_tables <- lapply(sheets, function(x) openxlsx::read.xlsx(Pred_table_path, sheet = x))
names(Pred_tables) <- sheets

#combine tables for all periods
Pred_table_long <- as.data.frame(rbindlist(Pred_tables))

#create dirs for all predictor categories
sapply(unique(Pred_table_long[["Predictor_category"]]), function(x) {
  dir.create(paste0(Prepped_layers_dir, "/", x), recursive = TRUE)
  })

#seperate unprepared/prepared layers
Preds_to_prepare <- Pred_table_long[Pred_table_long$Prepared != "Y",]

### =========================================================================
### C- Predictors from raw data
### =========================================================================

#The static predictors with a raw data raw are those already prepared in ValPar.CH
#at 25m resolution because they already use a grid of the same extent and CRS 
#processing is just aggregating to 100m. However the dynamic climatic predictors need
#to be aggregated from several layers. 

#seperate the preds that have a Raw path
Preds_raw <- Preds_to_prepare[!is.na(Preds_to_prepare$Raw_data_path),]

#First process the static predictors
Preds_static <- Preds_raw[Preds_raw$Static_or_dynamic == "static",]

#reduce to unique predictors
Preds_static_unique <- Preds_static[!duplicated(Preds_static$Covariate_ID), c("Covariate_ID", "Predictor_category", "URL", "Raw_data_path", "Prepared_data_path")]  

#loop over the preds loading the raw data, processing and saving, returning a prepared data_path
for(i in 1:nrow(Preds_static_unique)){
  
  #load data
  Raw_dat <- readRDS(unlist(Preds_static_unique[i,"Raw_data_path"]))

  #aggregate
  Agg_dat <- aggregate(Raw_dat, fact=4, fun=mean)
  
  #vector save path
  layer_path <- paste0(Prepped_layers_dir, "/", Preds_static_unique[i,"Predictor_category"] ,"/", Preds_static_unique[i,"Covariate_ID"], ".tif")

  #save
  writeRaster(Agg_dat, layer_path, overwrite=TRUE)
  
  #add the prepared path to the table
  Pred_table_long[Pred_table_long$Covariate_ID == Preds_static_unique[i,"Covariate_ID"], "Prepared_data_path"] <- layer_path
  Pred_table_long[Pred_table_long$Covariate_ID == Preds_static_unique[i,"Covariate_ID"], "Prepared"] <- "Y"
  
  #clean up
  rm(Raw_dat, Agg_dat, layer_path)
}


#Process the dynamic predictors
Preds_dynamic <- Preds_raw[Preds_raw$Static_or_dynamic == "dynamic",]

#Loop over the predictors, Calculating periodic averages for each
#re-scaling the rasters, saving and updating predictor table
for(i in 1:nrow(Preds_dynamic)){
    #read in rasters as stack
    raster_stack <- stack(lapply(list.files(Preds_dynamic[i,"Raw_data_path"], full.names = TRUE), readRDS))
    
    #calculate mean on the stack
    raster_mean <- mean(raster_stack)
            
    #aggregate
    Agg_dat <- aggregate(raster_mean, fact=4, fun=mean)
  
    #vector save path
    layer_path <- paste0(Prepped_layers_dir, "/", Preds_dynamic[i,"Predictor_category"] ,"/", Preds_dynamic[i,"Covariate_ID"],"_",Preds_dynamic[i,"period"],".tif")

    #save
    writeRaster(Agg_dat, layer_path, overwrite=TRUE)
  
    #add the prepared path to the table
    Pred_table_long[which(Pred_table_long$Covariate_ID == Preds_dynamic[i,"Covariate_ID"] & Pred_table_long$period == Preds_dynamic[i,"period"]), "Prepared_data_path"] <- layer_path
    Pred_table_long[which(Pred_table_long$Covariate_ID == Preds_dynamic[i,"Covariate_ID"] & Pred_table_long$period == Preds_dynamic[i,"period"]), "Prepared"] <- "Y"

    rm(raster_stack, raster_mean, Agg_dat, layer_path)
}

### =========================================================================
### D- Predictors from source
### =========================================================================    
#The predictors that are prepared directly from source require different
#processing operations as such they are grouped into seperate chunks below    
    
### -------------------------------------------------------------------------
### D.1- Socio_economic: Employment
### -------------------------------------------------------------------------

#In order to have a common variable between the historic data and the future
#economic scenarios, the variables we will derive will be:
#1. Average annual change in number of full time equivalent employees in the primary sector
#2. Average annual change in number of full time equivalent employees in the secondary and tertiary sectors combined

#As the future scenarios of employment are expressed at the scale
#of labour market regions rather than municipalities then the historic data
#needs to be aggregated to this scale

#The historic employment data from the Federal statistical office
#comes from two raws:
#1. The business census (conducted from 1995-2008 for the timepoints: 1995, 2001, 2005, 2008)
#2. The business structure statistics (STATENT, conducted annual from 2011)

#The primary, secondary and tertiary sectors are themselves aggregations
#of all of the individual economic industries each of which as an official
#classification under the NOGA schematic. This schematic did change in 2008 with
#definitions/codes used for some industries being changed.
#It is difficult to match the old/new NOGA schemes exactly 
#however changes are minor in the scope of the number of industries them exactly  

#get urls for variable and convert to named list
All_urls <- str_split(Preds_to_prepare[grep(Preds_to_prepare$Covariate_ID, pattern="Avg_chg_FTE"), "URL"][1], ",")[[1]]
named_urls <- lapply(str_split(All_urls, pattern = " = "), function(x) trimws(x[2]))
names(named_urls) <- lapply(str_split(All_urls, pattern = " = "), function(x) trimws(x[1]))

### Statent data
#seperate the Statent urls 
Statent_urls <- named_urls[4:length(named_urls)]

#create dir for raw data
Statent_dir <- "Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Statent"

#Download and unzip all datasets
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
rm(Statent_data_by_year)

#rasterize
coordinates(Statent_merged) <- ~E_KOORD+N_KOORD
gridded(Statent_merged) <- TRUE 
crs(Statent_merged) <- crs(Ref_grid)
Statent_brick <- brick(Statent_merged)
Statent_brick <- raster::resample(Statent_brick, Ref_grid, method = 'ngb') # reproject to match extent

### Business census data
Biz_census_urls <- named_urls[1:3]

#specify dir and download datasets
Biz_census_dir <- c("Data/Preds/Raw/Socio_economic/Employment/Historic_employment/Business_census")
sapply(Biz_census_urls, function(x) lulcc.downloadunzip(url = x,
                                             save_dir = Biz_census_dir))

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

#rasterize
coordinates(BC_merged) <- ~X+Y
gridded(BC_merged) <- TRUE 
crs(BC_merged) <- crs(Ref_grid)
BC_brick <- brick(BC_merged)
extent(BC_brick) <- extent(Ref_grid)
BC_brick <- raster::resample(BC_brick, Ref_grid)
 
#Combine the two bricks together
Data_stack <- stack(Statent_brick, BC_brick)

#intersect with labour market regions
#load shapefile of labour market regions
LMR_shp <- sf::st_read("Data/Preds/Raw/CH_geoms/2022_GEOM_TK/03_ANAL/Gesamtfläche_gf/K4_amre_20180101_gf/K4amre_20180101gf_ch2007Poly.shp")

#sum data in each labour market region
FTE_lab_market <-  raster::extract(Data_stack, LMR_shp, fun=sum, na.rm=TRUE, df=TRUE)
FTE_lab_market$name <- LMR_shp$name

#split into sectors, vector sector numbers
sector_nums <-c(1,2,3)

#loop over sector numbers separating data and perform linear model based interpolation
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

#Divide the changes in municipal employment estimated for the AS flying periods by
#the number of years to get an average annual rate of change for each period
#because this can also be calculated for the future projected data

#Outer loop over LULC_change_periods
Period_sector_values <- rbindlist(lapply(LULC_change_periods, function(period_dates){

#calc period length
Duration <- abs(diff(as.numeric(period_dates)))
  
#Inner loop over sector_extrapolations
Sector_values <- rbindlist(mapply(function(Sector_data, Sector_name, period_dates){

  #subset data
  dat <- Sector_data[, paste0(Sector_name, "_", period_dates)] 
  Sector_data$Avg.diff <- (dat[,1]-dat[,2])/Duration
  return(Sector_data[, c("ID", "name", "Avg.diff")])
  }, Sector_data = Sector_extrapolations,
  Sector_name = names(Sector_extrapolations),
  MoreArgs = list(period_dates = period_dates),
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
FTE_rasts <- subs(LMR_rast, 
                  LMR_values, 
                  by='ID',
                  which=2:ncol(LMR_values))

#base dir
Prepared_FTE_dir <- "Data/Preds/Prepared/Layers/Socio_economic/Employment"
dir.create(Prepared_FTE_dir, recursive = TRUE)

#vector file names
FTE_file_names <- paste0(Prepared_FTE_dir, "/", "Avg_chg_FTE_",
                                    names(LMR_values)[2:length(LMR_values)],".tif") 

#save a seperate file for each layer
writeRaster(FTE_rasts, 
            filename = FTE_file_names,
            bylayer=TRUE,
            format="GTiff",
            overwrite = TRUE)

#update the predictor table with the file paths
Pred_table_long[grep(Pred_table_long$Covariate_ID, pattern="Avg_chg_FTE"), "Prepared_data_path"] <- FTE_file_names
Pred_table_long[grep(Pred_table_long$Covariate_ID, pattern="Avg_chg_FTE"), "Prepared"] <- "Y"

### -------------------------------------------------------------------------
### D.2- Biophysical: Soil, continentality and light (Descombes et al. 2020)
### -------------------------------------------------------------------------

#grab url from table
Biophys_url <- str_split(Preds_to_prepare[grep(Preds_to_prepare$Data_citation, pattern= "Descombes et al. 2020"), "URL"][1], ",")[[1]]

#use function to download and unpack
Biophys_dir <- c("Data/Preds/Raw/Biophysical")
lulcc.downloadunzip(url = Biophys_url, save_dir = Biophys_dir)

#download metadata
Biophys_meta <- read.xlsx("https://www.envidat.ch/dataset/4ab13d14-6f96-41fd-96b0-b3ea45278b3d/resource/81c046c3-8d1d-45bc-a833-7d8240cebd12/download/predictors_description.xlsx")

#clean required column names 
names(Biophys_meta)[1:3] <- c("Layer_name", "Abbrev", "Desc_name") 

#correct spelling mistake
Biophys_meta$Desc_name[25] <- "Continentality"

#get layer names using variable names
Biophys_var_names <- unique(Preds_to_prepare[Preds_to_prepare$Data_citation == "Descombes et al. 2020", "Variable_name"])
Biophys_layer_names <- Biophys_meta[Biophys_meta$Desc_name %in% Biophys_var_names, "Layer_name"]

#Get variable descriptive names
Biophys_desc_names <- Biophys_meta[Biophys_meta$Desc_name %in% Biophys_var_names, "Desc_name"]

#Match descriptive names with the pred table and return the covariate ID
names(Biophys_layer_names) <- unique(sapply(Biophys_desc_names, function(x){Preds_to_prepare[Preds_to_prepare$Variable_name == x, "Covariate_ID"]}))

#get layer paths
Biophys_paths <- lapply(Biophys_layer_names, function(x) {
  list.files(Biophys_dir, full.names = TRUE, pattern = x, recursive = TRUE)
})

#loop over paths and process layers
for(i in 1:length(Biophys_paths)){
  Var_path <- Biophys_paths[[i]]
  Var_name <- names(Biophys_paths)[i]
    
  #load data 
  Raw_dat <- raster(Var_path)

  #re-project and change resolution
  Prepped_dat <- projectRaster(Raw_dat, crs= crs(Ref_grid), res = res(Ref_grid))
  Prepped_dat_resamp <- resample(Prepped_dat, Ref_grid)
  
  #vector save path
  layer_path <- paste0(Prepped_layers_dir, "/", unique(Preds_to_prepare[Preds_to_prepare$Covariate_ID == Var_name, "Predictor_category"]) ,"/", Var_name, ".tif")
  
  #save
  writeRaster(Prepped_dat_resamp, layer_path, overwrite=TRUE)
  
  #add the prepared path to the table
  Pred_table_long[Pred_table_long$Covariate_ID == Var_name, "Prepared_data_path"] <- layer_path
  Pred_table_long[Pred_table_long$Covariate_ID == Var_name, "Prepared"] <- "Y"
}

### -------------------------------------------------------------------------
### D.3- Population
### -------------------------------------------------------------------------    
    
### Prepare municipality population data incorporating mutations

#use if statement to only perform prep if layers have not already been prepared
if(str_contains(Preds_to_prepare$Covariate_ID, "Muni_pop")){
#read in PX data from html and convert to DF
px_data <- data.frame(read.px("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master"))


#subset to desired rows based on conditions:
#Total population on 1st of January;
#Total populations (Swiss and foreigners)                      
#Total population (Men and Women)
raw_mun_popdata <- px_data[px_data$Demografische.Komponente == "Bestand am 31. Dezember" &
                     px_data$Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total" &
                     px_data$Geschlecht == "Geschlecht - Total", ]

#Identify municipalities records by matching on the numeric contained in their name
raw_mun_popdata <- raw_mun_popdata[grepl(".*?([0-9]+).*", raw_mun_popdata$Kanton.......Bezirk........Gemeinde.........), c(4:6)]
names(raw_mun_popdata) <- c("Name_Municipality", "Year", "Population")
raw_mun_popdata <- raw_mun_popdata %>% pivot_wider(names_from = "Year",
                                                values_from = "Population")
#Remove the periods in the name column
raw_mun_popdata$Name_Municipality <- gsub("[......]","",as.character(raw_mun_popdata$Name_Municipality))

#Seperate BFS number from name
raw_mun_popdata$BFS_NUM <- as.numeric(gsub(".*?([0-9]+).*", "\\1", raw_mun_popdata$Name_Municipality)) 
  
#Remove BFS number from name
raw_mun_popdata$Name_Municipality <- gsub("[[:digit:]]", "", raw_mun_popdata$Name_Municipality)

# subset to only municipalities existing in 2021
raw_mun_popdata <- raw_mun_popdata[raw_mun_popdata$`2021`> 0,]

# import municipality shape file
lulcc.downloadunzip(url = "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2021-07/swissboundaries3d_2021-07_2056_5728.shp.zip",
                    save_dir = "Data/Preds/Raw/CH_geoms")

Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]

#Import data of municipality mutations from FSO web service using conditions:
#1. Muations between 01/01/1981 and 01/05/2022
#2. Name change mutations only (other mutations captured by shapefile)

#scrape content from html address
content <- read_html("https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True")
muni_mutations <- html_table(content, fill = TRUE)[[1]]

#remove 1st row
muni_mutations <- muni_mutations[-1,]

#rename columns
colnames(muni_mutations) <- c("Mutation_Number", "Pre_canton_ID", 
                                    "Pre_District_num", "Pre_BFS_num", 
                                    "Pre_muni_name", "Post_canton_ID",
                                    "Post_district_num", "Post_BFS_num", 
                                    "Post_muni_name", "Change_date")

#identify which municipalities have mutations associated with them
mutation_index <- match(raw_mun_popdata$BFS_NUM, muni_mutations$Pre_BFS_num) # which have changed

#change municpality BFS number in population table according to the mutation
#for each row in the pop df if there is an NA in the mutation index do not replace the BFS number 
#If there is not an NA then replace with the new BFS number of the mutation table. 

for (i in 1:nrow(raw_mun_popdata)){
  if (!is.na(mutation_index[i])){ 
    raw_mun_popdata$BFS_NUM[[i]] <- muni_mutations[[mutation_index[[i]], "Post_BFS_num"]]}
}

#If after introducing the mutations we have multiple rows with the same BFS numbers
#then we need to combine their populations values as these indicate municipalities merging

if(length(unique(raw_mun_popdata$BFS_NUM)) != nrow(raw_mun_popdata)){
#get the indices of columns that represent the years
Time_points <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames(raw_mun_popdata))))

#create a empty df for results
Muni_pop_final <- as.data.frame(matrix(ncol= length(Time_points), 
                         nrow = length(unique(raw_mun_popdata$BFS_NUM))))
colnames(Muni_pop_final) <- Time_points

#Add column for BFS number
Muni_pop_final$BFS_NUM <- sort(unique(raw_mun_popdata$BFS_NUM))

#loop over date cols and rows summing values where BFS number is non-unique
for (j in Time_points){
  for (i in 1:length(unique(raw_mun_popdata$BFS_NUM))){
    Muni_pop_final[i, paste(j)] <- sum(raw_mun_popdata[raw_mun_popdata$BFS_NUM == Muni_pop_final[i, "BFS_NUM"], paste(j)])
  }
}
#replace old data with revised data
raw_mun_popdata <- Muni_pop_final
} #close if statement

#Add canton number
raw_mun_popdata$KANTONSNUM <- sapply(raw_mun_popdata$BFS_NUM, function(x){
unique(Muni_shp@data[Muni_shp@data$BFS_NUMMER == x, "KANTONSNUM"])
})

#save a copy raw_mun_popdata for preparation of future population layers
saveRDS(raw_mun_popdata, "Data/Preds/Raw/Socio_economic/Population/raw_muni_pop_historic.rds")


### Create historic municipality population rasters

#seperate pop data for LULC years 
#(minus 2018 as only one layer is required for each period)
pop_in_LULC_years <- raw_mun_popdata[,c("BFS_NUM", LULC_years[1:3])]

Var_name <- "Muni_pop"

#link with spatial municipality data, rasterize and save
Muni_save_paths <- sapply(LULC_years[1:3], function(i){

  #file_path
  save_path <- paste0(Prepped_layers_dir, "/", unique(Preds_to_prepare[Preds_to_prepare$Covariate_ID == Var_name, "Predictor_category"]), "/Population/", Var_name ,"_", i, ".tif")
  
  #loop over the BFS numbers of the polygons and match to population values
  Muni_shp@data[paste0("Pop_", i)] <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
  pop_value <- as.numeric(pop_in_LULC_years[pop_in_LULC_years$BFS_NUM == Muni_num, paste(i)])
  }, simplify = TRUE))

  #rasterize
  pop_rast <- rasterize(Muni_shp, Ref_grid, field = paste0("Pop_", i))

  #save
  raster::writeRaster(pop_rast, save_path, overwrite = TRUE)
  
  return(save_path)
})# close loop over LULC years

  #Add prepared path to predictor table
  Pred_table_long[Pred_table_long$Covariate_ID == Var_name, "Prepared_data_path"] <- Muni_save_paths
  Pred_table_long[Pred_table_long$Covariate_ID == Var_name, "Prepared"] <- "Y"
    
} #close if statement

### =========================================================================
### X- Update predictor table for SA predictors
### =========================================================================    

#load predictor_table as workbook to add sheets
Pred_table_update <- openxlsx::loadWorkbook(file = Pred_table_path)
      
#split the table back into Dfs for each period and save
Periodic_pred_tables <- split(Pred_table_long, Pred_table_long$period)  

#loop over period tables adding sheets and adding the predictors to them
for(i in names(Periodic_pred_tables)){
#the try() is necessary in case sheets already exist
try(addWorksheet(Pred_table_update, sheetName = paste(i)))
writeData(Pred_table_update, sheet = paste(i), x = Periodic_pred_tables[[i]])
}

#save workbook
openxlsx::saveWorkbook(Pred_table_update, Pred_table_path, overwrite = TRUE)    

cat(paste0(' Preparation of Suitability and accessibility predictor layers complete \n'))

### =========================================================================
### X- Create Neighbourhood predictors
### =========================================================================

#This process is lengthy so is presented in a seperate script,which includes
#updating of the predictor table after layer creation

#raw script
source("Scripts/preparation/Nhood_predictor_prep.R", echo = TRUE)

