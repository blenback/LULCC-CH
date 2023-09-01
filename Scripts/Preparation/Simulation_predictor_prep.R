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
Ref_grid <- raster(Ref_grid_path)

#Fetch simulation start and end times from simulation control table
Simulation_control <- read.csv(Sim_control_path)
Simulation_start <- min(Simulation_control$Scenario_start.real)
Simulation_end <- max(Simulation_control$Scenario_end.real)
Step_length <- unique(Simulation_control$Step_length.real)

#vector time steps for future predictions
Time_steps <- seq(Simulation_start, Simulation_end, Step_length)

#base dir for saving
Prepared_layers_dir <- "Data/Preds/Prepared/Layers"

### =========================================================================
### A- Preparing data from future economic scenarios
### =========================================================================

#use Zenodo API service to get URLs for file downloads
#Get record info
zenodo <- ZenodoManager$new()
rec <- zenodo$getRecordByDOI("10.5281/zenodo.4774914")
files <- rec$listFiles(pretty = TRUE)

#remove files for sensitivity analysis
ES_data_URLs <- files[-c(5,6),][["download"]]

#create dir
ES_dir <- "Data/Preds/Raw/Socio_economic/Employment/Employment_scenarios"

#loop over URLS, downloading and unzipping
for(i in ES_data_URLs){
  lulcc.downloadunzip(url = i, save_dir = ES_dir)
}

#because the future projected economic values as expressed using codes for the
#various Spatial aggregations we need a look up table to parse them to
#class values for spatializing 

#Load in the sheet of the metadata file which details the spatial aggregations (regions)
#use read.xlsx2 to not ommit blank columns
Metadat_regions <- xlsx::read.xlsx2("Data/Preds/Raw/Socio_economic/Employment/Employment_scenarios/Metadata.xlsx",
                        sheetName = "Region")
#remove empty row
Metadat_regions <- Metadat_regions[-c(1),]

#replace whitespaces with NA
Metadat_regions <- Metadat_regions %>% mutate_all(na_if,"")

n <- colSums(is.na(Metadat_regions)) == nrow(Metadat_regions) #identify empty columns
cs <- cumsum(n) + 1 #create division vector assign unique number to each group of columns
cs <- cs[!n] #remove empty cols from division vector

#split df according to grouped columns
Lookup_tables_regions <- lapply(unique(cs), function(x){
Dat <- Metadat_regions[, names(cs[cs== x])]
Dat <- Dat[complete.cases(Dat),]
})

#no easy programmatic way to name the different aggregation tables
#so we vector based on manual inspection
#1. Master table with all designations for each municipality
#2. Employment basins (labour market regions) disaggregated by canton
#3. MS (spatial mobility) region designation (106)
#4. Employment basins from 2018 (labour market regions) (101): FSO (2019)
#5. Cantons
#6. Large labour market area (see 4.) (16)
#7. Large Region
#8. Urban-rural typology
names(Lookup_tables_regions) <- c("Master",
                                  "LMR_by_Canton",
                                  "Spatial_mobility_regions",
                                  "LMR_regions",
                                  "Cantons",
                                  "Large_LMR",
                                  "Large_region",
                                  "Urban-rural_type")

#because we are interested in spatializing the projected employment data at the
#level of the labour market regions then only the corresponding lookup table is needed
LMR_lookup <- Lookup_tables_regions[["LMR_regions"]]

#clean names
names(LMR_lookup) <- c("ID", "Region_name")

#remove prefix of ID number
LMR_lookup$ID <- str_remove_all(LMR_lookup$ID, "BE")

#remove leading zeros
LMR_lookup$ID <- sub("^0+", "", LMR_lookup$ID) 

#In addition we need a second look up table for aggregating the economic sector
#names in the future data to the primary,secondary and tertiary sectors
#in line with the historic data

#The economic sectors are grouped by their division numbers as detailed in this report:
#https://www.bfs.admin.ch/bfsstatic/dam/assets/347016/master
#Primary sector = divisions 1-3
#Secondary sector = divisions 4-43
#Tertiary sector = 45-98
Sector_divisions <- list(Sec1 = c(0,3),
                         Sec2 = c(4,43),
                         Sec3 = c(45,98))

#Load sheet of metadata linking economic division numbers to category names
Sector_lookup <- xlsx::read.xlsx2("Data/Preds/Raw/Socio_economic/Employment/Employment_scenarios/Metadata.xlsx",
                        sheetName = "Industry")

#remove empty row
Sector_lookup <- Sector_lookup[-1,]

#change column name to avoid accent
names(Sector_lookup)[2] <- "Category"

#Alter division column to single values
Sector_lookup$max_div <- sapply(Sector_lookup$Division, function(x){
  if(grepl(x, pattern = "-")== TRUE){max_value <- as.numeric(str_split(x, "-")[[1]][2])}else{
    max_value <- x
  }
})

#add column with aggregated sector name
Sector_lookup$Sector <- sapply(Sector_lookup$max_div, function(x){
  Sec_test <- sapply(Sector_divisions, function(y){
    between(x, y[1], y[2])
  })
  Sec_name <- names(which(Sec_test == TRUE))
})

#get file paths of future data under all scenarios
Econ_data_paths <- as.list(list.files("Data/Preds/Raw/Socio_economic/Employment/Employment_scenarios", full.names = TRUE))
names(Econ_data_paths) <- str_remove_all(list.files("Data/Preds/Raw/Socio_economic/Employment/Employment_scenarios", full.names = FALSE), ".xlsx")

#exclude the metadata file
Econ_data_paths$Metadata <- NULL

#The string at the start of the file name is an abbreviation of the
#scenario name and the regional variation

Scenario_name <- list(c = "Combined",
                      e = "Ecolo",
                      t = "Techno",
                      r = "Ref")

Scenario_vari <- list(n1 = "Central",
                      ru = "Urban",
                      rp = "Peri_urban")

#clean scenario names
names(Econ_data_paths) <- sapply(names(Econ_data_paths), function(x){
  name <- Scenario_name[substr(x, 1, 1)]
  vari <- Scenario_vari[substr(x, 2, 3)]
  paste0(name, "_", vari)
})

#Subset to which econ scenarios are required for our scenarios
#load scenario specifications table
Scenario_data_table <- openxlsx::read.xlsx("Tools/Scenario_specifications.xlsx", sheet = "Predictor_data")
Scenario_corr <- as.list(Scenario_data_table$Econ_scenario)
names(Scenario_corr) <- Scenario_data_table[,"Scenario_ID"]

#load shapefile of labour market regions
LMR_shp <- sf::st_read("Data/Preds/Raw/CH_geoms/2022_GEOM_TK/03_ANAL/Gesamtfläche_gf/K4_amre_20180101_gf/K4amre_20180101gf_ch2007Poly.shp")  
  
#rasterize
LMR_shp$name <- as.factor(LMR_shp$name)
LMR_shp$rast_ID <- seq(1:nrow(LMR_shp))
LMR_rast <- rasterize(LMR_shp, Ref_grid, field = "rast_ID", fun='last', background=NA)

#upper loop over scenario list
for(i in 1:length(Scenario_corr)){
  
  #load future dataset
  Econ_dat <- readxl::read_excel(Econ_data_paths[[Scenario_corr[[i]]]], sheet = "Bassins d'emploi")
  
  #subset to relevant columns
  Econ_dat <- Econ_dat[,c("time", "EmpFTE", "sector", "regionBE")]
  
  #subset to years of time steps
  Econ_dat <- Econ_dat[Econ_dat$time %in% Time_steps,]
  
  #add column for economic sector
  Econ_dat$Agg_sec <- sapply(Econ_dat$sector, function(x){
     Sector_lookup[Sector_lookup$Category == x, "Sector"]
  })
  
  #add region name
  Econ_dat$Region_name <- sapply(Econ_dat$regionBE, function(x){
    LMR_lookup[LMR_lookup$ID == x, "Region_name"]
  })
  
  #Calculate the average annual difference 
  Region_values <- as.data.frame(Econ_dat %>% group_by(Agg_sec, Region_name, time) %>% #grouping
                                   dplyr::summarize(SumFTE = sum(EmpFTE)) %>% #sum over the economic activities
                                      mutate(Avg_chg_FTE = (SumFTE - lag(SumFTE))/Step_length)) %>% #calculate avg. annual change between time points
                                          select(-(SumFTE))

  #remove incomplete rows (2020 time point)
  Region_values <- Region_values[complete.cases(Region_values), ]
  
  #add ID for LMR region to match raster values
  Region_values$ID <- sapply(Region_values$Region_name, function(x){
    id <- LMR_shp[LMR_shp$name == x, "rast_ID"][[1]] 
    })
  
  #split by sector
  Sectoral_values <- split(Region_values, Region_values$Agg_sec)
  
  #loop over sectors, pivoting data to wide and creating rasters
  Future_FTE_file_paths <- lapply(Sectoral_values, function(Sector_dat){
    
    #pivot to wide
    Sector_dat_wide <- as.data.frame(Sector_dat %>% pivot_wider(names_from = time,
                                                  values_from = Avg_chg_FTE,
                                                  names_sep = "_"))
    
    #vector column indices for timepoints
    Date_cols <- which(colnames(Sector_dat_wide) %in% paste(Time_steps[-1]))
    names(Date_cols) <- sapply(1:(length(Time_steps)-1), function(x) {
            paste0(Time_steps[x], "_", Time_steps[x+1])})
    
    #vector file names
    FTE_file_names <- paste0(Prepared_layers_dir, "/Socio_economic/Employment/Avg_chg_FTE_", unique(Sector_dat_wide[,"Agg_sec"]),
                             "_", names(Date_cols), "_",
                                    names(Scenario_corr)[i],".tif")
    
    #use subs to match raster values based on ID and repeat across all columns
    Sector_rasts <- subs(LMR_rast, 
                  Sector_dat_wide, 
                  by='ID',
                  which= Date_cols) #-1 used to exclude '2020'
    
    #save a seperate file for each layer
    writeRaster(Sector_rasts, 
            filename = FTE_file_names,
            bylayer=TRUE,
            format="GTiff",
            overwrite = TRUE)
    
    return(FTE_file_names)
    }) #close loop over sectors 

} #close loop over scenarios

#list the files match on the scenario names
Future_FTE_file_paths <- list.files(paste0(Prepared_layers_dir, "/Socio_economic/Employment"), full.names = TRUE, pattern = paste(names(Scenario_corr), collapse = "|"))

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

#reload historic muni pop data produced in historic predictor prep
raw_mun_popdata <- readRDS("Data/Preds/Raw/Socio_economic/Population/raw_muni_pop_historic.rds")

#load kanton shapefile
Canton_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

#read in population data from html and convert to DF
px_data <- data.frame(read.px("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master"))

# subset px_data to historic cantonal population data
raw_can_popdata <- px_data[px_data$Demografische.Komponente == "Bestand am 31. Dezember" &
                     px_data$Staatsangehörigkeit..Kategorie. == "Staatsangehörigkeit (Kategorie) - Total" &
                     px_data$Geschlecht == "Geschlecht - Total", c(4:6)]
names(raw_can_popdata) <- c("Name_Canton", "Year", "Population")
raw_can_popdata$Name_Canton <- as.character(raw_can_popdata$Name_Canton)

#remove municipalities records by inverse-matching on the numeric contained in their name
raw_can_popdata <- raw_can_popdata[grep(".*?([0-9]+).*", raw_can_popdata$Name_Canton, invert = TRUE),]

#all cantons 
raw_can_popdata <- raw_can_popdata[grep(">>", raw_can_popdata$Name_Canton, invert = TRUE),]
raw_can_popdata <- raw_can_popdata[grep(paste0(unique(Canton_shp@data[["NAME"]]), collapse = "|"), raw_can_popdata$Name_Canton),]

#pivot to wide
raw_can_popdata <- raw_can_popdata %>% pivot_wider(names_from = "Year",
                                                values_from = "Population")

#match Canton names to the shape file
for(i in unique(Canton_shp@data[["NAME"]])){
raw_can_popdata$Name_Canton[grep(i, raw_can_popdata$Name_Canton)] <- i  
}

#add cantons numbers
raw_can_popdata$KANTONSNUM <- sapply(raw_can_popdata$Name_Canton, function(x){
unique(Canton_shp@data[Canton_shp@data$NAME == x, "KANTONSNUM"])
}) 

#get the indices of columns that represent the years
date_cols <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames(raw_can_popdata))))

#-------------------------------------------------------------------------------
# B.2- Calculate % cantonal population per municipality
#-------------------------------------------------------------------------------

pop_percentages <- do.call(cbind, sapply(date_cols, function(year){
  
#loop over canton numbers
muni_percs <- rbindlist(sapply(unique(raw_can_popdata$KANTONSNUM), function(canton_num){

#subset cantonal data by year  
can_data <- raw_can_popdata[raw_can_popdata$KANTONSNUM == canton_num, c(paste(year), "KANTONSNUM")]

#subset the municipality data by kanton name and year
muni_data <- raw_mun_popdata[raw_mun_popdata$KANTONSNUM == can_data$KANTONSNUM, c(paste(year), "KANTONSNUM")]
muni_data$KANTONSNUM <- NULL

#loop over municipalities
muni_data[[paste0("Perc_", year)]]  <- as.numeric(sapply(muni_data[[paste(year)]], function(year_value){
perc_value <- year_value/can_data[,paste(year)]*100
},simplify = TRUE)) #close loop over municipalities

return(muni_data)
}, simplify = FALSE))#close loop over kantons  

return(muni_percs)
}, simplify = FALSE)) #close loop over years

#add back in BFS and Cantons numbers
pop_percentages$BFS_NUM <- raw_mun_popdata$BFS_NUM
pop_percentages$KANTONSNUM <- raw_mun_popdata$KANTONSNUM

#------------------------------------------------------------------------------
#B.3- Calculate % urban area per municipality
#------------------------------------------------------------------------------

#Load the most recent LULC map
current_LULC <- raster("Data/Historic_LULC/LULC_2018_agg.gri")

#subset to just urban cell
Urban_rast <- current_LULC == 10

#Zonal stats to get urban area per kanton
Canton_urban_areas <- raster::extract(Urban_rast, Canton_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Canton ID 
Canton_urban_areas$Canton_num <- Canton_shp$KANTONSNUM

#combine areas for cantons with multiple polygons
Canton_urban_areas <- Canton_urban_areas %>%
  dplyr::group_by(Canton_num) %>%
  dplyr::summarise(across(c(layer), sum))

#load the municipality shape file
Muni_shp <- shapefile("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ] 

#Zonal stats to get number of Urban cells per Municipality polygon
#sum is used as a function because urban cells = 1 all others = 0
Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Canton and Municipality IDs
Muni_urban_areas$Canton_num <- Muni_shp@data[["KANTONSNUM"]]
Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
Muni_urban_areas$Perc_urban <- 0

#loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
for(i in Canton_urban_areas$Canton_num){

#vector kanton urban area
Kan_urban_area <- as.numeric(Canton_urban_areas[Canton_urban_areas$Canton_num == i, "layer"])  

#subset municipalities to this canton number 
munis_indices <- which(Muni_urban_areas$Canton_num == i)

#loop over municipalities in the Canton and calculate their urban areas as a % of the Canton's total  
for(muni in munis_indices){
Muni_urban_areas$Perc_urban[muni] <- (Muni_urban_areas[muni, "layer"]/Kan_urban_area)*100
  } #close inner loop 
} #close outer loop

#------------------------------------------------------------------------------
#B.4- Model relationship between cantonal % population and % urban area
#------------------------------------------------------------------------------

#subset pop percentages to 2018
Muni_percs <- pop_percentages[, c("BFS_NUM", "KANTONSNUM", "Perc_2018")]
colnames(Muni_percs) <-  c("BFS_NUM", "KANTONSNUM", "Perc_pop")

#combine with % urban values
Muni_percs$Perc_urban <- sapply(Muni_percs$BFS_NUM, function(x){
sum(Muni_urban_areas[Muni_urban_areas$Muni_num == x, "Perc_urban"])  
})

#loop over kantons and model relationship
Canton_models <- lapply(unique(Muni_percs$KANTONSNUM), function(canton_num){

#subset to data for this canton
kanton_data <- Muni_percs[Muni_percs$KANTONSNUM == canton_num,]

#produce GLM
Canton_model <- glm(data = kanton_data, formula = Perc_pop ~ Perc_urban, family = gaussian())  
  
return(Canton_model)
})
names(Canton_models) <- unique(Muni_percs$KANTONSNUM)

#save models
saveRDS(Canton_models, "Data/Preds/Tools/Dynamic_pop_models.rds")

### B.5- Prepare Cantonal projections of population development 

raw_data_path <- "Data/Preds/Raw/Socio_economic/Population/raw_pop_projections.xlsx"

#Download the required file 
download.file(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/12107013/master", destfile = raw_data_path, mode="wb")

#Vector existing sheet names
Sheet_names <- getSheetNames(raw_data_path)

#name the sheet names with the english names
names(Sheet_names) <- c("Ref", "High", "Low")

#Create a workbook to add the ckleaned data too
pop_proj_wb <- openxlsx::createWorkbook()

#The population projection table does not contain the canton numbers and because
#the Canton names are in German they cannot be matched with the shapefile
#to get the numbers (e.g Wallis - Valais etc.) instead load a different FSO
#table to get the numbers 
Canton_lookup <- openxlsx::read.xlsx("https://www.atlas.bfs.admin.ch/core/projects/13.40/xshared/xlsx/134_131.xlsx", rows = c(4:31), cols = c(2,3))
Canton_lookup$Canton_num <- seq(1:nrow(Canton_lookup))

#loop over time steps adding sheets and adding the predictors to them
#i=1
for (i in 1:length(Sheet_names)){

org_sheet_name <- Sheet_names[i]
new_sheet_name <- names(Sheet_names)[i]
   
#load correct sheet of raw data
tempdf <- openxlsx::read.xlsx(raw_data_path, sheet = org_sheet_name, startRow = 2)
colnames(tempdf)[1] = "Canton_name" #rename first column
tempdf <- tempdf[tempdf$Canton_name != "Schweiz",] # remove the total for switzerland
tempdf <- tempdf[complete.cases(tempdf),] #remove incomplete or empty rows

#vector the names of the year columns
year_cols <- names(tempdf)[2:ncol(tempdf)]

#add the canton number
tempdf$Canton_num <- sapply(tempdf$Canton_name, function(x) {
Canton_lookup[grepl(x, Canton_lookup$X2), "Canton_num"]
})

#replace values of non-matching names
tempdf[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num), "Canton_num"] <- Canton_lookup$Canton_num[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num)]
rownames(tempdf) <- 1:nrow(tempdf) #correct row names

#check if any time-points for scenarios are missing
Missing_years <- Time_steps[!paste(Time_steps) %in% colnames(tempdf)]

if(length(Missing_years)>0){
#extrapolate the missing years
#add columns for missing years
tempdf[paste(Missing_years)] <- NA
  
#loop over rows (cantons)
for(r in 1:nrow(tempdf)){

  row_dat <-data.frame(t(tempdf[r, year_cols]))
  names(row_dat) <- "Pred_pop"
  row_dat$year <- as.numeric(year_cols) 
 
  #create linear model
  mod <- lm(formula = Pred_pop~year, data = row_dat)
  
  #predict missing years
  tempdf[r, paste(Missing_years)] <- round(predict(mod, data.frame(year = Missing_years)),0)
  } #close loop over rows
} #close if statement

#create sheet in workbook, the try() is necessary in case sheets already exist  
try(addWorksheet(pop_proj_wb, sheetName = new_sheet_name))  

#write the data to the sheet
writeData(pop_proj_wb, sheet = new_sheet_name, x = tempdf)

}
#save workbook
openxlsx::saveWorkbook(pop_proj_wb, "Data/Preds/Tools/Population_projections.xlsx", overwrite = TRUE)

### =========================================================================
### C- Preparing future climatic data
### =========================================================================

#The climatic data for the future time points has been download in the script:
# "Calibration_predictor_prep" it's native resolution is 25m so it needs to be
#rescaled to 100m

#list the files of the raw climatic variables
Clim_var_paths <- list.files("Data/Preds/Raw/Climatic/Future", recursive = TRUE, full.names = TRUE)

#loop over then re-scaled and saving layers
Prep_clim_vars_paths <-lapply(Clim_var_paths, function(x){
  
  #alter file base name to match covariate ID from calibration period
  file_name <- str_replace_all(basename(x), c(Tave="Average_Avg_ann_temp",
                                              Prec= "Average_Avg_precip",
                                              "0_degrees" = "Average_Sum_gdays_0deg",
                                              "3_degrees" = "Average_Sum_gdays_3deg",
                                              "5_degrees" = "Average_Sum_gdays_5deg"))
  
  #vector save path
  layer_path <-paste0(Prepared_layers_dir, "/Climatic/", file_name)
  
  #load data
  #Raw_dat <- raster(x)

  #aggregate
  #Agg_dat <- aggregate(Raw_dat, fact=4, fun=mean)
  
  #save
  #writeRaster(Agg_dat, layer_path, overwrite=TRUE)
  
  return(layer_path)
})

### =========================================================================
### D- Prepare a table of info for future predictors to be added to predictor lookup table 
### =========================================================================

#bind together vectors of file paths for future predictors that have been created
Future_pred_paths <- unlist(c(Future_FTE_file_paths, Prep_clim_vars_paths))

#load the predictor table sheet for the most recent calibration period
Predictor_table <- read.xlsx(Pred_table_path, sheetName = "2009_2018")

#load the sheet of the scenario table for RCP designations
Scenario_RCPs <- Scenario_data_table$Climate_RCP
names(Scenario_RCPs) <- Scenario_data_table[,"Scenario_ID"]

#filtering to static predictors
Static_preds <- Predictor_table[Predictor_table$Static_or_dynamic == "static",]
Static_preds$Scenario <- "All"

#create DF for capturing info
Dynamic_preds <- data.frame(matrix(ncol = length(colnames(Static_preds)), nrow=length(Future_pred_paths)))
colnames(Dynamic_preds) <- colnames(Static_preds)

#fill in column details
Dynamic_preds[,"Prepared_data_path"] <- Future_pred_paths
Dynamic_preds$Predictor_category <- sapply(Dynamic_preds$Prepared_data_path, function(x) str_match(x, paste(c("Climatic", "Socio_economic"), collapse = "|")))  #instead grepl on the file path
Dynamic_preds$Static_or_dynamic <- "Dynamic"
Dynamic_preds$CA_category <- "Suitability"
Dynamic_preds$Prepared <- "Y"


Dynamic_preds$Scenario <- sapply(1:nrow(Dynamic_preds), function(i){
  
  #use if/else statement based on predictor category 
  if(Dynamic_preds[i, "Predictor_category"] == "Climatic"){
    
    #match on the RCP string in the file path and return the scenario name
    #becuase multiple scenarios use the same RCP this will be a vector
    Scenario_name <-names(Scenario_RCPs)[which(Scenario_RCPs == c(str_match(Dynamic_preds[i,"Prepared_data_path"], paste(c(Scenario_RCPs), collapse = "|"))))]    

    }else if(Dynamic_preds[i, "Predictor_category"] == "Socio_economic"){
    
    #match on the scenario name
    Scenario_name <- str_match(Dynamic_preds[i,"Prepared_data_path"], paste(c(names(Scenario_RCPs)), collapse = "|"))
    }

  return(Scenario_name)
})
  
Dynamic_preds$Original_resolution <- sapply(1:nrow(Dynamic_preds), function(i){
  
  #use if/else statement based on predictor category 
  if(Dynamic_preds[i, "Predictor_category"] == "Climatic"){
      res <- "25m"  
    }else if(Dynamic_preds[i, "Predictor_category"] == "Socio_economic"){
      res <- "Labour market region"
    }
})

Dynamic_preds$URL <- sapply(1:nrow(Dynamic_preds), function(i){
  if(Dynamic_preds[i, "Predictor_category"] == "Climatic"){
      "https://zenodo.org/record/7590103#.Y-KKIpDYqUk"
    }else if(Dynamic_preds[i, "Predictor_category"] == "Socio_economic"){
      "https://zenodo.org/record/4774914#.Y-KKQJDYqUn"
    }
})

Dynamic_preds$Data_citation <- sapply(1:nrow(Dynamic_preds), function(i){
  if(Dynamic_preds[i, "Predictor_category"] == "Climatic"){
      "Broennimann (2023)"
    }else if(Dynamic_preds[i, "Predictor_category"] == "Socio_economic"){
      "Cretegny and Muller (2020)"
    }
})


#match covariate IDs found in the predictor table to the file paths
Dynamic_preds$Covariate_ID <- sapply(Dynamic_preds$Prepared_data_path, function(x){
  var_name <- na.omit(c(str_match(x, regex(Predictor_table$Covariate_ID, ignore_case = T)))) 
  })

Dynamic_preds$period <- sapply(1:nrow(Dynamic_preds), function(i){
  
  #seperate file path
  dat_path <- Dynamic_preds[i, "Prepared_data_path"]
  
  #vector largest numeric value contained in file path
  largest_year <- max(as.numeric(unlist(regmatches(dat_path, gregexpr("[[:digit:]]+", dat_path)))))
  
  #use if/else statement based on predictor category 
  if(Dynamic_preds[i, "Predictor_category"] == "Climatic"){
      
    #socio_economic variables are for the period after that contained in the path
    Period <- paste0(largest_year, "_", (largest_year+Step_length))
    
    }else if(Dynamic_preds[i, "Predictor_category"] == "Socio_economic"){
      
    #socio_economic variables are for the period express in the path
    Period <- paste0((largest_year-Step_length), "_", largest_year)
      
    }
  return(Period)
})
  
#This is not 100% accurate because the climatic layers actual
#using only the four previous years instead of 5
# but for the table it is not important
Dynamic_preds$Temporal_coverage <- Dynamic_preds$period 

### =========================================================================
### E- Add static/dynamic variable data to sheets  for future time points 
### =========================================================================

#loop over time steps binding static and dynamic preds
Combined_vars_for_time_steps <- sapply(Time_steps, function(sim_year){
  
  sim_period <- paste0(sim_year, "_", sim_year+Step_length)
  
  #subset dynamic variables to the current time_period
  # Dynamic_preds <- Dynamic_preds[sapply(Dynamic_preds$period, function(data_period){
  #   dates <- as.numeric(str_split(data_period, "_")[[1]])
  #   between(sim_year, dates[1], dates[2])
  # }),]
  Dynamic_preds <- Dynamic_preds[Dynamic_preds$period == sim_period,]
  
  Combined_vars <- rbind(Static_preds, Dynamic_preds)
  Combined_vars$period <- sim_period
  return(Combined_vars)
}, simplify = FALSE)
names(Combined_vars_for_time_steps) <- Time_steps

#load predictor_table as workbook to add sheets
pred_workbook <- openxlsx::loadWorkbook(file = Pred_table_path)

#loop over time steps adding sheets and adding the predictors to them
for(i in Time_steps){
#the try() is necessary in case sheets already exist
try(addWorksheet(pred_workbook, sheetName = i))
writeData(pred_workbook, sheet = paste(i), x = Combined_vars_for_time_steps[[paste(i)]])
}

#save workbook
openxlsx::saveWorkbook(pred_workbook, Pred_table_path, overwrite = TRUE)

### =========================================================================
### F- create predictor variable stack for each scenario/time point
### =========================================================================

#To do: implement a check in the loop to make sure there are no duplicate
#predictors being included in the stacks

#vector scenario names
Scenario_names <- Scenario_data_table$Scenario_ID

#upper loop over time steps
sapply(Time_steps, function(sim_year){

  #load corresponding sheet of predictor table
  pred_details <- openxlsx::read.xlsx(Pred_table_path, sheet = paste(sim_year))
  
  #convert scenario column back to character vectors
  pred_details$Scenario <- sapply(pred_details$Scenario, function(x) unlist(strsplit(x, ",")))
  
  #loop over scenario names  
  sapply(Scenario_names, function(scenario){
    
    #first seperate static variables scenario = "All""
    preds_static <- pred_details[pred_details$Scenario == "All",] 
    
    #then those relevant for the scenario, necessary to do it this way
    #because some rows contain multiple values for scenario
    preds_scenario <- pred_details[grep(scenario, pred_details$Scenario),]
    
    #bind static and scenario specific preds
    preds_scenario <- rbind(preds_static, preds_scenario)
    
    #use file paths to stack
    pred_stack <- raster::stack(preds_scenario$Prepared_data_path)
    
    #name layers
    names(pred_stack@layers) <- preds_scenario$Covariate_ID
  
    #save
    saveRDS(pred_stack, file = paste0("Data/Preds/Prepared/Stacks/Simulation/SA_preds/SA_pred_", scenario, "_", sim_year, ".rds"))
    }) #close loop over scenarios
}) #close loop over time steps


