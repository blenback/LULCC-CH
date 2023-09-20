#############################################################################
## Convert predictor (covariate) data layers into a uniform 100m resolution, crs and extent
## Layers produced are saved in a logical folder structure with the suffix of each file name corresponding to the covariate number
## detailed in the data table: C:/Users/bblack/switchdrive/Private/PhD/Modelling/Dry_run/Documentation/Covariate data for dry-run.xlsx
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
         "sf", "tiff", "bfsMaps", "rjstat")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#Load in the grid to use use for re-projecting the CRS and extent of covariate data
Ref_grid <- raster("Data/Ref_grid.gri")

Geoms_path <- "Data/Preds/Raw/CH_geoms"
dir.create(Geoms_path)

#download folder of basic map geometries for Switzerland
DownloadBfSMaps(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/21245514/master",
path = Geoms_path)

#create directories
dir.create("Data/Preds/Prepared/Stacks", recursive = TRUE)
dir.create("Data/Preds/Prepared/Layers", recursive = TRUE)

#modification to predictor table
#get names of sheets to loop over
sheets <- excel_sheets("Data/Preds/Predictor_table.xlsx")

#load all sheets as a list
# Pred_tables <- sapply(sheets, function(x) read.xlsx("Data/Preds/Predictor_table.xlsx", sheet = x))
# 
# Pred_tables_update <- lapply(Pred_tables, function(x){
# x$File_name <- str_replace_all(str_remove_all(x$File_name, "Prepared/"), "Raw/", "Prepared/Layers/")  
# return(x)
# })
# 
# #save the updated tables
# openxlsx::write.xlsx(Pred_tables_update, file = "Data/Preds/Predictor_table.xlsx", overwrite = TRUE)

### =========================================================================
### A- Changes to municipalities
### =========================================================================

#Over time there has been many changes to the desinations of municipalities
#in Switzerland e.g aggregations, disaggregations, name changes etc. 
#We need to harmonize data from different time periods to the same
#schematic of municipalities in order to spatialize it accurately

#load table of municipaliy mutations for current study data i.e. upto 2022
muni_mutations <- readxl::read_excel("PopulationModel_Valpar/Data/01_Raw_Data/MutationGemeinde/Mutierte_Gemeinden_namechange.xlsx", 
                      range = "A2:J55")

## rename columns
colnames(muni_mutations) <- c("Mutation_Number", "Pre_canton_ID", 
                                    "Pre_District_num", "Pre_BFS_num", 
                                    "Pre_muni_name", "Post_canton_ID",
                                    "Post_district_num", "Post_BFS_num", 
                                    "Post_muni_name", "Change_date")

# import municipality shape file
Muni_shp <- shapefile("PopulationModel_ValPar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]

### =========================================================================
### X- Load in covariate layers that need to be re-projected and aggregated###
### =========================================================================

#create a list of raw data rasters then loop over them and change extent, res and crs
#before saving to the correct location in: Preds/Prepared/Layers/'pred name'/'layer' 
test <- list.files("E:/LULCC_CH/Data/Preds/Raw", recursive = TRUE, full.names = TRUE)

### =========================================================================
### X- Socio_economic: Employment
### =========================================================================

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

##OLD APPROACH USING DATA SOURCES ALREADY AGGREGATED TO THE MUNICIPALITY LEVEL
#THIS INTRODUCES PROBLEMS DUE TO MUNICIPALITIES CHANGING OVER TIME

#Start with the Business census data 
#Gerecke et al. 2019 provide this data and a method to extrapolate it
# to match the years of the first 3 Areal statistik datapoints (1985, 1997,2009)
#this is easier than using the raw data from original source

#Download data from Gerecke et al. 
lulcc.downloadunzip(url = "https://www.envidat.ch/dataset/c259ac71-37ea-477e-94b0-c463cc195304/resource/3f13ea3a-3dd2-48a3-98a1-284b565b2c2d/download/data.zip",
                    save_dir = "Data/Preds/Raw/Socio_economic/Employment/Municipality_employment" )

#load table of municipal employment figures for different sectors
muni_employment <- read.csv(list.files(wpath, recursive = TRUE, pattern = "workers_sectortest"))

#identify which municipalities have mutations associated with them
mutation_index <- match(muni_employment$gmde, muni_mutations$Pre_muni_name) # which have changed

#change municpality BFS number in population table according to the mutation
#for each row in the pop df if there is an NA in the mutation index do not replace the BFS number 
#If there is not an NA then replace with the new BFS number of the mutation table. 
for (i in 1:nrow(muni_employment)){
  if (!is.na(mutation_index[i])){ 
    muni_employment$BFS[[i]] <- muni_mutations[[mutation_index[[i]], "Post_BFS_num"]]}else{
      muni_employment$BFS[[i]] <- muni_employment$nr[[i]]
    }
}

#If after introducing the mutations we have multiple rows with the same BFS numbers
#then we need to combine their populations values as these indicate municipalities merging
## to have one number for sec1 and sec23 per municipality, create new matrix where those with the same municipality are added together
sec <- matrix(ncol=9,nrow = length(unique(muni_employment$BFS)))
colnames(sec) <- c("BFS","sec1.95","sec23.95","sec1.01","sec23.01","sec1.05","sec23.05","sec1.08","sec23.08")

sec[,1] <- sort(unique(muni_employment$BFS))
for (j in 1:4){
  for (i in 1:length(unique(muni_employment$BFS))){
    sec[i,(2*j)] <- sum(muni_employment[muni_employment$BFS==sec[i,1],(4+(j-1)*3)])
    sec[i,(2*j+1)] <- sum(muni_employment[muni_employment$BFS==sec[i,1],(5+(j-1)*3)]) + sum(muni_employment[muni_employment$BFS==sec[i,1],(6+(j-1)*3)])
  }
}

#Statent data from 2011-2020
#vector JSON query
body_list <- '{
  "query": [
    {
      "code": "Wirtschaftssektor",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2",
          "3"
        ]
      }
    },
    {
      "code": "Beobachtungseinheit",
      "selection": {
        "filter": "item",
        "values": [
          "4"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'

#Post query to API
Statent_return <- rawToChar(POST("https://www.pxweb.bfs.admin.ch/api/v1/de/px-x-0602010000_102/px-x-0602010000_102.px", body = body_list, encode = "json")[["content"]])

#concert Json-stat object to dataframe
Statent_data <- rjstat::fromJSONstat(Statent_return, naming = "label", use_factors = FALSE, silent = FALSE)[[1]]

#remove the rows for Swiss totals
Statent_data <- Statent_data[-c(grep("Schweiz", Statent_data$Gemeinde)),]

#remove the column containing the variable name
Statent_data$Beobachtungseinheit <- NULL

#seperate municipality number and name
Statent_data$BFS <- as.numeric(gsub(".*?([0-9]+).*", "\\1", Statent_data$Gemeinde))
Statent_data$Gemeinde <- trimws(gsub("[[:digit:]]", "", Statent_data$Gemeinde))

#replace col names
colnames(Statent_data) <- c("Year", "Municipality", "Sector", "value", "BFS") 

#pivot to wide
Statent_wide <- Statent_data %>% pivot_wider(names_from = "Year",
                                             names_prefix = "Y",
                                                values_from = "value")

#recode sector names
names_key <- c("sec1", "sec2", "sec3")
names(names_key) <- unique(Statent_wide$Sector) 
Statent_wide <- Statent_wide %>%
  mutate(Sector = recode(Sector, !!!names_key))

#identify which municipalities have mutations associated with them
mutation_index <- match(Statent_wide$BFS, muni_mutations$Pre_BFS_num) # which have changed

#change municpality BFS number in population table according to the mutation
#for each row in the pop df if there is an NA in the mutation index do not replace the BFS number 
#If there is not an NA then replace with the new BFS number of the mutation table. 
for (i in 1:nrow(Statent_wide)){
  if (!is.na(mutation_index[i])){ 
    Statent_wide$BFS[[i]] <- muni_mutations[[mutation_index[[i]], "Post_BFS_num"]]}
}

#seperate the sector 1/ sector 2/3 data
Statent_sec1 <- Statent_wide[Statent_wide$Sector== "sec1",]
Statent_sec23 <- Statent_wide[Statent_wide$Sector != "sec1",]

#loop over BFS numbers and add together the values in each column
Statent_sec23_combined <- matrix(ncol=11, nrow= length(unique(Statent_sec23$BFS))) #create empty matrix
colnames(Statent_sec23_combined) <- c("BFS", grep("Y", colnames(Statent_sec23), value = TRUE))
for(i in 1:length(unique(Statent_sec23$BFS))){
BFS <- unique(Statent_sec23$BFS)[i] #identify current BFS number
Statent_sec23_combined[i, c("BFS")] <- BFS
#loop over year columns
Statent_sec23_combined[i, 2:ncol(Statent_sec23_combined)] <- apply(Statent_sec23[Statent_sec23$BFS == BFS, 4:13], 2, function(x) sum(x, na.rm = TRUE))
}

setdiff(Statent_sec23_combined[,"BFS"], unique(Muni_shp@data[["BFS_NUMMER"]]))
setdiff(sector23[,"BFS"], Muni_shp@data[["BFS_NUMMER"]])
#combine with the business census data

#Model change per municipality based on absolute numbers to extrapolate values
#for the AS statistik years for sector 1 and sector 2/3 seperately

#Sector 1
sector1 <- matrix(ncol=8, nrow=nrow(sec)) #create empty matrix
sector1[,1:5] <- sec[,c("BFS", grep("sec1", colnames(sec), value = TRUE))] #select columns for BFS and sector 1
colnames(sector1) <- c("BFS","Y1995","Y2001", "Y2005", "Y2008", "Y1985", "Y1997", "Y2009")

for (i in 1:nrow(sector1)){
  mod1 <- lm(sector1[i,c(2:5)]~c(1995,2001,2005,2008)) ## linear model
  sector1[i,c("Y1985")] <- round(coef(mod1)[1]+coef(mod1)[2]*1985,0)
  sector1[i,c("Y1997")] <- round(coef(mod1)[1]+coef(mod1)[2]*1997,0)
  sector1[i,c("Y2009")] <- round(coef(mod1)[1]+coef(mod1)[2]*2009,0)
}

#same for Sector 2&3
sector23 <- matrix(ncol=8, nrow=nrow(sec))
sector23[,1:5] <- sec[,c("BFS", grep("sec23", colnames(sec), value = TRUE))]
colnames(sector23) <- c("BFS","Y1995","Y2001", "Y2005", "Y2008", "Y1985", "Y1997", "Y2009")

for (i in 1:nrow(sector23)){
  mod1 <- lm(sector23[i,c(2:5)]~c(1995,2001,2005,2008))
  sector23[i,c("Y1985")] <- round(coef(mod1)[1]+coef(mod1)[2]*1985,0)
  sector23[i,c("Y1997")] <- round(coef(mod1)[1]+coef(mod1)[2]*1997,0)
  sector23[i,c("Y2009")] <- round(coef(mod1)[1]+coef(mod1)[2]*2009,0)
}


#Divide the changes in municipal employment estimated for the AS flying periods by
#the number of years to get an average annual rate of change for each period
#because this can also be calculated for the future projected data
#vector years of LULC data
LULC_years <- c("1985", "1997", "2009", "2018")
LULC_change_periods <- c()
for (i in 1:(length(LULC_years)-1)) {
            LULC_change_periods[[i]] <- c(LULC_years[i],LULC_years[i+1])
        }
names(LULC_change_periods) <- sapply(LULC_change_periods, function(x) paste(x[1], x[2], sep = "_"))



Period_length <- as.numeric(Raster_combo[2])- as.numeric(Raster_combo[1])
Num_steps <- ceiling(Period_length/Step_length)



#aggregate to labour regions
#use St_intersects or the metadata for the future scenarios which lists
#which economic region each municipality falls into 

#load shapefile of labour market regions
LMR_shp <- sf::st_read("E:/LULCC_CH/Data/Preds/Raw/CH_geoms/2022_GEOM_TK/03_ANAL/Gesamtfläche_gf/K4_amre_20180101_gf/K4amre_20180101gf_ch2007Poly.shp")




        
Sect1_1985 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/SecTor1_1985.tif")
Sect1_1997 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/SECTor1_1997.tif")
Sect1_2009 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/Sector1_2009.tif")
Sect23_1985 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/Sector23_1985.tif")
Sect23_1997 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/Sector23_1997.tif")
Sect23_2009 <- raster("Covariates/Socio_economic/Employment/Municipality_employment/Original/Sector23_2009.tif")
        
#change extent of layers
Sect1_1985 <- setExtent(Sect1_1985, Ref_grid) 
Sect1_1997 <- setExtent(Sect1_1997, Ref_grid) 
Sect1_2009 <- setExtent(Sect1_2009, Ref_grid) 
Sect23_1985 <- setExtent(Sect23_1985, Ref_grid)  
Sect23_1997 <- setExtent(Sect23_1997, Ref_grid) 
Sect23_2009 <- setExtent(Sect23_2009, Ref_grid) 
        
#removing negative values and NAs
values(Sect1_1985)[is.na(values(Sect1_1985))] <- 0
values(Sect1_1997)[is.na(values(Sect1_1997))] <- 0
values(Sect1_2009)[is.na(values(Sect1_2009))] <- 0
values(Sect23_1985)[is.na(values(Sect23_1985))] <- 0
values(Sect23_1997)[is.na(values(Sect23_1997))] <- 0
values(Sect23_2009)[is.na(values(Sect23_2009))] <- 0
Sect1_1985[Sect1_1985<0]<-0
Sect1_1997[Sect1_1997<0]<-0
Sect1_2009[Sect1_2009<0]<-0
Sect23_1985[Sect23_1985<0]<-0
Sect23_1997[Sect23_1997<0]<-0
Sect23_2009[Sect23_2009<0]<-0
        
#calculating change in employment numbers between periods
Primary_period1 <- Sect1_1997-Sect1_1985
Primary_period2 <- Sect1_2009-Sect1_1997
S23_period1 <- Sect23_1997-Sect23_1985
S23_period2 <- Sect23_2009-Sect23_1997
        
#resampling
Primary_period1_res <- resample(Primary_period1, Ref_grid)
Primary_period2_res <- resample(Primary_period2, Ref_grid)
S23_period1_res <- resample(S23_period1, Ref_grid)
S23_period2_res <- resample(S23_period2, Ref_grid)
        
        #write rasters
        writeRaster(Sect1_1985, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor1_1985", overwrite = TRUE)
        writeRaster(Sect1_1997, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor1_1997", overwrite = TRUE)
        writeRaster(Sect1_2009, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor1_2009", overwrite = TRUE)
        writeRaster(Sect23_1985, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor23_1985", overwrite = TRUE)
        writeRaster(Sect23_1997, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor23_1997", overwrite = TRUE)
        writeRaster(Sect23_2009, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/SecTor23_2009", overwrite = TRUE)
        
        writeRaster(Primary_period1_res, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/Primary_employment_change_85_97_cov10.tif", overwrite = TRUE)
        writeRaster(Primary_period2_res, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/Primary_employment_change_97_09_cov11.tif", overwrite = TRUE)
        writeRaster(S23_period1_res, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/Sect23_employment_change_85_97_cov12.tif", overwrite = TRUE)
        writeRaster(S23_period2_res, "Covariates/Socio_economic/Employment/Municipality_employment/Prepared/Sect23_employment_change_97_09_cov13.tif", overwrite = TRUE)

        
                
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
    
#Topographic covariates:
    #Elevation (mean) 
    Elevation_mean <- readRDS("Covariates/Topographic/dem/Original/ch_topo_alti3d2016_pixel_dem_mean2m.rds")
    Elevation_100m <- aggregate(Elevation_mean, fact=4, fun=mean)
    res(Elevation_100m)
    extent(Elevation_100m)
    crs(Elevation_100m) 
    writeRaster(Elevation_100m, "Covariates/Topographic/dem/Prepared/Elevation_mean_100m_cov14.tif", overwrite=TRUE)

    #Aspect (mean) 
    Aspect_mean <- readRDS("Covariates/Topographic/aspect/Original/ch_topo_alti3d2016_pixel_aspect_mean2m.rds")
    Aspect_100m <- aggregate(Aspect_mean, fact=4, fun=mean)
    res(Aspect_100m)
    extent(Aspect_100m)
    crs(Aspect_100m) 
    writeRaster(Aspect_100m, "Covariates/Topographic/aspect/Prepared/Aspect_mean_100m_cov15.tif", overwrite=TRUE)

    #Hillshade (mean)
    Hillshade_mean <- readRDS("Covariates/Topographic/hillshade/Original/ch_topo_alti3d2016_pixel_hillshade_mean.rds")
    Hillshade_100m <- aggregate(Hillshade_mean, fact=4, fun=mean)
    res(Hillshade_100m)
    extent(Hillshade_100m)
    crs(Hillshade_100m) 
    writeRaster(Hillshade_100m, "Covariates/Topographic/hillshade/Prepared/Hillshade_mean_100m_cov16.tif", overwrite=TRUE)

    #Slope
    Slope_mean <- readRDS("Covariates/Topographic/slope/Original/ch_topo_alti3d2016_pixel_slope_mean2m.rds")
    Slope_100m <- aggregate(Slope_mean, fact=4, fun=mean)
    res(Slope_100m)
    extent(Slope_100m)
    crs(Slope_100m) 
    writeRaster(Slope_100m, "Covariates/Topographic/slope/Prepared/Slope_mean_100m_cov17.tif", overwrite=TRUE)
    
    #light
    light_mean <- raster("Covariates/Topographic/light/Original/SPEEDMIND_SoilL.tif")
    #re-project and change resolution
    light_100m <- projectRaster(light_mean, crs= crs(Ref_grid), res = res(Ref_grid))
    light_100m <- resample(light_100m, Ref_grid)
    writeRaster(light_100m, "Covariates/Topographic/light/Prepared/light_100m_cov18.tif", overwrite=TRUE)
    res(light_100m)
    extent(light_100m)
    crs(light_100m) 
 
#Transport-related covariates
    #Noise pollution index (mean from 25m data)
    noise_25m <- readRDS("Covariates/Transport_related/Noise/Original/ch_transport_sonbase_pixel_noise.rds")
    noise_100m <- aggregate(noise_25m, fact=4, fun=mean)
    res(noise_100m)
    extent(noise_100m)
    crs(noise_100m) 
    writeRaster(noise_100m, "Covariates/Transport_related/Noise/Prepared/noise_mean_100m_cov19.tif", overwrite=TRUE)

    #Distance to roads (mean distance to all classes from 25m data)
    Distance_to_roads_25m <- readRDS("Covariates/Transport_related/Distance_to_roads/Original/ch_transport_tlm3d_pixel_dist2road_all.rds")
    Distance_to_roads_100m <- aggregate(Distance_to_roads_25m, fact=4, fun=mean)
    res(Distance_to_roads_100m)
    extent(Distance_to_roads_100m)
    crs(Distance_to_roads_100m) 
    writeRaster(Distance_to_roads_100m, "Covariates/Transport_related/Distance_to_roads/Prepared/Distance_to_roads_mean_100m_cov20.tif", overwrite=TRUE)
    plot(Distance_to_roads_25m)    
    plot(Distance_to_roads_100m)

#Distance to hydrological features covariates
    #Distance to lakes (mean distance from 25m data to all lakes of various categories) 
    Distance_to_lakes_25m <- readRDS("Covariates/Hydro/Distance_lakes/Original/ch_hydro_gwn07_pixel_dist2lake_all.rds")
    Distance_to_lakes_100m <- aggregate(Distance_to_lakes_25m, fact=4, fun=mean)
    res(Distance_to_lakes_100m)
    extent(Distance_to_lakes_100m)
    crs(Distance_to_lakes_100m) 
    writeRaster(Distance_to_lakes_100m, "Covariates/Hydro/Distance_lakes/Prepared/Distance_to_lakes_mean_100m_cov21.tif", overwrite=TRUE)
    plot(Distance_to_lakes_25m)    
    plot(Distance_to_lakes_100m)

    #Distance to rivers (mean distance from 25m data to all rivers of various categories) 
    Distance_to_rivers_25m <- readRDS("Covariates/Hydro/Distance_rivers/Original/ch_hydro_gwn07_pixel_dist2riverstrahler_all.rds")
    Distance_to_rivers_100m <- aggregate(Distance_to_rivers_25m, fact=4, fun=mean)
    res(Distance_to_rivers_100m)
    extent(Distance_to_rivers_100m)
    crs(Distance_to_rivers_100m) 
    writeRaster(Distance_to_rivers_100m, "Covariates/Hydro/Distance_rivers/Prepared/Distance_to_rivers_mean_100m_cov22.tif", overwrite=TRUE)
    plot(Distance_to_rivers_25m)    
    plot(Distance_to_rivers_100m)
    
       #Continentality
    Continentality <- raster("Covariates/Climatic/Continentality/Original/SPEEDMIND_SoilK.tif")
    #re-project and change resolution
    Continentality_100m <- projectRaster(Continentality, crs= crs(Ref_grid), res = res(Ref_grid))
    Continentality_100m <- resample(Continentality_100m, Ref_grid)
    res(Continentality_100m)
    extent(Continentality_100m)
    crs(Continentality_100m)
    writeRaster(Continentality_100m, "Covariates/Climatic/Continentality/Prepared/Continentality_100m_cov23.tif", overwrite=TRUE)

#Climatic covariates 
    
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
    
cat(paste0(' Preparation of Suitability and accessibility predictor layers complete \n'))
 
    
    
    
