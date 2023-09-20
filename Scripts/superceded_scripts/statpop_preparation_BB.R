#############################################################################
## Pop_data_prep: Preparing historic population data and models for
## future population projection
## Date: 01-10-2022
## Authors: Jan Streit and Ben Black
#############################################################################

### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
# mac wd  (USE THIS CODE ON MAC)
#setwd("/Volumes/plus_projects/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC_CH/PopulationModel_ValPar/Data")

# Setting the working directory on windows (USE THIS CODE ON WINDOWS)
# setwd("Y:/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC_CH/PopulationModel_ValPar/Data")

wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl","rlist", "tidyverse",
         "rstatix", "Dinamica", "raster", "openxlsx")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Load in the grid to use for rasterization
Ref_rast <- raster("Data/Ref_grid.gri")

### =========================================================================
### B- prepare municipality population data incorporating mutations
### =========================================================================

# import the xlsx data
raw_mun_popdata <- read_excel("PopulationModel_ValPar/Data/01_Raw_Data/px-x-0102020000_201.xlsx", 
                              range = "A3:AQ2169")

# rename the 1st column, remove 2nd
names(raw_mun_popdata)[1] <- "Name_Municipality"
raw_mun_popdata[2] <- NULL

#Remove the periods in the name column
raw_mun_popdata$Name_Municipality <- gsub("[......]","",as.character(raw_mun_popdata$Name_Municipality))

#Seperate BFS number from name
raw_mun_popdata$BFS_NUM <- as.numeric(gsub(".*?([0-9]+).*", "\\1", raw_mun_popdata$Name_Municipality)) 
  
#Remove BFS number from name
raw_mun_popdata$Name_Municipality <- gsub("[[:digit:]]", "", raw_mun_popdata$Name_Municipality)

# subset to only municipalities existing in 2021
raw_mun_popdata <- raw_mun_popdata[raw_mun_popdata$`2021`> 0,]

# import municipality shape file
Muni_shp <- shapefile("PopulationModel_ValPar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]

## TO DO: REPLACE THIS TABLE WITH A DOWNLOAD LINK 
# import data mutated municipalities
muni_mutations <- read_excel("PopulationModel_Valpar/Data/01_Raw_Data/MutationGemeinde/Mutierte_Gemeinden_namechange.xlsx", 
                      range = "A2:J55")

## rename columns
colnames(muni_mutations) <- c("Mutation_Number", "Pre_canton_ID", 
                                    "Pre_District_num", "Pre_BFS_num", 
                                    "Pre_muni_name", "Post_canton_ID",
                                    "Post_district_num", "Post_BFS_num", 
                                    "Post_muni_name", "Change_date")

## loop over the unique values - subset the table to only the unique rows

## THE PROBLEM IS THAT WE STILL HAVE TWO MUNICIPALITIES THAT CHANGED TWICE

#identify which municipalities have mutations associated with them
mutation_index <- match(raw_mun_popdata$BFS_NUM, muni_mutations$Pre_BFS_num) # which have changed

#change municpality BFS number in population table according tothe mutation
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

### =========================================================================
### C- create historic municipality population rasters
### =========================================================================

#vector the years population data required from the LULC data point
pop_years <- gsub(".*?([0-9]+).*", "\\1", list.files("E:/LULCC_CH/Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))

#seperate pop data for LULc years
pop_in_LULC_years <- raw_mun_popdata[,c("BFS_NUM", pop_years)]

#link with spatial municipality data, rasterize and save
for(i in pop_years){

#loop over the BFS numbers of the polygons and match to population values
Muni_shp@data[paste0("Pop_", i)] <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){

  pop_value <- as.numeric(pop_in_LULC_years[pop_in_LULC_years$BFS_NUM == Muni_num, paste(i)])  

  }, simplify = TRUE))     

#rasterize
pop_rast <- rasterize(Muni_shp, Ref_rast, field = paste0("Pop_", i))

#file_path
save_path <- paste0("Data/Preds/Raw/Socio_economic/Population/Prepared/", "Pop_", i, ".tif")

#save
raster::writeRaster(pop_rast, save_path)
  
}# close loop over LULC years

#add rasters to predictor table


### =========================================================================
### D- Prepare historic cantonal population data
### =========================================================================

# import of historic cantonal population data
raw_can_popdata <- read_excel("PopulationModel_ValPar/Data/01_Raw_Data/px-x-0102020000_201_20221013-090701.xlsx", range = "A3:AQ29")

#remove 2nd column
raw_can_popdata["...2"] <- NULL

#rename the canton name column
names(raw_can_popdata)[1] <- "Name_Canton"

#Delete the characters before the name
raw_can_popdata$Name_Canton <- substring(raw_can_popdata$Name_Canton, 3)

#load kanton shapefile
Kanton_shp <- shapefile("PopulationModel_ValPar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")

#match Kanton names to the shape file
for(i in unique(Kanton_shp@data[["NAME"]])){
raw_can_popdata$Name_Canton[grep(i, raw_can_popdata$Name_Canton)] <- i  
}

#add cantons numbers
raw_can_popdata$KANTONSNUM <- sapply(raw_can_popdata$Name_Canton, function(x){
unique(Kanton_shp@data[Kanton_shp@data$NAME == x, "KANTONSNUM"])
}) 

#get the indices of columns that represent the years
date_cols <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames(raw_can_popdata))))

#check that the raw municipality pop values sum to the raw cantonal pop values
raw_canton_sums <- do.call(cbind, lapply(date_cols, function(x){
pop_sum <- raw_mun_popdata %>%
  group_by(KANTONSNUM) %>%
  summarise(across(c(paste(x)), sum))
return(pop_sum)
}))

### =========================================================================
### E- Calculate % cantonal population per municipality
### =========================================================================

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

#add back in BFS and Kantons numbers
pop_percentages$BFS_NUM <- raw_mun_popdata$BFS_NUM
pop_percentages$KANTONSNUM <- raw_mun_popdata$KANTONSNUM

#check that muni pop percentages equate to 100%
pop_percentages_validation <- do.call(cbind, lapply(date_cols, function(x){
pop_sum <- pop_percentages %>%
  group_by(KANTONSNUM) %>%
  summarise(across(c(paste0("Perc_", x)), sum))
pop_sum$KANTONSNUM <- NULL
return(pop_sum)
}))

### =========================================================================
### F- Calculate % urban area per municipality
### =========================================================================

#Load one of the historic LULC maps to use as a dummy layer
#Variable name 
current_LULC <- raster("Data/Historic_LULC/LULC_2018_agg.gri")

#subset to just urban cell
Urban_rast <- current_LULC == 10

#Zonal stats to get urban area per kanton
Kanton_urban_areas <- raster::extract(Urban_rast, Kanton_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Kanton ID 
Kanton_urban_areas$Kanton_num <- Kanton_shp$KANTONSNUM

#combine areas for cantons with multiple polygons
Kanton_urban_areas <- Kanton_urban_areas %>%
  group_by(Kanton_num) %>%
  summarise(across(c(layer), sum))

#load the municipality shape file
Muni_shp <- shapefile("PopulationModel_Valpar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

#filter out non-swiss municipalities
Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ] 

#Zonal stats to get number of Urban cells per Municipality polygon
#sum is used as a function because urban cells = 1 all others = 0
Muni_urban_areas <- raster::extract(Urban_rast, Muni_shp, fun=sum, na.rm=TRUE, df=TRUE)

#append Kanton and Municipality IDs
Muni_urban_areas$Kanton_num <- Muni_shp@data[["KANTONSNUM"]]
Muni_urban_areas$Muni_num <- Muni_shp$BFS_NUMMER
Muni_urban_areas$Perc_kanton_urb <- 0

#loop over kanton numbers and calculate municipality urban areas as a % of canton urban area
for(i in Kanton_urban_areas$Kanton_num){

#vector kanton urban area
Kan_urban_area <- as.numeric(Kanton_urban_areas[Kanton_urban_areas$Kanton_num == i, "layer"])  

#subset municipalities to this canton number 
munis_indices <- which(Muni_urban_areas$Kanton_num == i)

#loop over municipalities in the Kanton and calculate their urban areas as a % of the Kanton's total  
for(muni in munis_indices){
Muni_urban_areas$Perc_kanton_urb[muni] <- (Muni_urban_areas[muni, "layer"]/Kan_urban_area)*100
  } #close inner loop 
} #close outer loop

### =========================================================================
### G- Model relationship between cantonal % population and % urban area
### =========================================================================

#subset pop percentages to 2018
Muni_percs <- pop_percentages[, c("BFS_NUM", "KANTONSNUM", "Perc_2018")]
colnames(Muni_percs) <-  c("BFS_NUM", "KANTONSNUM", "Perc_pop")

#combine with % urban values
Muni_percs$perc_urban <- sapply(Muni_percs$BFS_NUM, function(x){
sum(Muni_urban_areas[Muni_urban_areas$Muni_num == x, "Perc_kanton_urb"])  
})

#loop over kantons and model relationship
Kanton_models <- lapply(unique(Muni_percs$KANTONSNUM), function(canton_num){

#subset to data for this canton
kanton_data <- Muni_percs[Muni_percs$KANTONSNUM == canton_num,]

#produce GLM
Kanton_model <- glm(data = kanton_data, formula = Perc_pop ~ perc_urban)  
  
return(Kanton_model)
})
names(Kanton_models) <- unique(Muni_percs$KANTONSNUM)

#save models
saveRDS(Kanton_models, "Data/Preds/Tools/Dynamic_pop_models.rds")
