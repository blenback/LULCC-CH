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
wpath <- "E:/LULCC_CH"
setwd(wpath)

# Install packages if they are not already installed
packs<-c("data.table","stringi","stringr","plyr","readxl","rlist", "tidyverse",
         "rstatix", "Dinamica", "raster", "openxlsx", "pxR")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Load in the grid to use for rasterization
Ref_grid <- raster("Data/Ref_grid.gri")

### =========================================================================
### B- prepare municipality population data incorporating mutations
### =========================================================================

# #read in PX data from html and convert to DF
# px_data <- as.data.frame(read.px("https://dam-api.bfs.admin.ch/hub/api/dam/assets/23164063/master"))
# 
# #subset to desired rows based on conditions:
# #Total population on 1st of January;
# #Total populations (Swiss and foreigners)                      
# #Total population (Men and Women)
# raw_mun_popdata <- px_data[px_data$Demografische.Komponente == "Bestand am 31. Dezember" &
#                      px_data$Staatsangeh?rigkeit..Kategorie. == "Staatsangeh?rigkeit (Kategorie) - Total" &
#                      px_data$Geschlecht == "Geschlecht - Total", ]
# 
# #Identify municipalities records by matching on the numeric contained in their name
# raw_mun_popdata <- raw_mun_popdata[grepl(".*?([0-9]+).*", raw_mun_popdata$Kanton.......Bezirk........Gemeinde.........), c(4:6)]
# names(raw_mun_popdata) <- c("Name_Municipality", "Year", "Population")
# raw_mun_popdata <- raw_mun_popdata %>% pivot_wider(names_from = "Year",
#                                                 values_from = "Population")
# #Remove the periods in the name column
# raw_mun_popdata$Name_Municipality <- gsub("[......]","",as.character(raw_mun_popdata$Name_Municipality))
# 
# #Seperate BFS number from name
# raw_mun_popdata$BFS_NUM <- as.numeric(gsub(".*?([0-9]+).*", "\\1", raw_mun_popdata$Name_Municipality)) 
#   
# #Remove BFS number from name
# raw_mun_popdata$Name_Municipality <- gsub("[[:digit:]]", "", raw_mun_popdata$Name_Municipality)
# 
# # subset to only municipalities existing in 2021
# raw_mun_popdata <- raw_mun_popdata[raw_mun_popdata$`2021`> 0,]
# 
# # import municipality shape file
# Muni_shp <- shapefile("PopulationModel_ValPar/Data/01_Raw_Data/swissboundaries/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
# 
# #filter out non-swiss municipalities
# Muni_shp <- Muni_shp[Muni_shp@data$ICC == "CH" & Muni_shp@data$OBJEKTART == "Gemeindegebiet", ]
# 
# # import data of municipality mutations
# #TO DO: what were the conditions we applie don the BFS website to get this data? 
# 
# muni_mutations <- data.frame()
# 
# download.file(url = "https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True#:~:text=Suche%20anpassen-,Export,-(Excel%20XLSX)", "test_dl.xlsx", mode="wb")
# 
# 
# zip <- ("https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True#:~:text=Suche%20anpassen-,Export,-(Excel%20XLSX)")
# 
# muni_mutations <- unzip(zip)
# 
# muni_mutations <- unzip("https://www.agvchapp.bfs.admin.ch/de/mutated-communes/results?EntriesFrom=01.01.1981&EntriesTo=01.05.2022&NameChange=True#:~:text=Suche%20anpassen-,Export,-(Excel%20XLSX)")
# 
# 
# muni_mutations <- readxl::read_excel("PopulationModel_Valpar/Data/01_Raw_Data/MutationGemeinde/Mutierte_Gemeinden_namechange.xlsx", 
#                       range = "A2:J55")
# 
# ## rename columns
# colnames(muni_mutations) <- c("Mutation_Number", "Pre_canton_ID", 
#                                     "Pre_District_num", "Pre_BFS_num", 
#                                     "Pre_muni_name", "Post_canton_ID",
#                                     "Post_district_num", "Post_BFS_num", 
#                                     "Post_muni_name", "Change_date")
# 
# #identify which municipalities have mutations associated with them
# mutation_index <- match(raw_mun_popdata$BFS_NUM, muni_mutations$Pre_BFS_num) # which have changed
# 
# #change municpality BFS number in population table according to the mutation
# #for each row in the pop df if there is an NA in the mutation index do not replace the BFS number 
# #If there is not an NA then replace with the new BFS number of the mutation table. 
# 
# for (i in 1:nrow(raw_mun_popdata)){
#   if (!is.na(mutation_index[i])){ 
#     raw_mun_popdata$BFS_NUM[[i]] <- muni_mutations[[mutation_index[[i]], "Post_BFS_num"]]}
# }
# 
# #If after introducing the mutations we have multiple rows with the same BFS numbers
# #then we need to combine their populations values as these indicate municipalities merging
# 
# if(length(unique(raw_mun_popdata$BFS_NUM)) != nrow(raw_mun_popdata)){
# #get the indices of columns that represent the years
# Time_points <- na.omit(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames(raw_mun_popdata))))
# 
# #create a empty df for results
# Muni_pop_final <- as.data.frame(matrix(ncol= length(Time_points), 
#                          nrow = length(unique(raw_mun_popdata$BFS_NUM))))
# colnames(Muni_pop_final) <- Time_points
# 
# #Add column for BFS number
# Muni_pop_final$BFS_NUM <- sort(unique(raw_mun_popdata$BFS_NUM))
# 
# #loop over date cols and rows summing values where BFS number is non-unique
# for (j in Time_points){
#   for (i in 1:length(unique(raw_mun_popdata$BFS_NUM))){
#     Muni_pop_final[i, paste(j)] <- sum(raw_mun_popdata[raw_mun_popdata$BFS_NUM == Muni_pop_final[i, "BFS_NUM"], paste(j)])
#   }
# }
# #replace old data with revised data
# raw_mun_popdata <- Muni_pop_final
# } #close if statement
# 
# #Add canton number
# raw_mun_popdata$KANTONSNUM <- sapply(raw_mun_popdata$BFS_NUM, function(x){
# unique(Muni_shp@data[Muni_shp@data$BFS_NUMMER == x, "KANTONSNUM"])
# })
# 
# ### =========================================================================
# ### C- create historic municipality population rasters
# ### =========================================================================
# 
# #vector the years population data required from the LULC data point
# pop_years <- gsub(".*?([0-9]+).*", "\\1", list.files("E:/LULCC_CH/Data/Historic_LULC", full.names = FALSE, pattern = ".gri"))
# 
# #seperate pop data for LULc years
# pop_in_LULC_years <- raw_mun_popdata[,c("BFS_NUM", pop_years)]
# 
# #link with spatial municipality data, rasterize and save
# for(i in pop_years){
# 
# #loop over the BFS numbers of the polygons and match to population values
# Muni_shp@data[paste0("Pop_", i)] <- as.numeric(sapply(Muni_shp@data$BFS_NUMMER, function(Muni_num){
#   pop_value <- as.numeric(pop_in_LULC_years[pop_in_LULC_years$BFS_NUM == Muni_num, paste(i)])  
#   }, simplify = TRUE))     
# 
# #rasterize
# pop_rast <- rasterize(Muni_shp, Ref_grid, field = paste0("Pop_", i))
# 
# #file_path
# save_path <- paste0("Data/Preds/Raw/Socio_economic/Population/Prepared/", "Pop_", i, ".tif")
# 
# #save
# raster::writeRaster(pop_rast, save_path)
#   
# }# close loop over LULC years
# 
# # TO DO: add rasters to predictor table

### =========================================================================
### D- Prepare historic cantonal population data
### =========================================================================

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

#add back in BFS and Cantons numbers
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
  group_by(Canton_num) %>%
  summarise(across(c(layer), sum))

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

### =========================================================================
### G- Model relationship between cantonal % population and % urban area
### =========================================================================

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

### =========================================================================
### H- Prepare Cantonal projections of population development 
### =========================================================================

raw_data_path <- "Data/Preds/Raw/Socio_economic/Population/raw_pop_projections.xlsx"

#Download the required file 
download.file(url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/12107013/master", destfile = raw_data_path, mode="wb")

#Vector existing sheet names
Sheet_names <- getSheetNames(raw_data_path)

#name the sheet names with the new names
names(Sheet_names) <- c("Ref", "High", "Low")

#Create a workbook to add the ckleaned data too
pop_proj_wb <- createWorkbook()

#The population projection table does not contain the canton numbers and because
#the Canton names are in German they cannot be matched with the shapefile
#to get the numbers (e.g Wallis - Valais etc.) instead load a different FSO
#table to get the numbers 
Canton_lookup <- read.xlsx("https://www.atlas.bfs.admin.ch/core/projects/13.40/xshared/xlsx/134_131.xlsx", rows = c(4:31), cols = c(2,3))
Canton_lookup$Canton_num <- seq(1:nrow(Canton_lookup))

#loop over time steps adding sheets and adding the predictors to them
for (i in 1:length(Sheet_names)){

org_sheet_name <- Sheet_names[i]
new_sheet_name <- names(Sheet_names)[i]
   
#load correct sheet of raw data
tempdf <- read.xlsx(raw_data_path, sheet = org_sheet_name, startRow = 2)
colnames(tempdf)[1] = "Canton_name" #rename first column
tempdf <- tempdf[tempdf$Canton_name != "Schweiz",] # remove the total for switzerland
tempdf <- tempdf[complete.cases(tempdf),] #remove incomplete or empty rows
tempdf$Canton_num <- sapply(tempdf$Canton_name, function(x) {
Canton_lookup[grepl(x, Canton_lookup$X2), "Canton_num"]
})
#replace values of non-matching names
tempdf[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num), "Canton_num"] <- Canton_lookup$Canton_num[setdiff(Canton_lookup$Canton_num, tempdf$Canton_num)]
rownames(tempdf) <- 1:nrow(tempdf) #correct row names

#create sheet in workbook, the try() is necessary in case sheets already exist  
try(addWorksheet(pop_proj_wb, sheetName = new_sheet_name))  

#write the data to the sheet
writeData(pop_proj_wb, sheet = new_sheet_name, x = tempdf)
}

#save workbook
openxlsx::saveWorkbook(pop_proj_wb, "Data/Preds/Tools/Population_projections.xlsx", overwrite = TRUE)
