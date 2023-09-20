#############################################################################
## Use_geoadmin_api: Download geodata from Geoadmin.ch through the STAC API service
##
## Date: 18-11-2021
## Author: Ben Black
#############################################################################

# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#Vector packages for loading
packs<-c("httr", "jsonlite", "stringi", "stringr")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))



### =========================================================================
### A- Retrieving datasets using Geoadmin.ch API service
### =========================================================================

#specify directory for saving downloaded data
Save_dir <- "E:/LULCC_CH/Data/test_download"
dir.create(Save_dir, recursive = TRUE)

#Get details of data collection
Collections <- GET("http://data.geo.admin.ch/api/stac/v0.9/collections")

#Convert JSON output to interpretable data frame  
Collection_details <-  fromJSON(rawToChar(Collections$content))[["collections"]][,2:4]                
                
#Read dataset titles/decriptions and subset to those that you want
#e.g. use name based subsetting of datasets:
Desired_titles <- c("2000-Watt Sites")
Dataset_ids_for_dl <- Collection_details[Collection_details$title  %in% Desired_titles, "id"]
names(Dataset_ids_for_dl) <- Dataset_ids_for_dl

#loop over dataset ids, querying the API service
#and return the file types available for the desired datasets in a list
#with entries named using dataset IDs
Feature_data_types <- lapply(Dataset_ids_for_dl, function(Dataset_ID){

#split ID on 2nd period to extract feature ID
Feature_ID <- str_split(Dataset_ID, "\\.")[[1]][[3]]

#Make API call on collection and feature ID returning assets
Assets <- fromJSON(rawToChar(GET(paste0("http://data.geo.admin.ch/api/stac/v0.9/collections/", Dataset_ID, "/items/", Feature_ID))[["content"]]))[["assets"]]

Data_types <- unlist(lapply(Assets, function(feature){
Data_type <- feature[grepl("type", names(feature))]  
}))

#reformat and subset to unqiue
Data_types <- sapply(Data_types, function(x) str_remove(str_remove(x, "application/"), "x."))
unname(Data_types)
return(Data_types)
})
Feature_data_types[[1]] <- "csv+zip"

#User should review list and subset to only the data types desired for each dtaaset
print(Feature_data_types)

#loop over list of datasets with types, querying the API and saving datasets
for(i in 1:length(Feature_data_types)){
Dataset_ID <- names(Feature_data_types)[[i]]  

#split ID on 2nd period to extract feature ID
Feature_ID <- str_split(Dataset_ID, "\\.")[[1]][[3]]

#Make API call on collection and feature ID returning assets
Assets <- fromJSON(rawToChar(GET(paste0("http://data.geo.admin.ch/api/stac/v0.9/collections/", Dataset_ID, "/items/", Feature_ID))[["content"]]))[["assets"]]

#extract 'href' objects (html paths to download geodata) from Assets
DL_paths <- lapply(Assets, function(x){
  path <- if(grepl(paste0(gsub("[^[:alnum:]]", "\\.", Feature_data_types[[1]]), collapse = "|") , gsub("[^[:alnum:]]", "\\.", x[["type"]])) == TRUE ){
  x[["href"]]} 
  })
DL_paths <- DL_paths[!sapply(DL_paths,is.null)]

#create a directory for saving the data into
Save_path <- paste0(Save_dir, "/", Feature_ID)
dir.create(Save_path)

#loop over paths saving files
sapply(DL_paths, function(x){
  
#extract a folder name form the html path
File_name <- sapply(strsplit(x, "/"), tail, 1)

#Dowload file/folder
download.file(x, paste0(Save_path, "/", File_name))  
}) #close loop over features

}#close loop over datasets



 
