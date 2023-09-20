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

#Get details of data collections (each dataset is a collection)
Collections <- GET("http://data.geo.admin.ch/api/stac/v0.9/collections")

#Convert JSON output to interpretable data frame  
Collection_details <-  fromJSON(rawToChar(Collections$content))[["collections"]][,2:4]                
                
#Browse dataset titles/descriptions and subset to those that you want
#e.g. use name based subsetting of datasets:
Desired_titles <- c("2000-Watt Sites")
Dataset_ids_for_dl <- Collection_details[Collection_details$title  %in% Desired_titles, "id"]
names(Dataset_ids_for_dl) <- Dataset_ids_for_dl

#Each dataset (collection) contains multiple features which can be the dataset
#in different file types or split into spatial tiles if it is large
#Hence it is useful to query what type of features exists for the datasets of interest

#loop over dataset ids, querying the API service
#and return the file types available for the desired datasets in a list
#with entries named using dataset IDs
Feature_data_types <- lapply(Dataset_ids_for_dl, function(Dataset_ID){

#split ID on 2nd period to extract feature ID
Feature_ID <- str_split(Dataset_ID, "\\.")[[1]][[3]]

#Make API call on collection and feature ID returning assets
Content <- fromJSON(rawToChar(GET(paste0("http://data.geo.admin.ch/api/stac/v0.9/collections/", Dataset_ID, "/items"))[["content"]]))
Assets <- Content[["features"]][["assets"]]
Data_types <- unlist(lapply(Assets, function(feature){
Data_type <- feature[grepl("type", names(feature))]  
}))

#reformat and subset to unique
Data_types <- sapply(Data_types, function(x) str_remove(str_remove(x, "application/"), "x."))
unname(Data_types)
return(Data_types)
})

#User should review list and subset to only the data types desired for each dataset
view(Feature_data_types)
#E.g. subset to only the zipped csv version 
#(entry for each list item must use exact nomenclature from original list)
Feature_data_types[[1]] <- "csv+zip"

#loop over list of datasets titles with file types, querying the API
#and saving the datasets
for(i in 1:length(Feature_data_types)){
Dataset_ID <- names(Feature_data_types)[[i]]  

#split ID on 2nd period to extract feature ID
Feature_ID <- str_split(Dataset_ID, "\\.")[[1]][[3]]

#Make API call on collection and feature ID returning assets
Content <- fromJSON(rawToChar(GET(paste0("http://data.geo.admin.ch/api/stac/v0.9/collections/", Dataset_ID, "/items"))[["content"]]))
Assets <- Content[["features"]][["assets"]]

#extract 'href' objects (html paths to download geodata) from Assets
DL_paths <- lapply(Assets, function(x){
  path <- if(grepl(paste0(gsub("[^[:alnum:]]", "\\.", Feature_data_types[[1]]), collapse = "|") , gsub("[^[:alnum:]]", "\\.", x[["type"]])) == TRUE ){
  x[["href"]]} 
  })
DL_paths <- DL_paths[!sapply(DL_paths,is.null)] #remove NULL items
browser()
#create directory for saving the data into
Save_path <- paste0(Save_dir, "/", Feature_ID)
dir.create(Save_path)

#loop over download paths saving files
sapply(DL_paths, function(x){
  
#extract a file name from the html path
File_name <- sapply(strsplit(x, "/"), tail, 1)

#Dowload file/folder
download.file(x, paste0(Save_path, "/", File_name))  
}) #close loop over features

}#close loop over datasets

### =========================================================================
### A- Example running a query on the FSO Stat-tab API service
### =========================================================================

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

#convert Json-stat object to dataframe
Statent_data <- rjstat::fromJSONstat(Statent_return, naming = "label", use_factors = FALSE, silent = FALSE)[[1]]
 
