#############################################################################
## Simulated_map_finalisation: Finalise Dinamica simulated LULC maps 
## adding in water bodies
##
## Date: 25-10-2022
## Author: Ben Black
#############################################################################

#remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")

#vector other required packages
packs<-c("data.table", "raster", "tidyverse", "SDMTools", "doParallel", 
"sf", "tiff", "igraph", "readr", "foreach", "testthat", 
"sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", 
"rgeos", "bfsMaps", "rjstat", "future.apply", "future", "stringr", 
"stringi", "readxl", "rlist", "rstatix", "openxlsx", "pxR", "zen4R", 
"rvest", "viridis", "sp", "jsonlite", "httr", "xlsx", "callr",
"gdata", "landscapemetrics", "randomForest", "RRF", "future.callr", 
"ghibli", "ggpattern", "butcher", "ROCR", "ecospat", "caret", "Dinamica", 
"gridExtra", "extrafont", "ggpubr", "ggstatsplot","PMCMRplus", "reshape2",
"ggsignif", "ggthemes", "ggside", "gridtext", "grid", "slackr", "rstudioapi",
"landscapemetrics", "plotly", "networkD3", "ggalluvial", "ggthemes", "patchwork",
"extrafont", "tmap", "leaflet", "leaflet.extras", "leaflet.extras2",
"rcartocolor", "htmlwidgets", "leaflet.opacity", "leaflet.providers",
"leafem", "mapview", "webshot2", "magick", "png")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

list2env(readRDS("Tools/Model_tool_vars.rds"), .GlobalEnv)

ProjCH <- "+proj=somerc +init=epsg:2056"

Final_map_dir <- "Results/Finalised_LULC_maps"
dir.create(Final_map_dir)
Web_table_dir <- "C:/Users/bblack/polybox/LULC_validation_data/LULC_tables/Switzerland"
Web_maps_dir <- "C:/Users/bblack/polybox/LULC_validation_data/LULC_maps/Switzerland"

#load aggregation scheme
Aggregation_scheme <- read_excel(LULC_aggregation_path)

### =========================================================================
### B- Finalising simulated LULC maps
### =========================================================================

#Load in most recent non-aggregated LULC raster
Obs_LULC_paths <- list.files("Data/Historic_LULC/NOAS04_LULC/rasterized", full.names = TRUE, pattern = ".tif")
names(Obs_LULC_paths) <- str_remove_all(sapply(list.files("Data/Historic_LULC/NOAS04_LULC/rasterized", full.names = FALSE, pattern = ".tif"), function(x){str_split(x,"_")[[1]][2]}), ".tif")
names(Obs_LULC_paths)

#extract numerics
Max_LULC_year <- max(unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(Obs_LULC_paths)))))

#subset path to final year
Non_agg_LULC <- rast(Obs_LULC_paths[grepl(Max_LULC_year, names(Obs_LULC_paths))])

mask_values <- unlist(Aggregation_scheme[Aggregation_scheme$NOAS04_class_ENG %in% c("Lakes", "Rivers"), "NOAS04_ID"])
names(mask_values) <- c(20,21)

#Load simulation control table
Simulation_control <- read.csv(Sim_control_path)
Simulation_control <- Simulation_control[Simulation_control$Completed.string == "Y",]

#get unique values of Simulation ID
Sim_IDs <- unique(Simulation_control$Simulation_ID.string)

#get scenario names
Scenarios <- unique(Simulation_control$Scenario_ID.string)

#get earliest scenario start date
Scenario_start <- min(Simulation_control$Scenario_start.real)
Scenario_end <- max(Simulation_control$Scenario_end.real)

#use ID to identify directory for simulated LULC files
Scenario_lulc_paths <- lapply(Scenarios, function(x) {
  All_sim_LULC_paths <- list.files("Results/Dinamica_simulated_LULC", recursive = TRUE,
           pattern = Sim_IDs,
           full.names = TRUE)
  Scenario_paths <- All_sim_LULC_paths[grepl(x,All_sim_LULC_paths)]
  Scenario_paths <- Scenario_paths[grepl("\\.tif$", Scenario_paths)]
})
names(Scenario_lulc_paths) <- Scenarios

#loop over simulated LULC maps grouped by Scenario masking the areas that are
#water bodies (Lakes and rivers) in the most recent LULC maps for visual clarity
lapply(names(Scenario_lulc_paths), function(Scenario_ID){
  
  # glacier areas to be accurate we need to make sure that the initial LULC map has the correct
  #number of glacier cells according to glacial modelling
  
  #convert raster to dataframe
  LULC_dat <- terra::as.data.frame(Non_agg_LULC, na.rm = FALSE) 

  #add ID column to dataset
  LULC_dat$ID <- seq.int(nrow(Non_agg_LULC))

  #Get XY coordinates of cells
  xy_coordinates <- crds(Non_agg_LULC, na.rm=FALSE) 

  #cbind XY coordinates to dataframe and seperate rows where all values = NA
  LULC_dat <- cbind(LULC_dat, xy_coordinates)
      
  #load scenario specific glacier index
  Glacier_index <- readRDS(file = list.files("Data/Glacial_change/Scenario_indices",
                                                 full.names = TRUE,
                                                 pattern = Scenario_ID))[,c("ID_loc", paste(Scenario_start))]

  #seperate vector of cell IDs for glacier and non-glacer cells
  Non_glacier_IDs <- Glacier_index[Glacier_index[[paste(Scenario_start)]]==0, "ID_loc"]
  Glacier_IDs <- Glacier_index[Glacier_index[[paste(Scenario_start)]]==1, "ID_loc"] 
  
  #vector the pixel values of Glacier from the non_agg lulc layer
  Glacier_val <- unlist(Aggregation_scheme[Aggregation_scheme$NOAS04_class_ENG == "Glaciers, perpetual snow", "NOAS04_ID"]) 
  
  #add to mask values
  mask_values <- append(mask_values, Glacier_val)
  names(mask_values)[length(mask_values)] <- unlist(Aggregation_scheme[Aggregation_scheme$Class_abbreviation == "Glacier", "Aggregated_ID"])
  
  #replace the 1's and 0's with the correct LULC
  LULC_dat[LULC_dat$ID %in% Non_glacier_IDs, "NOAS04_2018"] <- unlist(Aggregation_scheme[Aggregation_scheme$NOAS04_class_ENG == "Rocks", "NOAS04_ID"])
  LULC_dat[LULC_dat$ID %in% Glacier_IDs, "NOAS04_2018"] <- Glacier_val
  
  #2nd step ensure that other glacial cells that do not match the glacier index
  #are also changed to static so that the transition rates calculate the
  #correct number of cell changes
  LULC_dat[which(LULC_dat$NOAS04_2018 == Glacier_val & !(LULC_dat$ID %in% Glacier_IDs)), "NOAS04_2018"] <- unlist(Aggregation_scheme[Aggregation_scheme$NOAS04_class_ENG == "Rocks", "NOAS04_ID"])   
 
  #convert back to raster
  Non_agg_LULC_update <- rast(LULC_dat[,c("x", "y", "NOAS04_2018")], crs = ProjCH) 
  
  #inner loop over seperate time point layers for scenario 
  lapply(Scenario_lulc_paths[[Scenario_ID]], function(x){
 
    #Mask the sim LULC raster by the lake classes in the non_agg lulc_raster
    Sim_rast <- rast(x)
    crs(Sim_rast) <- ProjCH
    
    #loop over mask values
    for(i in 1:length(mask_values)){
    Sim_rast <- terra::mask(x = Sim_rast,
                              mask = Non_agg_LULC_update,
                              maskvalues = mask_values[i],
                              updatevalue = as.numeric(names(mask_values)[i]))
  
      } #close for loop over mask values
    
    rast_tbl <- freq(Sim_rast)
    rast_tbl$layer <- NULL
    rast_tbl$class_name <- c(unlist(sapply(rast_tbl$value, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class"])),simplify = TRUE)), "Lake", "River")
    rast_tbl[rast_tbl$value ==20, "class_name"] <- "Lake"
    rast_tbl[rast_tbl$value ==21, "class_name"] <- "River"
    
    #save updated raster and table of frequency values
    write.csv(rast_tbl, paste0(Web_table_dir, "/", str_replace(basename(x), ".tif", ".csv")), row.names = FALSE)
    writeRaster(Sim_rast, paste0(Final_map_dir, "/", basename(x)), overwrite=TRUE)
    writeRaster(Sim_rast, paste0(Web_maps_dir, "/", basename(x)), overwrite=TRUE)
    }) # close inner loop over time point layers

  }) #close outer loop over scenarios

### =========================================================================
### C- Regionalising LULC maps for expert validation
### =========================================================================

# #Urban focus area: Canton Zurich
# Cantons <- vect("Data/Preds/Raw/CH_geoms/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")
# crs(Cantons) <- ProjCH
# 
# #subset to Zurich
# ZH_shp <- Cantons[Cantons$NAME == "Zürich", ]
# 
# #Rural focus area
# Muni_type_rast <- rast("Data/Spat_prob_perturb_layers/Municipality_typology/Muni_type_raster.grd")
# crs(Muni_type_rast) <- ProjCH
# 
# #seperate raster attribute table
# Muni_rat <- levels(Muni_type_rast)[[1]]
# 
# #get values for rural municipalities
# Rural_muni_values <- Muni_rat[Muni_rat$type %in% c(
# "Rural_agricultural_municipality_in_a_central_location"
# ,"Rural_industrial_municipality_in_a_central_location",
# "Tertiary_rural_municipality_in_a_central_location"
# ),"ID"]
# 
# #subset raster to only these municipalities
# Rural_rast <- ifel(Muni_type_rast %in% Rural_muni_values, 1, NA)
# 
# #Mountainous focus area
# Mount_muni_values <- c(334)
# Mount_rast <- ifel(Muni_type_rast %in% Mount_muni_values, 1, NA)
# plot(Mount_rast)
# 
# #create a list of layers to mask with named for each region
# Regions <- list(Urban = ZH_shp,
#                 Rural = Rural_rast,
#                 Mountain = Mount_rast)
# 
# #list files from Final dir
# Validation_maps_dir <- "C:/Users/bblack/polybox/LULC_validation_data/LULC_maps"
# Validation_tbl_dir <- "C:/Users/bblack/polybox/LULC_validation_data/LULC_tables"
# 
# #create directories for each region in both base dirs
# lapply(names(Regions), function(x){
#   dir.create(paste0(Validation_maps_dir, "/", x), recursive = TRUE)
#   dir.create(paste0(Validation_tbl_dir, "/", x), recursive = TRUE)
# })
#   
# #Get Swiss wide LULC map paths
# LULC_paths <- list.files(paste0(Validation_maps_dir, "/Switzerland"), full.names = TRUE)
# 
# #loop over LULC maps subsetting to regions and saving maps and tables
# lapply(LULC_paths, function(x){
#   
#   #load LULC layer
#   LULC <- rast(x)
#   
#   #inner loop over regions
#   lapply(1:length(Regions), function(Reg_num){
#     
#     #mask LULC by region
#     LULC_region <- terra::mask(LULC, Regions[[Reg_num]])
#     # plot(LULC_region)
#     # browser()
#     #produce frequency table from masked region
#     rast_tbl <- freq(LULC_region)
#     rast_tbl$layer <- NULL
#     rast_tbl$class_name <- c(unlist(sapply(rast_tbl$value, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class"])),simplify = TRUE)), "Lake", "River")
#     rast_tbl[rast_tbl$value ==20, "class_name"] <- "Lake"
#     rast_tbl[rast_tbl$value ==21, "class_name"] <- "River"
#     
#     #save updated raster and table of frequency values
#     write.csv(rast_tbl, paste0(Validation_tbl_dir, "/", names(Regions)[[Reg_num]], "/", paste0(tools::file_path_sans_ext(basename(x)), "_",names(Regions)[[Reg_num]], ".csv")), row.names = FALSE)
#     writeRaster(LULC_region, paste0(Validation_maps_dir, "/", names(Regions)[[Reg_num]], "/", paste0(tools::file_path_sans_ext(basename(x)), "_",names(Regions)[[Reg_num]], ".tif")), overwrite=TRUE)
#     
#   }) #close loop over regions
#   
# })

### =========================================================================
### D- Leaflet time-slider map of scenario results
### =========================================================================

#load an exempler layer to produce a raster attribute table from
exp_rast <- raster(list.files(Final_map_dir, recursive = TRUE,
           pattern = Sim_IDs,
           full.names = TRUE)[1])

#create a raster attribute table
LULC_rat <- data.frame(
  ID = sort(unique(values(exp_rast)))
)
LULC_rat$lulc_name <- c(unlist(sapply(LULC_rat$ID, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class"])),simplify = TRUE)), "Lake", "River")

#use Scenario IDs to identify Final LULC files, subset to only the start and 
#end years, add raster attribute table and load as stacks
Final_lulc_maps <- lapply(Scenarios, function(x) {
  All_sim_LULC_paths <- list.files(Final_map_dir, recursive = TRUE,
           pattern = Sim_IDs,
           full.names = TRUE)
  Scenario_paths <- All_sim_LULC_paths[grepl(x,All_sim_LULC_paths)]
  Start_end_paths <- Scenario_paths[grepl(paste(c(Scenario_start, Scenario_end), collapse='|'), Scenario_paths)]
  Start_end_layers <- stack(lapply(Start_end_paths, function(y){
    lyr <- raster(y)
    raster_with_att <- ratify(lyr)
    levels(raster_with_att) <- LULC_rat
    return(raster_with_att)
  }))
  names(Start_end_layers@layers) <- c(paste(Scenario_start), paste(Scenario_end))
  names(Start_end_layers) <- c(paste(Scenario_start), paste(Scenario_end))
  return(Start_end_layers)
})
names(Final_lulc_maps) <- Scenarios

#colour palette
pal <- colorFactor(c(
"#BB0011", #Urban
"#DDDDDD", #static
"#668822", #Open forest
"#117733", #closed forest
"#44AA88", #Shrubland
"#FFDD44", #Intensive agriculture
"#558877", #Alpine pastures
"#AADDCC", #Grassland
"#DDCC66", #Permanet crops
"#E8ECFB", #Glacier
"#5566AA", #Rivers
"#5566AA"), LULC_rat$ID, na.color = "transparent")

Scenario_maps <- leaflet() |> 
  addMapPane("right", zIndex = 0) |> 
  addMapPane("left",  zIndex = 0) |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "base", layerId = "baseid1", options = pathOptions(pane = "right")) |> 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "base", layerId = "baseid2", options = pathOptions(pane = "left")) |> 
  addRasterImage(x = Final_lulc_maps[["BAU"]][[paste0("X", Scenario_start)]], colors = pal, options = leafletOptions(pane = "left"), group = "Business as Usual", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["BAU"]][[paste0("X", Scenario_end)]], colors = pal, options = leafletOptions(pane = "right"), group = "Business as Usual", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["EI_NAT"]][[paste0("X", Scenario_start)]], colors = pal, options = leafletOptions(pane = "left"), group = "EI for Nature", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["EI_NAT"]][[paste0("X", Scenario_end)]], colors = pal, options = leafletOptions(pane = "right"), group = "EI for Nature", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["EI_SOC"]][[paste0("X", Scenario_start)]], colors = pal, options = leafletOptions(pane = "left"), group = "EI for Society", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["EI_SOC"]][[paste0("X", Scenario_end)]], colors = pal, options = leafletOptions(pane = "right"), group = "EI for Society", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["GR_EX"]][[paste0("X", Scenario_start)]], colors = pal, options = leafletOptions(pane = "left"), group = "Growth and Extinction", project = FALSE) |>
  addRasterImage(x = Final_lulc_maps[["GR_EX"]][[paste0("X", Scenario_end)]], colors = pal, options = leafletOptions(pane = "right"), group = "Growth and Extinction", project = FALSE) |>
  leaflet.extras::addResetMapButton() |>
  #add an opacity slider but only works for one half of the map
  #addOpacitySlider(layerId = "raster1") |> 
  #addOpacitySlider(layerId = "raster2") |> 
  # Add an inset minimap
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.WorldTopoMap,
    toggleDisplay = TRUE,
    minimized = FALSE) |> 
  addLayersControl(overlayGroups = c("Business as Usual", "EI for Nature", "EI for Society", "Growth and Extinction")) |>  
  addSidebyside(layerId = "sidecontrols",
                rightId = "baseid1",
                leftId  = "baseid2") |> 
   addControl("2020", position = "bottomleft")|> 
   addControl("2060", position = "bottomright")|>
    addLegend("bottomright", pal = pal,
            values = LULC_rat$ID,
            title = "Legend",
            labFormat = labelFormat(
            transform = function(y) {
              LULC_rat[LULC_rat$ID == y, "lulc_name"]
            }),
            #labels = rast_labels,
            opacity = 1)

htmlwidgets::saveWidget(Scenario_maps, file="Results/Finalised_LULC_maps/Scenario_maps_widget.html")

 
