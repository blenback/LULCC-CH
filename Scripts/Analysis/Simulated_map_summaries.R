#############################################################################
## Simulated map summaries: 
##
## Date: 25-10-2022
## Author: Ben Black
#############################################################################

### =========================================================================
### - Preparation
### =========================================================================

# Install and load packages

#install dev versions of certain packages
remotes::install_github("rstudio/chromote")
remotes::install_github("r-spatial/mapview")
devtools::install_github("ssp3nc3r/ggSankeyGrad", ref = "master")

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
"leafem", "mapview", "webshot2", "magick", "png", "ggpubr", "ggSankeyGrad")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Import fonts using extrafont (only needs to be done once)
#font_import()

#send model tool vars to global environment
list2env(readRDS("Tools/Model_tool_vars.rds"), .GlobalEnv)

ProjCH <- "+proj=somerc +init=epsg:2056"

#Dir of Finalized LULC maps
Final_map_dir <- "Results/Finalised_LULC_maps"

#Dir for saving output
PA_maps_dir <- "Results/PA_maps_temp"
dir.create(PA_maps_dir)

#Dir for analysis results
Analysis_dir <- "Results/Map_analysis"
dir.create(Analysis_dir)

#Dir for sankey tables and diagrams
Sankey_dir <- "Results/Sankey_tables_diagrams"
dir.create(Sankey_dir)

#Load simulation control table
Simulation_control <- read.csv(Sim_control_path)
#Simulation_control <- Simulation_control[Simulation_control$Completed.string == "Y",]

#get unique values of Simulation ID
Sim_IDs <- unique(Simulation_control$Simulation_ID.string)

#get scenario names
Scenario_names <- unique(Simulation_control$Scenario_ID.string)
names(Scenario_names) <- Scenario_names

#get earliest scenario start date
Scenario_start <- min(Simulation_control$Scenario_start.real)
Scenario_end <- max(Simulation_control$Scenario_end.real)

#get time steps
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

#list of final LULC rasters 
LULC_paths <- list.files(Final_map_dir, full.names = TRUE, pattern = ".tif")
names(LULC_paths) <- basename(LULC_paths) 

#subset LULC paths to only 2020 and 2060
Start_end_LULC <- LULC_paths[grepl(paste0(c(Scenario_start, Scenario_end), collapse = "|"), LULC_paths)]

#name each path for it's year
names(Start_end_LULC) <- sapply(Start_end_LULC, function(x) {
na.omit(str_match(x, paste0(c(Scenario_start, Scenario_end), collapse = "|")))
})

#seperate final lulc map paths
Final_lulc_paths <- LULC_paths[grepl(Scenario_end, LULC_paths)]

#name according to scenarios
names(Final_lulc_paths) <- sapply(Final_lulc_paths, function(x) {
na.omit(str_match(x, Scenario_names))
})


#Load LULC aggregation table
LULC_agg <- readxl::read_xlsx("tools/LULC_class_aggregation.xlsx")

#create a raster attribute table
LULC_rat <- data.frame(
  ID = sort(unique(values(rast(LULC_paths[[1]]))))
)
LULC_rat$lulc_name <- c(unlist(sapply(LULC_rat$ID, function(y) unique(unlist(LULC_agg[LULC_agg$Aggregated_ID == y, "Aggregated_class"])),simplify = TRUE)), "Lake", "River")

#define colour palette
  pal <- list("Settlement/urban/amenities" = '#BB0011', #Urban
  "Static class" = "#DDDDDD", #static
  "Open Forest" = "#668822", #Open forest
  "Closed forest" = "#117733", #closed forest
  "Overgrown/shrubland/unproductive vegetation" = "#44AA88", #Shrubland
  "Intensive agriculture" =  "#FFDD44", #Intensive agriculture
  "Alpine pastures" = "#558877", #Alpine pastures
  "Grassland or meadows" = "#AADDCC", #Grassland
  "Permanent crops" = "#DDCC66", #Permanet crops
  "Glacier" = "#E8ECFB",
  "River" = "#5566AA",
  "Lake" = "#5566AA")

#subset aggregation table to distinct value of the aggregated LULC classes
subset_agg <- LULC_agg %>% distinct(Aggregated_ID, .keep_all=TRUE)

#add colours to class info
subset_agg$colours <- sapply(subset_agg$Aggregated_class, function(x){
  colour <- pal[[paste(x)]]
})

LULC_rat$colour <- sapply(LULC_rat$lulc_name, function(x){
  colour <- pal[[paste(x)]]
})



### =========================================================================
### A- Protected area data
### =========================================================================

#Get the 2020 and 2055 i.e. existing PA map paths for each scenario
PA_map_paths <- list.files("data/Spat_prob_perturb_layers/Protected_areas/Future_PAs", full.names = TRUE, pattern = paste0(c("2020","2055"), collapse = "|"))

#loop over scenario names and combine the rasters for each
for(i in Scenario_names[2:4]){
  
  #subset raster paths
  Scenario_lyrs <- rast(grep(i, PA_map_paths, value = TRUE))
  
  #set NAs in both layers to 1 giving booleans different values
  Scenario_lyrs[[1]] <- ifel(!is.na(Scenario_lyrs[[1]]), 3, 1)
  Scenario_lyrs[[2]] <- ifel(!is.na(Scenario_lyrs[[2]]), 2, 1)
  
  #key
  # 3 = New PAs
  # 1 = NAs
  # 6 = Old PA
  
  #apply function over the layers
  Multi_lyr <- lapp(Scenario_lyrs, fun=function(x,y){x*y})
  
  #Convert 1's back to NAs
  Multi_lyr[Multi_lyr == 1] <- NA
  Multi_lyr[Multi_lyr == 6] <- 1
  freq(Multi_lyr)
  #save
  writeRaster(Multi_lyr,file= paste0(PA_maps_dir, "/",i, "_PAs.tif"), overwrite=TRUE)
  }

#Plot maps for each scenario
lapply(list.files(PA_maps_dir, full.names = TRUE), function(x){
  plot(rast(x))
})



### =========================================================================
### B- Plotly: Sankey diagram for start/end dates
### =========================================================================

#loop over the scenarios
Scenario_crosstabs_SE <- as.data.frame(rbindlist(lapply(Scenario_names, function(x){

  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(grep(x, Start_end_LULC, value = TRUE))

  #cross tabulate the rasters
  Scenario_tbl <- terra::crosstab(Scenario_LULC, long = TRUE)

  #rename columns
  colnames(Scenario_tbl) <- c("From","To", "ncell")

  #remove entries for lakes and rivers
  Scenario_tbl <- Scenario_tbl[!(Scenario_tbl$From %in% c(20:21)),]

  #calculate % of class_transitions and % of total_transitions
  Scenario_tbl$Perc_class_change <- NA
  Scenario_tbl$Perc_total_change <- NA

  for(i in 1:nrow(Scenario_tbl)){

    #calculate  ncell as % of total amount of changes
    Scenario_tbl[i, "Perc_total_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl$ncell))*100

    #calculate  ncell as % of total amount of changes in 'from' class
    Scenario_tbl[i, "Perc_class_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl[Scenario_tbl$From == Scenario_tbl[i, "From"], "ncell"]))*100
  }

  return(Scenario_tbl)

}), idcol = "Scenario"))

#save result
saveRDS(Scenario_crosstabs_SE, file = paste0(Sankey_dir, "/Scenario_crosstabulations_start_end_timepoints.rds"))

#Loop over scenarios creating interactive sankey plots and saving tables if necessary
lapply(Scenario_names, function(x){
  
  #subset data
  dat <- Scenario_crosstabs_SE[Scenario_crosstabs_SE$Scenario == x,]
  
  #uncomment this to remove class persitences
  #dat <- dat[dat$From != dat$To,]
  
  #add From/to  lulc class names to data
  dat$LULC_from <- sapply(dat$From, function(q){
    subset_agg[subset_agg$Aggregated_ID == q, "Aggregated_class"]
  })
  
  dat$LULC_to <- sapply(dat$To, function(q){
    subset_agg[subset_agg$Aggregated_ID == q, "Aggregated_class"]
  }) 
  
  #the nodes object in the plot_ly call requires a dataframe
  #containing the number of rows equalling the number of unique from and to classes
  
  #identify unique classes and name with a unique number to seperate from the 'to' classes
  unique_from <- as.character(unique(dat$LULC_from))
  names(unique_from) <- seq(from=100, by=1, length.out= length(unique_from))

  unique_to <- as.character(unique(dat$LULC_to))
  names(unique_to) <- seq(from=200, by=1, length.out = length(unique_to))
  
  #bind as dataframe with columns for node name
  LULC_names <- as.data.frame(c(unique_from, unique_to))
  colnames(LULC_names) <- "name"
  
  #column for unique values
  LULC_names$val <- c(names(unique_from), names(unique_to))

  #for the 'links' object in the plotly call to be aligned with the
  #nodes object in must be zero-indexed (i.e. the ID of the first node must be 0)
  #because it is not possible to set rownumbers as 0 in R use an additonal column
  LULC_names$row <- seq(0,nrow(LULC_names)-1,1)
  
  #match lulc class names for the from and to classes to the unique zero-index row numbers
  dat$source <- sapply(dat$LULC_from, function(j){
    matches <- LULC_names[LULC_names$name == j, "row"]
    matches[1]
    })
  
  dat$target <- sapply(dat$LULC_to, function(j){
    matches <- LULC_names[LULC_names$name == j, "row"]
    matches[length(matches)]
    })

  # Alternate option with networkD3
  # p <- sankeyNetwork(Links = dat, Nodes = LULC_names, Source = "source",
  #                    Target = "target", Value = "ncell", NodeID = "name",
  #                    units = "Ha", fontSize = 12, nodeWidth = 30)
  
  #with plotly
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    valuesuffix = "ha",
    node = list(
      label = LULC_names[["name"]], #IMPORTant to subset to vector to include zero-indexing
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = dat$source,
      target = dat$target,
      value =  dat$ncell #possible to change value if desired
    )
  ) 
  
  htmlwidgets::saveWidget(
    widget = fig, 
    file = paste0(Sankey_dir, "/", x, "_start_end_sankey_diagram.html"), 
    selfcontained = TRUE #creates a single html file
  )
  
  
  write.csv(Scenario_crosstabs_SE[Scenario_crosstabs_SE$Scenario == x,], file = paste0(Sankey_dir, "/", x, "_start_end_sankey_table.csv"), row.names = FALSE)
})

### =========================================================================
### C- Plotly: Sankey diagram for all simulation dates
### =========================================================================

#Upper loop over the scenarios
Scenario_crosstabs_all <- as.data.frame(rbindlist(lapply(Scenario_names, function(x){

  #seperate paths for scenario
  Scenario_paths <- grep(x, LULC_paths, value = TRUE)

  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(Scenario_paths)
  names(Scenario_LULC) <- str_match_all(Scenario_paths, paste(Time_steps))

  #Inner loop over time points: performing cross tabulation for each consecutive
  #pair of time points in the raster stack
  Cross_tab_all_years <- lapply(1:(nlyr(Scenario_LULC)-1), function(i){

    #combine rasters
    comb <- c(Scenario_LULC[[i]], Scenario_LULC[[i+1]])

    #cross tabulate
    Time_tbl <- terra::crosstab(comb, long = TRUE)

    #rename columns
    colnames(Time_tbl) <- c("From","To", "ncell")

    #remove entries for lakes and rivers
    Time_tbl <- Time_tbl[!(Time_tbl$From %in% c(20:21)),]

    #calculate % of class_transitions and % of total_transitions
    Time_tbl$Perc_class_change <- NA
    Time_tbl$Perc_total_change <- NA

    for(i in 1:nrow(Time_tbl)){

      #calculate  ncell as % of total amount of changes
      Time_tbl[i, "Perc_total_change"] <- (Time_tbl[i, "ncell"]/sum(Time_tbl$ncell))*100

      #calculate  ncell as % of total amount of changes in 'from' class
      Time_tbl[i, "Perc_class_change"] <- (Time_tbl[i, "ncell"]/sum(Time_tbl[Time_tbl$From == Time_tbl[i, "From"], "ncell"]))*100
    }
    return(Time_tbl)
    }) # close loop over time points

  #name the results by combining the timepoints of each layer
  names(Cross_tab_all_years) <- sapply(1:(nlyr(Scenario_LULC)-1), function(i){
    paste0(names(Scenario_LULC[[i]]), "-", names(Scenario_LULC[[i+1]]))
    })

  #bind results together
  Crosstab_bound <- rbindlist(Cross_tab_all_years, idcol = "years")
}), idcol = "Scenario"))

#save result
saveRDS(Scenario_crosstabs_all, file = paste0(Sankey_dir, "/Scenario_crosstabulations_all_timepoints.rds"))

#Loop over scenarios producing seperate sankey plots
Scenario_plots <- lapply(Scenario_names, function(x){
  
  #subset data to scenario
  dat <- Scenario_crosstabs_all[Scenario_crosstabs_all$Scenario == x,]
  
  #vector list of time period names
  time_tags <- unique(dat$years)
  
  #add extra columns combining the year tag and From/to lulc class names 
  dat$LULC_from <- sapply(1:nrow(dat), function(i){
    LULC <- dat$From[i]
    year <- dat$years[i]
    paste0(year, "_", subset_agg[subset_agg$Aggregated_ID == LULC, "Aggregated_class"])
  })
  
  #The year in the 'to' tags has to go to the subsequent time step in order for 
  #the node to be drawn as intermediate rather than a new node
  dat$LULC_to <- sapply(1:nrow(dat), function(i){
    
    LULC <- dat$To[i]
    
    #current time step tag
    year <- str_remove_all(dat$years[i], " ")
    
    #next tag
    if(match(year, time_tags) < length(time_tags)){
    subseq_year <- time_tags[[(match(year, time_tags)+1)]]
    col_val <- paste0(subseq_year , "_", subset_agg[subset_agg$Aggregated_ID == LULC, "Aggregated_class"])
    } else {
    col_val <-paste0(year , "_", subset_agg[subset_agg$Aggregated_ID == LULC, "Aggregated_class"]) 
    }
    return(col_val)
  })
  
  #remove class persistence for the final year otherwise they create loops at the end
  #dat <- dat[-(which(dat$years == time_tags[length(time_tags)] & dat$From == dat$To)),]
  dat <- dat[dat$From != dat$To,]

  #the nodes object in the plot_ly call requires a dataframe
  #containing the number of rows equalling the number of unique from and to classes
  
  #identify unique classes and name with a unique number to seperate from the 'to' classes
  unique_from <- as.character(unique(dat$LULC_from))
  unique_to <- as.character(unique(dat$LULC_to))

  #bind as dataframe with columns for node name
  #LULC_names <- as.data.frame(c(unique(unique_from, unique_to)))
  nodes <- as.data.frame(union(unique_from, unique_to))
  colnames(nodes) <- "name"

  #for the 'links' object in the plotly call to be aligned with the
  #nodes object in must be zero-indexed (i.e. the ID of the first node must be 0)
  #because it is not possible to set rownumbers as 0 in R use an additonal column
  nodes$row <- seq(0,nrow(nodes)-1,1)
  
  #match lulc class names for the from and to classes to the unique zero-index row numbers
  dat$source <- sapply(dat$LULC_from, function(j){
    matches <- nodes[nodes$name == j, "row"]
    matches[1]
    })
  
  dat$target <- sapply(dat$LULC_to, function(j){
    matches <- nodes[nodes$name == j, "row"]
    matches[length(matches)]
    })
  
  #change names of nodes to clean them up
  nodes$clean_name <- sapply(nodes$name, function(y) na.omit(str_match(y, subset_agg$Aggregated_class)))
  
  #add  columns for colours
  nodes$colour <- sapply(nodes$clean_name, function(y) paste(pal[names(pal)[names(pal) == y]]))
  
  #add x and Y positions
  x_pos <- seq.int(from = 0.1, by = 0.1, length.out = length(Time_steps))
  y_pos <- seq.int(from = 0.1, by = 0.1, length.out = length(unique(nodes$clean_name)))
  
  names(time_tags) <- x_pos[1:length(time_tags)]
  nodes$x <- sapply(nodes$name, function(y){
    names(time_tags[time_tags == str_split_i(y, "_", 1)])
  })
  
  LULC_names <- unique(nodes$clean_name)
  names(LULC_names) <- y_pos[1:length(LULC_names)]
  nodes$y <- sapply(nodes$clean_name, function(y){
    names(LULC_names[LULC_names == y])
  })
  
  #get position of 1st instance of each unique value in column
  unique_index <- sapply(split(seq_along(nodes$clean_name), nodes$clean_name),"[[",1)
  
  #change non-1st instances to blank
  nodes[-(unique_index), "clean_name"] <- ""
  
  # #Alternate option with networkD3
  #  p <- sankeyNetwork(Links = dat, 
  #                     Nodes = nodes,
  #                     Source = "source",
  #                     Target = "target",
  #                     Value = "ncell",
  #                     NodeID = "clean_name",
  #                     units = "Ha",
  #                     fontSize = 12,
  #                     nodeWidth = 30,
  #                     colourScale = nodes[["colour"]])
   
  #with plotly
  fig <- plot_ly(
    type = "sankey",
    arrangement = "fixed",
    orientation = "h",
    valuesuffix = "ha",
    node = list(
      #label = nodes[["clean_name"]],#important to subset to vector to include zero-indexing
      color = nodes[["colour"]],
      x = nodes[["x"]],
      y = nodes[["y"]],
      pad = 15,
      thickness = 80,
      line = list(
        color = "black",
        width = 0.5)
    ),
    
    link = list(
      source = dat$source,
      target = dat$target,
      value =  dat$ncell #possible to change value if desired
      
    )) %>%
  #   add_annotations(
  #   x = x_pos,
  #   y = 1.01,
  #   xref="x",
  #   yref="paper",
  #   align = "left",
  #   text = paste(Time_steps),
  #   font = list(size = 8),
  #   showarrow = FALSE,
  #   inherit = TRUE
  # ) %>% 
  layout(xaxis = list(showgrid = FALSE,
                      showticklabels=FALSE),
         yaxis = list(showgrid = FALSE,
                      showticklabels=FALSE))
  
  htmlwidgets::saveWidget(
    widget = fig, 
    file = paste0(Sankey_dir, "/", x, "_multistep_sankey_diagram.html"), 
    selfcontained = TRUE #creates a single html file
  )
  
  #write.csv(Scenario_crosstabs[Scenario_crosstabs$Scenario == x,], file = paste0(Sankey_dir, "/", x, "_sankey_table.csv"), row.names = FALSE)
  return(fig)
  })

#add domains to plotly plots
#https://stackoverflow.com/questions/73496559/preventing-plots-from-overlapping-in-r

Scenario_plots[[1]] <- style(Scenario_plots[[1]], domain = list(x = c(0, 0.5), y = c(0.6, 1)))
Scenario_plots[[2]] <- style(Scenario_plots[[2]], domain = list(x = c(0.5, 1), y = c(0.6, 1)))
Scenario_plots[[3]] <- style(Scenario_plots[[3]], domain = list(x = c(0, 0.5), y = c(0, 0.4)))
Scenario_plots[[4]] <- style(Scenario_plots[[4]], domain = list(x = c(0.5, 1), y = c(0, 0.4)))

Scenario_plots[[1]] <- style(Scenario_plots[[1]], domain = list(x = c(0, 1), y = c(0.6, 8)))
Scenario_plots[[2]] <- style(Scenario_plots[[2]], domain = list(x = c(0, 1), y = c(0.4, 0.6)))
Scenario_plots[[3]] <- style(Scenario_plots[[3]], domain = list(x = c(0, 1), y = c(0.2, 0.4)))
Scenario_plots[[4]] <- style(Scenario_plots[[4]], domain = list(x = c(0, 1), y = c(0, 0.2)))

subplot(Scenario_plots[[1]],Scenario_plots[[2]],Scenario_plots[[3]],Scenario_plots[[4]],
        margin = 0.5)

subplot(Scenario_plots[[3]],Scenario_plots[[4]],
        margin = 0.5)

#combine plot_ly plots for each scenario
subplot(Scenario_plots, nrows = length(Scenario_plots), shareX = FALSE, shareY = FALSE)


### =========================================================================
### D- ggalluvial: Sankey diagram for all simulation dates 
### =========================================================================

#loop over the scenarios producing the dataframes necessary for sankey plots
#under two options: 
#1. including instances cells that display persistence for
#the whole simulation time i.e. they do not change)
#2. Not including these instances which allows for clearer visualisations of the proportions of transitions

Scenario_sankey_alluvial_dfs <- lapply(Scenario_names, function(x){
  
  #seperate paths for scenario
  Scenario_paths <- grep(x, LULC_paths, value = TRUE)

  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(Scenario_paths)
  names(Scenario_LULC) <- str_match_all(Scenario_paths, paste(Time_steps))
  
  #get raster values from both rasters
  rast_values <- as.data.frame(Scenario_LULC) %>% mutate_if(is.numeric,as.character)
  
  #create seperate dataframes to include/exclude persitence instances
  persist_options <- list("with_persist" = rast_values,
  "without_persist" = rast_values[apply(rast_values, 1, function(y) length(unique(y[!is.na(y)])) != 1),])
  
  #loop over both options wrangling data
  Persist_dfs_clean <- lapply(persist_options, function(rast_df){
  
    #change column names to add prefix "year_"
    colnames(rast_df) <- paste0("year_", names(Scenario_LULC))
  
    #add unique id col
    rast_df$id <- seq.int(1, nrow(rast_df),1)
  
    #perform counts of combinations of unique values acorss columns
    #no clear programmatic way to use all column names
    rast_df_counts <- dplyr::count(rast_df, year_2020,year_2025,year_2030, year_2035,year_2040, year_2045, year_2050, year_2055, year_2060) %>%
    mutate(id = row_number())
  
    #make long data
    rast_df_long <- gather(rast_df_counts, value, key,-n,-id)
  
    # clean up data for graph
    rast_df_clean <- rast_df_long %>%
    mutate(
    year = as.numeric(str_remove(value, "year_"))
    #,LULC_class = as.factor(key),
    )
  
    #column for LULC class name
    rast_df_clean$LULC_class <- sapply(rast_df_clean$key, function(y){
    unlist(subset_agg[subset_agg$Aggregated_ID == y, "Aggregated_class"])
    })
 
    #rast_df_clean$LULC_class <- as.factor(unlist(rast_df_clean$LULC_class))
  
    #column for colours using defined palette
    rast_df_clean$colour <- sapply(rast_df_clean$key, function(y){
      unlist(LULC_rat[LULC_rat$ID == y, "colour"])
    })
    return(rast_df_clean)
  }) #close loop over different persistence option dfs
  
  return(Persist_dfs_clean)
  }) # close loop over scenarios

#save data for reloading
saveRDS(Scenario_sankey_alluvial_dfs, file= paste0(Sankey_dir, "/Sankey_alluvial_data_all_time_points_dfs.rds"))
Scenario_sankey_alluvial <- readRDS(paste0(Sankey_dir, "/Sankey_alluvial_data_all_time_points_dfs.rds"))

#subset to choice of 'with persistence' or 'without persistence'
Scenario_sankey_dfs_without <- lapply(Scenario_sankey_alluvial_dfs, function(x) x[["without_persist"]])
Scenario_sankey_dfs_with <- lapply(Scenario_sankey_alluvial_dfs, function(x) x[["with_persist"]])

#loop over datasets producing plots for each scenario
Scenario_Sankey_plots_without <- lapply(1:length(Scenario_sankey_dfs_without), function(scenario_num){

  #seperate scenario data
  dat <- Scenario_sankey_dfs_without[[scenario_num]]
  dat$LULC_class <- as.factor(unlist(dat$LULC_class))
  
  #plot call
  Scenario_plot <- dat %>%
  ggplot(aes(
  x = year,
  stratum = LULC_class,
  alluvium = id,
  y = n,
  fill = LULC_class
  )) +
  geom_flow(alpha = .5, aes(colour = LULC_class), width = 0, size = 0.7) +
  geom_stratum(alpha = .75, width = 0.75) +
  theme_tufte(base_size = 18) +
  labs(x = "Simulation year",
       title = str_replace(names(Scenario_sankey_dfs_without)[[scenario_num]], "_", "-"))+
  scale_fill_manual(name = "LULC class", values = unlist(pal))+ 
  scale_colour_manual(name = "LULC class", values = unlist(pal))+ 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"),
        axis.line = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=12, vjust = -0.2, face = "bold"), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="right",
        plot.margin = unit(c(0,0,0.5,0), "cm"))
})

Scenario_Sankey_plots_with <- lapply(1:length(Scenario_sankey_dfs_with), function(scenario_num){

  #seperate scenario data
  dat <- Scenario_sankey_dfs_with[[scenario_num]]
  dat$LULC_class <- as.factor(as.character(dat$LULC_class))
  
  #plot call
  Scenario_plot <- dat %>%
  ggplot(aes(
  x = year,
  stratum = LULC_class,
  alluvium = id,
  y = n,
  fill = LULC_class
  )) +
  geom_flow(alpha = .5, aes(colour = LULC_class), width = 0, size = 0.7) +
  geom_stratum(alpha = .75, width = 0.75) +
  theme_tufte(base_size = 18) +
  labs(x = "Simulation year",
       title = str_replace(names(Scenario_sankey_dfs_with)[[scenario_num]], "_", "-"))+
  scale_fill_manual(name = "LULC class", values = unlist(pal))+ 
  scale_colour_manual(name = "LULC class", values = unlist(pal))+ 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"),
        axis.line = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=12, vjust = -0.2, face = "bold"), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="right",
        plot.margin = unit(c(0,0,0.5,0), "cm"))
})

#arrange plots with patchwork

#Define common x axis label if needed
# gg_axis <- cowplot::get_plot_component(ggplot() +
#                                          labs(x = "Simulation year") +
#                                          theme(axis.title.x = element_text(hjust = 0.25)),
#   "xlab-b")

Combined_Sankey_without <- (wrap_plots(Scenario_Sankey_plots_without, nrow =3, ncol = 2)+
                   guide_area())+
                   plot_layout(guides = "collect")+
                  plot_annotation(caption = "Note: for visual clarity instances of persistence over the whole simulation time\n(i.e. cells not undergoing change in any time step) were removed.")&
                   theme(legend.position = "right",
                          plot.caption.position = "plot",
                          plot.caption = element_text(vjust = 3, size = 16))

Combined_Sankey_with <- (wrap_plots(Scenario_Sankey_plots_with, nrow =3, ncol = 2)+
                   guide_area())+
                   plot_layout(guides = "collect")+
                  plot_annotation(caption = "Note: for visual clairty instances of persistence over the whole simulation time\n(i.e. cells not undergoing change in any time step) were removed.")&
                   theme(legend.position = "right",
                          plot.caption.position = "plot",
                          plot.caption = element_text(vjust = 3, size = 16))


#save plot
ggsave(plot = Combined_Sankey_without,
       filename = paste0(Sankey_dir, "/Combined_sankey_all_timesteps_without_persistence_portrait.tif"),
       device='tiff',
       dpi=300,
       width = 20,
       height = 25,
       units = "cm")

ggsave(plot = Combined_Sankey_with,
       filename = paste0(Sankey_dir, "/Combined_sankey_all_timesteps_with_persistence_portrait.tif"),
       device='tiff',
       dpi=300,
       width = 20,
       height = 25,
       units = "cm")


### =========================================================================
### E- ggalluvial: Sankey diagram for start end dates
### =========================================================================

Scenario_sankey_alluvial_dfs_start_end <- lapply(Scenario_names, function(x){
  
  #seperate paths for scenario
  Scenario_paths <- grep(x, Start_end_LULC, value = TRUE)
  
  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(Scenario_paths)
  names(Scenario_LULC) <- sapply(Scenario_paths, function(y){na.omit(str_match(y, paste(Time_steps)))})

  
  #get raster values from both rasters
  rast_values <- as.data.frame(Scenario_LULC) %>% mutate_if(is.numeric,as.character)
  
  #create seperate dataframes to include/exclude persitence instances
  persist_options <- list("with_persist" = rast_values,
                          "without_persist" = rast_values[apply(rast_values, 1, function(y) length(unique(y[!is.na(y)])) != 1),])
  
  #loop over both options wrangling data
  Persist_dfs_clean <- lapply(persist_options, function(rast_df){
    
    #change column names to add prefix "year_"
    colnames(rast_df) <- paste0("year_", names(Scenario_LULC))
    
    #add unique id col
    rast_df$id <- seq.int(1, nrow(rast_df),1)
    
    #perform counts of combinations of unique values acorss columns
    #no clear programmatic way to use all column names
    rast_df_counts <- dplyr::count(rast_df, year_2020, year_2060) %>%
      mutate(id = row_number())
    
    #make long data
    rast_df_long <- gather(rast_df_counts, value, key,-n,-id)
    
    # clean up data for graph
    rast_df_clean <- rast_df_long %>%
      mutate(
        year = as.numeric(str_remove(value, "year_"))
        #,LULC_class = as.factor(key),
      )
    
    #column for LULC class name
    rast_df_clean$LULC_class <- sapply(rast_df_clean$key, function(y){
      unlist(subset_agg[subset_agg$Aggregated_ID == y, "Aggregated_class"])
    })
    
    #rast_df_clean$LULC_class <- as.factor(unlist(rast_df_clean$LULC_class))
    
    #column for colours using defined palette
    rast_df_clean$colour <- sapply(rast_df_clean$key, function(y){
      unlist(LULC_rat[LULC_rat$ID == y, "colour"])
    })
    return(rast_df_clean)
  }) #close loop over different persistence option dfs
  
  return(Persist_dfs_clean)
}) # close loop over scenarios

#save data for reloading
saveRDS(Scenario_sankey_alluvial_dfs_start_end, file= paste0(Sankey_dir, "/Sankey_alluvial_data_start_end_dfs.rds"))
Scenario_sankey_alluvial_start_end <- readRDS(paste0(Sankey_dir, "/Sankey_alluvial_data_start_end_dfs.rds"))

#subset to choice of 'with persistence' or 'without persistence'
Scenario_sankey_dfs_SE_without <- lapply(Scenario_sankey_alluvial_dfs_start_end, function(x) x[["without_persist"]])
Scenario_sankey_dfs_SE_with <- lapply(Scenario_sankey_alluvial_dfs_start_end, function(x) x[["with_persist"]])

#loop over datasets producing plots for each scenario
Scenario_Sankey_plots_SE_without <- lapply(1:length(Scenario_sankey_dfs_SE_without), function(scenario_num){
  
  #seperate scenario data
  dat <- Scenario_sankey_dfs_SE_without[[scenario_num]]
  dat$LULC_class <- as.character(dat$LULC_class)
  
  #plot call
  Scenario_plot <- dat %>%
    ggplot(aes(
      x = year,
      stratum = LULC_class,
      alluvium = id,
      y = n,
      fill = LULC_class
    )) +
    geom_flow(alpha = .5, aes(colour = LULC_class), width = 0, size = 0.7) +
    geom_stratum(alpha = .75, width = 0.95) +
    scale_x_continuous(breaks = c(2020,2060), expand = c(.1, .1))+
    theme_tufte(base_size = 18) +
    labs(x = "Simulation year",
         title = str_replace(names(Scenario_sankey_dfs_SE_without)[[scenario_num]], "_", "-"))+
    scale_fill_manual(name = "LULC class", values = unlist(pal))+ 
    scale_colour_manual(name = "LULC class", values = unlist(pal))+ 
    theme(text = element_text(family = "Times New Roman"),
          plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"),
          axis.line = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          legend.key.size = unit(0.5, 'cm'),
          legend.title = element_text(size=12, vjust = -0.2, face = "bold"), #change legend title font size
          legend.text = element_text(size=10), #change legend text font size
          legend.key = element_rect(fill = "white", colour = "white"),
          legend.title.align=0.5,
          legend.position="right",
          plot.margin = unit(c(0,0,0.5,0), "cm"))
})

Scenario_Sankey_plots_SE_with <- lapply(1:length(Scenario_sankey_dfs_SE_with), function(scenario_num){
  
  #seperate scenario data
  dat <- Scenario_sankey_dfs_SE_with[[scenario_num]]
  dat$LULC_class <- as.character(dat$LULC_class)
  
  #plot call
  Scenario_plot <- dat %>%
    ggplot(aes(
      x = year,
      stratum = LULC_class,
      alluvium = id,
      y = n,
      fill = LULC_class
    )) +
    geom_flow(alpha = .5, aes(colour = LULC_class), width = 0, size = 0.7) +
    geom_stratum(alpha = .75, width = 0.75) +
    theme_tufte(base_size = 18) +
    scale_x_continuous(breaks = c(2020,2060), expand = c(.1, .1))+
    labs(x = "Simulation year",
         title = str_replace(names(Scenario_sankey_dfs_SE_with)[[scenario_num]], "_", "-"))+
    scale_fill_manual(name = "LULC class", values = unlist(pal))+ 
    scale_colour_manual(name = "LULC class", values = unlist(pal))+ 
    theme(text = element_text(family = "Times New Roman"),
          plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"),
          axis.line = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          legend.key.size = unit(0.5, 'cm'),
          legend.title = element_text(size=12, vjust = -0.2, face = "bold"), #change legend title font size
          legend.text = element_text(size=10), #change legend text font size
          legend.key = element_rect(fill = "white", colour = "white"),
          legend.title.align=0.5,
          legend.position="right",
          plot.margin = unit(c(0,0,0.5,0), "cm"))
})

Combined_Sankey_SE_without <- (wrap_plots(Scenario_Sankey_plots_SE_without, nrow =3, ncol = 2)+
                              guide_area())+
  plot_layout(guides = "collect",widths = 1)+
  plot_annotation(caption = "Note: for visual clarity instances of persistence over the whole simulation time\n(i.e. cells not undergoing change in any time step) were removed.")&
  theme(legend.position = "right",
        plot.caption.position = "plot",
        plot.caption = element_text(vjust = 3, size = 16))

Combined_Sankey_SE_with <- (wrap_plots(Scenario_Sankey_plots_SE_with, nrow =3, ncol = 2)+
                           guide_area())+
  plot_layout(guides = "collect")+
  plot_annotation(caption = "Note: for visual clairty instances of persistence over the whole simulation time\n(i.e. cells not undergoing change in any time step) were removed.")&
  theme(legend.position = "right",
        plot.caption.position = "plot",
        plot.caption = element_text(vjust = 3, size = 16))


#save plot
ggsave(plot = Combined_Sankey_SE_without,
       filename = paste0(Sankey_dir, "/Combined_sankey_start_end_without_persistence_portrait.tif"),
       device='tiff',
       dpi=300,
       width = 20,
       height = 25,
       units = "cm")

ggsave(plot = Combined_Sankey_SE_with,
       filename = paste0(Sankey_dir, "/Combined_sankey_start_end_with_persistence_portrait.tif"),
       device='tiff',
       dpi=300,
       width = 20,
       height = 25,
       units = "cm")

### =========================================================================
### F- ggsankeygrad: Sankey diagram for start end dates with gradient fill
### =========================================================================

#loop over the scenarios
Scenario_crosstabs_SE <- as.data.frame(rbindlist(lapply(Scenario_names, function(x){
  
  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(grep(x, Start_end_LULC, value = TRUE))
  
  #cross tabulate the rasters
  Scenario_tbl <- terra::crosstab(Scenario_LULC, long = TRUE)
  
  #rename columns
  colnames(Scenario_tbl) <- c("From","To", "ncell")
  
  #remove entries for lakes and rivers
  Scenario_tbl <- Scenario_tbl[!(Scenario_tbl$From %in% c(20:21)),]
  
  #calculate % of class_transitions and % of total_transitions
  Scenario_tbl$Perc_class_change <- NA
  Scenario_tbl$Perc_total_change <- NA
  
  for(i in 1:nrow(Scenario_tbl)){
    
    #calculate  ncell as % of total amount of changes
    Scenario_tbl[i, "Perc_total_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl$ncell))*100
    
    #calculate  ncell as % of total amount of changes in 'from' class
    Scenario_tbl[i, "Perc_class_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl[Scenario_tbl$From == Scenario_tbl[i, "From"], "ncell"]))*100
  }
  
  return(Scenario_tbl)
  
}), idcol = "Scenario"))

#save result
saveRDS(Scenario_crosstabs_SE, file = paste0(Sankey_dir, "/Scenario_crosstabulations_start_end_timepoints.rds"))

ggSankeyGrad <- function(c1, c2, col1 = "gray", col2 = "gray",
                         values, padding = 2, alpha = 0.4, label = FALSE, label_color = TRUE, label_fontface = 'bold', label_size = 10, color_steps = 100) {
  
  stopifnot(requireNamespace("tidyr"))
  stopifnot(requireNamespace("dplyr"))
  stopifnot(requireNamespace("ggplot2"))
  
  df <- data.frame(c1, c2, values)
  
  # calculate beginning bottom and top of flow ribbons
  d1 <- df %>%
    group_by(c1, c2) %>%
    summarise(lengths = sum(values)) %>%
    ungroup() %>%
    mutate(b1 = lag(cumsum(lengths +
                             ifelse(row_number() < n() &
                                      c1 == lead(c1),
                                    0, padding )))) %>%
    mutate(b1 = ifelse(is.na(b1), 0, b1)) %>%
    mutate(t1 = b1 + lengths) %>%
    select(-lengths)
  
  # calculate ending bottom and top of flow ribbons
  d2 <- df %>%
    group_by(c2, c1) %>%
    summarise(lengths = sum(values)) %>%
    ungroup() %>%
    mutate(b2 = lag(cumsum(lengths +
                             ifelse(row_number() < n() &
                                      c2 == lead(c2),
                                    0, padding )))) %>%
    mutate(b2 = ifelse(is.na(b2), 0, b2)) %>%
    mutate(t2 = b2 + lengths) %>%
    select(-lengths)
  
  # combine calculations with beginning and ending colors for each flow
  d <- left_join(d1, d2, by = c("c1", "c2"))
  d$col1 <- col1
  d$col2 <- col2
  
  # create a factor for each flow
  d <- d %>% mutate(cat = as.integer(as.factor(paste0(c1, c2))))
  
  # x value for each vertical line
  x <- seq(0, 1, length = color_steps + 1)
  
  # calculate bottom and top y value for each line
  bez <- data.frame()
  for(i in seq(nrow(d))) {
    bot <- with(d[i,],
                matrix(c(0,b1, 1,b1, 3,b2, 4,b2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    top <- with(d[i,],
                matrix(c(0,t1, 1,t1, 3,t2, 4,t2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    col <- with(d[i,],
                colorRampPalette(c(as.character(col1),
                                   as.character(col2)))( length(x) ))
    bez <- bind_rows(bez,
                     data.frame(cat = i,
                                x = x,
                                col = col,
                                bez_b = bezier::bezier(t = x, p = bot)[,2],
                                bez_t = bezier::bezier(t = x, p = top)[,2],
                                stringsAsFactors = FALSE) )
  }
  
  # create four x, y points for each polygon
  b1 <- bez %>%
    group_by(cat) %>%
    mutate(x1 = x,
           x2 = lead(x),
           x3 = lead(x),
           x4 = x,
           geom_n = paste0(cat, ".", row_number())) %>%
    pivot_longer(cols = c(x1, x2, x3, x4), names_to = "x_name", values_to = "x_path")
  
  b2 <- bez %>%
    group_by(cat) %>%
    mutate(y1 = bez_t,
           y2 = lead(bez_t),
           y3 = lead(bez_b),
           y4 = bez_b) %>%
    pivot_longer(cols = c(y1, y2, y3, y4), names_to = "y_name", values_to = "y_path")
  
  b1 <- bind_cols(b1, b2[,"y_path"])
  # b1 <- b1[complete.cases(b1),]
  return(b1)
  
  # create base plot with lines
  pl <- ggplot() +
    scale_x_continuous(limits = c(-0.2, 1.2)) +
    
    geom_polygon(data = b1,
                 aes(x = x_path,
                     y = y_path,
                     group = geom_n),
                 color = NA,
                 fill = b1$col,
                 size = 0,
                 alpha = alpha)
  
  # add labels for beginning (left) and ending (right) categories
  if(label == T) {
    loc <- d %>%
      group_by(c1) %>%
      slice(n()) %>%
      ungroup() %>%
      mutate(y = ifelse(row_number() == 1, t1/2 ,
                        (t1 + lag(t1) + padding)/2) )
    
    for(i in seq(nrow(loc))) {
      pl <- pl + annotate('text', x = -0.01, y = loc$y[i],
                          label = as.character(loc$c1[i]),
                          fontface = label_fontface,
                          hjust = 1, size = label_size/.pt, color = ifelse(label_color, loc$col1[i], '#000000') )
    }
    
    loc <- d %>%
      group_by(c2) %>%
      slice(n()) %>%
      ungroup() %>%
      mutate(y = ifelse(row_number() == 1, t2/2 ,
                        (t2 + lag(t2) + padding)/2) )
    
    
    for(i in seq(nrow(loc))) {
      pl <- pl + annotate('text', x = 1.01, y = loc$y[i],
                          label = as.character(loc$c2[i]),
                          fontface = label_fontface,
                          hjust = 0, size = label_size/.pt, color = ifelse(label_color, loc$col2[i], '#000000') )
    }
    
    
  }
  
  # remove all theme elements
  pl <- pl + theme_void()
  
  # return ggplot object
  return(pl)
}

#Loop over scenarios creating sankey plots 
Sankey_grad_plots <- lapply(Scenario_names, function(x){
  
  #subset data
  dat <- Scenario_crosstabs_SE[Scenario_crosstabs_SE$Scenario == x,]
  
  #uncomment this to remove class persitences
  dat <- dat[dat$From != dat$To,]
  
  #add From/to  lulc class names to data
  dat$LULC_from <- as.character(sapply(dat$From, function(q){
    subset_agg[subset_agg$Aggregated_ID == q, "Aggregated_class"]
  }))
  
  dat$LULC_to <- as.character(sapply(dat$To, function(q){
    subset_agg[subset_agg$Aggregated_ID == q, "Aggregated_class"]
  }))
  
  #add From/to colour columns
  dat$From_colour <- sapply(dat$From, function(y){
    unlist(LULC_rat[LULC_rat$ID == y, "colour"])
  })

  dat$To_colour <- sapply(dat$To, function(y){
    unlist(LULC_rat[LULC_rat$ID == y, "colour"])
  })
  
  Scenario_plot <- with(dat, ggSankeyGrad::ggSankeyGrad(c1=LULC_from,
                                c2=LULC_to,
                                col1 = From_colour,
                                col2 = To_colour,
                                values = ncell,
                                padding = 2,
                                alpha = 0.4,
                                label = TRUE,
                                label_color = FALSE,
                                #label_fontface = 'bold',
                                label_size = 5,
                                color_steps = 200))
  
  
})
  
  

### =========================================================================
### G- Scenario maps with leaflet
### =========================================================================

#Dir for saving map images
Map_image_dir <- "Results/Map_analysis/Map_images"

#load rasters
Final_lulc_rasts <- raster::stack(Final_lulc_paths)
names(Final_lulc_rasts) <- names(Final_lulc_paths)

#leaflet colour palette
leaflet_pal <- colorFactor(c("#BB0011", #Urban
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

#url for north arrow
img <- "https://cdn.pixabay.com/photo/2013/07/12/17/54/arrow-152596_960_720.png"

#loop over scenario maps creating leaflet maps, saving as images and returning
#file paths
Map_image_paths <- lapply(1:nlayers(Final_lulc_rasts), function(Scenario_key){
  
  #for the 1st map then save an image including an inset map,
  #north arrow and scale bar
  if(Scenario_key ==1){
    Scenario_map <- leaflet(padding =0,
          options = leafletOptions(zoomControl = FALSE,
                                   attributionControl=FALSE)) |>
          addRasterImage(x = Final_lulc_rasts[[Scenario_key]],
                 colors = leaflet_pal,
                 project = FALSE) |>
          addMiniMap(
            position = "topright",
            tiles = providers$CartoDB.Positron,
            zoomLevelOffset = -3.65,
            width = 160,
            height=160) |>
          addScaleBar(position = "bottomright",
                      options = scaleBarOptions(maxWidth = 120)) |>
          addLogo(img = img,
            position = "bottomright",
            width = 50,
            height = 60,
            offset.x = 10,
            offset.y = 50,
            alpha = 0.8) |>
          setMapWidgetStyle(list(background = "white"))
  }else{
    Scenario_map <- leaflet(padding =0,
          options = leafletOptions(zoomControl = FALSE,
                                   attributionControl=FALSE)) |>
          addRasterImage(x = Final_lulc_rasts[[Scenario_key]],
                 colors = leaflet_pal,
                 project = FALSE) |>
          setMapWidgetStyle(list(background = "white"))
  }

  #because of high resolution necessary to save map as html first before
  #using webshot2::webshot vs. mapview::mapshot
  
  Map_path <- paste0(Map_image_dir, "/", names(Final_lulc_rasts)[Scenario_key], "_final_map.png")
  
  mapshot2(x= Scenario_map,
           file =  Map_path,
           remove_controls = NULL,
                    zoom = 2,
        cliprect = "viewport")
  # saveWidget(Scenario_map,
  #            file = paste0(Map_image_dir, "/", names(Final_lulc_rasts)[Scenario_key], "_final_map.html"),
  #            selfcontained = FALSE)
  # webshot2::webshot(paste0(Map_image_dir, "/", names(Final_lulc_rasts)[Scenario_key], "_final_map.html"),
  #                   file = Map_path,
  #                   zoom = 8,
  #       cliprect = "viewport")
  
  return(Map_path)
  })
names(Map_image_paths) <- names(Final_lulc_rasts)

#arrange maps using patchwork

#read in map images as ggplot objects
Map_plots <- lapply(1:length(Map_image_paths), function(i){
  
  #img <- image_read(x)
  img <- png::readPNG(Map_image_paths[[i]])
  Map_plot <- ggplot() +
  background_image(img) +
  labs(title = str_replace(names(Map_image_paths)[[i]], "_", "-"))+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"))
  #+ coord_fixed()
})
names(Map_plots) <- names(Map_image_paths)

#combine list of plots with patchwork
#Combined_map_plot <- (wrap_plots(Map_plots, nrow =3, ncol = 2)+
#                   guide_area())

#plot(Combined_map_plot)

#save plot
# ggsave(plot = Combined_map_plot,
#        filename = paste0(Map_image_dir, "/Combined_final_maps_portrait.tif"),
#        device='tiff',
#        dpi=300,
#        width = 20,
#        height = 25,
#        units = "cm")


# |> addLegend("bottomright", pal = leaflet_pal,
#             values = LULC_rat$ID,
#             title = "Legend",
#             labFormat = labelFormat(
#             transform = function(y) {
#               LULC_rat[LULC_rat$ID == y, "lulc_name"]
#             }))


### =========================================================================
### H- bar charts
### =========================================================================

#loop over the scenarios
Scenario_bar_charts <- lapply(Scenario_names, function(x){
    
  Scenario_paths <- grep(x, Start_end_LULC, value = TRUE)
                                               
  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(Scenario_paths)
  names(Scenario_LULC) <- names(Scenario_paths)
  
  #get frequency tables
  Scenario_freq <- freq(Scenario_LULC,
                        usenames =TRUE,
                        bylayer = TRUE) |> pivot_wider(values_from = count, names_from = layer)
  
  #add lulc class name
  Scenario_freq$LULC_class <- as.factor(sapply(Scenario_freq$value, function(i){
    LULC_rat[LULC_rat$ID ==i, "lulc_name"]}))
  
  #remove lakes and rivers
  Scenario_freq <- Scenario_freq[!(Scenario_freq$LULC_class %in% c("Lake", "River")),]
  
  #add colour
  Scenario_freq$colour <- sapply(Scenario_freq$value, function(i){
    LULC_rat[LULC_rat$ID ==i, "colour"]})
  
  #calculate difference in area as % of initial class
  # Scenario_freq$perc_diff <- as.numeric(sapply(1:nrow(Scenario_freq), function(i){
  #   ((Scenario_freq[i,names(Scenario_paths)[2]]-Scenario_freq[i,names(Scenario_paths)[1]])/Scenario_freq[i,names(Scenario_paths)[1]])*100
  # }))
  
  #dummy values for example
  Scenario_freq$perc_diff <- c(3,-5,8,2,-12,18,7,-3,14,6)
  
  #tidy to clean label
  Scenario_freq$clean_lab <- paste0(round(Scenario_freq$perc_diff,2), "%")
  
  
  #TO DO: BAR CHART COLOURS ARE MESSED UP WITH SCALE_FILL_MANUAL
  
  #bar chart
  Scenario_plot <- ggplot(Scenario_freq, aes(y= perc_diff, x= LULC_class, fill = LULC_class)) + 
  geom_bar(stat="identity", alpha = 0.6)+
  scale_fill_manual(name = "LULC class", values = unlist(pal))+
  geom_text(aes(label= clean_lab, y = perc_diff + 2 * sign(perc_diff)),
            #position = position_stack(vjust = 0.7),
            size =3
            #fontface = "bold"
            )+
  geom_hline(aes(yintercept =0), colour = "Darkgrey") +
  scale_y_continuous(expand = expansion(mult = 0.1))+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12, vjust = 0.2, hjust = 0.5, face = "bold"),
        #axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.title = element_text(size=12, vjust = -0.2, face = "bold"), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="right"
        #,plot.margin = unit(c(0.5,0.5,0.5,0.5), unit = "cm")
        )
})

#combine maps with barcharts  with patchwork
Maps_and_plots <- append(Map_plots, Scenario_bar_charts)
Maps_and_plots <- Maps_and_plots[order(names(Maps_and_plots))]

Combined_map_bar_plot <- (wrap_plots(Maps_and_plots,
                                     nrow =6,
                                     ncol = 2,
                                     byrow = FALSE)+
                        guide_area())+
                   plot_layout(guides = "collect", heights = c(4,1,4,1,4,1))&
                   theme(legend.position = "right")


#plot(Combined_map_bar_plot)

#save plot
ggsave(plot = Combined_map_bar_plot,
       filename = paste0(Map_image_dir, "/Combined_final_maps_bars_portrait.tif"),
       device='tiff',
       dpi=300,
       width = 20,
       height = 25,
       units = "cm")

### =========================================================================
### I- relative class area change within building zones
### =========================================================================



#load building zone raster
BZ_rast <- rast("Data/Spat_prob_perturb_layers/Bulding_zones/BZ_raster.grd")
crs(BZ_rast) <- ProjCH

#calculate the relative change in LULC class area inside
#the building zones between scenarios (i.e. class area change between 2020-2060
#as a % of the total class area change for that scenario)

#loop over the scenarios
BZ_rel_change <- rbindlist(lapply(Scenario_names, function(x){
  
  Scenario_paths <- grep(x, Start_end_LULC, value = TRUE)
  
  #extract the scenario paths and load as multi-layer raster
  Scenario_LULC <- rast(Scenario_paths)
  crs(Scenario_LULC) <- ProjCH
  names(Scenario_LULC) <- names(Scenario_paths)
  
  #get total amounts of change in whole map
  Scenario_freq <- freq(Scenario_LULC,
                        usenames =TRUE,
                        bylayer = TRUE) |> pivot_wider(values_from = count, names_from = layer)
  
  #add lulc class name
  Scenario_freq$LULC_class <- as.factor(sapply(Scenario_freq$value, function(i){
    LULC_rat[LULC_rat$ID ==i, "lulc_name"]}))
  
  #remove lakes and rivers
  Scenario_freq <- Scenario_freq[!(Scenario_freq$LULC_class %in% c("Lake", "River")),]
  
  #mask raster to building zone
  BZ_LULC <- terra::mask(Scenario_LULC, BZ_rast)
  
  #get frequency tables
  BZ_freq <- freq(BZ_LULC,
                        usenames =TRUE,
                        bylayer = TRUE) |> pivot_wider(values_from = count, names_from = layer)
  
  #add lulc class name
  BZ_freq$LULC_class <- as.factor(sapply(BZ_freq$value, function(i){
    LULC_rat[LULC_rat$ID ==i, "lulc_name"]}))
  
  #remove lakes and rivers
  BZ_freq <- BZ_freq[!(BZ_freq$LULC_class %in% c("Lake", "River")),]
  
  #calculate change in building zone as a % of total change in the whole study area
  BZ_freq$rel_change <- as.numeric(sapply(1:nrow(BZ_freq), function(i){
     ((BZ_freq[i,names(Scenario_paths)[2]]-BZ_freq[i,names(Scenario_paths)[1]])/(Scenario_freq[i,names(Scenario_paths)[2]]-Scenario_freq[i,names(Scenario_paths)[1]]))*100
   }))
  
  #tidy to clean label
  BZ_freq$clean_lab <- paste0(round(BZ_freq$rel_change,2), "%")
  
  return(BZ_freq)
}), idcol = "Scenario")

#subset to only settle ments
Urban_rel_change <- BZ_rel_change[BZ_rel_change$LULC_class == "Settlement/urban/amenities",]

### =========================================================================
### J- relative class area change within PAs: normative scenarios vs. explorative
### =========================================================================

#seperate names of normative scenarios
Norm_scenarios <- Scenario_names[2:4]

#list paths of all PA rasters
All_PA_paths <- list.files("data/Spat_prob_perturb_layers/Protected_areas/Future_PAs", full.names = TRUE)

#upper loop over time points
PA_change_results <- lapply(paste(Time_steps), function(x){
  
  #subset all LULC paths by time step
  Time_paths <- grep(x, LULC_paths, value = TRUE)
  
  #because PA rasters only upto 2055 use this year for 2060
  if(x == "2060"){PA_paths_step <- grep("2055", All_PA_paths, value = TRUE)}else{
  PA_paths_step <- grep(x, All_PA_paths, value = TRUE)}

  #load rasters for exploratory scenarios for this time step
  BAU_rast <- rast(grep("BAU", Time_paths, value = TRUE))
  GE_rast <- rast(grep("GR_EX", Time_paths, value = TRUE))
  
  #inner loop over normative scenarios
  Scenario_results <- lapply(Norm_scenarios, function(scen){
    
    #load scenario rast for time point
    Scen_rast <- rast(grep(scen, Time_paths, value = TRUE))
    
    #stack rasters
    CH_stack <- c(BAU_rast, GE_rast, Scen_rast)
    crs(CH_stack) <- ProjCH
    names(CH_stack) <- c("BAU", "GR_EX", "scenario")
    
    #calculate class area changes in whole map
    CH_freq <- freq(CH_stack,
                          usenames =TRUE,
                          bylayer = TRUE) |> pivot_wider(values_from = count, names_from = layer)
    
    #add columns for lulc class names
    CH_freq$LULC_class <- as.factor(sapply(CH_freq$value, function(i){
      LULC_rat[LULC_rat$ID ==i, "lulc_name"]}))
    
    #remove lakes and rivers
    CH_freq <- CH_freq[!(CH_freq$LULC_class %in% c("Lake", "River")),]
    
    #add ID col
    CH_freq$ID <- "CH"
    
    #load PA layer for scenario for time point
    
    Scen_PA_path <- grep(scen, PA_paths_step , value = TRUE)
    Scen_PA <- rast(Scen_PA_path)
    crs(Scen_PA) <- ProjCH
    
    #mask all layers by PA
    PA_stack <- terra::mask(CH_stack, Scen_PA)
    
    #calculate class area changes in PAs
    PA_freq <- freq(PA_stack,
                    usenames =TRUE,
                    bylayer = TRUE) |> pivot_wider(values_from = count, names_from = layer)
    
    #add columns for lulc class names
    PA_freq$LULC_class <- as.factor(sapply(PA_freq$value, function(i){
      LULC_rat[LULC_rat$ID ==i, "lulc_name"]}))
    
    #remove lakes and rivers
    PA_freq <- PA_freq[!(PA_freq$LULC_class %in% c("Lake", "River")),]
    
    #add ID col
    PA_freq$ID <- "PA"
    
    Scenario_result <- rbind(CH_freq, PA_freq)
  }) #close inner loop over scenarios
  names(Scenario_results) <- Norm_scenarios
  Scenario_results_bound <- as.data.frame(rbindlist(Scenario_results, idcol= "Scenario", fill = TRUE))
  
  return(Scenario_results_bound)
  
}) #close outer loop over time steps

#name according to time steps
names(PA_change_results) <- paste(Time_steps)

info_cols <- PA_change_results[["2020"]][,!colnames(PA_change_results[["2020"]]) %in% c("BAU", "GR_EX", "scenario")]
diffs <- PA_change_results[["2060"]][,c("BAU", "GR_EX", "scenario")]-PA_change_results[["2020"]][,c("BAU", "GR_EX", "scenario")]
res <- cbind(info_cols, diffs)
  
#calculate the relative change in PAs vs. CH for the BAU, GR_EX and the scenario
PA_start_end_rel_diffs <- data.frame(matrix(nrow = nrow(res[res$ID == "PA",]), ncol = 0))
PA_start_end_rel_diffs$BAU <- (res[res$ID == "PA", "BAU"]/res[res$ID == "CH", "BAU"])*100
PA_start_end_rel_diffs$GR_EX <- (res[res$ID == "PA", "GR_EX"]/res[res$ID == "CH", "GR_EX"])*100
PA_start_end_rel_diffs$scenario <- (res[res$ID == "PA", "scenario"]/res[res$ID == "CH", "scenario"])*100
PA_start_end_rel_diffs$Scenario_name <- res[res$ID == "PA", "Scenario"]
PA_start_end_rel_diffs$LULC <- res[res$ID == "PA", "LULC_class"]

PA_urban_changes <- PA_start_end_rel_diffs[PA_start_end_rel_diffs$LULC == "Settlement/urban/amenities",]  
PA_static_changes <- PA_start_end_rel_diffs[PA_start_end_rel_diffs$LULC == "Static class",] 



#calculate relative changes for all time points although this is flawed as the
#PA area changes across the time points and hence some values will be negative
#loop over time points calculating difference between values inside and outside PAs
# PA_diffs <- rbindlist(lapply(1:(length(PA_change_results)-1), function(i){
#   
#   info_cols <- PA_change_results[[i]][,!colnames(PA_change_results[[i]]) %in% c("BAU", "GR_EX", "scenario")]
#   diffs <- PA_change_results[[i+1]][,c("BAU", "GR_EX", "scenario")]-PA_change_results[[i]][,c("BAU", "GR_EX", "scenario")]
#   res <- cbind(info_cols, diffs)
#   
#   #calculate the relative change in PAs vs. CH for the BAU, GR_EX and the scenario
#   Rel_diffs <- data.frame(matrix(nrow = nrow(res[res$ID == "PA",]), ncol = 0))
#   Rel_diffs$BAU <- (res[res$ID == "PA", "BAU"]/res[res$ID == "CH", "BAU"])*100
#   Rel_diffs$GR_EX <- (res[res$ID == "PA", "GR_EX"]/res[res$ID == "CH", "GR_EX"])*100
#   Rel_diffs$scenario <- (res[res$ID == "PA", "scenario"]/res[res$ID == "CH", "scenario"])*100
#   Rel_diffs$Scenario_name <- res[res$ID == "PA", "Scenario"]
#   Rel_diffs$LULC <- res[res$ID == "PA", "LULC_class"]
#   Rel_diffs$Period <- paste0(Time_steps[i], "_", Time_steps[[i+1]])
#   
#   return(Rel_diffs)
# }))



