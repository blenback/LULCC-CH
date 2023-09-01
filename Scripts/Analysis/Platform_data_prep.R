library(plotly)
library(networkD3)
library(tidyverse)
library(ggalluvial)
library(ggthemes)
library(viridis)

#Dir of Finaliyed LULC maps
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
Simulation_control <- Simulation_control[Simulation_control$Completed.string == "Y",]

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

#Load LULC aggregation table
LULC_agg <- readxl::read_xlsx("tools/LULC_class_aggregation.xlsx")

#subset aggregation table to distinct value of the aggregated LULC classes
subset_agg <- LULC_agg %>% distinct(Aggregated_ID, .keep_all=TRUE)

### =========================================================================
### A- Protected area data
### =========================================================================

#Get the 2020 and 2055 i.e. existing PA map paths for each scenario
PA_map_paths <- list.files("data/Spat_prob_perturb_layers/Protected_areas/Future_PAs", full.names = TRUE, pattern = paste0(c("2020","2055"), collapse = "|"))

#loop over scenario names and combine the rasters for each
for(i in Scenario_names[2:4]){
  
  #i = Scenario_names[2]
  
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
### B- Sankey diagram for start/end dates
### =========================================================================

#subset LULC paths to only 2020 and 2060
Start_end_LULC <- LULC_paths[grepl(paste0(c(Scenario_start, Scenario_end), collapse = "|"), LULC_paths)]

#loop over the scenarios
# Scenario_crosstabs_SE <- as.data.frame(rbindlist(lapply(Scenario_names, function(x){
#   
#   #extract the scenario paths and load as multi-layer raster
#   Scenario_LULC <- rast(grep(x, Start_end_LULC, value = TRUE))
#   
#   #cross tabulate the rasters
#   Scenario_tbl <- terra::crosstab(Scenario_LULC, long = TRUE)
#   
#   #rename columns
#   colnames(Scenario_tbl) <- c("From","To", "ncell")
#   
#   #remove entries for lakes and rivers
#   Scenario_tbl <- Scenario_tbl[!(Scenario_tbl$From %in% c(20:21)),]
#   
#   #calculate % of class_transitions and % of total_transitions
#   Scenario_tbl$Perc_class_change <- NA
#   Scenario_tbl$Perc_total_change <- NA
#   
#   for(i in 1:nrow(Scenario_tbl)){
#   
#     #calculate  ncell as % of total amount of changes
#     Scenario_tbl[i, "Perc_total_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl$ncell))*100
#   
#     #calculate  ncell as % of total amount of changes in 'from' class
#     Scenario_tbl[i, "Perc_class_change"] <- (Scenario_tbl[i, "ncell"]/sum(Scenario_tbl[Scenario_tbl$From == Scenario_tbl[i, "From"], "ncell"]))*100
#   }
#   
#   return(Scenario_tbl)
# 
# }), idcol = "Scenario"))

#save result
saveRDS(Scenario_crosstabs_SE, file = paste0(Sankey_dir, "/Scenario_crosstabulations_start_end_timepoints.rds"))

#Loop over scenarios creating sankey plots and saving tables
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
  
  #write.csv(Scenario_crosstabs[Scenario_crosstabs$Scenario == x,], file = paste0(Sankey_dir, "/", x, "_start_end_sankey_table.csv"), row.names = FALSE)
})

### =========================================================================
### C- Sankey diagram for all simulation dates
### =========================================================================

#Upper loop over the scenarios
# Scenario_crosstabs_all <- as.data.frame(rbindlist(lapply(Scenario_names, function(x){
#                                                      
#   #seperate paths for scenario
#   Scenario_paths <- grep(x, LULC_paths, value = TRUE)
#   
#   #extract the scenario paths and load as multi-layer raster
#   Scenario_LULC <- rast(Scenario_paths)
#   names(Scenario_LULC) <- str_match_all(Scenario_paths, paste(Time_steps))
#   
#   #Inner loop over time points: performing cross tabulation for each consecutive
#   #pair of time points in the raster stack
#   Cross_tab_all_years <- lapply(1:(nlyr(Scenario_LULC)-1), function(i){
#     
#     #combine rasters
#     comb <- c(Scenario_LULC[[i]], Scenario_LULC[[i+1]])
#     
#     #cross tabulate
#     Time_tbl <- terra::crosstab(comb, long = TRUE)
#     
#     
#     #rename columns
#     colnames(Time_tbl) <- c("From","To", "ncell")
#     
#     #remove entries for lakes and rivers
#     Time_tbl <- Time_tbl[!(Time_tbl$From %in% c(20:21)),]
#   
#     #calculate % of class_transitions and % of total_transitions
#     Time_tbl$Perc_class_change <- NA
#     Time_tbl$Perc_total_change <- NA
#   
#     for(i in 1:nrow(Time_tbl)){
#   
#       #calculate  ncell as % of total amount of changes
#       Time_tbl[i, "Perc_total_change"] <- (Time_tbl[i, "ncell"]/sum(Time_tbl$ncell))*100
#   
#       #calculate  ncell as % of total amount of changes in 'from' class
#       Time_tbl[i, "Perc_class_change"] <- (Time_tbl[i, "ncell"]/sum(Time_tbl[Time_tbl$From == Time_tbl[i, "From"], "ncell"]))*100
#     }
#     return(Time_tbl)
#     }) # close loop over time points
#   
#   #name the results by combining the timepoints of each layer
#   names(Cross_tab_all_years) <- sapply(1:(nlyr(Scenario_LULC)-1), function(i){
#     paste0(names(Scenario_LULC[[i]]), "-", names(Scenario_LULC[[i+1]]))
#     })
#   
#   #bind results together
#   Crosstab_bound <- rbindlist(Cross_tab_all_years, idcol = "years")
# }), idcol = "Scenario"))

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
  
  #colour palette
  pal <- list("Settlement/urban/amenities" = '#BB0011', #Urban
  "Static class" = "#DDDDDD", #static
  "Open Forest" = "#668822", #Open forest
  "Closed forest" = "#117733", #closed forest
  "Overgrown/shrubland/unproductive vegetation" = "#44AA88", #Shrubland
  "Intensive agriculture" =  "#FFDD44", #Intensive agriculture
  "Alpine pastures" = "#558877", #Alpine pastures
  "Grassland or meadows" = "#AADDCC", #Grassland
  "Permanent crops" = "#DDCC66", #Permanet crops
  "Glacier" = "#E8ECFB")
  
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





