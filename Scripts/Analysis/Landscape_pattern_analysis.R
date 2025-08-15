#############################################################################
## Landscape_pattern_analysis: Calaculating landscape metrics from simulated LULC maps 
## and producing visualisations
##
## Date: 25-6-2023
## Author: Ben Black
#############################################################################

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
         "leafem", "mapview", "webshot2", "magick", "png", "ggpubr", "extrafont", "tools", "grid", "ghibli")

#install new packages
new.packs <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

#Import fonts using extrafont (only needs to be done once)
#font_import()

#send model tool vars to global environment
list2env(readRDS("Tools/Model_tool_vars.rds"), .GlobalEnv)

#Dir of finalized maps
Final_map_dir <- "Results/Finalised_LULC_maps"

#crs for maps
ProjCH <- "+proj=somerc +init=epsg:2056"

#Dir for analysis results
LSM_dir <- "Results/Landscape_pattern_analysis"
dir.create(LSM_dir)

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

#get time steps
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

#list of final LULC rasters 
LULC_paths <- list.files(Final_map_dir, full.names = TRUE, pattern = ".tif")
names(LULC_paths) <- basename(LULC_paths) 

#subset LULC paths to only 2020 and 2060
Start_end_LULC <- LULC_paths[grepl(paste0(c(Scenario_start, Scenario_end), collapse = "|"), LULC_paths)]

### =========================================================================
### A- Checking implementation of deterministic transitions
### =========================================================================

#get file paths of end of simulation rasters
Final_raster_paths <- list.files("Results/Dinamica_simulated_lulc",
           full.names = TRUE,
           recursive = TRUE,
           pattern = paste(Scenario_end))

#remove tif.aux files
Final_raster_paths <- Final_raster_paths[!grepl(".aux", Final_raster_paths)]

#load as stack
Final_lulc_stack <- stack(Final_raster_paths)
names(Final_lulc_stack) <- str_match(Final_raster_paths, paste0(Scenario_names, collapse = "|"))

#get frequency tables
Rast_freq <- freq(Final_lulc_stack, merge=TRUE)

#remove row for NA
Rast_freq <- Rast_freq[1:(nrow(Rast_freq)-1),]

#add column for class names
LULC_agg_scheme <- readxl::read_excel(LULC_aggregation_path)

Rast_freq$LULC_name <- sapply(Rast_freq$value, function(x){
  unique(LULC_agg_scheme[LULC_agg_scheme$Aggregated_ID == x, "Class_abbreviation"])})

#convert cell counts to %'s
Total_area <- sum(Rast_freq$BAU)

LULC_percs <- Rast_freq
LULC_percs[,Scenario_names] <- apply(LULC_percs[,Scenario_names], c(1,2), function(x){(x/Total_area)*100})

#check expected glacier amount in 2060
Glacier_index <- readRDS("Data/Glacial_change/Scenario_indices/GR_EX_glacial_change.rds")
Glacier_ncells_2060 <- length(which(Glacier_index[["2060"]]==1))

### =========================================================================
### B- Calculate landscape metrics in whole map
### =========================================================================

#identify relevant landscape metrics at class and landscape level
class_metrics <- list_lsm(level = "class", type = c("aggregation metric", "complexity metric"))
lscape_metrics <-list_lsm(level = "landscape", type = c("aggregation metric", "complexity metric")) 
lscape_metrics <-lscape_metrics[lscape_metrics$type == "aggregation metric" | lscape_metrics$metric == "contag",]

#Create DF with info on desired metrics at both class and landscape level
All_lsms <- rbind(class_metrics, lscape_metrics)
All_lsms$metric_clean_name <- str_to_sentence(All_lsms$name)

#No easy programmatic way to get units or clean names so do so manually from help pages 
LSM_units <- list("ai" = "Aggregation index value (%)",
              "clumpy"= "Clumpiness index value (-1 <= value <= 1)",
              "cohesion"= "Patch cohesion index value (%)",
              "division"= "Landscape division index (LDI) value(0 <= LDI < 1)",
              "enn_cv" = "Coefficient of variation of euclidean nearest-neighbor distance (Meters)",
              "enn_mn"= "Mean of euclidean nearest-neighbor distance (Meters)",
              "enn_sd"="Standard deviation of euclidean nearest-neighbor distance (Meters)",
              "iji"= "Interspersion and Juxtaposition index value %", 
              "lsi"= "Landscape shape index (Unitless)",
              "mesh"= "Effective mesh size (Ha)",
              "nlsi"= "Normalized Landscape shape index (nlsi: 0 <= nlsi <= 1)",
              "np" = "Number of patches of class",
              "pd" = "Patch density per 100 hectares",
              "pladj" = "Percentage of Like Adjacencies (0 <= PLADJ <= 100)",
              "split" = "Splitting index value (unitless)",
              "contag"= "Contagion value (%)" )

All_lsms$units <- sapply(All_lsms$metric, function(x){
  unit <- unname(unlist(LSM_units[names(LSM_units) == x]))
})

#save df of LSM info
saveRDS(All_lsms, paste0(LSM_dir,"/LSMs_info.rds"))
All_lsms <- readRDS(paste0(LSM_dir,"/LSMs_info.rds"))

#calculate both class level and landscape level metrics on all LULC layers
# Do it in parallel as the metrics are time consuming
plan(multisession, workers = availableCores()-2)

#Parallel loop over LULC paths
LSM_res <- future_lapply(LULC_paths, function(LULC_path){
      
    #Load raster
    LULC_rast <- rast(LULC_path)
      
    #calculate metrics
    Layer_res <- as.data.frame(calculate_lsm(LULC_rast,
                                               what = All_lsms[,"Function_name"],
                                               directions = 8,
                                               neighbourhood = 8))
}) 
plan(sequential)

#bind to df
LSM_res <- rbindlist(LSM_res, idcol = "path")

#Extract scenario and time point info to cols
LSM_res$Scenario <- sapply(LSM_res$path, function(x){
  str_replace(na.omit(str_match(x, Scenario_names)), "_", "-")
})

LSM_res$Year <- sapply(LSM_res$path, function(x){
  na.omit(str_match(x, paste0(Time_steps)))
})

#exclude all results from the rivers and lakes classes
LSM_res <- LSM_res[!(LSM_res$class %in% c("20", "21")),]

#Load LULC aggregation table
LULC_agg <- readxl::read_xlsx("tools/LULC_class_aggregation.xlsx")

#Add column for clean LULC class name
LSM_res$LULC <- as.character(sapply(LSM_res$class, function(x){
  unlist(unique(LULC_agg[LULC_agg$Aggregated_ID == x, "Aggregated_class"]))
}, simplify = TRUE))

#save table for use in publication visualisations
saveRDS(LSM_res, paste0(LSM_dir,"/Simulated_LULC_LSMs_results.rds"))
LSM_res <- readRDS(paste0(LSM_dir,"/Simulated_LULC_LSMs_results.rds"))

### =========================================================================
### B.2- Calculate landscape metrics in PAs
### =========================================================================

#seperate names of normative scenarios
Norm_scenarios <- Scenario_names[2:4]

#subset LSMs
Subset_lsms <- All_lsms[All_lsms$metric %in% c("cohesion", "contag", "enn_mn", "mesh", "np", "pd"),]
metrics <- as.character(unlist(Subset_lsms[,"function_name"]))

#list paths of all PA rasters
All_PA_paths <- list.files("data/Spat_prob_perturb_layers/Protected_areas/Future_PAs", full.names = TRUE)

#upper loop over the start and end date
LSM_PAs <- lapply(paste(c(Scenario_start, Scenario_end)), function(x){
  
  #subset all LULC paths by time step
  Time_paths <- grep(x, LULC_paths, value = TRUE)
  
  #because PA rasters only upto 2055 use this year for 2060
  if(x == "2060"){PA_paths_step <- grep("2055", All_PA_paths, value = TRUE)}else{
  PA_paths_step <- grep(x, All_PA_paths, value = TRUE)}

  #load rasters for exploratory scenarios for this time step
  BAU_rast <- raster(grep("BAU", Time_paths, value = TRUE))
  GE_rast <- raster(grep("GR_EX", Time_paths, value = TRUE))
  
  plan(multisession, workers = availableCores()-2)
  
  #inner loop over normative scenarios
  Scenario_results <- future_lapply(Norm_scenarios, function(scen){
    
    #load scenario rast for time point
    Scen_rast <- raster(grep(scen, Time_paths, value = TRUE))
    
    #stack rasters
    CH_stack <- stack(BAU_rast, GE_rast, Scen_rast)
    crs(CH_stack) <- ProjCH
    layer_names <- c("BAU", "GR_EX", "scenario")
    names(CH_stack) <- layer_names
    
    #load PA layer for scenario for time point
    Scen_PA_path <- grep(scen, PA_paths_step , value = TRUE)
    Scen_PA <- raster(Scen_PA_path)
    crs(Scen_PA) <- ProjCH
    
    #mask all layers by PA
    PA_stack <- raster::mask(CH_stack, Scen_PA)
    
    #calculate landscape metrics
    Layer_res <- as.data.frame(calculate_lsm(PA_stack,
                                               what = metrics,
                                               directions = 8,
                                               neighbourhood = 8))
    #add scenario names instead of layer
    Layer_res$scenario <- sapply(Layer_res$layer, function(x){
      layer_names[x]
    })
    
    return(Layer_res)
  }) #close inner loop over scenarios
  plan(sequential)
  
  names(Scenario_results) <- Norm_scenarios
  Scenario_results_bound <- as.data.frame(rbindlist(Scenario_results, idcol= "PA_scenario", fill = TRUE))
  
  return(Scenario_results_bound)
  
}) #close outer loop over time steps
names(LSM_PAs) <- paste(c(Scenario_start, Scenario_end))

#bind to df
LSM_PAs <- rbindlist(LSM_PAs, idcol = "year")

#exclude all results from the rivers and lakes classes
LSM_PAs <- LSM_PAs[!(LSM_PAs$class %in% c("20", "21")),]

#Load LULC aggregation table
LULC_agg <- readxl::read_xlsx("tools/LULC_class_aggregation.xlsx")

#Add column for clean LULC class name
LSM_PAs$LULC <- as.character(sapply(LSM_PAs$class, function(x){
  unlist(unique(LULC_agg[LULC_agg$Aggregated_ID == x, "Aggregated_class"]))
}, simplify = TRUE))

#save table for use in publication visualisations
saveRDS(LSM_PAs, paste0(LSM_dir,"/Simulated_LULC_LSMs_PAs_results.rds"))
LSM_PAs <- readRDS(paste0(LSM_dir,"/Simulated_LULC_LSMs_PAs_results.rds"))


### =========================================================================
### c- Visualisation of landscape level metrics
### =========================================================================

# Testing: Plot landscape level metric results across time points/scenarios
lscape_plots <- lapply(unlist(unique(LSM_res[LSM_res$level == "landscape", "metric"])), function(metric_name){

  Metric_plot <- LSM_res[LSM_res$level == "landscape" & LSM_res$metric == metric_name & LSM_res$Year != "2020",] |> ggplot()+ 
    geom_line(aes(x= Year, y= value, group = Scenario, colour = Scenario), alpha = 0.7, size = 1)+
    geom_point(aes(x= Year, y= value, group = Scenario, colour = Scenario), alpha = 0.7, size = 2)+
      labs(y = unlist(All_lsms[All_lsms$metric == metric_name, "units"]),
        x= "Simulation year",
        colour = "Scenario",
        #title = All_lsms[All_lsms$metric == metric_name, "metric_clean_name"]
        )+
      theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = rel(1.1), hjust = -0.45),
        axis.line = element_line(1),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 7),
        legend.title = element_text(size=8, face = "bold"), #change legend title font size
        legend.text = element_text(size=8), #change legend text font size
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.title.align=0.5,
        legend.position="bottom")+
    scale_colour_ghibli_d("MononokeMedium")
  
  ggsave(plot = Metric_plot, filename = paste0(LSM_dir, "/landscape_", metric_name, "_plot.tif"), device='tiff', dpi=300,  width = 15, height = 15, units = "cm")
  return(Metric_plot)
})
names(lscape_plots) <- unlist(unique(LSM_res[LSM_res$level == "landscape", "metric"]))

#combining plots with patchwork
#Define common x axis label
gg_axis <- cowplot::get_plot_component(ggplot() +
                                         labs(x = "Simulation year"), "xlab-b")

#Metric_plots <- (wrap_plots(lscape_plots, nrow =3) & theme(legend.position = "right") & labs(x = NULL, title = NULL)) + plot_annotation(tag_levels = list(c('A.', 'B.', 'C.')))
Metric_plots <- (lscape_plots[[1]] / lscape_plots[[2]] / lscape_plots[[3]] + lscape_plots[[4]] + lscape_plots[[5]] + lscape_plots[[6]] & theme(legend.position = "right") & labs(x = NULL, title = NULL)) + plot_layout(ncol =2, nrow = 3)
Metric_plus_axis <- Metric_plots / gg_axis 
lscape_metrics_combined <- Metric_plus_axis + plot_layout(heights = unit(c(8,8,8,1), c('cm')), guides = "collect") + plot_annotation(tag_levels = list(c('A.', 'B.', 'C.', 'D.', 'E.', 'F.'))) &
  theme(axis.title.y = element_text(size = 6)) & 
  theme(plot.tag = element_text(size = 6, face = "bold"))

plot(lscape_metrics_combined)
#save plot
ggsave(plot = lscape_metrics_combined , filename = paste0(LSM_dir, "/LSMs_landscape_plot_portait.tif"), device='tiff', dpi=300,  width = 15, height = 25, units = "cm")

### =========================================================================
### D- Visualisation of LULC class level metrics
### =========================================================================

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


class_plots <- lapply(unlist(unique(LSM_res[LSM_res$level == "class", "metric"])), function(metric_name){
  
  Metric_plot <- LSM_res[LSM_res$level == "class" & LSM_res$metric == metric_name & LSM_res$Year == "2060" & LSM_res$LULC != "Glacier",] |> ggplot()+ 
    geom_point(aes(x= Scenario, y= value, colour = LULC), alpha = 0.7, size = 2)+
    labs(y = unlist(All_lsms[All_lsms$metric == metric_name, "units"]),
         x= "Scenario",
         colour = "LULC class",
         #title = All_lsms[All_lsms$Metric_abb == metric_name, "Metric_clean_name"]
         )+
    theme(text = element_text(family = "Times New Roman"),
          plot.title = element_text(size = rel(1.1), hjust = -0.45),
          axis.line = element_line(1),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(size = 7),
          legend.title = element_text(size=8, face = "bold"), #change legend title font size
          legend.text = element_text(size=8), #change legend text font size
          legend.key = element_rect(fill = "white", colour = "white"),
          legend.title.align=0.5,
          legend.position="right")+
    scale_colour_manual(name = "LULC class", values = unlist(pal))

  
  ggsave(plot = Metric_plot, filename = paste0(LSM_dir, "/class_", metric_name, "_plot.tif"), device='tiff', dpi=300)
  #,  width = 17, height = 17, units = "cm"
return(Metric_plot)
})





