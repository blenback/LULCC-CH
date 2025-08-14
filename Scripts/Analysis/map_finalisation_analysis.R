# Load libraries
packs <- c("stringr", "terra", "future", "future.apply", "readxl",
           "data.table", "tidyr", "yaml", "dplyr", "viridis", "ggplot2",
           "tidyterra", "treemapify", "jsonlite", "magick", "grDevices")
invisible(lapply(packs, require, character.only = TRUE))

# source save_indexed_png function
source("Scripts/Functions/save_indexed_png.R")

#crs for maps
ProjCH <- "+proj=somerc +init=epsg:2056"

base_dir <- "E:/NCCS-SSP-results"
if(!dir.exists(base_dir)){
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
}

#define colour palette as list using LULC_rat$lulc_name as names
LULC_pal <- list("Urban/amenities" = '#a8aba5', #Urban
            "Static" = "#d1d3cf", #static
            "Open Forest" = "#97d1d5", #Open forest
            "Closed forest" = "#29898f", #closed forest
            "Overgrown/shrubland" = "#bb8a75", #Shrubland
            "Intensive agriculture" =  "#f59f78", #Intensive agriculture
            "Alpine pastures" = "#6ca147", #Alpine pastures
            "Grassland or meadows" = "#c4e0a1", #Grassland
            "Permanent crops" = "#DDCC66", #Permanet crops
            "Glacier" = "#d5f1ff",
            "River" = "#93d0ee",
            "Lake" = "#93d0ee")

prepare_lulc_files <- function(
    lulcc_input_dir = "X:/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC-NCCS/lulcc_output",
    image_dir = "map_images",
    raster_dir = "raster_data",
    chart_data_dir = "tabular_data",
    plot_dir = "summary_plots",
    base_dir = base_dir,
    Sim_ctrl_tbl_path = Sim_ctrl_tbl_path,
    ProjCH = ProjCH,
    glacier_index_dir = "E:/NCCS-SSP-data/Data/glacier_scenario_indices",
    LULC_agg_path = "LULCC_CH_HPC/Tools/LULC_class_aggregation.xlsx",
    colour_pal = LULC_pal,
    Non_agg_lulc_path = "Data/NOAS04_2018.tif",
    Use_parallel = FALSE,
    num_workers = 4,
    map_masks = NULL,
    overwrite = TRUE
){
  
  # load the simulation control table
  Sim_ctrl_tbl <- read.csv(Sim_ctrl_tbl_path, stringsAsFactors = FALSE)
  
  # Get earliest scenario start date and latest end date
  Start_date <- min(Sim_ctrl_tbl$Scenario_start.real)
  End_date <- max(Sim_ctrl_tbl$Scenario_end.real)
  
  # Create seq of scenario time steps with Step_length.real
  Sim_time_steps <- seq(Start_date, End_date, by = Sim_ctrl_tbl$Step_length.real[1])
  
  # Vector IDs of configurations to be analysed
  # Note Manually adjust this to analyse specific configurations
  Config_IDs <- unique(Sim_ctrl_tbl$Simulation_num.)
  
  # get unique scenarios from the simulation control table
  unique_scenarios <- unique(Sim_ctrl_tbl$Scenario_ID.string)
  
  # add the unique scenarios as sub-dirs to the image_dir, raster_dir and chart_data_dir and plot_dir
  for(scenario in unique_scenarios){
    if(!dir.exists(file.path(base_dir, image_dir, scenario))){
      dir.create(file.path(base_dir, image_dir, scenario), recursive = TRUE, showWarnings = FALSE)
    }
    if(!dir.exists(file.path(base_dir, raster_dir, scenario))){
      dir.create(file.path(base_dir, raster_dir, scenario), recursive = TRUE, showWarnings = FALSE)
    }
    if(!dir.exists(file.path(base_dir, chart_data_dir, scenario))){
      dir.create(file.path(base_dir, chart_data_dir, scenario), recursive = TRUE, showWarnings = FALSE)
    }
    if(!dir.exists(file.path(base_dir, plot_dir, scenario))){
      dir.create(file.path(base_dir, plot_dir, scenario), recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Load the LULC aggregation scheme
  Aggregation_scheme <- read_excel(LULC_agg_path)
  
  # Loop over config_IDs creating a dataframe with paths for saving the rasters, pngs and chart data
  lulc_paths <- lapply(Config_IDs, function(Config_ID){
    
    # get the scenario for the current Config_ID
    Scenario <- Sim_ctrl_tbl[Sim_ctrl_tbl$Simulation_num. == Config_ID, "Scenario_ID.string"]
    
    # Create a vector of file paths for each time step
    lulc_time_paths <- sapply(Sim_time_steps, function(Time_step){
      
      # Create the file path for the LULC layer: simulated_LULC_simID_179_year_2020
      lulc_step_path <- file.path(lulcc_input_dir, Scenario, paste0( "simulated_LULC_simID_", Scenario, "_year_", Time_step, ".tif"))
    })
    
    # use Config_ID to subset Sim_ctrl_tbl to get scenario details
    Config_details <- Sim_ctrl_tbl[Sim_ctrl_tbl$Simulation_num. == Config_ID, ]
    
    # model path structure: NCP-Time_step-Config_details$Climate_scenario.string-Config_details$Econ_scenario.string-Config_details$Pop_scenario.string-Config_details$Scenario_ID.string-full.tif’
    lulc_paths_tif <- sapply(Sim_time_steps, function(Time_step){
      
      # ‘scenario-time_step.tif’
      file.path(base_dir, raster_dir, Config_details$Scenario_ID.string,
                paste0(Config_details$Scenario_ID.string, "-", Time_step, ".tif")) 
    })
    
    lulc_paths_png <- sapply(Sim_time_steps, function(Time_step){
      # ‘scenario-time_step.tif’
      file.path(base_dir, image_dir, Config_details$Scenario_ID.string,
                paste0(Config_details$Scenario_ID.string, "-", Time_step, ".png")) 
    })
    
    lulc_paths_perc_area <- sapply(Sim_time_steps, function(Time_step){
      # ‘lulc-2020-rcp26-low-ref_central-bau-full.tif’
      file.path(base_dir, chart_data_dir, Config_details$Scenario_ID.string,
                paste0(Config_details$Scenario_ID.string, "-", Time_step, "_perc_area.csv")) 
    })
    
    # add a path for saving a plot of the perc_area
    lulc_paths_area_plot <- sapply(Sim_time_steps, function(Time_step){
      # ‘lulc-2020-rcp26-low-ref_central-bau-full.tif’
      file.path(base_dir, plot_dir, Config_details$Scenario_ID.string,
                paste0(Config_details$Scenario_ID.string, "-", Time_step, "_perc_area_plot.png")) 
    })
    
    # combine all vectors of paths in a dataframe
    lulc_time_paths <- data.frame(Path = lulc_time_paths,
                                  lulc_path_tif = lulc_paths_tif, 
                                  lulc_path_png = lulc_paths_png,
                                  lulc_path_perc_area = lulc_paths_perc_area,
                                  lulc_path_area_plot = lulc_paths_area_plot
    )
    
    # Add column for Config_ID
    lulc_time_paths$Config_ID <- Config_ID
    
    # Add column for Time_step
    lulc_time_paths$Time_step <- Sim_time_steps
    
    # add column for Scenario_ID
    lulc_time_paths$Scenario_ID <- Config_details$Scenario_ID.string
    
    # loop over NCP_time_paths and check which exist
    lulc_time_paths$Exists <- file.exists(lulc_time_paths$Path)
    
    return(lulc_time_paths)
  })
  
  # Bind list of dataframes into a single dataframe
  lulc_df <- do.call(rbind, lulc_paths)
  
  # check if all files exist
  # if (all(lulc_df$Exists)) {
  #   message("All LULC files exist.")
  # } else {
  #   stop("Some LULC files do not exist. Please check the paths.")
  # }
  
  # next step is to replace the glacier , rivers and lakes values in the LULC maps
  # Using one of the LULC rasters and the aggregation scheme to create a raster attribute table
  # get unique class values from raster and add 20 and 21 to represent Lake and River
  LULC_rat <- data.frame(ID = c(sort(unique(values(rast(lulc_df$Path[1])))), 20, 21))
  LULC_rat$lulc_name <- c(unlist(sapply(LULC_rat$ID, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class_short"])),simplify = TRUE)), "Lake", "River")  
  
  #subset aggregation table to distinct value of the aggregated LULC classes
  subset_agg <- Aggregation_scheme %>% distinct(Aggregated_ID, .keep_all=TRUE)
  
  #add colours to class info
  subset_agg$colours <- sapply(subset_agg$Aggregated_class_short, function(x){
    colour <- colour_pal[[paste(x)]]
  })
  
  LULC_rat$colour <- sapply(LULC_rat$lulc_name, function(x){
    colour <- colour_pal[[paste(x)]]
  })
  
  # Create a named vector for color mapping
  col_map <- setNames(LULC_rat$colour, LULC_rat$ID)
  
  #Load in most recent non-aggregated LULC raster
  ref_LULC <- rast(Non_agg_lulc_path)
  
  # get raster values of lakes and rivers
  mask_values <- unlist(Aggregation_scheme[Aggregation_scheme$NOAS04_class_ENG %in% c("Lakes", "Rivers"), "NOAS04_ID"])
  names(mask_values) <- c(20,21)
  
  #vector the pixel values of Glacier from the non_agg lulc layer
  Glacier_val <- 19
  Non_glacier_val <- 11
  
  # because we need to adjust the glacier locations in the reference lulc map
  #according to each scenario for efficiency use an outer loop over the scenarios
  # and an inner loop over the scenario specific simulations
  
  if(Use_parallel){
    plan(multisession, workers = num_workers)
  } else {
    plan(sequential)
  }
  
  future_sapply(unique(lulc_df$Scenario_ID), function(Scenario){
    
    cat(paste("Processing scenario:", Scenario, "\n"))
  
    # separate the scenario specific lulc_df
    scenario_lulc_df <- lulc_df[lulc_df$Scenario_ID == Scenario, ]
    
    # loop over the rows of scenario_lulc_df for this scenario 
    for(i in 1:nrow(scenario_lulc_df)){
      
      # print the simulation id and time step
      cat(paste("Processing simulation:", scenario_lulc_df$Config_ID[i], 
                "at time step:", scenario_lulc_df$Time_step[i], "\n"))
      
      # check if the tif file exists
      if (scenario_lulc_df$Exists[i]) {
        
        # read the raster layer
        lulc_layer <- rast(scenario_lulc_df$Path[i])
        
        # add the crs to the raster layer
        crs(lulc_layer) <- ProjCH

        #loop over lulc mask values
        for(j in 1:length(mask_values)){
          
          lulc_layer <- terra::mask(x = lulc_layer,
                                    mask = ref_LULC,
                                    maskvalues = mask_values[j],
                                    updatevalue = as.numeric(names(mask_values)[j]))
        } #close for loop over mask values
        
        # load the glacier index for this time step
        Time_step_glacier_index <- readRDS(file = list.files(glacier_index_dir,
                                               full.names = TRUE,
                                               pattern = Scenario))[,c("ID_loc", paste(scenario_lulc_df$Time_step[i]))]
        
        # get the non-glacier and glacier IDs for this time step
        Non_glacier_IDs <- Time_step_glacier_index[Time_step_glacier_index[[paste(scenario_lulc_df$Time_step[i])]]==0, "ID_loc"]
        Glacier_IDs <- Time_step_glacier_index[Time_step_glacier_index[[paste(scenario_lulc_df$Time_step[i])]]==1, "ID_loc"]
        
        # replace the glacier values in the raster layer with the non-glacier and glacier values
        lulc_layer[Non_glacier_IDs] <- Non_glacier_val
        lulc_layer[Glacier_IDs] <- Glacier_val
        
        #2nd step ensure that other glacial cells that do not match the glacier index
        #are also changed to static so that the transition rates calculate the
        #correct number of cell changes
        # Logical index of cells matching Glacier_val
        is_glacier_val <- lulc_layer[] == Glacier_val

        # Logical index of cells NOT in Glacier_IDs
        not_in_ids <- !(seq_len(ncell(lulc_layer)) %in% Glacier_IDs)

        # Combine both conditions
        to_replace <- is_glacier_val & not_in_ids

        # Replace
        lulc_layer[to_replace] <- Non_glacier_val

        
        # Now loop over any masks provided in the map_masks list
        if(length(map_masks) > 0){
          
          cat("Masking map to specificed areas \n")
          
          for(mask_name in names(map_masks)){
            
            cat(paste("Applying mask:", mask_name, "\n"))
            
            # Check if the mask file exists
            mask_path <- map_masks[[mask_name]]
            
            # If the mask file exists, apply it to the raster layer
            if(file.exists(mask_path)){
              
              # modify the save paths for the masked raster layer, png and json files
              mask_path_tif <- gsub("full", mask_name, scenario_lulc_df$lulc_path_tif[i])
              map_path_png <- gsub("full", mask_name, scenario_lulc_df$lulc_path_png[i])
              mask_path_data <- gsub("full", mask_name, scenario_lulc_df$lulc_path_perc_area[i])
              
              # if all of these files already exist and overwrite == FALSE, skip to the next iteration
              if(!overwrite && 
                 file.exists(mask_path_tif) && 
                 file.exists(map_path_png) && 
                 file.exists(mask_path_data)){
                message(paste("Files already exist, skipping:", mask_path_tif, map_path_png, mask_path_data))
                next
              }
              
              # if the mask path contains shp extension, read it as a vector
              if(grepl("\\.shp$", mask_path)){
                message(paste("Applying mask from shapefile:", mask_path))
                mask_layer <- vect(mask_path)
              } else if(grepl("\\.tif$", mask_path)){
                message(paste("Applying mask from raster file:", mask_path))
                mask_layer <- rast(mask_path)
              } else {
                stop(paste("Unsupported mask file type for:", mask_path))
              }
              
              # apply the mask to the raster layer
              masked_lulc <- terra::mask(x = lulc_layer, 
                                         mask = mask_layer, 
                                         updatevalue = NA)
              
              
              # update the file path for the masked raster layer by replacing 'full'
              # in scenario_lulc_df$lulc_path_tif[i] with the mask name
             
              
              # save the masked raster layer
              writeRaster(masked_lulc,
                          filename = mask_path_tif,
                          overwrite = TRUE)
              
              cat(paste("Saved masked raster layer to:", mask_path_tif, "\n"))
              
              # get frequency table of the masked raster layer
              rast_tbl <- freq(masked_lulc)
              rast_tbl$layer <- NULL
              rast_tbl$class_name <- c(unlist(sapply(rast_tbl$value, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class_short"])),simplify = TRUE)), "Lake", "River")
              rast_tbl$value <- NULL
              rast_tbl$perc_area <- rast_tbl$count / sum(rast_tbl$count) * 100
              rast_tbl$count <- NULL
              
              # modify the data path
              mask_path_data <- gsub("full", mask_name, scenario_lulc_df$lulc_path_perc_area[i])
              
              # convert the df to json
              json_data <- toJSON(setNames(as.list(rast_tbl$class_name), rast_tbl$perc_area), pretty = TRUE)
              
              # save the json data to the mask_path_data
              write(json_data, file = mask_path_data)
              
              cat(paste("Saved table of LULC % areas to:", mask_path_data, "\n"))
              
              # plot the raster layer matching the values to colours in LULC_rat
              
              # use the save_indexed_png function to save the raster layer as a png
              save_indexed_png(
                raster_obj = masked_lulc, 
                output_path = map_path_png, 
                color_palette = col_map,
                width = 25, 
                height = 20, 
                resolution = 300,
                units = "cm",
                margins = c(0, 0, 0, 0),
                background = "transparent",
                colorspace = "sRGB",
                max_colors = 256,
                show_legend = FALSE,
                axes = FALSE,
                box = FALSE,
                cleanup_temp = TRUE,
                verbose = FALSE)
              
              cat(paste("Saved map image to:", map_path_png, "\n"))
  
            } else {
              message(paste("Mask file does not exist:", mask_path))
            }
          }
        } else {
          message("No masks provided.")
          
          # save the raster layer as a tif file
          writeRaster(lulc_layer,
                      filename = scenario_lulc_df$lulc_path_tif[i],
                      overwrite = TRUE)
          
          cat(paste("Saved raster layer to:", scenario_lulc_df$lulc_path_tif[i], "\n"))
              
          # get frequency table of the masked raster layer
          rast_tbl <- freq(lulc_layer)
          rast_tbl$layer <- NULL
          rast_tbl$class_name <- c(unlist(sapply(rast_tbl$value, function(y) unique(unlist(Aggregation_scheme[Aggregation_scheme$Aggregated_ID == y, "Aggregated_class_short"])),simplify = TRUE)), "Lake", "River")
          rast_tbl$value <- NULL
          rast_tbl$perc_area <- rast_tbl$count / sum(rast_tbl$count) * 100
          rast_tbl$count <- NULL
          
          # sort by large to small
          rast_tbl <- rast_tbl %>% arrange(desc(perc_area))
          
          # make sure the class_name is a factor with levels in the current order
          rast_tbl$class_name <- factor(rast_tbl$class_name, levels = rast_tbl$class_name)
          
          
          # save the tbl as csv
          write.csv(rast_tbl, 
                    file = scenario_lulc_df$lulc_path_perc_area[i], 
                    row.names = FALSE)
          
          cat(paste("Saved table of LULC % areas to:", scenario_lulc_df$lulc_path_perc_area[i], "\n"))
          
          # create bar chart of the LULC % areas
          perc_area_plot <- ggplot(rast_tbl, aes(x = class_name, y = perc_area, fill = class_name)) +
            geom_bar(stat = "identity") +
            # add a label of the percentage area on top of each bar
            geom_text(aes(label = paste0(round(perc_area, 2), "%")), vjust = -0.5) +
            scale_fill_manual(values = LULC_pal) +
            labs(title = paste("LULC % areas for", Scenario, "at time step", scenario_lulc_df$Time_step[i]),
                 x = "LULC Class",
                 y = "% area of class coverage",
                 fill = "LULC Class"
                 ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(),
                  legend.position = "none")
          
          # save the plot ussing lulc_path_area_plot
          ggsave(filename = scenario_lulc_df$lulc_path_area_plot[i], 
                 plot = perc_area_plot, 
                 width = 10,
                 height = 6,
                 dpi = 300)
          
          # plot the raster layer matching the values to colours in LULC_rat
          
          # use the save_indexed_png function to save the raster layer as a png
          save_indexed_png(
            raster_obj = lulc_layer, 
            output_path = scenario_lulc_df$lulc_path_png[i], 
            color_palette = col_map,
            width = 25, 
            height = 20, 
            resolution = 300,
            units = "cm",
            margins = c(0, 0, 0, 0),
            background = "transparent",
            colorspace = "sRGB",
            max_colors = 256,
            show_legend = FALSE,
            axes = FALSE,
            box = FALSE,
            cleanup_temp = TRUE,
            verbose = FALSE)
          
          cat(paste("Saved map image to:", scenario_lulc_df$lulc_path_png[i], "\n"))
          
        }
      } else {
        message(paste("File does not exist:", scenario_lulc_df$Path[i]))
      }
    }
    
    # now that we have calculated the % area of each class for each time step
    # we can calculate the difference in % class area between simulation start and end
    
    # get the first and last time step for this scenario
    first_time_step <- min(scenario_lulc_df$Time_step)
    last_time_step <- max(scenario_lulc_df$Time_step)
    
    # loop over each unique configuration ID for this scenario
    for(Config_ID in unique(scenario_lulc_df$Config_ID)){
      
      # get the paths for the first and last time step for this Config_ID
      first_path <- scenario_lulc_df[scenario_lulc_df$Config_ID == Config_ID & 
                                      scenario_lulc_df$Time_step == first_time_step, "lulc_path_perc_area"]
      last_path <- scenario_lulc_df[scenario_lulc_df$Config_ID == Config_ID & 
                                     scenario_lulc_df$Time_step == last_time_step, "lulc_path_perc_area"]
      
      # read the first and last time step data
      first_data <- read.csv(first_path)
      last_data <- read.csv(last_path)
      
      # calculate the difference in % area for each class as a % of the area at the start
      area_change <- merge(first_data, last_data, by = "class_name", suffixes = c("_start", "_end"))
      area_change$perc_area_change <- ((area_change$perc_area_end - area_change$perc_area_start)/area_change$perc_area_start)*100
      
      
      # sort from largest to smallest change
      area_change <- area_change %>% arrange(desc(perc_area_change))
      
      # make sure the class_name is a factor with levels in the current order
      area_change$class_name <- factor(area_change$class_name, levels = area_change$class_name)
      
      # remove the lakes and rivers from the area change data
      area_change <- area_change[!area_change$class_name %in% c("Lake", "River"), ]
      
      # save the area change data to a new csv file
      area_change_path <- file.path(base_dir, chart_data_dir, Scenario, 
                                    paste0(Scenario, "-perc_area_change_", first_time_step, "_to_", last_time_step, ".csv"))
      write.csv(area_change, file = area_change_path, row.names = FALSE)
      
      cat(paste("Saved area change data to:", area_change_path, "\n"))
      
      # create a bar chart of the area change
      area_change_plot <- ggplot(area_change, aes(x = class_name, y = perc_area_change, fill = class_name)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = LULC_pal) +
        geom_text(aes(label = paste0(round(perc_area_change, 2), "%")), vjust = -0.5) +
        labs(title = paste("LULC % Area Change for", Scenario, 
                           "from", first_time_step, "to", last_time_step),
             y = "Change in % area") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              legend.position = "none")
    
      # save the area change plot
      area_change_plot_path <- file.path(base_dir, plot_dir, Scenario, 
                                         paste0(Scenario, "-area_change_", first_time_step, "_to_", last_time_step, ".png"))
      ggsave(filename = area_change_plot_path, 
             plot = area_change_plot, 
             width = 10,
             height = 6,
             dpi = 300)
    
    } # close for loop over unique Config_IDs
  }) # close future_sapply
} # close prepare_lulc_files function

# test function
prepare_lulc_files(
    lulcc_input_dir = "X:/CH_ValPar.CH/03_workspaces/07_Modeling/LULCC-NCCS/lulcc_output",
    image_dir = "map_images",
    raster_dir = "raster_data",
    chart_data_dir = "tabular_data",
    plot_dir = "summary_plots",
    base_dir = base_dir,
    Sim_ctrl_tbl_path = "Tools/Simulation_control.csv",
    ProjCH = ProjCH,
    glacier_index_dir = "E:/NCCS-SSP-data/Data/Glacial_change/Scenario_indices",
    LULC_agg_path = "Tools/LULC_class_aggregation.xlsx",
    colour_pal = LULC_pal,
    Non_agg_lulc_path = "E:/NCCS-SSP-data/Data/Historic_LULC/NOAS04_LULC/rasterized/NOAS04_2018.tif",
    Use_parallel = FALSE,
    num_workers = 4,
    map_masks = NULL,
    overwrite = TRUE
)
