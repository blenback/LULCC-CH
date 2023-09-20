### =========================================================================
### A- Preparation
### =========================================================================
# Set working directory
wpath<-"E:/LULCC_CH"
setwd(wpath)

#navigate to the working directory in the files pane for easy viewing
rstudioapi::filesPaneNavigate(wpath)

# Install packages if they are not already installed
packs<-c("foreach", "doMC", "data.table", "raster", "tidyverse", "testthat", "sjmisc", "tictoc", "parallel", "terra", "pbapply", "rgdal", "rgeos", "sf", "tiff")

new.packs<-packs[!(packs %in% installed.packages()[,"Package"])]

if(length(new.packs)) install.packages(new.packs)

# Load required packages
invisible(lapply(packs, require, character.only = TRUE))

# Source custom functions
invisible(sapply(list.files("Scripts/Functions",pattern = ".R", full.names = TRUE, recursive=TRUE), source))

#vector years of LULC data
LULC_years <- c("1985", "1997", "2009", "2018")

#Dataframe of LULC labels and values
LULC_classes <- data.frame(label = c("Urban", "Static", "Open_Forest",
                                      "Closed_Forest","Shrubland", "Int_AG",
                                      "Alp_Past", "Grassland", "Perm_crops", "Glacier"),
                           value = c(10,11,12,13,14,15,16,17,18,19))

#Historic LULC data folder path
LULC_folder <- "Data/Historic_LULC"


#Vector duration of time steps to be used in modelling
Step_length <- 5

#earliest possible model start time is 1985 and end time is 2060
#we have initially agreed to use 5 year time steps
Scenario_start <- 1985
Scenario_end <- 2060 

#vector sequence of time points and suffix
Time_steps <- seq(Scenario_start, Scenario_end, Step_length)

### =========================================================================
### B- Multistep trans rate table for calibration periods
### =========================================================================

#Table for single step trans rates in different periods has already been prepared
#also prepare the same table but for multistep rates

#Load multi-step net transition tables produced for historic periods
Calibration_multistep_tables <- lapply(list.files("Data/Transition_tables/raw_trans_tables", full.names = TRUE, pattern = "multistep"), read.csv)
names(Calibration_multistep_tables) <- str_remove_all(list.files("Data/Transition_tables/raw_trans_tables", full.names = FALSE, pattern = "multistep"), paste(c("Calibration_", "_multistep_trans_table.csv"), collapse = "|"))

#Add columns to the tables with the LULC class names to make them easier to interpret
Viable_transitions_by_period <- lapply(Calibration_multistep_tables, function(x){
  x$Initial_class <- sapply(x$From., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Final_class <- sapply(x$To., function(y) {LULC_classes[LULC_classes$value == y, c("label")]})
  x$Trans_name <- paste(x$Initial_class, x$Final_class, sep = "_")
  
  #subset by transitions from static class
  x <- x[x$Initial_class != "Static",]
  
  x$Trans_ID <- sprintf("%02d", 1:nrow(x))
  return(x)
  })

#merge the trans_tables for possible extrapolation of rates over time
Trans_tables_bound <- rbindlist(Viable_transitions_by_period, idcol = "Period")
Trans_tables_bound$Trans_ID <- NULL
Trans_table_time <- pivot_wider(data = Trans_tables_bound, names_from = "Period", values_from = "Rate")

#because the same inclusion threshold does not apply to the multi-step rates
#instead subset by the trans names in the single step equivalent table
single_step_table <- read.csv("Data/Transition_tables/trans_rates_table_calibration_periods.csv")
trans_names <- single_step_table[!is.na(single_step_table$X2009_2018), "Trans_name"]

Trans_table_time <- Trans_table_time[Trans_table_time$Trans_name %in% trans_names,]

#save
write.csv(Trans_table_time, "Data/Transition_tables/trans_rates_table_calibration_periods_MS.csv")

### =========================================================================
### B- Performing extrapolation of trans rates until 2060
### =========================================================================

Extrap_dir <- "Data/Transition_tables/Extrapolations" 
dir.create(Extrap_dir)

#instantiate function to do extrapolation of trans rates and plotting
transratesextrapolation <- function(trans_rate_table_path, plot_dir, single_multi_step){

save_dir <- paste0(Extrap_dir, "/", single_multi_step)  
dir.create(save_dir)

trans_rate_table <- read.csv(trans_rate_table_path)

#exclude any rows with NAs in the 2009_2018 columns as these are transitions
#that are not modelled in the future
trans_rate_table <- trans_rate_table[!is.na(trans_rate_table$X2009_2018),]

#calculate average net transition rate over historical period
trans_rate_table$Rate <- rowMeans(trans_rate_table[c("X1985_1997", "X1997_2009", "X2009_2018")], na.rm = TRUE)

#subset to just the columns necessary for Dinamica
Dummy_BAU_trans_table <- trans_rate_table[c("From.", "To.", "Rate")]
Dummy_BAU_trans_table$ID <- paste(Dummy_BAU_trans_table$From., Dummy_BAU_trans_table$To., sep = "_")

#extrapolating rates for future time steps

#rename period columns to single date to create an interval variable
names(trans_rate_table)[7:9] <- c("1997", "2009", "2018")

#pivot to long format
trans_rates_long <- trans_rate_table %>% pivot_longer(cols = c("1997", "2009", "2018"),
                                                            names_to = "Year",
                                                            values_to = "Perc_rate")
trans_rates_long$Year <- as.numeric(trans_rates_long$Year)

#subset time steps to only those for simulations (>2020)
Simulation_steps <- Time_steps[Time_steps >=2020]

#create a df from these used for predicting
Pred_df <- as.data.frame(Simulation_steps)
names(Pred_df) <- "Year"

#create a duplicate table for storing extrapolated values with a column of
#unique Trans_IDs and a column for each simulation step
Extrap_calibration_rates <- data.frame(matrix(nrow = length(unique(trans_rates_long$Trans_name)),
                                                                   ncol = length(Simulation_steps)+1))
colnames(Extrap_calibration_rates) <- c("Trans_name", Simulation_steps)
Extrap_calibration_rates$Trans_name <- unique(trans_rate_table$Trans_name)

#loop over unique trans_IDs, create a linear model for the perc_rate
#and use it to predict future time points

#upper loop over Trans_names
for(Name in unique(trans_rates_long$Trans_name)){
  
#create model
Mod <-   lm(formula = Perc_rate ~ Year,
     data = trans_rates_long[trans_rates_long$Trans_name == Name,])

#predict
Pred <- predict(Mod, newdata = Pred_df)

#append to results df
Extrap_calibration_rates[Extrap_calibration_rates$Trans_name == Name, c(2:ncol(Extrap_calibration_rates))] <- Pred

#create scatterplot with fitted regression line
Trans_plot <- ggplot(trans_rates_long[trans_rates_long$Trans_name == Name,], aes(x = Year, y = Perc_rate)) + 
  geom_point() +
  stat_smooth(method = "lm")

#save plot
ggsave(filename = paste0(Name, "_", single_multi_step, "_trans_rate_plot.jpg") ,
       plot = Trans_plot,
       device = "jpeg",
       path = save_dir)
}

#combine historical and extrapolated values
Combined_table <- cbind(trans_rate_table, Extrap_calibration_rates[,c(paste(Simulation_steps))])
Combined_table$Rate <- NULL

#save
write.xlsx(Combined_table, paste0(save_dir, "/Extrapolated_trans_rates_", single_multi_step, ".xlsx"))
} #close function

transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_periods.csv",
                        single_multi_step = "single_step")

transratesextrapolation(trans_rate_table_path = "Data/Transition_tables/trans_rates_table_calibration_periods_MS.csv",
                        single_multi_step = "multi_step")



