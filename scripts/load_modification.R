## Author: Ranjit Deshmukh
## This script does the following
## 1) Reads in demand data from the Plexos formatted demand data files for India, Delhi, and Mumbai
## 2) Normalizes the data from 0 to 1

library(gdata)
library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
library(reshape2)
library(xlsx)


## INPUTS TO SCRIPT ##############################################################################################

inputFolder <- "india_data/load"
outputFolder <- "india_REV_input"
## WEIGHTS
weight_india <- c(1, 0.5, 0.5)
weight_mumbai <- c(0, 0.25, 0)
weight_delhi <- c(0, 0.25, 0.5)
india_load_profile_to_modify <- "2014" # Which year do you want to use as base load
forecast_load <- "yes" # Say yes if you use the present or past year as base load year. Say no, if you are modifying the forecast year directly.
minBaseToPeakRatio = 0.1 ## if the adjust base demand flag is yes, please specify the minimum base to peak ratio to which the low demand values will be adjusted to
expAdjustFactor = 5 ## This factor determined the rate of exponential decay in the Linear Exponential Fit function. 
movingAverageWindow = 5 ## This is the interval for moving average window. For a 15 minute dataset, this is equivalent to 75 minutes

## END OF INPUTS ##################################################################################################

#### set working directory #######################################################################################
# working.directory 
switch(Sys.info()[['sysname']],
       Windows= {"G:/Electricity_Models/renewable_energy_value/"},
       Linux  = {print("I'm a penguin.")},
       Darwin = {working.directory <- "/Volumes/RDExt1/Electricity_Models/renewable_energy_value/"})
## Set working directory ##
setwd(working.directory)
####################################################################################################################

source("scripts/datetimeConversionFunctions.R")
source("scripts/load_forecast_functions.R")

#### Input data ###
load_india_2030 <- fread(paste0(outputFolder, "/load2030_19EPS.csv"))
load_india_2014 <- fread(paste0(outputFolder,"/load2014.csv"))
load_mumbai_2014_plexos <- fread(paste0(inputFolder, "/2014_load_Mumbai.csv"))
load_delhi_2014_plexos <- fread(paste0(inputFolder, "/DemandHourly2014-Delhi.csv"))

##################

##########################################################################################################################################
#### FUNCTION TO STANDARDIZE DATA ##########
#### Data is standardized by subtracting the mean and dividing by the standard deviation. Essentially calculate the z score. 
data_standardization <- function(input_data, input_data_column, output_data_column){
  input_data[, eval(output_data_column) := (get(input_data_column) - mean(get(input_data_column)))/sd(get(input_data_column))]
  input_data
}

data_de_standardization <- function(input_data, input_data_column, output_data_column, output_mean, output_std_dev){
  input_data[, eval(output_data_column) := (get(input_data_column) * output_std_dev) + output_mean]
  input_data
}
################################################################################################################################

##########################################################################################################################################
#### FUNCTION TO NORMALIZE DATA from 0 and 1##########
data_normalization_01 <- function(input_data, input_data_column, output_data_column){
  input_data[, eval(output_data_column) := (get(input_data_column) - min(get(input_data_column)))/(max(get(input_data_column)) - min(get(input_data_column)))]
  input_data
}

data_de_normalization_01 <- function(input_data, input_data_column, output_data_column, output_min, output_max){
  input_data[, eval(output_data_column) := (get(input_data_column) - output_min)/(output_max - output_min)]
  input_data
}
################################################################################################################################

##########################################################################################################################################
#### FUNCTION TO NORMALIZE DATA BY DIVIDING BY MEAN ##########
data_normalization_mean <- function(input_data, input_data_column, output_data_column){
  input_data[, eval(output_data_column) := get(input_data_column)/mean(get(input_data_column))]
  input_data
}

data_de_normalization_mean <- function(input_data, input_data_column, output_data_column, output_mean){
  input_data[, eval(output_data_column) := get(input_data_column) * output_mean]
  input_data
}
################################################################################################################################



# Convert character date time to actual date time format and add month
load_india_2014 <- load_india_2014[, dateTime := mdy_hm(dateTime)]
load_india_2030 <- load_india_2030[, dateTime := mdy_hm(dateTime)]

## Add 3 days to Plexos data to match India 2030 load data
#load_mumbai_2014_plexos <- add.days.to.Plexos(load_mumbai_2014_plexos, 3)
#load_delhi_2014_plexos <- add.days.to.Plexos(load_delhi_2014_plexos, 3)

## Keep only 1 year of India demand data (no lookahead) 
load_india_2014 <- load_india_2014[year(dateTime) == "2014", ]
load_india_2030 <- load_india_2030[year(dateTime) == "2030", ]

## Convert Plexos data to datetime single row
load_mumbai_2014 <- PlexosToYMDinterval(load_mumbai_2014_plexos, "Interval", "mumbai")
load_delhi_2014 <- PlexosToYMDinterval(load_delhi_2014_plexos, "Interval", "delhi")

# Add dateTime column
load_mumbai_2014 <- YMDIntervaltoDateTime(load_mumbai_2014, "Interval")
load_delhi_2014 <- YMDIntervaltoDateTime(load_delhi_2014, "Interval")

# Delete Year, Month, Interval and add TimePoint and day of year
load_mumbai_2014[, Timepoint := seq(1, nrow(load_mumbai_2014), 1)]
load_mumbai_2014[, Day := rep(1:(nrow(load_mumbai_2014)/24), each = 24)]
load_mumbai_2014[, c("Year", "Month", "Interval") := NULL]
load_delhi_2014[, Timepoint := seq(1, nrow(load_delhi_2014), 1)]
load_delhi_2014[, Day := rep(1:(nrow(load_delhi_2014)/24), each = 24)]
load_delhi_2014[, c("Year", "Month", "Interval") := NULL]

# Choose which India load you want to use. 
if (india_load_profile_to_modify == "2014"){
  load_india <- load_india_2014
} else if(india_load_profile_to_modify == "2030"){
  load_india <- load_india_2030
} else {
  print("Error in india load specification")
}

# Modify India load using Mumbai and Delhi loads. Peak Mumbai load is about 17.5 GW and that of Delhi is 5.8 GW
# Load will be normalized by subtracting the mean and diving by standard deviation to preserve the shape of the load curve. 
# Or load can be normalized from 0 to 1 or by dividing the values by the mean. So the variations are multiplicative. 
# Note that normalizing by changing the scale from 0 to 1 does not preserve shape
# See https://stats.stackexchange.com/questions/138046/normalizations-dividing-by-mean
## Data Standardization
load_india <- data_standardization(load_india, "load", "load_std")
load_mumbai_2014 <- data_standardization(load_mumbai_2014, "mumbai", "mumbai_std")
load_delhi_2014 <- data_standardization(load_delhi_2014, "delhi", "delhi_std")

## Data Normalization 0 to 1
load_india <- data_normalization_01(load_india, "load", "load_std")
load_mumbai_2014 <- data_normalization_01(load_mumbai_2014, "mumbai", "mumbai_std")
load_delhi_2014 <- data_normalization_01(load_delhi_2014, "delhi", "delhi_std")

## Data Normalization Dividing by mean
load_india <- data_normalization_mean(load_india, "load", "load_std")
load_mumbai_2014 <- data_normalization_mean(load_mumbai_2014, "mumbai", "mumbai_std")
load_delhi_2014 <- data_normalization_mean(load_delhi_2014, "delhi", "delhi_std")

# First merge India and Mumbai
load_india_mod <- merge(load_india, load_mumbai_2014[, -c("Day", "dateTime")])
# Then merge India and Delhi
load_india_mod <- merge(load_india_mod, load_delhi_2014[, -c("Day", "dateTime")])

# Modify India load profile for current year
for (i in 1:length(weight_india)){
  # Different sets of weights
  load_india_mod[, paste0("load_mod_std_",i) := weight_india[i] * load_std + weight_mumbai[i] * mumbai_std + weight_delhi[i] * delhi_std]
  #load_india_mod[, paste0("load_mod_",i) := get(paste0("load_mod_std_",i)) * sd(load) + mean(load)]
  #load_india_mod <- data_de_standardization(load_india_mod, paste0("load_mod_std_",i), paste0("load_mod_",i), mean(load_india_mod[,load]), sd(load_india_mod[,load]))
  #load_india_mod <- data_de_normalization_01(load_india_mod, paste0("load_mod_std_",i), paste0("load_mod_",i), 0, max((load_india_mod[,load]))) # Use either 0 or minimum of load
  load_india_mod <- data_de_normalization_mean(load_india_mod, paste0("load_mod_std_",i), paste0("load_mod_",i), mean(load_india_mod[,load]))
  
  # Check if total energy consumption is the same and compare the peak load
  print(paste0("Original total energy consumption of unmodified base year is ", as.integer(sum(load_india_mod[, load]/10^6)), " TWh"))
  print(paste0("New total energy consumption of modified base year ", i," is ", as.integer(sum(load_india_mod[, get(paste0("load_mod_", i))]/10^6)), " TWh"))
  
  print(paste0("Original peak load of unmodified base year is ", as.integer(max(load_india_mod[, load]/10^3)), " GW"))
  print(paste0("New peak load of modified base year ", i," is ", as.integer(max(load_india_mod[, get(paste0("load_mod_", i))]/10^3)), " GW"))
  
}

# India, Delhi, Mumbai standardized load before modification and india load after modification
load_summary_IMD_byMonth_byHour <- load_india_mod[Day <= 365, lapply(.SD, sum), by = .(month(dateTime), hour(dateTime)), .SDcols = c("load_std", "mumbai_std", "delhi_std", "load_mod_std_1", "load_mod_std_2")]
load_summary_IMD_byMonth_byHour_toPlot <- melt(load_summary_IMD_byMonth_byHour, id.vars=c("month", "hour"), measure.vars=c("load_std", "mumbai_std", "delhi_std", "load_mod_std_1", "load_mod_std_2"), variable.name="load_type", value.name="load")
ggplot(load_summary_IMD_byMonth_byHour_toPlot, aes(x = hour, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~month, ncol=4)


## FORECAST LOAD FOR FUTURE YEAR
# Create an energy and peak load forecast based on the 2030 load. Columns include Year, state, energy, peakLoad. Energy is in GWh and peak load is in MW. So divide energy by 10^3.
# Treat "load" as state name.
energy_peakLoad_forecast <- data.table(Year = 2030, state = "india", energy = sum(as.numeric(load_india_2030[,load]))/10^3, peakLoad = max(load_india_2030[,load]))

load_india_forecasts_all <- copy(load_india_2030) 

base_columns <- c("load", "load_mod_1", "load_mod_2")
forecast_methods <- c("energyPeak", "energyOnly")

## Function to forecast load by specifying data (dateTime + input load column) and type of forecast (e.g. linear energy only - "energyOnly" or linear exponential fit - "energyPeak")
load_forecast_function <- function(input_data, method_forecast, input_column, forecast_region, energy_peakLoad_forecast){

  #setnames(load_india_mod_base, eval(paste0("load_mod_", i)), c("load"))
  input_data[, Interval := hour(dateTime)+1][, Day:= day(dateTime)][, Month:= month(dateTime)][, Year:= year(dateTime)]
  input_data[, dateTime := NULL]
  setnames(input_data, input_column, forecast_region)
  
  # Load Forecast
  # Treat input column name as state name so you don't have to change that column name again. 
  if (method_forecast == "energyPeak"){
    forecast_data <- linearExpFitTimeSeriesForecast(input_data, energy_peakLoad_forecast, forecast_region, 2014, 2030, expAdjustFactor, "Interval")
  }else if (method_forecast == "energyOnly"){
    forecast_data <- linearEnergyTimeSeriesForecast(input_data, energy_peakLoad_forecast, forecast_region, 2014, 2030, "Interval")
  }
  
  setnames(forecast_data, eval(forecast_region), eval(input_column))
  forecast_data <- YMDIntervaltoDateTime(forecast_data, "Interval")
  forecast_data[, Timepoint := seq(1, nrow(forecast_data), 1)]
  forecast_data[, Day := rep(1:(nrow(forecast_data)/24), each = 24)]
  forecast_data[, c("Year", "Month", "Interval") := NULL]
  forecast_data[, eval(input_column):= as.integer(get(input_column))]
  forecast_data
}

for(forecast_method in forecast_methods ){
  for(i in 1:length(weight_india)){
    print(forecast_method)
    input_column_name <- paste0("load_mod_", i)
    print(input_column_name)
    output_column_suffix <- paste0("D", weight_delhi[i], "_M", weight_mumbai[i], "_", forecast_method)
    load_base <- load_india_mod[, .SD, .SDcols = c("dateTime", eval(input_column_name))]
    load_india_forecast <- load_forecast_function(load_base, forecast_method, input_column_name, "india", energy_peakLoad_forecast)
    load_india_forecasts_all[, paste0("load_mod_", eval(output_column_suffix)) := load_india_forecast[, get(input_column_name)]]
    
    ## Write out csv files
    setnames(load_india_forecast, eval(input_column_name), "load")
    write.csv(load_india_forecast, file = paste0(outputFolder, "/load_forecasts/load2030_19EPS_mod_", output_column_suffix, ".csv"))
    
    # Error differences in energy and peak load between modified and unmodified load
    print(paste0("% difference between annual energy of modified and unmodified load forecast is ", (sum(as.numeric(load_india_forecasts_all[, get(paste0("load_mod_", output_column_suffix))])) - sum(as.numeric(load_india_forecasts_all[, load])))/sum(as.numeric(load_india_forecasts_all[, load]))*100, " %."))
    print(paste0("% difference between peak demand of modified and unmodified load forecast is ", (max(load_india_forecasts_all[, get(paste0("load_mod_", output_column_suffix))]) - max(load_india_forecasts_all[, load]))/max(load_india_forecasts_all[, load])*100, " %."))
    
  }
}

# Write out all forecasts in single file
write.csv(load_india_forecasts_all, file = paste0(outputFolder, "/load_forecasts/load2030_19EPS_mod_all", ".csv"))

###############



## Plot functions
# By month (Sum)
plot_comparison_byMonth <- function(input_data, input_columns, yAxisLabel, scenarios.labels, plot.colors){
  data_summary_byMonth <- input_data[,lapply(.SD, sum), by = .(month(dateTime)), .SDcols = input_columns]
  data_summary_byMonth_toPlot <- melt(data_summary_byMonth, id.vars=c("month"), measure.vars=input_columns, variable.name="type", value.name="value")
  ggplot(data_summary_byMonth_toPlot, aes(x = month, y = value/10^6, fill=type)) + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(labels = scenarios.labels, values=plot.colors) +
    labs(y = yAxisLabel, x = "Month") +
    scale_x_discrete(breaks=c(4,8,12)) +
    theme(legend.position="bottom", legend.title = element_blank()) 
}

#By Hour (Mean)
plot_comparison_byHour <- function(input_data, input_columns, yAxisLabel, scenarios.labels, plot.colors){
  data_summary_byHour <- input_data[,lapply(.SD, mean), by = .(hour(dateTime)), .SDcols = input_columns]
  data_summary_byHour_toPlot <- melt(data_summary_byHour, id.vars=c("hour"), measure.vars=input_columns, variable.name="type", value.name="value")
  ggplot(data_summary_byHour_toPlot, aes(x = hour, y = value/10^3, fill=type)) + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(labels = scenarios.labels, values=plot.colors) +
    labs(y = yAxisLabel, x = "Hour") +
    theme(legend.position="bottom", legend.title = element_blank()) 
}


#By month and hour (Mean)
plot_comparison_byMonth_byHour_line <- function(input_data, input_columns, scenarios.labels, yAxisLabel, plot.colors, plot.linetypes){
  data_summary_byMonth_byHour <- input_data[,lapply(.SD, mean), by = .(month(dateTime), hour(dateTime)), .SDcols = input_columns]
  data_summary_byMonth_byHour_toPlot <- melt(data_summary_byMonth_byHour, id.vars=c("month", "hour"), measure.vars=input_columns, variable.name="type", value.name="value")
  ggplot(data_summary_byMonth_byHour_toPlot, aes(x = hour, y = value/10^3, color = type, linetype = type)) + 
    geom_line(stat="identity") + 
    expand_limits(y = 0) +
    facet_wrap(~month, ncol=4) +
    labs(y = yAxisLabel, x = "Hour") +
    theme(legend.position="bottom", legend.title = element_blank()) +
    scale_color_manual(labels = scenarios.labels, values = plot.colors) +
    scale_linetype_manual(labels = scenarios.labels, values = plot.linetypes) +
    background_grid()
}

# Cumulative load distribution curves
#By month and hour (Mean)
plot_cum_load_distribution_curves <- function(input_data, input_columns, scenarios.labels, yAxisLabel, plot.colors, plot.linetypes){
  input_data_sorted <- input_data[, .SD, .SDcols = c("Timepoint")]
  for(input_column in input_columns){
    input_column_sorted <- as.data.table(input_data[, .SD, .SDcols = c(eval(input_column))])[order(-get(input_column))]
    input_data_sorted[, eval(input_column) := input_column_sorted] # sorted load from each column
  }
  data_to_plot <- melt(input_data_sorted, id.vars=c("Timepoint"), measure.vars=input_columns, variable.name="type", value.name="value")
  ggplot(data_to_plot, aes(x = Timepoint, y = value/10^3, color = type, linetype = type)) + 
    geom_line(stat="identity") + 
    expand_limits(y = 0) +
    labs(y = yAxisLabel, x = "Hour") +
    theme(legend.position="bottom", legend.title = element_blank()) +
    scale_color_manual(labels = scenarios.labels, values = plot.colors) +
    scale_linetype_manual(labels = scenarios.labels, values = plot.linetypes) +
    background_grid()
}

## Colors
colors_RD4 <- c("#fd8d3c", "black", "#2b8cbe", "#969696")
colors_RD5 <- c("#2b8cbe", "#fd8d3c", "#969696")
colors_RD7 <- c("#5588bb", "#66bbbb", "#aa6644", "#99bb55","#ee9944", "#444466", "#bb5555")
linetype_RD <- c("solid", rep("dashed", 6))
linetype_RD_dotted <- c("solid", rep("dotted", 6))

#### Load forecast comparison plots
columns.to.plot <- c("load", "load_mod_D0_M0_energyPeak", "load_mod_D0_M0_energyOnly", "load_mod_D0.25_M0.25_energyPeak", "load_mod_D0.25_M0.25_energyOnly", "load_mod_D0.5_M0_energyPeak", "load_mod_D0.5_M0_energyOnly")
labels.series <- c("CEA peak load and energy forecast orig", "CEA peak load and energy forecast", "CEA energy only forecast", "Modified 25% Delhi 25% Mumbai peak+energy", "Modified 25% Delhi 25% Mumbai energy only", "Modified 50% Delhi load peak+energy", "Modified 50% Delhi load energy only")

# Month-Hour 12-24 plots of load
plot_comparison_byMonth_byHour_line(load_india_forecasts_all[Day <=365,], columns.to.plot, labels.series, "Hourly average load (GW)", colors_RD7, linetype_RD)

# Cumulative load distribution curves
plot_cum_load_distribution_curves(load_india_forecasts_all[Day <=365,], columns.to.plot, labels.series, "Hourly load (GW)", colors_RD7, linetype_RD_dotted)
