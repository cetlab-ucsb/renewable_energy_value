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
## WEIGHTS
weight_india <- 0.5
weight_mumbai <- 0.25
weight_delhi <- 0.25
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
       Darwin = {working.directory <- "/Volumes/RD Ext1/Electricity_Models/renewable_energy_value/"})
## Set working directory ##
setwd(working.directory)
####################################################################################################################

source("scripts/datetimeConversionFunctions.R")
source("scripts/load_forecast_functions.R")

#### Input data ###
load_india_2030 <- fread("india_REV_input/load2030_19EPS.csv")
load_india_2014 <- fread("india_REV_input/load2014.csv")
load_mumbai_2014_plexos <- fread(paste0(inputFolder, "/2014_load_Mumbai.csv"))
load_delhi_2014_plexos <- fread(paste0(inputFolder, "/DemandHourly2014-Delhi.csv"))

##################

##########################################################################################################################################
#### FUNCTION TO STANDARDIZE DATA ##########
#### Data is standardized by subtracting the mean and dividing by the standard deviation
data_standardization <- function(input_data, data_column){
  input_data[, paste0(data_column, "_std") := (get(data_column) - mean(get(data_column)))/sd(get(data_column))]
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
# Note that normalizing by changing the scale from 0 to 1 does not preserve shape
load_india <- data_standardization(load_india, "load")
load_mumbai_2014 <- data_standardization(load_mumbai_2014, "mumbai")
load_delhi_2014 <- data_standardization(load_delhi_2014, "delhi")

# First merge India and Mumbai
load_india_mod <- merge(load_india, load_mumbai_2014[, -c("Day", "dateTime")])
# Then merge India and Delhi
load_india_mod <- merge(load_india_mod, load_delhi_2014[, -c("Day", "dateTime")])

# Modify India load profile
load_india_mod[, load_mod_std := weight_india * load_std + weight_mumbai * mumbai_std + weight_delhi * delhi_std]
load_india_mod[, load_mod := load_mod_std * sd(load) + mean(load)]


# Check if total energy consumption is the same and compare the peak load
print(paste0("Original total energy consumption of unmodified base year is ", as.integer(sum(load_india_mod[, load]/10^6)), " TWh"))
print(paste0("New total energy consumption of modified base year is ", as.integer(sum(load_india_mod[, load_mod]/10^6)), " TWh"))

print(paste0("Original peak load of unmodified base year is ", as.integer(max(load_india_mod[, load]/10^3)), " GW"))
print(paste0("New peak load of modified base year is ", as.integer(max(load_india_mod[, load_mod]/10^3)), " GW"))

# India Load before and after modification by month and load by hour summary
# By month
load_summary_byMonth <- load_india_mod[Day <= 365, lapply(.SD, sum), by = .(month(dateTime)), .SDcols = c("load", "load_mod")]
load_summary_byMonth_toPlot <- melt(load_summary_byMonth, id.vars=c("month"), measure.vars=c("load", "load_mod"), variable.name="load_type", value.name="load")
ggplot(load_summary_byMonth_toPlot, aes(x = month, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))

#By Hour
load_summary_byHour <- load_india_mod[Day <= 365, lapply(.SD, sum), by = .(hour(dateTime)), .SDcols = c("load", "load_mod")]
load_summary_byHour_toPlot <- melt(load_summary_byHour, id.vars=c("hour"), measure.vars=c("load", "load_mod"), variable.name="load_type", value.name="load")
ggplot(load_summary_byHour_toPlot, aes(x = hour, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))

#By month and hour
load_summary_byMonth_byHour <- load_india_mod[Day <= 365, lapply(.SD, sum), by = .(month(dateTime), hour(dateTime)), .SDcols = c("load", "load_mod")]
load_summary_byMonth_byHour_toPlot <- melt(load_summary_byMonth_byHour, id.vars=c("month", "hour"), measure.vars=c("load", "load_mod"), variable.name="load_type", value.name="load")
ggplot(load_summary_byMonth_byHour_toPlot, aes(x = hour, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00")) +
  facet_wrap(~month, ncol=4)

# India, Delhi, Mumbai standardized load before modification and india load after modification
load_summary_IMD_byMonth_byHour <- load_india_mod[Day <= 365, lapply(.SD, sum), by = .(month(dateTime), hour(dateTime)), .SDcols = c("load_std", "mumbai_std", "delhi_std", "load_mod_std")]
load_summary_IMD_byMonth_byHour_toPlot <- melt(load_summary_IMD_byMonth_byHour, id.vars=c("month", "hour"), measure.vars=c("load_std", "mumbai_std", "delhi_std", "load_mod_std"), variable.name="load_type", value.name="load")
ggplot(load_summary_IMD_byMonth_byHour_toPlot, aes(x = hour, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~month, ncol=4)

# Create the final data set by replacing the original load column with the modified column, delete other columns
load_india_mod_base <- load_india_mod[, .(dateTime, load_mod)]
setnames(load_india_mod_base, c("load_mod"), c("load"))
load_india_mod_base[, Interval := hour(dateTime)+1][, Day:= day(dateTime)][, Month:= month(dateTime)][, Year:= year(dateTime)]
load_india_mod_base[, dateTime := NULL]

# Create an energy and peak load forecast based on the 2030 load. Columns include Year, state, energy, peakLoad. Energy is in GWh and peak load is in MW. So divide energy by 10^3.
# Treat "load" as state name.
energy_peakLoad_forecast <- data.table(Year = 2030, state = "load", energy = sum(as.numeric(load_india_2030[,load]))/10^3, peakLoad = max(load_india_2030[,load]))

# Load Forecast
# Treat "load" as state name so you don't have to change that column name again. 
load_india_mod_forecast <- linearExpFitTimeSeriesForecast(load_india_mod_base, energy_peakLoad_forecast, "load", 2014, 2030, expAdjustFactor, "Interval")
load_india_mod_forecast <- YMDIntervaltoDateTime(load_india_mod_forecast, "Interval")
load_india_mod_forecast[, Timepoint := seq(1, nrow(load_india_mod_forecast), 1)]
load_india_mod_forecast[, Day := rep(1:(nrow(load_india_mod_forecast)/24), each = 24)]
load_india_mod_forecast[, c("Year", "Month", "Interval") := NULL]

# Error differences in energy and peak load between modified and unmodified load
print(paste0("% difference between annual energy of modified and unmodified load forecast is ", (sum(as.numeric(load_india_mod_forecast[, load])) - sum(as.numeric(load_india_2030[, load])))/sum(as.numeric(load_india_2030[, load]))*100, " %."))
print(paste0("% difference between annual energy of modified and unmodified load forecast is ", (max(load_india_mod_forecast[, load]) - max(load_india_2030[, load]))/max(load_india_2030[, load])*100, " %."))

# Load forecast comparison
load_india_2030[, load_mod := load_india_mod_forecast[, load]]
plot_comparison_byMonth(load_india_2030[Day <=365,], "load", "load_mod", "load")
plot_comparison_byHour(load_india_2030[Day <=365,], "load", "load_mod", "load")
plot_comparison_byMonth_byHour(load_india_2030[Day <=365,], "load", "load_mod", "load")

## Plot functions
# By month
plot_comparison_byMonth <- function(input_data, column1, column2, yAxisLabel){
  data_summary_byMonth <- input_data[,lapply(.SD, sum), by = .(month(dateTime)), .SDcols = c(eval(column1), eval(column2))]
  data_summary_byMonth_toPlot <- melt(data_summary_byMonth, id.vars=c("month"), measure.vars=c(eval(column1), eval(column2)), variable.name="type", value.name="value")
  ggplot(data_summary_byMonth_toPlot, aes(x = month, y = value/10^6, fill=type)) + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))
}

#By Hour
plot_comparison_byHour <- function(input_data, column1, column2, yAxisLabel){
  data_summary_byHour <- input_data[,lapply(.SD, sum), by = .(hour(dateTime)), .SDcols = c(eval(column1), eval(column2))]
  data_summary_byHour_toPlot <- melt(data_summary_byHour, id.vars=c("hour"), measure.vars=c(eval(column1), eval(column2)), variable.name="type", value.name="value")
  ggplot(data_summary_byHour_toPlot, aes(x = hour, y = value/10^6, fill=type)) + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))
}

#By month and hour
plot_comparison_byMonth_byHour <- function(input_data, column1, column2, yAxisLabel){
  data_summary_byMonth_byHour <- load_india_mod[lapply(.SD, sum), by = .(month(dateTime), hour(dateTime)), .SDcols = c(eval(column1), eval(column2))]
  data_summary_byMonth_byHour_toPlot <- melt(load_summary_byMonth_byHour, id.vars=c("month", "hour"), measure.vars=c(eval(column1), eval(column2)), variable.name="type", value.name="value")
  ggplot(data_summary_byMonth_byHour_toPlot, aes(x = hour, y = value/10^6, fill=type)) + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00")) +
    facet_wrap(~month, ncol=4)
}

                                       