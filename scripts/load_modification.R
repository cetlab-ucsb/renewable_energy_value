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


## Add 3 days to Plexos data to match India 2030 load data
load_mumbai_2014_plexos <- add.days.to.Plexos(load_mumbai_2014_plexos, 3)
load_delhi_2014_plexos <- add.days.to.Plexos(load_delhi_2014_plexos, 3)

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

# Modify India load using Mumbai and Delhi loads. Peak Mumbai load is about 17.5 GW and that of Delhi is 5.8 GW
# Load will be normalized by subtracting the mean and diving by standard deviation to preserve the shape of the load curve. 
# Note that normalizing by changing the scale from 0 to 1 does not preserve shape
load_india_2030 <- data_standardization(load_india_2030, "load")
load_mumbai_2014 <- data_standardization(load_mumbai_2014, "mumbai")
load_delhi_2014 <- data_standardization(load_delhi_2014, "delhi")

# Delete dataTime column from India data because it's a character, not a double. Adopt the dateTime column from mumbai data
#load_india_2030[, dateTime := NULL]
# First merge India and Mumbai
load_india_2030_mod <- merge(load_india_2030, load_mumbai_2014[, -c("Day", "dateTime")])
# Then merge India and Delhi
load_india_2030_mod <- merge(load_india_2030_mod, load_delhi_2014[, -c("Day", "dateTime")])

# Modify India load profile
load_india_2030_mod[, load_mod_std := weight_india * load_std + weight_mumbai * mumbai_std + weight_delhi * delhi_std]
load_india_2030_mod[, load_mod := load_mod_std * sd(load) + mean(load)]

# Convert character date time to actual date time format and add month
load_india_2030_mod[, dateTime := mdy_hm(dateTime)]
#load_india_2030_mod[, Month := month(dateTime)]
#load_india_2030_mod[, Year := year(dateTime)]
#load_india_2030_mod[, Hour := hour(dateTime)]

# Check if total energy consumption is the same and compare the peak load
print(paste0("Original total energy consumption is ", as.integer(sum(load_india_2030_mod[, load]/10^6)), " TWh"))
print(paste0("New total energy consumption is ", as.integer(sum(load_india_2030_mod[, load_mod]/10^6)), " TWh"))

print(paste0("Original peak load is ", as.integer(max(load_india_2030_mod[, load]/10^3)), " GW"))
print(paste0("New peak load is ", as.integer(max(load_india_2030_mod[, load_mod]/10^3)), " GW"))

# Load by month and load by hour summary
load_summary_byMonth <- load_india_2030_mod[Day <= 365, lapply(.SD, sum), by = .(month(dateTime)), .SDcols = c("load", "load_mod")]
load_summary_byHour <- load_india_2030_mod[Day <= 365, lapply(.SD, sum), by = .(hour(dateTime)), .SDcols = c("load", "load_mod")]

load_summary_byMonth_toPlot <- melt(load_summary_byMonth, id.vars=c("month"), measure.vars=c("load", "load_mod"), variable.name="load_type", value.name="load")
load_summary_byHour_toPlot <- melt(load_summary_byHour, id.vars=c("hour"), measure.vars=c("load", "load_mod"), variable.name="load_type", value.name="load")

ggplot(load_summary_byMonth_toPlot, aes(x = month, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))

ggplot(load_summary_byHour_toPlot, aes(x = hour, y = load/10^6, fill=load_type)) + 
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#999999", "#E69F00"))


# Create the final data set by replacing the original load column with the modified column, delete other columns

