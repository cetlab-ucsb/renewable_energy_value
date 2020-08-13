## Author: Ranjit Deshmukh 11/18/2016
## This script does the following
## 1) Screening curves and net load curves


## Revision history
## v0 - Basic script


library(gdata)
library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
library(reshape2)
library(xlsx)
library(Hmisc)
library(gridExtra)
library(xts)
library(stringr)
library(cowplot)
library(dplyr)
library(rgdal) # for maps
library(ggmap) # comes with a them_nothing() function
library(Cairo) # creates high quality vector and bitmap images
library(rgeos) # for maps
library(maptools) # for maps
library(bit64)
library(csvread)
library(scales)


## INPUTS TO SCRIPT ##############################################################################################

input.folder <- "../india_REV_input/"
output.folder <- "../india_REV_output/"
results.folder <- "results/processed_results_coalLC/"
input.results.fname <- "results_all_scenarios_lowCapCostCoal_55min_07282017.csv"
screening.curve.plot.fname <- "screening_curve_plot.csv"
INR_USD = 65

## Set working directory ##
setwd('/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/scripts/')

# -----------------------------------------------------------------------------|
# FUNCTION TO GRAB LEGEND
#------------------------------------------------------------------------------|
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#------------------------------------------------------------------------------|


## BEGIN SCRIPT #####################
# Import the results and other parameters
results.input <- fread(paste0(output.folder, input.results.fname))
results.input <- results.input[order(rank)]
screening.curve.plot.input <- fread(paste0(input.folder, screening.curve.plot.fname))

net_load <- NULL

for(sc in 1:length(results[,scenario])){
  # Read net load files
  net_load_scenario <- fread(paste0(input.folder, "net_load/", "net_load_hydro_", results[,scenario][sc], ".csv"))
  net_load_scenario[, scenario:= results[,scenario][sc]]
  net_load_scenario <- net_load_scenario[order(-net_load_final)]
  net_load_scenario[, Timepoint_sorted:= seq(1:8760)]
  net_load <- rbind(net_load, net_load_scenario)
}

# Merge with scenario_build
net_load <- merge(net_load, results[, .(scenario, scenario_build, scenario_split)], by.x = "scenario", by.y = "scenario")

# COnverst net load MW to GW
net_load[,net_load_final:= net_load_final/10^3]

## Plots
## High Coal Capital cost of $2890/kW gives intercepts of 1016 h and 3662 h for CT-CCGT and CCGT-Coal. 
## Low Coal Capital cost of $1000/kW gives intercept of 467 h for CT-Coal

p_net_load <- ggplot(net_load, aes(Timepoint_sorted, net_load_final, color = scenario_build, line_type = scenario_split)) +
  geom_line() +
  labs(y = "Net Load (GW) \n ", x = "") +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = " \n \n ")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 298) + 
  geom_vline(xintercept = 922) + 
  background_grid()

technology.order <- c("Coal", "CCGT-LNG", "CT-LNG")
screening.curve.plot.input$technology <- reorder.factor(screening.curve.plot.input$technology, new.order=technology.order)
screening.curve.plot.input[,value:= value]
p_screening_curve <- ggplot(screening.curve.plot.input, aes(hour, value, group = technology, color = technology)) +
  geom_line(size = 1) +
  labs(y = "Annual cost (USD/kW/y)", x = "Sorted Hour of Year") +
  scale_color_manual(values = colors_RD3) + 
  scale_y_continuous(sec.axis = sec_axis(~.*INR_USD, name = "INR/kW/y")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 298) + 
  geom_vline(xintercept = 922) + 
  background_grid()

p_combo <- plot_grid(p_net_load, p_screening_curve, labels = c('A', 'B'), ncol=1)

file_suffix <- "coalLC"
ggsave(file = paste0(results.folder, "net_load_screening_curve-", file_suffix, ".png"), p_combo)


##################################################

