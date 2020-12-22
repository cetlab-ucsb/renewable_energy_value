## Author: Ranjit Deshmukh 11/13/2016
## This script does the following
## 1) Provides a summary of the total installed costs


## Revision history
## v0 - Basic script
## v1 - Plot two scenarios
## v2 - cleaned up the input gen cost csv. plots also individual scenarios.



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
#library(Cairo) # creates high quality vector and bitmap images
library(rgeos) # for maps
library(maptools) # for maps
library(bit64)
library(csvread)
library(scales)


## INPUTS TO SCRIPT ##############################################################################################

# working.directory 
switch(Sys.info()[['sysname']],
       Windows= {"G:/Electricity_Models/renewable_energy_value/"},
       Linux  = {print("I'm a penguin.")},
       Darwin = {working.directory <- "/Volumes/RD External/Electricity_Models/renewable_energy_value/"})
## Set working directory ##
setwd(working.directory)


input.folder <- "india_REV_input/"
output.folder <- "india_REV_output/"
#results.folder <- "processed_results_coalHC_coalLC_SolarLC20p/"
input.results.fname <- "results_all_scenarios.csv"
emissions.fname <- "emissions.csv"
gen.cost.fname <- "generator_cost_all.csv"
existing.gen.capacity.fname <- "existing_generation.csv"
scenario.inputs.fname <- "REvalue_input_csv.csv"

# USER SCENARIO INPUTS
scenarios <- c("base", "coal_55mingen", "wind10LC", "wind20LC", "wind30LC", "solar10LC", "solar20LC", "solar30LC", "wind30LC_solar30LC", "wind120HH_solar1A", 
               "battery15", "battery30", "battery15B25LC", "battery30B25LC", "battery15B50LC", "battery30B50LC") # These are the cost scenarios from the generator cost csv. 

#scenarios.dispatch <- c("coalLC_55min", "coalLC_70min") # Choose the dispatch scenario based on the column in results.csv. Set it in the same order as the scenarios. 
# vre_cost <- "low_solar_20pc" # "base", "low_wind_10pc", "low_solar_10pc", "low_vre_10pc", "low_wind_20pc", "low_solar_20pc", "low_vre_20pc"
INR_USD <- 65
disc_rate <- 0.07 # CERC discount rate is 10.8%
plant_life <- 25 # in years
battery_life <- 20 # in years
plot.individual.scenario = "yes"


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
emissions <- fread(paste0(input.folder, emissions.fname))
gen.cost <- fread(paste0(input.folder, gen.cost.fname))
existing.gen.capacity <- fread(paste0(input.folder, existing.gen.capacity.fname))
scenario.inputs <- fread(paste0(input.folder, scenario.inputs.fname))


# Results processed data table
results.processed <- data.table()
results.costs.emissions.processed <- data.table()

## Lists for saving plots
p_vre <- list()
p_vre_curt <- list()
p_vre_storage_curt <- list()
p_vre_sys <- list()
p_value <- list()
p_vre_share <- list()
p_curt <- list()
p_avg_em <- list()
p_avg_em_bar <- list()
p_mar_em <- list()
p_em_combo <- list()
p_em_red_perc <- list()
p_em_tot <- list()
p_vre_share_uncurt <- list()
p_buildout <- list()
p_vre_sys_pLoad <- list()


# Capital recovery factor (CRF)
crf <- (disc_rate*(1+disc_rate)^plant_life)/((1+disc_rate)^plant_life - 1)
crf.battery <- (disc_rate*(1+disc_rate)^battery_life)/((1+disc_rate)^battery_life - 1)

# Results folder
# if(length(scenarios) == 1){
#   results.folder <- paste0("results/processed_results_", scenarios[1], "-", scenarios.dispatch[1], "/")
# }else if(length(scenarios) == 2){
#   results.folder <- paste0("results/processed_results_", scenarios[1], "-", scenarios.dispatch[1], "_", scenarios[2], "-", scenarios.dispatch[2], "/")
# }
#ifelse(!dir.exists(file.path(results.folder)), dir.create(file.path(results.folder), recursive=TRUE), FALSE)

# Split the scenario column into 2. This should have been done in the python script
results.input[, c("scenario", "scenario_econ_dispatch") := tstrsplit(scenario, "_", fixed=TRUE)]

## Replace the S0W0 scenario rows for all battery scenarios with S0W0 base scenario ##
cols.to.replace <- colnames(results.input[,!c("scenario_main", "scenario_econ_dispatch")])
results.input[grepl("bat", scenario_main) & scenario == "S0W0",cols.to.replace] <- 
  results.input[grepl("base", scenario_main) & scenario == "S0W0",.SD, .SDcols = !c("scenario_main", "scenario_econ_dispatch")]

for (sc in 1:length(scenarios)){
  print(sc)
  
  # Scenario names for scenario abbreviations and generator cost
  sc.gen.cost <- scenario.inputs[parameter == "generator_cost_suffix", get(scenarios[sc])]
  sc.econ.dispatch <- scenario.inputs[parameter == "economic_dispatch_folder_suffix", get(scenarios[sc])]
  sc.sensitivity <- scenario.inputs[parameter == "scenario_suffix", get(scenarios[sc])]
  
  
  # Choose data based on scenario
  results <- results.input[scenario_econ_dispatch == sc.econ.dispatch, ]
  results[,scenario_sensitivity := sc.sensitivity] # Specify the correct scenario name
  results[,scenario_main := scenarios[sc]] # Specify the correct main scenario name
  
  # Choose data based on scenario anc calculate annual 
  ann_fc_coal_cost <- gen.cost[technology=="coal" & cost_type == "capital", get(sc.gen.cost)] * crf + gen.cost[technology=="coal" & cost_type == "om", get(sc.gen.cost)]
  ann_fc_ccgt_cost <- gen.cost[technology=="ccgt" & cost_type == "capital", get(sc.gen.cost)] * crf + gen.cost[technology=="ccgt" & cost_type == "om", get(sc.gen.cost)]
  ann_fc_ct_cost <- gen.cost[technology=="ct" & cost_type == "capital", get(sc.gen.cost)] * crf + gen.cost[technology=="ct" & cost_type == "om", get(sc.gen.cost)]
  ann_fc_solar_cost <- gen.cost[technology=="solarPV" & cost_type == "capital", get(sc.gen.cost)] * crf + gen.cost[technology=="solarPV" & cost_type == "om", get(sc.gen.cost)]
  ann_fc_wind_cost <- gen.cost[technology=="wind" & cost_type == "capital", get(sc.gen.cost)] * crf + gen.cost[technology=="wind" & cost_type == "om", get(sc.gen.cost)]
  ann_fc_battery_cost <- gen.cost[technology=="battery" & cost_type == "capital", get(sc.gen.cost)] * crf.battery + gen.cost[technology=="battery" & cost_type == "om", get(sc.gen.cost)]
 
  # ann_fc_coal_cost <- gen.cost[technology=="coal" & cost_type == "capital", get(scenarios[sc])] * crf + gen.cost[technology=="coal" & cost_type == "om", get(scenarios[sc])]
  # ann_fc_ccgt_cost <- gen.cost[technology=="ccgt" & cost_type == "capital", get(scenarios[sc])] * crf + gen.cost[technology=="ccgt" & cost_type == "om", get(scenarios[sc])]
  # ann_fc_ct_cost <- gen.cost[technology=="ct" & cost_type == "capital", get(scenarios[sc])] * crf + gen.cost[technology=="ct" & cost_type == "om", get(scenarios[sc])]
  # ann_fc_solar_cost <- gen.cost[technology=="solarPV" & cost_type == "capital", get(scenarios[sc])] * crf + gen.cost[technology=="solarPV" & cost_type == "om", get(scenarios[sc])]
  # ann_fc_wind_cost <- gen.cost[technology=="wind" & cost_type == "capital", get(scenarios[sc])] * crf + gen.cost[technology=="wind" & cost_type == "om", get(scenarios[sc])]
  
  #### Estimate total annual capacity costs
  results[,ann_cap_cost_coal_mil:= new_capacity_coal_MW * ann_fc_coal_cost/10^3]
  results[,ann_cap_cost_ccgt_mil:= new_capacity_gas_ccgt_MW * ann_fc_ccgt_cost/10^3]
  results[,ann_cap_cost_ct_mil:= new_capacity_gas_ct_MW * ann_fc_ct_cost/10^3]
  results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * ann_fc_solar_cost/10^3]
  results[,ann_cap_cost_wind_mil:= capacity_wind_MW * ann_fc_wind_cost/10^3]
  results[,ann_cap_cost_battery_mil:= capacity_bat_storage_MW * ann_fc_battery_cost/10^3]

  results[,total_cost_new_conv:= ann_cap_cost_coal_mil + ann_cap_cost_ccgt_mil + ann_cap_cost_ct_mil]
  results[,total_cost_vre:= ann_cap_cost_solar_mil + ann_cap_cost_wind_mil]
  
  # Capacity, energy and total costs
  results[,total_cost_capacity:= total_cost_new_conv + total_cost_vre + ann_cap_cost_battery_mil] # Conventional gen + RE gens + battery
  results[,total_cost_energy:= dispatch_cost/INR_USD/10^6]
  results[, total_cost:= total_cost_capacity + total_cost_energy]
  
  # Cost of No RE scenario
  total_cost_new_conv_noRE <- results[scenario=="S0W0", total_cost_capacity]
  total_cost_energy_noRE <- results[scenario=="S0W0", total_cost_energy]
  total_cost_noRE <- total_cost_new_conv_noRE + total_cost_energy_noRE
  
  # Alternate Cost of No RE scenario for battery scenarios (USE no RE costs from base scenario without any battery B0)
  # One option is in the results.inputs data table replace the S0W0 scenarios for B15 and B30 with B0 scenario results. That's the easiest. 
  
  
  # Total cost differences / increases between scenarios and No RE scenario
  results[scenario!="S0W0", system_cost_vre:= total_cost - total_cost_noRE]
  
  # Cost of VRE per MWh
  results[,cost_vre_pMWh:= total_cost_vre*10^6/ann_gen_vre_MWh]
  results[,cost_vre_nocurt_pMWh:= total_cost_vre*10^6/ann_gen_vre_nocurt_MWh]
  
  # Cost of battery storage per MWh
  results[,cost_battery_pMWh:= ann_cap_cost_battery_mil*10^6/ann_gen_vre_MWh]
  results[,cost_battery_nocurt_pMWh:= ann_cap_cost_battery_mil*10^6/ann_gen_vre_nocurt_MWh]
  
  # Value per MWh
  results[scenario!="S0W0",capacity_value_pMWh:= (total_cost_new_conv_noRE - total_cost_new_conv)*10^6/ann_gen_vre_MWh]
  results[scenario!="S0W0",energy_value_pMWh:= (total_cost_energy_noRE - total_cost_energy)*10^6/ann_gen_vre_MWh]
  results[scenario!="S0W0", system_cost_vre_pMWh:= cost_vre_pMWh + cost_battery_pMWh - capacity_value_pMWh - energy_value_pMWh]
  
  # Additional cost of implementing VRE per MWh load served
  results[scenario!="S0W0", system_cost_add_pMWh_load:= (total_cost - total_cost_noRE)*10^6/ ann_gen_total_MWh]
  
  # Additional overall cost % for implementing VRE compared to No RE
  results[scenario!="S0W0", system_cost_add_perc:= (total_cost - total_cost_noRE)/ total_cost_noRE]
  
  # VRE metrics #
  # VRE share
  results[scenario!="S0W0", vre_share_after_curt:= ann_gen_vre_MWh/ann_gen_total_MWh]
  results[scenario!="S0W0", vre_curt:= (ann_gen_vre_nocurt_MWh - ann_gen_vre_MWh)/ann_gen_vre_nocurt_MWh]
  results[scenario!="S0W0", vre_share_nocurt:= ann_gen_vre_nocurt_MWh/ann_gen_total_MWh]
  
  # Emissions #
  # Total emissions
  results[, total_emissions_co2_milTonnes:= (ann_gen_coal_MWh * emissions[technology=="coal", co2_tonnes_mwh] +
                                               ann_gen_gas_ccgt_MWh * emissions[technology=="ccgt", co2_tonnes_mwh] +
                                               ann_gen_gas_ct_MWh * emissions[technology=="ct", co2_tonnes_mwh] +
                                               ann_gen_diesel_MWh * emissions[technology=="diesel", co2_tonnes_mwh]) / 10^6]
  
  # Grid emissions factor kg/kWh
  results[, grid_emissions_factor_kg_kWh:= total_emissions_co2_milTonnes * 10^6 / ann_gen_total_MWh]
  
  # Emissions reduction in %
  total_emissions_co2_milTonnes_noRE <- results[scenario=="S0W0", total_emissions_co2_milTonnes]
  results[scenario!="S0W0", total_emissions_reduced_milTonnes:= (total_emissions_co2_milTonnes_noRE - total_emissions_co2_milTonnes)]
  results[scenario!="S0W0", emissions_reduction:= total_emissions_reduced_milTonnes/total_emissions_co2_milTonnes_noRE]
  
  # Average cost of emissions mitigation
  results[scenario!="S0W0", cost_emissions_reduction_pTonneCO2_avg:= system_cost_vre / total_emissions_reduced_milTonnes]
  
  # Marginal and average cost of emissions mitigation
  results.costs.emissions <- results[, .(scenario, scenario_main, scenario_sensitivity, scenario_build, scenario_split, total_cost, total_emissions_co2_milTonnes)]
  results.costs.emissions <- rbind(results.costs.emissions[rep(1, 4),], results.costs.emissions) # repeat the 0-0 row, so we can use difference by group
  results.costs.emissions[scenario=="S0W0", scenario_split:= unique(results.costs.emissions[scenario!="S0W0", scenario_split])]
  results.costs.emissions[, system_cost_inc_marginal:= c(NA, diff(total_cost)), by=scenario_split] # System cost increase
  results.costs.emissions[, system_emissions_dec_marginal:= c(NA, diff(total_emissions_co2_milTonnes))*-1, by=scenario_split] # emissions decrease from previous RE target
  results.costs.emissions[, cost_emissions_reduction_pTonneCO2_marginal:= system_cost_inc_marginal / system_emissions_dec_marginal] # Marginal emissions mitigation cost
  # Average cost of emissions mitigation again
  results.costs.emissions[scenario!="S0W0", system_cost_inc_total:= total_cost - total_cost_noRE] # System cost increase from S0W0
  results.costs.emissions[scenario!="S0W0", system_emissions_dec_total:= total_emissions_co2_milTonnes_noRE - total_emissions_co2_milTonnes] # emissions decrease from base S0W0
  results.costs.emissions[scenario!="S0W0", cost_emissions_reduction_pTonneCO2_average:= system_cost_inc_total / system_emissions_dec_total] # Average emissions mitigation cost
  
  
  ## Cost of wind and solar uncurtailed [ Note total VRE costs in the selected scenarios are equal to either total solar or total wind costs.]
  results.vre.costs <- results[scenario %in% c("S0W200", "S0W300", "S0W400"), .(scenario, total_cost_vre, ann_gen_vre_nocurt_MWh)]
  results.vre.costs[, technology:= "Wind"]
  results.vre.costs <- rbind(results.vre.costs, results[scenario %in% c("S0W200", "S0W300", "S0W400"), .(scenario, total_cost_vre, ann_gen_vre_nocurt_MWh)], fill=TRUE)
  results.vre.costs[is.na(technology), technology:= "Solar PV"]
  results.vre.costs[, Capacity:= c(200,300,400, 200, 300, 400)]
  
  # Avoided conventional generation capacity
  total_cap_new_conv_noRE <- results[scenario=="S0W0", new_capacity_coal_MW + new_capacity_gas_ccgt_MW + new_capacity_gas_ct_MW]
  results[scenario!="S0W0", avoided_conv_cap_mw_perVREmw:= (total_cap_new_conv_noRE - (new_capacity_coal_MW + new_capacity_gas_ccgt_MW + new_capacity_gas_ct_MW))/capacity_vre_MW]
  
  results.processed <- rbind(results.processed, results)
  results.costs.emissions.processed <- rbind(results.costs.emissions.processed, results.costs.emissions)
  
}
  
write.csv(results.processed, file = "results_processed.csv")

##############################################################
## INDIVIDUAL PLOTS FOR INDIVIDUAL SCENARIOS #################
##############################################################

## USER INPUT: Scenarios for individual plots ################
scenarios <- c("base", "coal_55mingen", "wind10LC", "wind20LC", "wind30LC", "solar10LC", "solar20LC", "solar30LC", "wind30LC_solar30LC", "wind120HH_solar1A", 
               "battery15", "battery30", "battery15B25LC", "battery30B25LC", "battery15B50LC", "battery30B50LC")

scenarios.sensitivity <- ""
for (sc in 1:length(scenarios)){
  scenarios.sensitivity[[sc]] <- scenario.inputs[parameter == "scenario_suffix", get(scenarios[sc])]
}

## Theme ##
theme_set(theme_cowplot(font_size=18))
colors_RD3 <- c("#fd8d3c", "#2b8cbe", "#969696")
colors_RD5 <- c("#2b8cbe", "#fd8d3c", "#969696")
colors_RD7 <- c("#5588bb", "#66bbbb", "#aa6644", "#99bb55","#ee9944", "#444466", "#bb5555")
colors_emissions_RD4 <- c("#d95f0e", "#999999", "#ca0020", "#a6611a", "#3a3938" )

width.bar = 0.7

positions <- c("S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0")
positions2 <- c("0-100", "25-75", "50-50", "75-25", "100-0")
positions3 <- c("S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0")
#positions.tech <- c("Existing Other", "Existing Nuclear", "Existing Hydro RoR", "Existing Hydro Storage", "Existing Diesel", "Existing Gas CT", "Existing Gas CCGT", "Existing Coal", "Wind", "Solar PV", "New Gas CT", "New Gas CCGT", "New Coal")
positions.tech <- c("New Coal", "New Gas CCGT", "New Gas CT", "Solar PV", "Wind", "Existing Coal", "Existing Gas CCGT", "Existing Gas CT", "Existing Diesel", "Existing Hydro Storage", "Existing Hydro RoR", "Existing Nuclear", "Existing Other")
#Labels
share.labels <- c("S 0% - W 100%", "S 25% - W 75%", "S 50% - W 50%", "S 75% - W 25%", "S 100% - W 0%", "S 0% - W 100%", "S 25% - W 75%", "S 50% - W 50%", "S 75% - W 25%", "S 100% - W 0%", "S 0% - W 100%", "S 25% - W 75%", "S 50% - W 50%", "S 75% - W 25%", "S 100% - W 0%")
share.labels2 <- c("S 0 - W 0", "S 0 - W 200", "S 50 - W 150", "S 100 - W 100", "S 150 - W 50", "S 200 - W 0", "S 0 - W 300", "S 75 - W 225", "S 150 - W 150", "S 225 - W 75", "S 300 - W 0", "S 0 - W 400", "S 100 - W 300", "S 200 - W 200", "S 300 - W 100", "S 400 - W 0")


for (sc in 1:length(scenarios)){
  print(sc)
  
  ## VRE cost per MWh - both before and after curtailment
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, cost_vre_pMWh, cost_vre_nocurt_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="cost_vre_nocurt_pMWh", variable:= "No Curtailment"][variable=="cost_vre_pMWh", variable:= "After Curtailment"]
  
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar, position="dodge") +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Levelized Cost of VRE Generation \n USD/MWh", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="top", legend.title = element_blank(), legend.text = element_text(size = 18)) +
    scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh")) +
    background_grid()

  ## VRE cost per MWh - Only after curtailment
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, cost_vre_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="cost_vre_pMWh", variable:= "Cost of RE"]
  
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre_curt[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar, position="dodge") +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Levelized Cost of VRE Generation \n USD/MWh (after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD5) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="top", legend.title = element_blank(), legend.text = element_text(size = 18)) +
    scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh")) +
    background_grid()
  
  ## VRE + Battery Storage cost per MWh of VRE - Only after curtailment ####
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, cost_battery_pMWh, cost_vre_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="cost_vre_pMWh", variable:= "Cost of RE"][variable=="cost_battery_pMWh", variable:= "Cost of Battery Storage"]
  data.plot$variable <- reorder.factor(data.plot$variable, new.order = c("Cost of Battery Storage", "Cost of RE"))
  
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre_storage_curt[[sc]] <- ggplot(data.plot[order(data.plot$variable)], aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Cost of VRE and Storage \n USD/MWh (VRE after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="top", legend.title = element_blank(), legend.text = element_text(size = 18)) +
    scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh of VRE (after curtailment)")) +
    background_grid()

  
  ## System cost for VRE implementation per MWh VRE absorbed
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, rank, system_cost_vre_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre_sys[[sc]] <- ggplot(data.plot, aes(scenario, system_cost_vre_pMWh, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Additional Cost of VRE Implementation \nUSD/MWh of VRE (after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position = "none") +
    scale_y_continuous(limits = c(-20, 80), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh of VRE (after curtailment)")) +
    background_grid()
  
  ## System cost for VRE implementation per MWh Load served
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, rank, system_cost_add_pMWh_load)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre_sys_pLoad[[sc]] <- ggplot(data.plot, aes(scenario, system_cost_add_pMWh_load, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Additional Cost of VRE Implementation \nUSD/MWh of Load", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) + 
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position = "none") +
    scale_y_continuous(limits = c(-5, 15), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh of Load")) +
    background_grid()
  
  
  
  ##Energy and capacity value per MWh
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, energy_value_pMWh, capacity_value_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="capacity_value_pMWh", variable:= "Capacity Value"][variable=="energy_value_pMWh", variable:= "Energy Value"]
  
  p_value[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Value of VRE \nUSD/MWh of VRE (after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="top", legend.title = element_blank(), legend.text = element_text(size = 18)) +
    scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis(~.*INR_USD/1000, name = "INR/kWh of VRE (after curtailment)")) +
    background_grid()
  
  
  ## VRE share
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, vre_share_nocurt, vre_share_after_curt)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="vre_share_nocurt", variable:= "No Curtailment"][variable=="vre_share_after_curt", variable:= "With Curtailment"]
  
  p_vre_share[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar, position="dodge") +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "VRE share of total electricity generation", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, 0.5), labels=percent) +
    background_grid()
  
  # VRE potential share - only uncurtailed
  data.plot <- data.plot[variable=="No Curtailment",]
  p_vre_share_uncurt[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Potential VRE share of total electricity generation", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="none", legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, 0.5), labels=percent) +
    background_grid()
  
  
  ## VRE curtailment
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, rank, vre_curt)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  
  p_curt[[sc]] <- ggplot(data.plot, aes(scenario, vre_curt, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Annual VRE curtailment", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.6), labels=percent) +
    background_grid()
  

  ## EMISSIONS
  # % of emissions reduced
  data.plot <- results.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, emissions_reduction, total_emissions_co2_milTonnes)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  p_em_red_perc[[sc]] <- ggplot(data.plot, aes(scenario, emissions_reduction, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = expression(CO[2]~Emissions~Reduction~from~No~RE~Scenario), x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="none") +
    scale_y_continuous(limits = c(0, 0.5), labels=percent) +
    background_grid()
  
  # Total emissions 
  p_em_tot[[sc]] <- ggplot(data.plot, aes(scenario, total_emissions_co2_milTonnes, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = expression(Total~CO[2]~Emissions~(million~tonnes)), x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="none") +
    background_grid()
  
  ## CO2 emissions cost of mitigation
  # Average cost of emissions
  data.plot <- results.costs.emissions.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_avg_em[[sc]] <- ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_average , group=scenario_split, color = scenario_split, linetype = scenario_split)) +
    geom_line(size=1) +
    labs(y = expression(Average~Cost~of~CO[2]~Emissions~Mitigation~(USD/tonne~CO[2])), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-20, 100)) +
    background_grid()
  
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  
  p_avg_em_bar[[sc]] <- ggplot(data.plot, aes(scenario, cost_emissions_reduction_pTonneCO2_average, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = expression(Average~Cost~of~CO[2]~Emissions~Mitigation~(USD/tonne~CO[2])), x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    scale_x_discrete(breaks=positions, labels=share.labels) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.position="none") +
    scale_y_continuous(limits = c(-20, 100)) +
    background_grid()
  
  
  # Marginal cost of emissions
  data.plot <- results.costs.emissions.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  #data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario_split)) # Use if sorting screws up
  
  p_mar_em[[sc]] <- ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_marginal , group=scenario_split, color = scenario_split, linetype = scenario_split)) +
    geom_line(size=1) +
    labs(y = expression(Marginal~Cost~of~CO[2]~Emissions~Mitigation~(USD/tonne~CO[2])), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-100, 600)) +
    background_grid()
  
  
  ## Average and marginal emissions cost 
  data.plot <- results.costs.emissions.processed[scenario!="S0W0" & scenario_main == scenarios[sc], .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build", "scenario_split"))
  data.plot[variable=="cost_emissions_reduction_pTonneCO2_marginal", variable:= "Marginal Cost"][variable=="cost_emissions_reduction_pTonneCO2_average", variable:= "Average Cost"]
  
  p_em_combo[[sc]] <- ggplot(data.plot, aes(scenario_build, value , group=interaction(scenario_split, variable), color = scenario_split, linetype = variable)) +
    geom_line(size=1) +
    labs(y = expression(Cost~of~CO[2]~Emissions~Mitigation~(USD/tonne~CO[2])), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-20, 600)) +
    background_grid()
  
  ## Capacity buildout stack #########
  capacity.buildout <- results.processed[scenario_main == scenarios[sc], .(scenario, scenario_build, new_capacity_coal_MW, new_capacity_gas_ccgt_MW, new_capacity_gas_ct_MW, capacity_solarPV_MW, capacity_wind_MW)]
  setnames(capacity.buildout, c("new_capacity_coal_MW", "new_capacity_gas_ccgt_MW", "new_capacity_gas_ct_MW", "capacity_solarPV_MW", "capacity_wind_MW"), c("New Coal", "New Gas CCGT", "New Gas CT", "Solar PV", "Wind"))
  capacity.buildout.melt <- melt(capacity.buildout, id.vars = c("scenario", "scenario_build"))
  setnames(capacity.buildout.melt, c("variable", "value"), c("technology", "capacity_MW"))
  
  # Existing capacity
  scenario_list <- capacity.buildout[, .(scenario, scenario_build)]
  existing.gen.capacity.all.scenarios <- cbind(existing.gen.capacity[rep(seq(nrow(existing.gen.capacity)), 16)], scenario_list[rep(seq_len(nrow(scenario_list)), each=9),])
  
  # join the new and existing buildouts
  capacity.all <- rbind(capacity.buildout.melt, existing.gen.capacity.all.scenarios)
  capacity.all[, capacity_GW:= capacity_MW/10^3]
  capacity.all[technology=="coal", technology:= "Existing Coal"]
  capacity.all[technology=="gas_ccgt", technology:= "Existing Gas CCGT"]
  capacity.all[technology=="gas_ct", technology:= "Existing Gas CT"]
  capacity.all[technology=="diesel", technology:= "Existing Diesel"]
  capacity.all[technology=="hydro_storage", technology:= "Existing Hydro Storage"]
  capacity.all[technology=="hydro_pondage", technology:= "Existing Hydro RoR"]
  capacity.all[technology=="hydro_ror", technology:= "Existing Hydro RoR"]
  capacity.all[technology=="nuclear", technology:= "Existing Nuclear"]
  capacity.all[technology=="other", technology:= "Existing Other"]
  
  # Plot the buildout
  capacity.all$technology <- reorder.factor(capacity.all$technology, new.order=positions.tech)
  capacity.all$scenario <- reorder.factor(capacity.all$scenario, new.order=positions3)
  capacity.all <- capacity.all[order(-technology)]
  
  colors.capacity.buildout <- c("#CC79A7", "#D55E00", "#56B4E9", "#0072B2", "#000000", "#CC3333", "#FF6600", "#999999", "#FF9900", "#006699", "#CC3333", "#FF6600", "#999999")
  colors.capacity.buildout <- c("#666666", "#FF6600", "#CC3333", "#FF9900", "#006699", "#999999", "#FF6600", "#CC3333", "#000000", "#0072B2", "#56B4E9", "#D55E00", "#CC79A7")
                                
  p_buildout[[sc]] <- ggplot(capacity.all, aes(scenario, capacity_GW, fill = technology, order = technology)) +
    geom_bar(stat = "identity", width = width.bar) +
    labs(y = "Installed Capacity (MW)", x = NULL) +
    scale_fill_manual(values = colors.capacity.buildout) + 
    scale_x_discrete(breaks=positions3, labels=share.labels2) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.title = element_blank()) +
    background_grid()
  
}

## Individual Plots #############
if(plot.individual.scenario == "yes"){
  for(sc in 1:length(scenarios)){
  
    # Get the scenario suffix corresponding to the scenario name
    sc.sensitivity <- scenario.inputs[parameter == "scenario_suffix", get(scenarios[sc])]
    
    results.folder <- paste0("results/processed_results_", scenarios[sc], "-", scenarios.sensitivity[sc], "/")
    ifelse(!dir.exists(file.path(results.folder)), dir.create(file.path(results.folder), recursive=TRUE), FALSE)
    
    
    file_suffix <- paste0(scenarios.sensitivity[sc])
    
    ## Levelized cost of VRE before and after curtailment
    ggsave(file = paste0(results.folder, "vre_levelized-cost-", file_suffix, ".png"), p_vre[[sc]])
    
    ## Levelized cost of VRE after curtailment
    ggsave(file = paste0(results.folder, "vre_curt-levelized-cost-", file_suffix, ".png"), p_vre_curt[[sc]])
    
    ## Levelized cost of VRE and storage per MWh VRE absorbed
    ggsave(file = paste0(results.folder, "vre_curt-storage-levelized-cost-", file_suffix, ".png"), p_vre_storage_curt[[sc]])
    
    ## System cost of VRE per MWh VRE absorbed
    ggsave(file = paste0(results.folder, "vre_integration-cost-", file_suffix, ".png"), p_vre_sys[[sc]])
    
    ## System cost of VRE per MWh load served
    ggsave(file = paste0(results.folder, "vre_integration-cost-per-load-", file_suffix, ".png"), p_vre_sys_pLoad[[sc]])
    
    ## Value of VRE
    ggsave(file = paste0(results.folder, "vre_value-", file_suffix, ".png"), p_value[[sc]])
    
    ## VRE share not curtailed
    ggsave(file = paste0(results.folder, "vre_share-uncurt-", file_suffix, ".png"), p_vre_share_uncurt[[sc]])
    
    ## VRE share - both curtailed and not curtailed
    ggsave(file = paste0(results.folder, "vre_share-curt-uncurt-", file_suffix, ".png"), p_vre_share[[sc]])
    
    ## VRE curtailment
    ggsave(file = paste0(results.folder, "vre_curtailment-", file_suffix, ".png"), p_curt[[sc]])
    
    ## Emissions 
    ggsave(file = paste0(results.folder, "emissions-mar-", file_suffix, ".png"), p_mar_em[[sc]])
    
    ggsave(file = paste0(results.folder, "emissions-avg-", file_suffix, ".png"), p_avg_em[[sc]])
    
    ggsave(file = paste0(results.folder, "emissions-avg-bar-", file_suffix, ".png"), p_avg_em_bar[[sc]])
    
    ## Emissions average and marginal combined in one
    plot.legend <- g_legend(p_em_combo[[1]])
    ggsave(file = paste0(results.folder, "emissions-avg-mar-", file_suffix, ".png"), p_em_combo[[sc]])
    
    ## Emissions average and marginal combined but separate plots
    plot.legend <- g_legend(p_avg_em[[sc]])
    combo.plot <- arrangeGrob(arrangeGrob(p_avg_em[[sc]] + theme(legend.position="none") + ggtitle("A") ,
                                          p_mar_em[[sc]] + theme(legend.position="none") + ggtitle("B"),nrow=1),
                              plot.legend, nrow=2,heights=c(10,1))
    ggsave(file = paste0(results.folder, "emissions-avg-mar-separate-", file_suffix, ".png"), combo.plot)
    
    
    ## Emissions reduction in %
    ggsave(file = paste0(results.folder, "emissions-reduction-perc-", file_suffix, ".png"), p_em_red_perc[[sc]])
    
    ## Emissions total
    ggsave(file = paste0(results.folder, "emissions-total-", file_suffix, ".png"), p_em_tot[[sc]])
    
    ## Capacity buildout
    ggsave(file = paste0(results.folder, "capacity-buildout", file_suffix, ".png"), p_buildout[[sc]])
    
  }
}









# 
# if(length(scenarios) > 1){
#   file_suffix <- paste0(scenarios[1], "-", scenarios.dispatch[1], "_", scenarios[2], "-", scenarios.dispatch[2])
#   
#   ## Levelized cost of VRE after curtailment
# #   p.grid <- plot_grid(p_vre[[1]], p_vre[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
# #   ggsave(file = paste0(results.folder, "vre_curt-levelized-cost-high-low-cost-coal", ".png"), p.grid)
#   
#   plot.legend <- g_legend(p_vre[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_vre[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_vre[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "vre_curt-levelized-cost-", file_suffix, ".png"), combo.plot)
#   
#   ## System cost of VRE per MWh VRE absorbed
#   p.grid <- plot_grid(p_vre_sys[[1]], p_vre_sys[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
#   ggsave(file = paste0(results.folder, "vre_integration-cost-", file_suffix, ".png"), p.grid)
#   
#   ## System cost of VRE per MWh load served
#   p.grid <- plot_grid(p_vre_sys_pLoad[[1]], p_vre_sys_pLoad[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
#   ggsave(file = paste0(results.folder, "vre_integration-cost-per-load-", file_suffix, ".png"), p.grid)
#   
#   ## Value of VRE
# #   p.grid <- plot_grid(p_value[[1]], p_value[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
# #   ggsave(file = paste0(results.folder, "vre_value-high-low-cost-coal", ".png"), p.grid)
# 
#   plot.legend <- g_legend(p_value[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_value[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_value[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "vre_value-", file_suffix, ".png"), combo.plot)
#   
#   ## VRE share not curtailed
#   ggsave(file = paste0(results.folder, "vre_share-uncurt-", file_suffix, ".png"), p_vre_share_uncurt[[1]])
#   
#   ## VRE share - both curtailed and not curtailed
# #   p.grid <- plot_grid(p_vre_share[[1]], p_vre_share[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'), ncol = 2)
# #   ggsave(file = paste0(results.folder, "vre_share-curt-uncurt-high-low-cost-coal", ".png"), p.grid)
#   
#   plot.legend <- g_legend(p_vre_share[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_vre_share[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_vre_share[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "vre_share-curt-uncurt-", file_suffix, ".png"), combo.plot)
#   
#   ## VRE curtailment
#   p.grid <- plot_grid(p_curt[[1]], p_curt[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
#   ggsave(file = paste0(results.folder, "vre_curtailment-", file_suffix, ".png"), p.grid)
#   
#   ## Emissions 
#   plot.legend <- g_legend(p_mar_em[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_mar_em[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_mar_em[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "emissions-mar-", file_suffix, ".png"), combo.plot)
#   
#   plot.legend <- g_legend(p_avg_em[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_avg_em[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_avg_em[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "emissions-avg-", file_suffix, ".png"), combo.plot)
#   
#   ## Emissions average and marginal
#   plot.legend <- g_legend(p_em_combo[[1]])
#   combo.plot <- arrangeGrob(arrangeGrob(p_em_combo[[1]] + theme(legend.position="none") + ggtitle("A") ,
#                                         p_em_combo[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
#                             plot.legend, nrow=2,heights=c(10,1))
#   ggsave(file = paste0(results.folder, "emissions-avg-mar-", file_suffix, ".png"), combo.plot)
#   
#   ## Emissions reduction in %
#   p.grid <- plot_grid(p_em_red_perc[[1]], p_em_red_perc[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
#   ggsave(file = paste0(results.folder, "emissions-reduction-perc-", file_suffix, ".png"), p.grid)
#   
#   ## Emissions Total
#   p.grid <- plot_grid(p_em_tot[[1]], p_em_tot[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
#   ggsave(file = paste0(results.folder, "emissions-total-", file_suffix, ".png"), p.grid)
#   
# }
# 
