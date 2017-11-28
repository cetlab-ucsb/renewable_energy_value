## Author: Ranjit Deshmukh 11/13/2016
## This script does the following
## 1) Provides a summary of the total installed costs


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

working.directory <- "G:/SWITCH/india_ED/"
input.folder <- "india_ED_input/"
output.folder <- "india_ED_output/"
results.folder <- "processed_results/"
input.results.fname <- "results_all_scenarios_mod.csv"
emissions.fname <- "emissions.csv"
gen.cost.fname <- "generator_cost.csv"
existing.gen.capacity.fname <- "existing_generation.csv"

coal_cost <- "high" # Choose high for 2890 and low for 1000
INR_USD <- 65


## Set working directory ##
setwd(working.directory)


## BEGIN SCRIPT #####################
# Import the results and other parameters
results <- fread(paste0(output.folder, input.results.fname))
results <- results[order(rank)]
emissions <- fread(paste0(input.folder, emissions.fname))
gen.cost <- fread(paste0(input.folder, gen.cost.fname))
existing.gen.capacity <- fread(paste0(input.folder, existing.gen.capacity.fname))

# Total cost #
if(coal_cost == "high"){
  ann_fc_coal_cost <- gen.cost[technology=="coal", annualized_fc_high_coal]
  }else{
  ann_fc_coal_cost <- gen.cost[technology=="coal", annualized_fc_low_coal]
}
  
results[,ann_cap_cost_coal_mil:= new_capacity_coal_MW * ann_fc_coal_cost/10^3]
results[,ann_cap_cost_ccgt_mil:= new_capacity_gas_ccgt_MW * gen.cost[technology=="ccgt", annualized_fc_high_coal]/10^3]
results[,ann_cap_cost_ct_mil:= new_capacity_gas_ct_MW * gen.cost[technology=="ct", annualized_fc_high_coal]/10^3]
results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost[technology=="solarPV", annualized_fc_high_coal]/10^3]
results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost[technology=="wind", annualized_fc_high_coal]/10^3]

results[,total_cost_new_conv:= ann_cap_cost_coal_mil + ann_cap_cost_ccgt_mil + ann_cap_cost_ct_mil]
results[,total_cost_vre:= ann_cap_cost_solar_mil + ann_cap_cost_wind_mil]

# Capacity, energy and total costs
results[,total_cost_capacity:= total_cost_new_conv + total_cost_vre]
results[,total_cost_energy:= dispatch_cost/INR_USD/10^6]
results[, total_cost:= total_cost_capacity + total_cost_energy]

# Cost of No RE scenario
total_cost_new_conv_noRE <- results[scenario=="S0W0", total_cost_new_conv]
total_cost_energy_noRE <- results[scenario=="S0W0", total_cost_energy]
total_cost_noRE <- total_cost_new_conv_noRE + total_cost_energy_noRE

# Total cost differences / increases between scenarios and No RE scenario
results[scenario!="S0W0", system_cost_vre:= total_cost - total_cost_noRE]

# Cost of VRE per MWh
results[,cost_vre_pMWh:= total_cost_vre*10^6/ann_gen_vre_MWh]
results[,cost_vre_nocurt_pMWh:= total_cost_vre*10^6/ann_gen_vre_nocurt_MWh]

# Value per MWh
results[scenario!="S0W0",capacity_value_pMWh:= (total_cost_new_conv_noRE - total_cost_new_conv)*10^6/ann_gen_vre_MWh]
results[scenario!="S0W0",energy_value_pMWh:= (total_cost_energy_noRE - total_cost_energy)*10^6/ann_gen_vre_MWh]
results[scenario!="S0W0", system_cost_vre_pMWh:= cost_vre_pMWh - capacity_value_pMWh - energy_value_pMWh]

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
results.costs.emissions <- results[, .(scenario, scenario_build, scenario_split, total_cost, total_emissions_co2_milTonnes)]
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
results.vre.costs <- results[scenario %in% c("S0W200", "S0W400", "S0W600"), .(scenario, total_cost_vre, ann_gen_vre_nocurt_MWh)]
results.vre.costs[, technology:= "Wind"]
results.vre.costs <- rbind(results.vre.costs, results[scenario %in% c("S200W0", "S400W0", "S600W0"), .(scenario, total_cost_vre, ann_gen_vre_nocurt_MWh)], fill=TRUE)
results.vre.costs[is.na(technology), technology:= "Solar PV"]
results.vre.costs[, Capacity:= c(200,400,600, 200, 400, 600)]

## Plots ##
theme_set(theme_cowplot(font_size=16))
colors_RD3 <- c("#fd8d3c", "#2b8cbe", "#969696")
colors_RD7 <- c("#5588bb", "#66bbbb", "#aa6644", "#99bb55","#ee9944", "#444466", "#bb5555")
colors_emissions_RD4 <- c("#d95f0e", "#999999", "#ca0020", "#a6611a", "#3a3938" )

width.bar = 0.7
positions <- c("S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0", "S0W600", "S150W450", "S300W300", "S450W150", "S600W0")

## VRE cost per MWh
data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, cost_vre_pMWh, cost_vre_nocurt_pMWh)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
data.plot[variable=="cost_vre_nocurt_pMWh", variable:= "Not Curtailed"][variable=="cost_vre_pMWh", variable:= "After Curtailment"]

# data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up

ggplot(data.plot, aes(scenario, value, fill = variable)) +
  geom_bar(stat = "identity", width = width.bar, position="dodge") +
  facet_wrap(~scenario_build, scales = "free_x") + 
  labs(y = "Average Levelized Cost of VRE Generation", x = NULL) +
  scale_fill_manual(values = colors_RD3) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
  background_grid()

## System cost for VRE implementation
data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, rank, system_cost_vre_pMWh)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
# data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up

ggplot(data.plot, aes(scenario, system_cost_vre_pMWh, fill = scenario_build)) +
  geom_bar(stat = "identity", width = width.bar) +
  facet_wrap(~scenario_build, scales = "free_x") + 
  labs(y = "Average Cost of VRE Integration (USD/MWh VRE)", x = NULL) +
  scale_fill_manual(values = colors_RD3) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position = "none") +
  background_grid()


##Energy and capacity value per MWh
data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, capacity_value_pMWh, energy_value_pMWh)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
data.plot[variable=="capacity_value_pMWh", variable:= "Capacity Value"][variable=="energy_value_pMWh", variable:= "Energy Value"]

ggplot(data.plot, aes(scenario, value, fill = variable)) +
  geom_bar(stat = "identity", width = width.bar) +
  facet_wrap(~scenario_build, scales = "free_x") + 
  labs(y = "Average Value (USD/MWh)", x = NULL) +
  scale_fill_manual(values = colors_RD3) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
  background_grid()


## VRE share
data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, vre_share_nocurt, vre_share_after_curt)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
data.plot[variable=="vre_share_nocurt", variable:= "Not Curtailed"][variable=="vre_share_after_curt", variable:= "After Curtailment"]

ggplot(data.plot, aes(scenario, value, fill = variable)) +
  geom_bar(stat = "identity", width = width.bar, position="dodge") +
  facet_wrap(~scenario_build, scales = "free_x") + 
  labs(y = "VRE share", x = NULL) +
  scale_fill_manual(values = colors_RD3) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
  scale_y_continuous(labels=percent) +
  background_grid()


## VRE curtailment
data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, rank, vre_curt)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)

ggplot(data.plot, aes(scenario, vre_curt, fill = scenario_build)) +
  geom_bar(stat = "identity", width = width.bar) +
  facet_wrap(~scenario_build, scales = "free_x") + 
  labs(y = "VRE curtailment", x = NULL) +
  scale_fill_manual(values = colors_RD3) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position = "none") +
  scale_y_continuous(labels=percent) +
  background_grid()

## CO2 emissions cost of mitigation
# Total emissions reduction



# Average emissions
data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
# data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up

ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_average , group=scenario_split, color = scenario_split)) +
  geom_line() +
  labs(y = "Average Cost of CO2 Emissions Mitigation (USD/tonne CO2)", x = NULL) +
  scale_fill_manual(values = colors_RD7) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position = "none") +
  background_grid()

# Marginal emissions
data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
# data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up

ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_marginal , group=scenario_split, color = scenario_split)) +
  geom_line() +
  labs(y = "Marginal Cost of CO2 Emissions Mitigation (USD/tonne CO2)", x = NULL) +
  scale_fill_manual(values = colors_RD7) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position = "none") +
  background_grid()


## Average and marginal emissions cost 
data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build", "scenario_split"))
data.plot[variable=="cost_emissions_reduction_pTonneCO2_marginal", variable:= "Marginal Cost"][variable=="cost_emissions_reduction_pTonneCO2_average", variable:= "Average Cost"]

ggplot(data.plot, aes(scenario_build, value , group=interaction(scenario_split, variable), color = scenario_split, linetype = variable)) +
  geom_line() +
  labs(y = "Cost of CO2 Emissions Mitigation (USD/tonne CO2)", x = NULL) +
  scale_fill_manual(values = colors_RD7) + 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position = "top", legend.title = element_blank()) +
  background_grid()