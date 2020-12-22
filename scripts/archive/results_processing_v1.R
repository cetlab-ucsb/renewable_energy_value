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
results.folder <- "processed_results_coalHC_coalLC_SolarLC20p/"
input.results.fname <- "results_all_scenarios_mod_highCoalCost_allCoal.csv"
emissions.fname <- "emissions.csv"
gen.cost.conv.fname <- "generator_cost_conv.csv"
gen.cost.vre.fname <- "generator_cost_vre.csv"
existing.gen.capacity.fname <- "existing_generation.csv"

coal_cost <- c("high", "low") # Choose high for 2890 and low for 1000
vre_cost <- "low_solar_20pc" # "base", "low_wind_10pc", "low_solar_10pc", "low_vre_10pc", "low_wind_20pc", "low_solar_20pc", "low_vre_20pc"
INR_USD <- 65


## Set working directory ##
setwd(working.directory)

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
gen.cost.conv <- fread(paste0(input.folder, gen.cost.conv.fname))
gen.cost.vre <- fread(paste0(input.folder, gen.cost.vre.fname))
existing.gen.capacity <- fread(paste0(input.folder, existing.gen.capacity.fname))

## Lists for saving plots
p_vre <- list()
p_vre_sys <- list()
p_value <- list()
p_vre_share <- list()
p_curt <- list()
p_avg_em <- list()
p_mar_em <- list()
p_em_combo <- list()
p_em_combo <- list()
p_em_red_perc <- list()
p_em_tot <- list()
p_vre_share_uncurt <- list()

for (sc in 1:length(coal_cost)){
  print(sc)
  
  # Total cost #
  # Choose data based on high or low coal cost
  if(coal_cost[sc] == "high"){
    ann_fc_coal_cost <- gen.cost.conv[technology=="coal", annualized_fc_high_coal]
    results <- results.input[scenario_conv == "high_coal_cost", ]
  }else if(coal_cost[sc] == "low"){
    ann_fc_coal_cost <- gen.cost.conv[technology=="coal", annualized_fc_low_coal]
    results <- results.input[scenario_conv == "low_coal_cost", ]
  }else{
    print("error")
  }
  
  # Choose data based on high or low RE cost
  if(vre_cost == "base"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_vre_base]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_vre_base]/10^3]
  }else if(vre_cost == "low_wind_10pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_wind_low_10pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_wind_low_10pc]/10^3]
  }else if(vre_cost == "low_solar_10pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_solar_low_10pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_solar_low_10pc]/10^3]
  }else if(vre_cost == "low_vre_10pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_vre_low_10pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_vre_low_10pc]/10^3]
  }else if(vre_cost == "low_wind_20pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_wind_low_20pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_wind_low_20pc]/10^3]
  }else if(vre_cost == "low_solar_20pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_solar_low_20pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_solar_low_20pc]/10^3]
  }else if(vre_cost == "low_vre_20pc"){
    results[,ann_cap_cost_solar_mil:= capacity_solarPV_MW * gen.cost.vre[technology=="solarPV", annualized_fc_vre_low_20pc]/10^3]
    results[,ann_cap_cost_wind_mil:= capacity_wind_MW * gen.cost.vre[technology=="wind", annualized_fc_vre_low_20pc]/10^3]
  }
  
 
  
  #### REDO THIS CODE. ONLY ACCEPT CAPITAL COSTS. USE A DISCOUNT RATE AND PLANT LIFE TO ESTIMATE THE ANNUAL COSTS. HAVE ONE COLUMN FOR CAPITAL COSTS OF ALL GENERATION TECHNOLOGIES
  results[,ann_cap_cost_coal_mil:= new_capacity_coal_MW * ann_fc_coal_cost/10^3]
  results[,ann_cap_cost_ccgt_mil:= new_capacity_gas_ccgt_MW * gen.cost.conv[technology=="ccgt", annualized_fc_high_coal]/10^3]
  results[,ann_cap_cost_ct_mil:= new_capacity_gas_ct_MW * gen.cost.conv[technology=="ct", annualized_fc_high_coal]/10^3]

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
  positions2 <- c("0-100", "25-75", "50-50", "75-25", "100-0")
  
  ## VRE cost per MWh
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, cost_vre_pMWh, cost_vre_nocurt_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="cost_vre_nocurt_pMWh", variable:= "No Curtailment"][variable=="cost_vre_pMWh", variable:= "After Curtailment"]
  
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar, position="dodge") +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Levelized Cost of VRE Generation \n USD/MWh", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
    background_grid()
  
  ## System cost for VRE implementation
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, rank, system_cost_vre_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_vre_sys[[sc]] <- ggplot(data.plot, aes(scenario, system_cost_vre_pMWh, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Additional Cost of VRE Implementation \nUSD/MWh of VRE (after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position = "none") +
    scale_y_continuous(limits = c(-10, 120)) +
    background_grid()
  
  
  ##Energy and capacity value per MWh
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, energy_value_pMWh, capacity_value_pMWh)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="energy_value_pMWh", variable:= "Energy Value"][variable=="capacity_value_pMWh", variable:= "Capacity Value"]
  
  p_value[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Average Value of VRE \nUSD/MWh of VRE (after curtailment)", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, 80)) +
    background_grid()
  
  
  ## VRE share
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, vre_share_nocurt, vre_share_after_curt)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build"))
  data.plot[variable=="vre_share_nocurt", variable:= "No Curtailment"][variable=="vre_share_after_curt", variable:= "With Curtailment"]
  
  p_vre_share[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar, position="dodge") +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "VRE share of total electricity generation", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position="top", legend.title = element_blank()) +
    scale_y_continuous(labels=percent) +
    background_grid()
  
  # VRE potential share - only uncurtailed
  data.plot <- data.plot[variable=="No Curtailment",]
  p_vre_share_uncurt[[sc]] <- ggplot(data.plot, aes(scenario, value, fill = variable)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Potential VRE share of total electricity generation", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1), legend.position="none", legend.title = element_blank()) +
    scale_y_continuous(labels=percent) +
    background_grid()
  
  
  ## VRE curtailment
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, rank, vre_curt)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  
  p_curt[[sc]] <- ggplot(data.plot, aes(scenario, vre_curt, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = "Annual VRE curtailment", x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position = "none") +
    scale_y_continuous(labels=percent) +
    background_grid()
  

  ## EMISSIONS
  # % of emissions reduced
  data.plot <- results[scenario!="S0W0", .(scenario, scenario_build, emissions_reduction, total_emissions_co2_milTonnes)]
  data.plot$scenario <- reorder.factor(data.plot$scenario, new.order=positions)
  p_em_red_perc[[sc]] <- ggplot(data.plot, aes(scenario, emissions_reduction, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = expression(CO^{2}~Emissions~Reduction~from~No~RE~Scenario), x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position="none") +
    scale_y_continuous(labels=percent) +
    background_grid()
  
  # Total emissions reduction
  p_em_tot[[sc]] <- ggplot(data.plot, aes(scenario, total_emissions_co2_milTonnes, fill = scenario_build)) +
    geom_bar(stat = "identity", width = width.bar) +
    facet_wrap(~scenario_build, scales = "free_x") + 
    labs(y = expression(Total~CO^{2}~Emissions~(million~tonnes)), x = NULL) +
    scale_fill_manual(values = colors_RD3) + 
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1), legend.position="none") +
    background_grid()
  
  ## CO2 emissions cost of mitigation
  # Average emissions
  data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  # data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario)) # Use if sorting screws up
  
  p_avg_em[[sc]] <- ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_average , group=scenario_split, color = scenario_split, linetype = scenario_split)) +
    geom_line(size=1) +
    labs(y = expression(Average~Cost~of~CO^{2}~Emissions~Mitigation~(USD/tonne~CO^{2})), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-10, 120)) +
    background_grid()
  
  # Marginal emissions
  data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  #data.plot_sorted <- as.data.table(data.plot %>% arrange(scenario_split)) # Use if sorting screws up
  
  p_mar_em[[sc]] <- ggplot(data.plot, aes(scenario_build, cost_emissions_reduction_pTonneCO2_marginal , group=scenario_split, color = scenario_split, linetype = scenario_split)) +
    geom_line(size=1) +
    labs(y = expression(Marginal~Cost~of~CO^{2}~Emissions~Mitigation~(USD/tonne~CO^{2})), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-10, 600)) +
    background_grid()
  
  
  ## Average and marginal emissions cost 
  data.plot <- results.costs.emissions[scenario!="S0W0", .(scenario, scenario_build, scenario_split, cost_emissions_reduction_pTonneCO2_marginal, cost_emissions_reduction_pTonneCO2_average)]
  data.plot$scenario_split <- reorder.factor(data.plot$scenario_split, new.order=positions2)
  data.plot <- melt(data.plot, id.vars = c("scenario", "scenario_build", "scenario_split"))
  data.plot[variable=="cost_emissions_reduction_pTonneCO2_marginal", variable:= "Marginal Cost"][variable=="cost_emissions_reduction_pTonneCO2_average", variable:= "Average Cost"]
  
  p_em_combo[[sc]] <- ggplot(data.plot, aes(scenario_build, value , group=interaction(scenario_split, variable), color = scenario_split, linetype = variable)) +
    geom_line(size=1) +
    labs(y = expression(Cost~of~CO^{2}~Emissions~Mitigation~(USD/tonne~CO^{2})), x = NULL) +
    scale_fill_manual(values = colors_RD7) + 
    theme(axis.text.x = element_text(angle=0, vjust=1), legend.position = "bottom", legend.title = element_blank()) +
    scale_y_continuous(limits = c(-10, 600)) +
    background_grid()
  
}

## Combo Plots #############

## Levelized cost of VRE after curtailment
p.grid <- plot_grid(p_vre[[1]], p_vre[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "vre_curt-levelized-cost-high-low-cost-coal", ".png"), p.grid)

plot.legend <- g_legend(p_vre[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_vre[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_vre[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "vre_curt-levelized-cost-high-low-cost-coal", ".png"), combo.plot)


## System cost of VRE
p.grid <- plot_grid(p_vre_sys[[1]], p_vre_sys[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "vre_integration-cost-high-low-cost-coal", ".png"), p.grid)


## Value of VRE
p.grid <- plot_grid(p_value[[1]], p_value[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "vre_value-high-low-cost-coal", ".png"), p.grid)

plot.legend <- g_legend(p_value[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_value[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_value[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "vre_value-high-low-cost-coal", ".png"), combo.plot)


## VRE share not curtailed
ggsave(file = paste0(results.folder, "vre_share-uncurt-high-low-cost-coal", ".png"), p_vre_share_uncurt[[1]])

## VRE share - both curtailed and not curtailed
p.grid <- plot_grid(p_vre_share[[1]], p_vre_share[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'), ncol = 2)
ggsave(file = paste0(results.folder, "vre_share-curt-uncurt-high-low-cost-coal", ".png"), p.grid)

plot.legend <- g_legend(p_vre_share[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_vre_share[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_vre_share[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "vre_share-curt-uncurt-high-low-cost-coal", ".png"), combo.plot)


## VRE curtailment
p.grid <- plot_grid(p_curt[[1]], p_curt[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "vre_curtailment-high-low-cost-coal", ".png"), p.grid)


## Emissions 
plot.legend <- g_legend(p_mar_em[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_mar_em[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_mar_em[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "emissions-mar-high-low-cost-coal", ".png"), combo.plot)


plot.legend <- g_legend(p_avg_em[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_avg_em[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_avg_em[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "emissions-avg-high-low-cost-coal", ".png"), combo.plot)


## Emissions average and marginal
plot.legend <- g_legend(p_em_combo[[1]])
combo.plot <- arrangeGrob(arrangeGrob(p_em_combo[[1]] + theme(legend.position="none") + ggtitle("A") ,
                                      p_em_combo[[2]] + theme(legend.position="none", axis.title.y=element_blank()) + ggtitle("B"),nrow=1),
                          plot.legend, nrow=2,heights=c(10,1))
ggsave(file = paste0(results.folder, "emissions-avg-mar-high-low-cost-coal", ".png"), combo.plot)

## Emissions reduction in %
p.grid <- plot_grid(p_em_red_perc[[1]], p_em_red_perc[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "emissions-reduction-perc-high-low-cost-coal", ".png"), p.grid)

## Emissions reduction in %
p.grid <- plot_grid(p_em_tot[[1]], p_em_tot[[2]] + theme(axis.title.y=element_blank()), labels = c('A', 'B'))
ggsave(file = paste0(results.folder, "emissions-total-high-low-cost-coal", ".png"), p.grid)

