# Function for plotting supply curves for the REV paper
# May 11 2019
# Ranjit Deshmukh


setwd('/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/scripts/')


scenario.econ.dispatch = "ClcC70m" #sc.econ.dispatch
scenario.new.conv.capacity = "coallc"  #sc.new.conv.capacity
scenario_mix = c("S0W0", "S50W150", "S100W300", "S150W450")

p_supply_demand <- list()

for (sc_mix in 1:length(scenario_mix)){
  results.econ.dispatch <- fread(paste0(output.folder, scenario.econ.dispatch, "/", "dispatch_all_gen_", scenario_mix[sc_mix], "_", scenario.econ.dispatch, ".csv"))
  
  ## Calculate the net load with and without curtailment and with and without must run generation ##
  
  # with curtailment means after curtailment
  results.econ.dispatch[, net_load_w_curt_w_mustrun := Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other + `HYDRO-PONDAGE` + `HYDRO-ROR` + NUCLEAR]
  
  # without curtailment means before curtailment due to technical/economic reasons. So conventional generators need to satisfy less energy if we 
  # were able to absorb all the renewable energy that would otherwise have been curtailed. IN this case, net load is lower.
  results.econ.dispatch[, net_load_wo_curt_w_mustrun := Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other + `HYDRO-PONDAGE` + `HYDRO-ROR` + NUCLEAR - curtailment]
  
  # with curtailment means after curtailment. Net load after must run dispatch
  results.econ.dispatch[, net_load_w_curt_wo_mustrun := Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other]
  
  # without curtailment means before curtailment due to technical/economic reasons. IN this case, net load is lower. Net load after must run dispatch
  results.econ.dispatch[, net_load_wo_curt_wo_mustrun := Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other - curtailment]
  
  
  ## Create the supply curves from existing and new generation ##
  gen.list.costs.existing <- fread(paste0(input.folder, "gen_all_input_cc_ccgt_diesel", ".csv"))
  gen.list.costs.new <- fread(paste0(input.folder,"new_conventional_capacity/", scenario.new.conv.capacity, "/2030_new_conventional_capacity_", scenario_mix[sc_mix], "_", scenario.new.conv.capacity, ".csv"))
  gen.list.costs.all <- rbind(gen.list.costs.existing, gen.list.costs.new)
  gen.list.costs.all <- gen.list.costs.all[order(var_cost)] # sort by var cost
  gen.list.costs.all[, var_cost_usd := var_cost /INR_USD] # Convert var cost to usd per MWh
  gen.list.costs.all[, gen_capacity_GW := gen_capacity/10^3] # Convert generation capacity to GW
  gen.list.costs.all <- gen.list.costs.all[type != "hydro",] # remove dispatchable hydro
  gen.list.costs.all <- gen.list.costs.all[var_cost_usd < 200,]
  
  # Apply outage rates (See python economic dispatch script for outage rates.)
  outage_rate_coal = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
  outage_rate_gas_ct = 0.1 
  outage_rate_gas_ccgt = 0.1 
  outage_rate_diesel = 0.2 
  outage_rate_other = 0.3 
  gen.list.costs.all[type %in% c("coal", "gas_ccgt", "gas_ct"), gen_capacity_GW := gen_capacity_GW * (1-outage_rate_coal)] # Combined 3 techs here, but change if outage rates are different
  gen.list.costs.all[type == "diesel", gen_capacity_GW := gen_capacity_GW * (1-outage_rate_diesel)]
  gen.list.costs.all[type == "other", gen_capacity_GW := gen_capacity_GW * (1-outage_rate_other)]
  
  # Rename the types
  gen.list.costs.all[type == "coal", type := "Coal"]
  gen.list.costs.all[type == "gas_ccgt", type := "Gas CCGT"]
  gen.list.costs.all[type == "gas_ct", type := "Gas CT"]
  gen.list.costs.all[generator == "NUCLEAR", type := "Nuclear"]
  gen.list.costs.all[generator == "HYDRO-ROR", type := "Hydro RoR"]
  gen.list.costs.all[generator == "HYDRO-PONDAGE", type := "Hydro RoR"]
  gen.list.costs.all[type == "other", type := "Other"]
  gen.list.costs.all[type == "diesel", type := "Diesel"]
  
  # Generator list without mustrun
  gen.list.costs.all.wo.mustrun <- gen.list.costs.all[!type %in% c("Nuclear", "Hydro RoR"),]
  # Cumulative generation capacity
  
  gen.list.costs.all[, gen_capacity_cumulative := cumsum(gen_capacity_GW)]
  gen.list.costs.all.wo.mustrun[, gen_capacity_cumulative := cumsum(gen_capacity_GW)]
  
  ## Plot supply curves and net demand
  
  colors.conv.gen <- c("#5588bb", "#66bbbb", "#aa6644", "#99bb55","#ee9944", "#444466", "#bb5555")
  
  # p_supply_demand <- ggplot(gen.list.costs.all, aes(gen_capacity_cumulative, var_cost_usd, color = type)) +
  #   geom_point(stat = "identity") +
  #   #theme_bw() +
  #   labs(y = "Variable Cost (USD/MWh)", x = "Generation and Demand (GW)" ) +
  #   scale_color_manual(values = colors.conv.gen) + 
  #   theme(legend.title = element_blank()) 
  
  p_supply_demand[[sc_mix]] <- ggplot(results.econ.dispatch, aes(y = net_load_w_curt_w_mustrun/10^3, x = 75)) + 
    geom_boxplot(color = "black", size = 0.2, width = 150) + coord_flip()
  
  p_supply_demand[[sc_mix]] <- p_supply_demand[[sc_mix]] + geom_point(data = gen.list.costs.all, aes(y = gen_capacity_cumulative, x = var_cost_usd, color = type), stat = "identity") +
    labs(x = "Variable Cost\n(USD/MWh)", y = "Supply and Demand (GW)" ) +
    scale_color_manual(values = colors.conv.gen) + 
    scale_y_continuous(limits = c(0, 400)) +
    scale_x_continuous(limits = c(0, 160)) +
    theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank()) + 
    annotate("text", x=130, y=20, label=scenario_mix[sc_mix], angle=0, size=3, color = "#636363")
  
}

g_supply_demand_RE0 <- ggplotGrob(p_supply_demand[[1]] + theme(legend.position = "none", axis.title.y = element_blank()))

g_supply_demand_RE200 <- ggplotGrob(p_supply_demand[[2]] + theme(legend.position = "none", axis.title = element_blank()))

g_supply_demand_RE400 <- ggplotGrob(p_supply_demand[[3]] + theme(legend.position = "none", axis.title = element_blank()))

g_supply_demand_RE600 <- ggplotGrob(p_supply_demand[[4]] + theme(legend.position = "none", axis.title = element_blank()))

plot.legend <- g_legend(p_supply_demand[[1]] + theme(legend.title = element_blank()))

grob_supply_demand.plot <- 
  arrangeGrob(arrangeGrob(grobs = 
                list(rbind(g_supply_demand_RE600, g_supply_demand_RE400, g_supply_demand_RE200, g_supply_demand_RE0, size = "first")), ncol = 1,
                left=textGrob("Variable Cost (USD/MWh)", gp=gpar(fontsize=10), rot=90)),
              plot.legend, ncol=2,widths=c(4,1))
              
grid.newpage()
grid.draw(grob_supply_demand.plot)

ggsave(file = paste0(output.folder.paper, "Supply_Demand_Base_S25pcW75pc_S0W0", "_v1", ".png"), grob_supply_demand.plot, width=7, height=5)
