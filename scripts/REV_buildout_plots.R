# Function for plotting new capacity buildout for the REV paper
# May 12 2019
# Ranjit Deshmukh


setwd('/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/scripts/')

scenarios.function <- c("load_modD50M0_energyOnly") # "load_modD50M0_energyOnly"
# scenario.econ.dispatch = "ClcC70m" #sc.econ.dispatch
# scenario.new.conv.capacity = "coallc"  #sc.new.conv.capacity
# scenario_mix = c("S0W0", "S50W150", "S100W300", "S150W450")

p_buildout.func <- list()
p_buildout_new_conv.func <- list()
p_buildout_vre.func <- list()
sc.func = 1
theme_set(theme_cowplot(font_size=11))

## Capacity buildout stack #########
capacity.buildout <- results.processed[scenario_main == scenarios.function[sc.func], .(scenario, scenario_build, new_capacity_coal_MW, new_capacity_gas_ccgt_MW, new_capacity_gas_ct_MW, capacity_solarPV_MW, capacity_wind_MW, capacity_bat_storage_MW)]
setnames(capacity.buildout, c("new_capacity_coal_MW", "new_capacity_gas_ccgt_MW", "new_capacity_gas_ct_MW", "capacity_solarPV_MW", "capacity_wind_MW", "capacity_bat_storage_MW"), c("New Coal", "New Gas CCGT", "New Gas CT", "Solar PV", "Wind", "Battery Storage"))
capacity.buildout.melt <- melt(capacity.buildout, id.vars = c("scenario", "scenario_build"))
setnames(capacity.buildout.melt, c("variable", "value"), c("technology", "capacity_MW"))
capacity.buildout.melt[, capacity_GW:= capacity_MW/10^3]

## PLots with new and existing conventional generators plus solar and wind and battery capacities
# Existing capacity
scenario_list <- capacity.buildout[, .(scenario, scenario_build)]
existing.gen.capacity.all.scenarios <- cbind(existing.gen.capacity[rep(seq(nrow(existing.gen.capacity)), 16)], scenario_list[rep(seq_len(nrow(scenario_list)), each=9),])
existing.gen.capacity.all.scenarios[technology=="coal", technology:= "Existing Coal"]
existing.gen.capacity.all.scenarios[technology=="gas_ccgt", technology:= "Existing Gas CCGT"]
existing.gen.capacity.all.scenarios[technology=="gas_ct", technology:= "Existing Gas CT"]
existing.gen.capacity.all.scenarios[technology=="diesel", technology:= "Existing Diesel"]
existing.gen.capacity.all.scenarios[technology=="hydro_storage", technology:= "Existing Hydro Storage"]
existing.gen.capacity.all.scenarios[technology=="hydro_pondage", technology:= "Existing Hydro RoR"]
existing.gen.capacity.all.scenarios[technology=="hydro_ror", technology:= "Existing Hydro RoR"]
existing.gen.capacity.all.scenarios[technology=="nuclear", technology:= "Existing Nuclear"]
existing.gen.capacity.all.scenarios[technology=="other", technology:= "Existing Other"]
existing.gen.capacity.all.scenarios[, capacity_GW:= capacity_MW/10^3]

# join the new and existing buildouts
capacity.all <- rbind(capacity.buildout.melt, existing.gen.capacity.all.scenarios)

# Plot the buildout
capacity.all$technology <- reorder.factor(capacity.all$technology, new.order=positions.tech)
capacity.all$scenario <- reorder.factor(capacity.all$scenario, new.order=positions3)
capacity.all <- capacity.all[order(-technology)]

#colors.capacity.buildout <- c("#CC79A7", "#D55E00", "#56B4E9", "#0072B2", "#000000", "#CC3333", "#FF6600", "#999999", "#FF9900", "#006699", "#CC3333", "#FF6600", "#999999")
colors.capacity.buildout <- c("#666666", "#FF6600", "#CC3333", "#2ca25f", "#FF9900", "#006699", "#999999", "#FF6600", "#CC3333", "#000000", "#0072B2", "#56B4E9", "#D55E00", "#CC79A7")

p_buildout.func[[sc.func]] <- ggplot(capacity.all, aes(scenario, capacity_GW, fill = technology, order = technology)) +
  geom_bar(stat = "identity", width = width.bar) +
  labs(y = "Installed Capacity (MW)", x = NULL) +
  scale_fill_manual(values = colors.capacity.buildout) + 
  scale_x_discrete(breaks=positions3, labels=share.labels2) +
  theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.title = element_blank()) +
  background_grid()

## PLots with new conventional generators plus inverted solar and wind and battery capacities
# Separate the new conventional and storage buildouts
capacity.conv.bat <- capacity.buildout.melt[technology %in% c("New Coal", "New Gas CCGT", "New Gas CT", "Battery Storage"), ]
capacity.vre <- capacity.buildout.melt[technology %in% c("Solar PV", "Wind"), ]

positions.tech.conv.bat <- c("Battery Storage", "New Gas CT", "New Gas CCGT", "New Coal")
positions.tech.vre <- c("Solar PV", "Wind")

# Plot the buildout
capacity.conv.bat$technology <- reorder.factor(capacity.conv.bat$technology, new.order=positions.tech.conv.bat)
capacity.conv.bat$scenario <- reorder.factor(capacity.conv.bat$scenario, new.order=positions3)
capacity.conv.bat <- capacity.conv.bat[order(-technology)]

capacity.vre$technology <- reorder.factor(capacity.vre$technology, new.order=positions.tech.vre)
capacity.vre$scenario <- reorder.factor(capacity.vre$scenario, new.order=positions3)
capacity.vre <- capacity.vre[order(technology)]

#colors.capacity.buildout <- c("#CC79A7", "#D55E00", "#56B4E9", "#0072B2", "#000000", "#CC3333", "#FF6600", "#999999", "#FF9900", "#006699", "#CC3333", "#FF6600", "#999999")
colors.capacity.buildout.conv.bat <- c("#2ca25f", "#CC3333", "#FF6600", "#666666")
colors.capacity.buildout.vre <- c("#FF9900", "#006699")

p_buildout_new_conv.func[[sc.func]] <- ggplot(capacity.conv.bat, aes(scenario, capacity_GW, fill = technology, order = technology)) +
  geom_bar(stat = "identity", width = width.bar) +
  facet_grid(.~scenario_build, scales = "free_x", space = "free_x") +
  labs(y = "New Conventional \nInstalled Capacity (MW)", x = NULL) +
  scale_fill_manual(values = colors.capacity.buildout.conv.bat) + 
  scale_x_discrete(breaks=positions3, labels=share.labels2) +
  scale_y_continuous(limits = c(0, 600)) +
  theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1), legend.title = element_blank(), legend.position = c(0, 0.65)) +
  background_grid()

p_buildout_vre.func[[sc.func]] <- ggplot(capacity.vre, aes(scenario, capacity_GW, fill = technology, order = technology)) +
  geom_bar(stat = "identity", width = width.bar) +
  facet_grid(.~scenario_build, scales = "free_x", space = "free_x") +
  labs(y = "RE Installed Capacity \n(MW)", x = NULL) +
  scale_fill_manual(values = colors.capacity.buildout.vre) + 
  scale_y_reverse() +
  scale_x_discrete(breaks=positions3, labels=share.labels2, position = "top") +
  theme(legend.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.background = element_blank(), strip.text.x = element_blank(), legend.position = c(0, 0.23)) +
  background_grid()

g_buildout_new_conv <- ggplotGrob(p_buildout_new_conv.func[[1]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))

g_buildout_vre <- ggplotGrob(p_buildout_vre.func[[1]]) 

grob_new_buildout.plot <- arrangeGrob(grobs = list(rbind(g_buildout_new_conv, g_buildout_vre, size = "first")), ncol = 1)

grid.newpage()
grid.draw(grob_new_buildout.plot)

ggsave(file = paste0(output.folder.paper, "new_buildout_", scenarios.function[sc.func], "_v0", ".png"), grob_new_buildout.plot, width=6, height=4)
