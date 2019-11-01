# Function for plotting heat maps and  for the REV paper
# May 25 2019
# Ranjit Deshmukh


library(RColorBrewer)

setwd('/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/scripts/')


scenario.econ.dispatch = c("ClcC70m") #sc.econ.dispatch
scenario.new.conv.capacity = c("coallc")  #sc.new.conv.capacity
scenario_mix = c("S0W0", "S50W150", "S100W300", "S150W450", "S150W50", "S300W100", "S450W150")

for (sc_econ_disp in 1:length(scenario.econ.dispatch)){
  for (sc_mix in 1:length(scenario_mix)){
    results.econ.dispatch <- fread(paste0(output.folder, scenario.econ.dispatch[sc_econ_disp], "/", "dispatch_all_gen_", scenario_mix[sc_mix], "_", scenario.econ.dispatch[sc_econ_disp], ".csv"))
    
    ## Calculate the net load with and without curtailment and with and without must run generation ##
    
    # with curtailment means after curtailment
    results.econ.dispatch[, net_load_w_curt_w_mustrun := (Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other + `HYDRO-PONDAGE` + `HYDRO-ROR` + NUCLEAR)/10^3]
    
    # without curtailment means before curtailment due to technical/economic reasons. So conventional generators need to satisfy less energy if we 
    # were able to absorb all the renewable energy that would otherwise have been curtailed. IN this case, net load is lower.
    results.econ.dispatch[, net_load_wo_curt_w_mustrun := (Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other + `HYDRO-PONDAGE` + `HYDRO-ROR` + NUCLEAR - curtailment)/10^3]
    
    # with curtailment means after curtailment. Net load after must run dispatch
    results.econ.dispatch[, net_load_w_curt_wo_mustrun := (Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other)/10^3]
    
    # without curtailment means before curtailment due to technical/economic reasons. IN this case, net load is lower. Net load after must run dispatch
    results.econ.dispatch[, net_load_wo_curt_wo_mustrun := (Coal + `Gas-CCGT` + `Gas-CT` + Diesel + Other - curtailment)/10^3]
    
    # VRE potential generation in GW (including curtailment)
    results.econ.dispatch[, vre_gen_before_curtailment_GW := (wind + solarPV + curtailment)/10^3]
    
    # VRE generation in GW (after curtailment)
    results.econ.dispatch[, vre_gen_after_curtailment_GW := (wind + solarPV)/10^3]
    
    ## Add date, month and hour columns and data
    results.econ.dispatch[, day_of_year := floor((Timepoint-1)/24)]
    results.econ.dispatch[, date_result := as.Date(day_of_year, origin = "2030-01-01")]
    results.econ.dispatch[, month_result := month(date_result)]
    results.econ.dispatch[, day_of_month := day(date_result)]
    results.econ.dispatch[, month_abbr_result := month(date_result, label = TRUE, abbr = TRUE)]
    results.econ.dispatch$hour_of_day = rep(seq(1:24), 365)
    
    ## Top peak net load hours - Sort net demand and plot top net demand hours by hour of day and month
    highest.values.perc.to.plot <- 0.05
    
    results.econ.dispatch.toPlot <- results.econ.dispatch[order(-net_load_w_curt_wo_mustrun)][, .SD[1:(8760 * highest.values.perc.to.plot)]]
    
    month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    results.econ.dispatch.toPlot$month_abbr_result_levels <- factor(df$month_abbr_result, levels=month_levels)
    
    p_hist_peak_net_load <- ggplot(results.econ.dispatch.toPlot, aes(x = hour_of_day)) +
      geom_histogram(binwidth = 0.5, aes(y = ..count..), colour = "royalblue2", fill = "royalblue2") + # if you need to plot density, use "aes(y =..density..)"
      facet_wrap(~month_abbr_result, ncol=6, drop=FALSE) +
      scale_x_continuous(limits = c(-1,24), breaks = seq(0,24,6)) + labs(x = "hour of the day") + 
      ggtitle(paste0("Top ", highest.values.perc.to.plot*100, "% of peak net load hours for ", scenario_mix[sc_mix], " and ", scenario.econ.dispatch[sc_econ_disp], " scenario"))
    p_hist_peak_net_load
    
    ggsave(file = paste0(output.folder.paper, "net_load/peak_net_load_hours_", highest.values.perc.to.plot*100, "perc_", scenario_mix[sc_mix], "_", scenario.econ.dispatch[sc_econ_disp], "_v0", ".png"), p_hist_peak_net_load, width=7, height=5)
  
    ## Heat map of net load hours [ Use net_load_w_curt_wo_mustrun - This exactly how much conventional fossil generation you need.]
    # [Important to exclude must run and hydro dispatch because the latter is optimized by the model. Top net load hours will not have curtailment.]
    hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
    
    p_net_load_heat_map <- ggplot(results.econ.dispatch, aes(day_of_month, hour_of_day, fill=net_load_w_curt_wo_mustrun))+
      geom_tile(color= "white",size=0) + 
      scale_fill_gradientn(name = "Net Load (GW)", colours = hm.palette(100)) + 
      #scale_fill_gradient(name = "Net Load (GW)", low = "blue", high = "red") + 
      facet_grid(~month_abbr_result) +
      scale_y_continuous(trans = "reverse", breaks = c(6, 12, 18, 24)) +
      #scale_y_continuous(trans = "reverse", breaks = unique(results.econ.dispatch$hour_of_day)) +
      scale_x_continuous(breaks =c(1,15,31)) +
      theme_minimal(base_size = 8) +
      labs(x="Day", y="Hour") +
      theme(legend.position = "bottom", legend.title=element_text(size=8), legend.text=element_text(size=6))+
      theme(plot.title=element_text(size = 14, hjust=0), strip.background = element_rect(colour="white"))+
      theme(axis.text=element_text(size=8), axis.ticks=element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    p_net_load_heat_map
    ggsave(file = paste0(output.folder.paper, "net_load/net_load_", scenario_mix[sc_mix], "_", scenario.econ.dispatch[sc_econ_disp], "_v0", ".png"), p_net_load_heat_map, width=8, height=4)
    
    # Heat map of VRE generation after curtailment
    
    p_vre_gen_heat_map <- ggplot(results.econ.dispatch, aes(day_of_month, hour_of_day, fill=vre_gen_after_curtailment_GW))+
      geom_tile(color= "white",size=0) + 
      scale_fill_gradientn(name = "VRE potential generation (GW)", colours = hm.palette(100)) + 
      #scale_fill_gradient(name = "Wind (GW)", low = "blue", high = "red") + 
      facet_grid(~month_abbr_result) +
      scale_y_continuous(trans = "reverse", breaks = c(6, 12, 18, 24)) +
      #scale_y_continuous(trans = "reverse", breaks = unique(results.econ.dispatch$hour_of_day)) +
      scale_x_continuous(breaks =c(1,15,31)) +
      theme_minimal(base_size = 8) +
      labs(x="Day", y="Hour") +
      theme(legend.position = "bottom", legend.title=element_text(size=8), legend.text=element_text(size=6))+
      theme(plot.title=element_text(size = 14, hjust=0), strip.background = element_rect(colour="white"))+
      theme(axis.text=element_text(size=8), axis.ticks=element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    p_vre_gen_heat_map
    ggsave(file = paste0(output.folder.paper, "vre/vre_after_curt_", scenario_mix[sc_mix], "_", scenario.econ.dispatch[sc_econ_disp], "_v0", ".png"), p_vre_gen_heat_map, width=8, height=4)
    
  }
}