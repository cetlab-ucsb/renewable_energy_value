# Function for plotting individual plots for the REV paper
# May 13 2019
# Ranjit Deshmukh


setwd('/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/scripts/')

theme_set(theme_cowplot(font_size=14))
linetype_vre <- c(rep("solid", 5)) # c("solid", rep("dashed", 3), "solid")

scenarios.to.include.vre <- c("base", "wind30LC", "solar30LC", "wind30LC_solar30LC", "wind30LC_solar30LC_coalHC")
scenarios.labels.vre <- c("base cost wind, solar & coal", "low cost wind", "low cost solar", "low cost wind & solar", "low cost wind & solar + high cost coal")


for sc.to.plot

## Cost of CO2 emissions mitigation
p_cost_emissions_avg <- plot.facet.allLines(results.processed, "cost_emissions_reduction_pTonneCO2_avg", 
                                            scenarios.to.include.vre, scenarios.labels.vre, positions, 
                                            colors_RD7, linetype_vre, expression(Cost~of~CO[2]~Emissions~Mitigation), 
                                            y_limits_cost_emissions, expression(USD/tonne~CO[2]), 
                                            expression(INR/kg~CO[2]), INR_USD/1000)

p_cost_emissions_avg1 <- p_cost_emissions_avg + ggtitle("Wind, solar, and coal costs") + 
  theme(axis.text.x = element_blank(), legend.position = "none")


p_vre_split1 <- p_vre_split + theme(legend.position = "none", axis.text.x = element_blank(), 
                                    axis.title.x = element_blank(), strip.background = element_blank(), 
                                    strip.text.x = element_blank(), axis.text.y.right = element_blank(), 
                                    axis.title.y.right = element_text(size = 26), 
                                    axis.ticks.x.bottom = element_blank(), 
                                    axis.ticks.y.right = element_blank())


gA=ggplot_gtable(ggplot_build(p_cost_emissions_avg1))
gB=ggplot_gtable(ggplot_build(p_vre_split1))
maxWidth = grid::unit.pmax(gA$widths, gB$widths)
gA$widths <- as.list(maxWidth)
gB$widths <- as.list(maxWidth)

grob_emissionsCost.plot4 <- arrangeGrob(gA,gB,nrow=2,heights=c(.8,.3))
grid.newpage()
grid.draw(grob_emissionsCost.plot4)

ggsave(file = paste0(output.folder.paper, "emissions_costs_base_scenario_vre_split", "_v0", ".png"), grob_emissionsCost.plot4, width=7, height=5)
