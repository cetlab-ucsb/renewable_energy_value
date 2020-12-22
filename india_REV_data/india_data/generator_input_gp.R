# Author - Ranjit
# Date - 09/29/2016
# Date v1 - 10/18/2020

# This script creates the data sets for the India unit commitment and economic dispatch model

## Revision history
## v0 - Basic script
## v1 - Outputs for gridpath. Create csvs for generators, hydro energy budgets. 


library(gdata)
library(data.table)
library(lubridate)
library(ggplot2)
library(grid)
library(reshape2)
#library(xlsx)
#library(Hmisc)
library(gridExtra)
#library(xts)
library(stringr)
library(cowplot)
library(dplyr)

## INPUTS #######################################################################################################
working.directory <- "/Users/ranjitster/Dropbox/renewable_energy_value/renewable_energy_value/india_data"

setwd(working.directory)
data.source <- "cea" # "posoco" or "cea"
generator.data <- fread(paste0("base_network_", data.source, "/generator.data.csv")) # Generator data with capacity
node.data <- fread(paste0("base_network_", data.source, "/node.data.csv")) # Nodes with regions and zones
gen.fuel.data <- fread(paste0("map_generators_to_fuel_", data.source, ".csv")) # generators with fuels
gen.var.cost <- fread(paste0("map_generators_to_vom_charges_", data.source, ".csv")) # generators with variable cost
gen.transport.cost <- fread(paste0("transport_charges_cea.csv")) # generators with transport cost; only for CEA
gen.ownership.raw <- fread(paste0("ownerships_", data.source, ".csv")) # generators with ownership
gen.ownership <- gen.ownership.raw[V1 == "Generator",] 
colnames(gen.ownership) <- c("class1", "class2", "collection", "Generator", "owner_type")
fuel.type.mapping <- fread("fuel_type.csv") # fuel and type mapping table for GP
heat.rates <- fread("heat_rate_by_fuel.csv") # heat rates, for CEA
fuel.prices <- fread("fuel_price.csv") # fuel prices, for CEA Note these are Pseudo fuel prices. 
hydro.data <- fread("hydro_combo_table_daily.csv") # Hydro data with scada and RLDC daily min and max MW and daily generation
## Load
load.data <- fread("Demandhourly2014-2032-India.csv") # Load data from 2014 to 2032
## India zones mapping
india.zones.mapping <- fread("../india_gp_data/india_zones.csv")
## Mapping CEA and POSOCO generators
cea.posoco.gen.mapping <- fread("map_cea_posoco_gens.csv") # cea posoco generator mapping

## User inputs
create.single.gen.hydro = "no"
create.single.gen.mustrun = "no"

## KEY PARAMETERS
nuclear.outage.rate <- 0.5 # This rate will define the CFs for the must run generators. Gas and coal outages are defined in the python code for ED
load.year <- c(2022, 2030) # Set the load years

## FUNCTION ########################################################################################################
########### MOVING AVERAGE ########################################################################################
## sides says whether the mean needs to look at both sides of the value, or just one side. n is window size. 
## A similar in-built function is in zoo called rollmean, but that one excludes NAs (which we want)
mav <- function(x , n=5){stats::filter(x, rep(1/n, n), sides=2)}
#################################################################################################################

## ANALYSIS ######################################################################################################
# In generator data, keep only select columns (columns differ for CEA and POSOCO. Deleting min stable level for posoco data)
generator.data <- generator.data[, c("Generator", "Node", "Max Capacity")]
# Join generators with regions and zones (Not including owner of Node)
generator.data <- merge(generator.data, node.data[,!c("Owner")], by = "Node", all.x = TRUE)
print(paste0("Number of rows with no region association: ", sum(is.na(generator.data$Region))))
# Join generators with fuel
generator.data <- merge(generator.data, gen.fuel.data, by = "Generator", all.x = TRUE)
print(paste0("Number of rows with no fuel association: ", sum(is.na(generator.data$Fuel))))

## Heat rates
heat.rates <- heat.rates[((MaxOutputBreaks == 500) | is.na(MaxOutputBreaks)) & Band == 1, ][, c("Fuel", "Heat Rate")]

## Posoco generator cost data is all in var cost. CEA data is split in vom and transport and fuel costs. 
# Join generators with variable cost
generator.data <- merge(generator.data, gen.var.cost, by = "Generator", all.x = TRUE)
print(paste0("Number of rows with no cost data association: ", sum(is.na(generator.data[, "VO&M Charge"]))))
print(paste0("Total generator capacity with non-NA VO&M charges ", sum(generator.data[!is.na(`VO&M Charge`), `Max Capacity`])))
# Add a column for total variable cost. This will be same as VO&M cost for posoco but will be calculated for cea. 
generator.data[, var_cost := `VO&M Charge`]
if(data.source == "cea") {
  # Join generators with transport charges ($/GJ) and heat rates
  generator.data <- merge(generator.data, gen.transport.cost, by = c("Generator", "Fuel"), all.x = TRUE)
  generator.data <- merge(generator.data, heat.rates, by = c("Fuel"), all.x = TRUE)
  generator.data <- merge(generator.data, fuel.prices, by = c("Fuel"), all.x = TRUE)
  
  # Clean up
  # Delete rows with no fuel association [CEA data has 18 rows with generators that are repeated]
  generator.data <- generator.data[!is.na(Fuel), ]
  # Delete rows with VO&M charges as NAs. 
  generator.data <- generator.data[!is.na(`VO&M Charge`), ]
  # Delete rows with generators that are not built
  generator.data <- generator.data[Fuel != "notbuilt", ]
  # Delete those thermal generators that have a heat rate but no transport charge (there is one diesel generator that fits this)
  generator.data <- generator.data[!((Fuel == "DIESEL") & is.na(`Transport Charge`)), ]
  
  # Calculate VO&M charges that include pseudo transport charge (which for GTG was important for multi-band heat rates)
  generator.data[!is.na(`Transport Charge`), `VO&M Charge` := `Transport Charge` * `Heat Rate` + `VO&M Charge`]
  # Calculate total variable cost
  generator.data[`Heat Rate` > 0, var_cost := `VO&M Charge` + Price * `Heat Rate`]
  # Drop the Transport charge column
  generator.data <- generator.data[, !c("Transport Charge")]
}else if (data.source == "posoco"){
  # Change the cost of hydro to zero (mainly for posoco data. cea data is clean)
  generator.data <- generator.data[Fuel %in% c("HYDRO-ROR", "HYDRO-PONDAGE", "HYDRO-PUMPED", "HYDRO-STORAGE"), var_cost := 0]
  # Correcting RAPS nuclear generator capacity
  generator.data <- generator.data[Generator=="GEN_132005_RAPS_A2_220_1", `Max Capacity`:= 100]
}

# Join generators with ownership
generator.data <- merge(generator.data, gen.ownership[, c("Generator", "owner_type")], by = "Generator", all.x = TRUE)
print(paste0("Number of rows with no ownership association: ", sum(is.na(generator.data["owner_type"]))))

# change column names"
if (data.source == "cea"){
  setnames(generator.data, c("Generator", "Node", "Max Capacity", "VO&M Charge", "Fuel", "Region", "Zone", "Voltage", "Heat Rate", "Price"), 
           c("generator_cea", "node", "gen_capacity", "vom", "fuel", "state", "region", "voltage", "heat_rate", "fuel_price"))
}else if (data.source == "posoco"){
  setnames(generator.data, c("Generator", "Node", "Max Capacity", "VO&M Charge", "Fuel", "Region", "Zone", "Voltage"), 
           c("generator_posoco", "node", "gen_capacity", "vom", "fuel", "state", "region", "voltage"))
}

# Join generator names from the "other" data set
setnames(cea.posoco.gen.mapping, c("Generator.Name.cea", "Generator.Name.posoco"), c("generator_cea", "generator_posoco"))
if (data.source == "cea"){
  generator.data <- merge(generator.data, cea.posoco.gen.mapping, by = "generator_cea", all.x = TRUE)
  print(paste0("Capacity for only matched Posoco generators ", sum(generator.data[!is.na(generator_posoco), gen_capacity]), 
               " MW out of ", sum(generator.data[,gen_capacity])))
  print(paste0("Capacity for only matched Posoco HYDRO generators ", sum(generator.data[!is.na(generator_posoco) & like(fuel, "HYDRO"), gen_capacity]), 
               " MW out of ", sum(generator.data[like(fuel, "HYDRO"), gen_capacity])))
}else if (data.source == "posoco"){
  generator.data <- merge(generator.data, cea.posoco.gen.mapping, by = "generator_posoco", all.x = TRUE)
  print(paste0("Capacity for only matched CEA generators ", sum(generator.data[!is.na(generator_cea), gen_capacity]), 
               " MW out of ", sum(generator.data[,gen_capacity])))
}

# Mapping fuel with the type of generator as treated by the ED algorithm
generator.data <- merge(generator.data, fuel.type.mapping, by = "fuel", all.x = TRUE)
generator.data <- merge(generator.data, india.zones.mapping, by = "state", all.x = TRUE)

# Summarize variable cost by fuel and state
var_cost.mean <- generator.data %>%
  group_by(fuel) %>%
  dplyr::summarise(var_cost = mean(var_cost, na.rm=TRUE))

# find mean and premium/adder by fuel type
setDT(var_cost.mean)[,var_cost_min := min(var_cost), by = fuel]
var_cost.mean[, var_cost_adder := round(var_cost - var_cost_min, 2)]


############# HYDRO DATA PROCESSING ########################
## Hydro data has daily min and max values in MW, and daily generation values in GWh  from both RLDC "new" data and scada data ##

## Create 3 new columns for daily min, max and energy
hydro.data <- hydro.data[,c("daily.GWh", "daily.min", "daily.max") := 0]

## Use RLDC "new" data if it is > 0
hydro.data <- hydro.data[daily.GWh.new > 0, daily.GWh:= daily.GWh.new]
hydro.data <- hydro.data[daily.GWh.new > 0 & !(daily.max.new<=0), daily.max:= daily.max.new]
hydro.data <- hydro.data[daily.GWh.new > 0 & !(daily.min.new<=0), daily.min:= daily.min.new]
num.rows.total <- nrow(hydro.data)
num.rows.RLDC <- nrow(hydro.data[daily.GWh.new > 0,])
print(paste0("Number of rows that are NAs in RLDC data: ", nrow(hydro.data[is.na(daily.GWh.new),])))
print(paste0("Number of rows that are used from RLDC data: ", num.rows.RLDC, ", ", num.rows.RLDC/num.rows.total*100, "% of total"))
print(paste0("Total hydro energy from daily GWh RLDC data: ", sum(hydro.data[,daily.GWh])))

## Use scada data if RLDC data <=0 and scada data >0
hydro.data <- hydro.data[!(daily.GWh.new > 0) & daily.GWh.scada > 0, daily.GWh:= daily.GWh.scada]
hydro.data <- hydro.data[!(daily.GWh.new > 0) & daily.GWh.scada > 0 & !(daily.max.scada <= 0), daily.max:= daily.max.scada]
hydro.data <- hydro.data[!(daily.GWh.new > 0) & daily.GWh.scada > 0 & !(daily.min.scada <= 0), daily.min:= daily.min.scada]
num.rows.scada <- nrow(hydro.data[daily.GWh > 0,]) - num.rows.RLDC
print(paste0("Number of rows that are used from SCADA data: ", num.rows.scada, ", ", num.rows.scada/num.rows.total*100, "% of total"))
print(paste0("Total hydro energy from daily GWh data: ", sum(hydro.data[,daily.GWh])))
print(paste0("Number of rows with non-zero daily GWh: ", nrow(hydro.data[daily.GWh > 0,])))
print(paste0("Number of rows with non-zero daily min: ", nrow(hydro.data[daily.min > 0,])))
print(paste0("Number of rows with non-zero daily max: ", nrow(hydro.data[daily.max > 0,])))

## Hydro data cleaning
# Deleting generators with no daily generation data
setnames(hydro.data, c("Generator.Name"), c("generator_posoco"))
print(paste0("Total generators in hydro.data: ", length(unique(hydro.data[,generator_posoco]))))
hydro.no.data <- hydro.data[!(day %in% seq(1,31)),]
print(paste0("Generators with no data: ", nrow(hydro.no.data), "; capacity: ", sum(hydro.no.data[,`Max Capacity`]), "; ", sum(hydro.no.data[,`Max Capacity`])/sum(hydro.data[,`Max Capacity`])*100, "% of total capacity"))
hydro.yes.data <- hydro.data[(day %in% seq(1,31)),]
print(paste0("Generators with daily generation data: ", nrow(hydro.yes.data)/365))

# Intersecting the generation table with the hydro table and keeping only the intersected generators
generator.data.hydro <- generator.data[fuel %in% c("HYDRO-STORAGE", "HYDRO-PONDAGE", "HYDRO-ROR", "HYDRO-PUMPED"),] # Fetch the hydro generators from the generator data table
print(paste0("Total hydro generators in generator.data: ", length(unique(generator.data.hydro[,generator_cea]))))
hydro.data.merged <- merge(hydro.yes.data, generator.data.hydro[, .SD, .SDcols=c("generator_posoco", "generator_cea", "gen_capacity", "zones_gp")])
#hydro.data.not.merged <- anti_join(hydro.yes.data, generator.data.hydro[, .SD, .SDcols=c("generator_posoco", "gen_capacity")])
print(paste0("Number of generators removed after merging: ", (nrow(hydro.yes.data)-nrow(hydro.data.merged))/365, "; Capacity reduced: ", (sum(hydro.yes.data[,`Max Capacity`])-sum(hydro.data.merged[,gen_capacity]))/365, " out of ", sum(hydro.yes.data[,`Max Capacity`])/365, "MW"))

# Take out the pumped hydro units before any data is manipulated, because generation data from those plants may not conform to the data rules for other hydro
hydro.data.pumped <- setDT(hydro.data.merged)[Fuel %in% c("HYDRO-PUMPED"),]

# If daily max energy >= gen_capacity*24, then change daily max energy to capacity*24. Ensure that the total daily energy does not exceed 100% CF
hydro.data.merged <- setDT(hydro.data.merged)[daily.GWh > gen_capacity*24, daily.GWh:= gen_capacity*24]

# If daily max generation > gen capacity, then change daily max gen to gen capacity. Can't generate more than the rated capacity
hydro.data.merged <- hydro.data.merged[daily.max > gen_capacity, daily.max:= gen_capacity]

# if daily min generation > gen capacity AND [daily scada min gen >0 but < gen capacity], then daily min generation = daily min gen from scada [ because most likely these data are from RLDC, and we should use good scada data]
hydro.data.merged <- hydro.data.merged[daily.min > gen_capacity & daily.min.scada > 0 & daily.min.scada < gen_capacity, daily.min:= daily.min.scada]

# If daily min generation is still > gen capacity, then change daily min to be equal to gen capacity
hydro.data.merged <- hydro.data.merged[daily.min > daily.max, daily.min:= daily.max]

# Add date format column and then add week and day of the year column
hydro.data.merged <- hydro.data.merged[,date:= as.Date(paste0(day, "-", month, "-", year), format="%d-%m-%Y")]
hydro.data.merged <- hydro.data.merged[,week:= as.numeric(format(date, "%U"))]
hydro.data.merged <- hydro.data.merged[,Day:= yday(date)] # Capital 'D' Day is for day of the year. small d is day of the month. Day is used in output

# Estimate weekly average energy and min gen
hydro.data.merged <- hydro.data.merged[,daily.mwh.weekly.avg:= mean(daily.GWh)*1000, by=.(generator_posoco,week)]
hydro.data.merged <- hydro.data.merged[,daily.min.weekly.avg:= mean(daily.min), by=.(generator_posoco,week)]

# Estimate monthly average energy and min gen and max gen
hydro.data.merged <- hydro.data.merged[,daily.mwh.monthly.avg:= mean(daily.GWh)*1000, by=.(generator_posoco,month)]
hydro.data.merged <- hydro.data.merged[,daily.min.monthly.avg:= mean(daily.min), by=.(generator_posoco,month)]
hydro.data.merged <- hydro.data.merged[,daily.max.monthly.avg:= mean(daily.max), by=.(generator_posoco,month)]

# Depending on the preference, estimate daily min gen and daily energy based on moving average (or equate to weekly or monthly min gen)
movAvgWindow <- 7 # weekly moving average window
hydro.data.merged <- hydro.data.merged[, daily.min.7day.movavg:= mav(.SD, movAvgWindow), .SDcols=c("daily.min")]
hydro.data.merged <- hydro.data.merged[is.na(daily.min.7day.movavg), daily.min.7day.movavg:= daily.min.weekly.avg] #Convert the NAs at the beginning and end of the year to weekly averages

hydro.data.merged <- hydro.data.merged[, daily.mwh.7day.movavg:= mav(.SD, movAvgWindow)*1000, .SDcols=c("daily.GWh")]
hydro.data.merged <- hydro.data.merged[is.na(daily.mwh.7day.movavg), daily.mwh.7day.movavg:= daily.mwh.weekly.avg] #Convert the NAs at the beginning and end of the year to weekly averages

# For storage hydro, set daily energy value to weekly average energy (could use monthly average, but weekly trends are important)
hydro.data.merged <- hydro.data.merged[, daily.mwh.final:= 0]
hydro.data.merged <- hydro.data.merged[Fuel == "HYDRO-STORAGE", daily.mwh.final:= daily.mwh.weekly.avg]

# For Hydro RoR and Pondage, estimate the daily energy final using a moving average
hydro.data.merged <- hydro.data.merged[Fuel %in% c("HYDRO-ROR", "HYDRO-PONDAGE"), daily.mwh.final:= mav(.SD, movAvgWindow)*1000, .SDcols=c("daily.GWh")]
hydro.data.merged <- hydro.data.merged[is.na(daily.mwh.final), daily.mwh.final:= daily.mwh.weekly.avg] #Convert the NAs at the beginning and end of the year to weekly averages

# For pumped hydro, set daily energy value to weekly average energy (could use monthly average, but weekly trends are important)
hydro.data.merged <- hydro.data.merged[Fuel == "HYDRO-PUMPED", daily.mwh.final:= daily.mwh.weekly.avg]

# Make moving average OR weekly avg OR monthly avg for min gen as final
hydro.data.merged <- hydro.data.merged[, daily.min.final := daily.min.7day.movavg]

# Ensure that the daily min generation throughout the day does not exceed the daily energy
hydro.data.merged <- hydro.data.merged[daily.min.final*24 > daily.mwh.final, daily.min.final:= daily.mwh.final/24]
# for weekly or monthly energy limits, ensure that the sum of daily min gens across week or month * 24 <= weekly energy or monthly energy.
# Need to add code for this

# Fraction of capacity for monthly average, min, and max and daily average (for ror and pondage) - for GP
hydro.data.merged <- hydro.data.merged[, daily.mwh.monthly.avg.fraction := daily.mwh.monthly.avg/(gen_capacity * 24)]
hydro.data.merged <- hydro.data.merged[, daily.min.monthly.avg.fraction := daily.min.monthly.avg/(gen_capacity)]
hydro.data.merged <- hydro.data.merged[, daily.max.monthly.avg.fraction := daily.max.monthly.avg/(gen_capacity)]
hydro.data.merged <- hydro.data.merged[, daily.mwh.avg.fraction := daily.mwh.final/(gen_capacity * 24)]

# Ensure that min fraction is lower than average. Could also ensure avg is less than max, but would likely make max as 1.
# Ensure all fractions are less than 1.
# hydro.data.merged <- hydro.data.merged[daily.max.monthly.avg.fraction > 1, daily.max.monthly.avg.fraction := 1]
#hydro.data.merged <- hydro.data.merged[daily.mwh.monthly.avg.fraction > daily.max.monthly.avg.fraction, daily.mwh.monthly.avg.fraction := daily.max.monthly.avg.fraction]
hydro.data.merged <- hydro.data.merged[daily.mwh.monthly.avg.fraction > 1, daily.mwh.monthly.avg.fraction := 1]
hydro.data.merged <- hydro.data.merged[daily.min.monthly.avg.fraction > daily.mwh.monthly.avg.fraction, daily.min.monthly.avg.fraction := daily.mwh.monthly.avg.fraction]

# Add a column for daily.MWh.final if using MWh in ED
# hydro.data.merged <- hydro.data.merged[,daily.MWh.final:= daily.GWh.final*1000]

# Print the major stats
print(paste0("total hydro generation: ", sum(hydro.data.merged[,daily.mwh.final]), "MWh"))

hydro.capacity.factor.summary <- hydro.data.merged[, .(mean.daily.fraction = mean(daily.mwh.monthly.avg.fraction), capacity = mean(gen_capacity)), by = generator_posoco]
print(paste0("Fraction of hydro capacity that has 0 capacity factor is ", round(sum(hydro.capacity.factor.summary[mean.daily.fraction == 0, capacity])/
               sum(hydro.capacity.factor.summary[mean.daily.fraction != 0, capacity]), 2)))


# Generator-wise monthly avg, min, and max fractions
columns.to.include <- c("generator_posoco", "generator_cea", "zones_gp", "Region", "Zone", "Node.Name", "Fuel", "year", "month", "daily.mwh.monthly.avg", "daily.min.monthly.avg", "daily.max.monthly.avg",
                        "daily.mwh.monthly.avg.fraction", "daily.min.monthly.avg.fraction", "daily.max.monthly.avg.fraction")
hydro.data.merged.monthly <- hydro.data.merged[, .(gen._capacity_mw = mean(gen_capacity)), by = columns.to.include]


## OUTPUTS ################################
hydro.generators.filter = "yes"
if (hydro.generators.filter == "yes"){
  # Keeping only generators in hydro.data table
  generator.data.no.hydro <- generator.data[!(fuel %in% c("HYDRO-STORAGE", "HYDRO-PONDAGE", "HYDRO-ROR", "HYDRO-PUMPED")),] # non-hydro generators from the generator data table
  generator.data.hydro.filtered <- generator.data.hydro[generator_cea %in% unique(hydro.data.merged[,generator_cea]),] # Only hydro generators from hydro.data that have daily energy limits
  print(paste0("Hydro generators in generator.data: ", nrow(generator.data.hydro), " and now remaining from hydro.data: ", nrow(generator.data.hydro.filtered)))
  
  print(paste0("Total number of generators before hydro filtering: ", nrow(generator.data), " Hydro Capacity: ", sum(generator.data[like(fuel, "HYDRO"), gen_capacity])))
  generator.data.output <-rbind(generator.data.no.hydro, generator.data.hydro.filtered)
  print(paste0("Total number of generators after hydro filtering: ", nrow(generator.data.output), " Hydro Capacity: ", sum(generator.data.output[like(fuel, "HYDRO"), gen_capacity])))
}

# Generator types to exclude
fuel.to.exclude <- c("WIND", "SOLAR-PV")
generator.data.output <- generator.data.output[!(fuel %in% fuel.to.exclude), ]

# calculate the total generation capacity and average cost for each fuel and use it to convert certain fuels to single generators
# generator.data.output.capacity <- generator.data.output[, lapply(.SD,sum), by=fuel, .SDcols=c("gen_capacity")]
# generator.data.output.cost <- generator.data.output[, lapply(.SD,mean), by=fuel, .SDcols=c("var_cost")]


################### generator output for GP #################
write.table(generator.data.output,file=paste0("../india_gp_data/existing_generators_", data.source, ".csv"),sep=",",row.names=F)
write.table(hydro.data.merged,file=paste0("../india_gp_data/hydro_daily_data", ".csv"),sep=",",row.names=F)
write.table(hydro.data.merged.monthly,file=paste0("../india_gp_data/hydro_monthly_data_", ".csv"),sep=",",row.names=F)

################### generator output for ED #################
#cols.to.output <- c("generator", "gen_capacity", "var_cost", "type") # for RE value study
cols.to.output <- colnames(generator.data)
generator.data.output.writeout <- generator.data.output[,.SD, .SDcols = cols.to.output]

# Create single generators for hydro storage, hydro ror/pondage and nuclear
if (create.single.gen.hydro == "yes"){
  generator.data.output.writeout <- generator.data.output.writeout[!(type == "hydro")] # First exclude all hydro storage
  row.to.add <- data.table(generator = "HYDRO-STORAGE", gen_capacity = generator.data.output.capacity[fuel=="HYDRO-STORAGE", gen_capacity], 
                          var_cost = generator.data.output.cost[fuel=="HYDRO-STORAGE", var_cost], type = "hydro") # Add single hydro storage plant
  generator.data.output.writeout <- rbind(generator.data.output.writeout, row.to.add)
}

mustrun.fuel <- fuel.type.mapping[type=="mustrun", fuel] # Get the fuels corresponding to must run generators

if (create.single.gen.mustrun == "yes"){
  generator.data.output.writeout <- generator.data.output.writeout[!(type == "mustrun")] # First exclude all must run generators
  for (f in mustrun.fuel){
    row.to.add <- data.table(generator = f, gen_capacity = generator.data.output.capacity[fuel==f, gen_capacity], 
                             var_cost = generator.data.output.cost[fuel==f, var_cost], type = "mustrun") # Add single must run plant
    generator.data.output.writeout <- rbind(generator.data.output.writeout, row.to.add)
  }
}


write.table(generator.data.output.writeout[,.SD, .SDcols = cols.to.output],file="../india_gp_data/existing_generators_", data.source, ".csv",sep=",",row.names=F)

# hydro_max_energy output for storage hydro for ED #################
hydro.max.energy <- hydro.data.merged[Fuel == "HYDRO-STORAGE",.SD, .SDcols = c("Day", "generator", "daily.MWh.final")] # use capital D for Day as day of the year
if (create.single.gen.hydro == "yes"){
  hydro.max.energy.output <- hydro.max.energy[, lapply(.SD, sum), by=Day, .SDcols=c("daily.MWh.final")]
  setnames(hydro.max.energy.output, "daily.MWh.final", "HYDRO-STORAGE")
}else{
  hydro.max.energy.output <- dcast(hydro.max.energy, Day ~ generator, value.var = "daily.MWh.final")
}
write.table(hydro.max.energy.output,file="../india_ED_input/hydro_max_energy.csv",sep=",",row.names=F)

# hydro min gen CFs output for storage hydro for ED ####################
hydro.min.gen <- hydro.data.merged[Fuel == "HYDRO-STORAGE",.SD, .SDcols = c("Day", "generator", "daily.min.final", "gen_capacity")]
if (create.single.gen.hydro == "yes"){
  hydro.min.gen.output <- hydro.min.gen[, lapply(.SD, sum), by=Day, .SDcols=c("daily.min.final", "gen_capacity")]
  hydro.min.gen.output$daily.min.final.CF <- signif(as.numeric(hydro.min.gen.output$daily.min.final)/hydro.min.gen.output$gen_capacity, 3)
  hydro.min.gen.output <- hydro.min.gen.output[,.SD, .SDcols=c("Day", "daily.min.final.CF")]
  setnames(hydro.min.gen.output, "daily.min.final.CF", "HYDRO-STORAGE")
}else{
  hydro.min.gen$daily.min.final.CF <- signif(as.numeric(hydro.min.gen$daily.min.final)/hydro.min.gen$gen_capacity, 3)
  hydro.min.gen.output <- dcast(hydro.min.gen, Day ~ generator, value.var = "daily.min.final.CF")
}
write.table(hydro.min.gen.output,file="../india_ED_input/hydro_min_gen.csv",sep=",",row.names=F)

# must run generators - Hydro ROR, Hydro Pondage, and Nuclear
hydro.mustrun.gen <- hydro.data.merged[Fuel %in% c("HYDRO-ROR", "HYDRO-PONDAGE"),.SD, .SDcols = c("Day", "generator", "daily.MWh.final", "gen_capacity", "Fuel")]
if (create.single.gen.mustrun == "yes"){
  hydro.mustrun.gen.output <- hydro.mustrun.gen[, lapply(.SD, sum), by=.(Day, Fuel), .SDcols=c("daily.MWh.final", "gen_capacity")]
  hydro.mustrun.gen.output$CF <- signif(hydro.mustrun.gen.output$daily.MWh.final/24/hydro.mustrun.gen.output$gen_capacity, 3) # Estimate the capacity factor of generation
  hydro.mustrun.gen.output <- dcast(hydro.mustrun.gen.output, Day ~ Fuel, value.var = "CF")
}else{
  hydro.mustrun.gen$CF <- signif(hydro.mustrun.gen$daily.MWh.final/24/hydro.mustrun.gen$gen_capacity, 3) # Estimate the capacity factor of generation
  hydro.mustrun.gen.output <- dcast(hydro.mustrun.gen, Day ~ generator, value.var = "CF")
}

mustrun.gen.output <- copy(hydro.mustrun.gen.output) # Make a separate copy for the must run generation output that will include nuclear generators


if (create.single.gen.mustrun == "yes"){
  mustrun.gen.output$NUCLEAR <- 1-nuclear.outage.rate
}else{
  nuclear.generators <- generator.data[fuel == "NUCLEAR",generator]
  mustrun.gen.output <- mustrun.gen.output[,eval(nuclear.generators):= (1-nuclear.outage.rate)]
}

write.table(mustrun.gen.output,file="../india_ED_input/mustrun_gen.csv",sep=",",row.names=F)

# Write out all hydro generators list
write.table(hydro.min.gen.output,file="../india_ED_input/hydro_min_gen.csv",sep=",",row.names=F)

# Load data for all specified years
for (y in load.year){
  load.output <- load.data[Year == y, .SD, .SDcols = c("Interval", "India", "dateTime")]
  load.output$Day <- yday(load.output$dateTime)
  setnames(load.output, c("Interval", "India"), c("Timepoint", "load"))
  write.table(load.output,file=paste0("../india_ED_input/load", y, ".csv"),sep=",",row.names=F)
}

