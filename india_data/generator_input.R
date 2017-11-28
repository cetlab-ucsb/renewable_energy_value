# Author - Ranjit
# Date - 09/29/2016

# This script creates the data sets for the India unit commitment and economic dispatch model

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

## INPUTS #######################################################################################################
working.directory <- "G:/SWITCH/india_ED/india_data"

setwd(working.directory)
generator.data <- fread("generator.data.csv") # Generator data with capacity
node.data <- fread("node.data.csv") # Nodes with regions and zones
gen.fuel.data <- fread("map_generators_to_fuel.csv") # generators with fuels
gen.var.cost <- fread("map_generators_to_vom_charges.csv") # generators with variable cost
gen.ownership.raw <- fread("ownerships_posoco.csv") # generators with ownership
gen.ownership <- gen.ownership.raw[`/ BEGIN` == "Generator",] 
colnames(gen.ownership) <- c("class1", "class2", "collection", "Generator", "owner_type")
fuel.type.mapping <- fread("fuel_type.csv") # fuel and type mapping table for ED algorithm
hydro.data <- fread("hydro_combo_table_daily.csv") # Hydro data with scada and RLDC daily min and max MW and daily generation
## Load
load.data <- fread("Demandhourly2014-2032-India.csv") # Load data from 2014 to 2032

## User inputs
create.single.gen.hydro = "yes"
create.single.gen.mustrun = "yes"

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
# Join generators with regions and zones
generator.data <- left_join(generator.data, node.data)
print(paste0("Number of rows with no region association: ", nrow(generator.data[is.na(Region)])))
# Join generators with fuel
generator.data <- left_join(generator.data, gen.fuel.data)
print(paste0("Number of rows with no fuel association: ", nrow(generator.data[is.na(Fuel)])))
# Join generators with variable cost
generator.data <- left_join(generator.data, gen.var.cost)
print(paste0("Number of rows with no cost data association: ", nrow(generator.data[is.na(`VO&M Charge`)])))
# Join generators with ownership
generator.data <- left_join(generator.data, gen.ownership)
print(paste0("Number of rows with no ownership association: ", nrow(generator.data[is.na(owner_type)])))

# change column names"
setnames(generator.data, c("Generator", "Node", "Max Capacity", "VO&M Charge", "Fuel", "Min Stable Level", "Region", "Zone"), c("generator", "node", "gen_capacity", "var_cost", "fuel", "min_gen", "state", "region"))

# Mapping fuel with the type of generator as treated by the ED algorithm
generator.data <- left_join(generator.data, fuel.type.mapping)

# Change the cost of hydro to zero
generator.data <- generator.data[fuel %in% c("HYDRO-ROR", "HYDRO-PONDAGE", "HYDRO-PUMPED", "HYDRO-STORAGE"), var_cost:= 0]
# Correcting RAPS nuclear generator capacity
generator.data <- generator.data[generator=="GEN_132005_RAPS_A2_220_1", gen_capacity:= 100]

# Summarize variable cost by fuel and state
var_cost.mean <- generator.data %>%
  group_by(fuel, state) %>%
  dplyr::summarise(var_cost = mean(var_cost, na.rm=TRUE))

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
setnames(hydro.data, c("Generator.Name"), c("generator"))
print(paste0("Total generators in hydro.data: ", length(unique(hydro.data[,generator]))))
hydro.no.data <- hydro.data[!(day %in% seq(1,31)),]
print(paste0("Generators with no data: ", nrow(hydro.no.data), "; capacity: ", sum(hydro.no.data[,`Max Capacity`]), "; ", sum(hydro.no.data[,`Max Capacity`])/sum(hydro.data[,`Max Capacity`])*100, "% of total capacity"))
hydro.yes.data <- hydro.data[(day %in% seq(1,31)),]
print(paste0("Generators with daily generation data: ", nrow(hydro.yes.data)/365))

# Intersecting the generation table with the hydro table and keeping only the intersected generators
generator.data.hydro <- generator.data[fuel %in% c("HYDRO-STORAGE", "HYDRO-PONDAGE", "HYDRO-ROR", "HYDRO-PUMPED"),] # Fetch the hydro generators from the generator data table
print(paste0("Total hydro generators in generator.data: ", length(unique(generator.data.hydro[,generator]))))
hydro.data.merged <- inner_join(hydro.yes.data, generator.data.hydro[, .SD, .SDcols=c("generator", "gen_capacity")])
hydro.data.not.merged <- anti_join(hydro.yes.data, generator.data.hydro[, .SD, .SDcols=c("generator", "gen_capacity")])
print(paste0("Number of generators removed after merging: ", (nrow(hydro.yes.data)-nrow(hydro.data.merged))/365, "; Capacity reduced: ", (sum(hydro.yes.data[,`Max Capacity`])-sum(hydro.data.merged[,gen_capacity]))/365, " out of ", sum(hydro.yes.data[,`Max Capacity`])/365, "MW"))

# Take out the pumped hydro units before any data is manipulated, because generation data from those plants may not conform to the data rules for other hydro
hydro.data.pumped <- hydro.data.merged[Fuel %in% c("HYDRO-PUMPED"),]

# If daily max energy >= gen_capacity*24, then change daily max energy to capacity*24. Ensure that the total daily energy does not exceed 100% CF
hydro.data.merged <- hydro.data.merged[daily.GWh > gen_capacity*24, daily.GWh:= gen_capacity*24]

# If daily max generation > gen capacity, then change daily max gen to gen capacity. Can't generate more than the rated capacity
hydro.data.merged <- hydro.data.merged[daily.max > gen_capacity, daily.max:= gen_capacity]

# if daily min generation > gen capacity AND [daily scada min gen >0 but < gen capacity], then daily min generation = daily min gen from scada [ because most likely these data are from RLDC, and we should use good scada data]
hydro.data.merged <- hydro.data.merged[daily.min > gen_capacity & daily.min.scada > 0 & daily.min.scada < gen_capacity, daily.min:= daily.min.scada]

# If daily min generation is still > gen capacity, then change daily min to be equal to gen capacity
hydro.data.merged <- hydro.data.merged[daily.min > gen_capacity, daily.min:= gen_capacity]

# Add date format column and then add week and day of the year column
hydro.data.merged <- hydro.data.merged[,date:= as.Date(paste0(day, "-", month, "-", year), format="%d-%m-%Y")]
hydro.data.merged <- hydro.data.merged[,week:= as.numeric(format(date, "%U"))]
hydro.data.merged <- hydro.data.merged[,Day:= yday(date)] # Capital 'D' Day is for day of the year. small d is day of the month. Day is used in output

# Estimate weekly average energy and min gen
hydro.data.merged <- hydro.data.merged[,daily.GWh.weekly.avg:= mean(daily.GWh), by=.(generator,week)]
hydro.data.merged <- hydro.data.merged[,daily.min.weekly.avg:= mean(daily.min), by=.(generator,week)]

# Estimate monthly average energy and min gen
hydro.data.merged <- hydro.data.merged[,daily.GWh.monthly.avg:= mean(daily.GWh), by=.(generator,month)]
hydro.data.merged <- hydro.data.merged[,daily.min.monthly.avg:= mean(daily.min), by=.(generator,month)]

# Depending on the preference, estimate daily min gen based on moving average (or equate to weekly or monthly min gen)
movAvgWindow <- 7 # weekly moving average window
hydro.data.merged <- hydro.data.merged[, daily.min.final:= mav(.SD, movAvgWindow), .SDcols=c("daily.min")]
hydro.data.merged <- hydro.data.merged[is.na(daily.min.final), daily.min.final:= daily.min.weekly.avg] #Convert the NAs at the beginning and end of the year to weekly averages

# For storage hydro, set daily energy value to weekly average energy (could use monthly average, but weekly trends are important)
hydro.data.merged <- hydro.data.merged[, daily.GWh.final:= 0]
hydro.data.merged <- hydro.data.merged[Fuel == "HYDRO-STORAGE", daily.GWh.final:= daily.GWh.weekly.avg]

# For Hydro RoR and Pondage, estimate the daily energy final using a moving average
hydro.data.merged <- hydro.data.merged[Fuel %in% c("HYDRO-ROR", "HYDRO-PONDAGE"), daily.GWh.final:= mav(.SD, movAvgWindow), .SDcols=c("daily.GWh")]
hydro.data.merged <- hydro.data.merged[is.na(daily.GWh.final), daily.GWh.final:= daily.GWh.weekly.avg] #Convert the NAs at the beginning and end of the year to weekly averages

# For pumped hydro, set daily energy value to weekly average energy (could use monthly average, but weekly trends are important)
hydro.data.merged <- hydro.data.merged[Fuel == "HYDRO-PUMPED", daily.GWh.final:= daily.GWh.weekly.avg]

# Ensure that the daily min generation throughout the day does not exceed the daily energy
hydro.data.merged <- hydro.data.merged[daily.min.final*24 > daily.GWh.final*1000, daily.min.final:= daily.GWh.final*1000/24]
# for weekly or monthly energy limits, ensure that the sum of daily min gens across week or month * 24 <= weekly energy or monthly energy.
# Need to add code for this

# Add a column for daily.MWh.final if using MWh in ED
hydro.data.merged <- hydro.data.merged[,daily.MWh.final:= daily.GWh.final*1000]

# Print the major stats
print(paste0("total hydro generation: ", sum(hydro.data.merged[,daily.GWh.final]), "MWh"))


## OUTPUTS ################################
# Keeping only generators in hydro.data table
generator.data.no.hydro <- generator.data[!(fuel %in% c("HYDRO-STORAGE", "HYDRO-PONDAGE", "HYDRO-ROR", "HYDRO-PUMPED")),] # non-hydro generators from the generator data table
generator.data.hydro.filtered <- generator.data.hydro[generator %in% unique(hydro.data.merged[,generator]),] # Only hydro generators from hydro.data that have daily energy limits
print(paste0("Hydro generators in generator.data: ", nrow(generator.data.hydro), " and now remaining from hydro.data: ", nrow(generator.data.hydro.filtered)))

print(paste0("Total number of generators before hydro filtering: ", nrow(generator.data)))
generator.data <-rbind(generator.data.no.hydro, generator.data.hydro.filtered)
print(paste0("Total number of generators after hydro filtering: ", nrow(generator.data)))

# Generator types to exclude
fuel.to.exclude <- c("HYDRO-PUMPED", "WIND", "SOLAR-PV")
generator.data.output <- generator.data[!(fuel %in% fuel.to.exclude), ]

# calculate the total generation capacity and average cost for each fuel and use it to convert certain fuels to single generators
generator.data.output.capacity <- generator.data.output[, lapply(.SD,sum), by=fuel, .SDcols=c("gen_capacity")]
generator.data.output.cost <- generator.data.output[, lapply(.SD,mean), by=fuel, .SDcols=c("var_cost")]

################### generator output for ED #################
cols.to.output <- c("generator", "gen_capacity", "var_cost", "type")
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


write.table(generator.data.output.writeout[,.SD, .SDcols = cols.to.output],file="../india_ED_input/gen_all_input_cc_ccgt_diesel.csv",sep=",",row.names=F)

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

