## Author: Ranjit Deshmukh
## Initiated 09/05/2018
## This script does the following
## 1) Functions for load forecasts


## FUNCTION ########################################################################################################
########### FUNCTION TO CALCULATE TOTAL ENERGY, PEAK LOAD, BASE LOAD, LOAD FACTOR AND OTHER STATISTICS ###########
#### Provide input table with column names as the jurisdiction/ state, and "Year", "Month", "Day', "Hour" columns

statisticsEnergyData <- function(dataEnergy, timeIntervalColumn){
  columnNames <- colnames(dataEnergy[,!c("Year", "Month", "Day", eval(timeIntervalColumn)), with=FALSE])
  #  dailyTimeInt.num <- max(as.integer(as.character(dataEnergy[,get(timeIntervalColumn)]))) # Get the time intervals in a day
  dailyTimeInt.num <- max(dataEnergy[,get(timeIntervalColumn)]) # Get the time intervals in a day
  dataEnergy.m1 <- melt(dataEnergy, measure.vars = columnNames, variable.name = "state", value.name = "demand")
  statisticsEnergy <- dataEnergy.m1[, .(sum(demand)/(1000*dailyTimeInt.num/24), max(demand), min(demand), sum(demand)/(max(demand)*.N), min(demand)/max(demand), quantile(demand, c(0.01))), by = .(Year, state)]
  setnames(statisticsEnergy, c("V1", "V2", "V3", "V4", "V5", "V6"), c("energy", "peakLoad", "baseLoad", "loadFactor", "baseToPeakRatio", "quantileLoad1pc"))
  print("Demand Below Zero")
  print(dataEnergy.m1[demand<=0, .N, by = .(Year, state)])
  return(statisticsEnergy)
}

#################################################################################################################

## FUNCTION ########################################################################################################
########### FUNCTION TO ADJUST LOW DEMAND VALUES TO ZERO OR SOME BASE LOAD LEVEL ###########
#### Provide input table with column names as the jurisdiction/ state, and "Year", "Month", "Day', "Hour" columns
#### This function returns the adjusted demand in the same format as the input data. 

adjustLowDemandValues <- function(dataEnergy, baseLoadToPeakDemand){
  columnNames <- colnames(dataEnergy[,!c("Year", "Month", "Day", "Hour"), with=FALSE])
  dataEnergy.m1 <- melt(dataEnergy, measure.vars = columnNames, variable.name = "state", value.name = "demand")
  
  ## Covert all the negative values to zero
  dataEnergy.m1[demand<0, demand:=0]
  ## Calculate the peak demand for the year for each state
  dataEnergy.m1[, peakDemand:= .(max(demand)), by = .(Year, state)]
  ## Display the number of records that have a demand value that is below the level determined by the user defined ratio of base load to peak load 
  print(paste0("Demand below ", baseLoadToPeakDemand*100, "% of peak load"))
  print(dataEnergy.m1[demand < baseLoadToPeakDemand*peakDemand, .N, by = .(Year, state)])
  print("Adjusting demand to minimum base load level")
  ## Convert the values that are below the minimum base load to the minimum base load
  dataEnergy.m1[demand < baseLoadToPeakDemand*peakDemand, demand:=baseLoadToPeakDemand*peakDemand]
  ## Recast the data table into the original form
  dataEnergy.m1 <- dataEnergy.m1[, !c("peakDemand"), with=FALSE] ## Remove the added column of peakDemand
  dataEnergyOutput <- dcast(dataEnergy.m1, Year + Month + Day + Hour ~ state, value.var = "demand")
  return(dataEnergyOutput)  
}

#################################################################################################################

## FUNCTION ############################################################################################
########## INTERMITTENT TO CONTINUOUS FORECAST FUNCTION ##############################################
## THIS FUNCTION TAKES AN INTERMITTENT FORECAST E.G. FORECAST FOR JUST A FEW INTERMITTENT YEARS
## AND FILLS THE FORECAST FOR THE REMAINING YEARS

intermittentToContinousForecast <- function(baseValue, intermittentForecast, baseYear, lastYear){
  forecastYears = seq(baseYear, lastYear)
  # Create empty data tables for energy demand and peak demand forecasts for all years
  forecast.allYears <- data.table("Year"= forecastYears)
  ## calculate the rate for each interval of forecast years
  # First create the foreast table with years
  #rate.forecast <- dcast.data.table(melt(intermittentForecast, id.vars = "stateNode"), variable ~ stateNode)
  rate.forecast <- intermittentForecast
  setnames(rate.forecast, colnames(rate.forecast), c("year", "forecast"))
  # Second insert the base year
  rate.forecast <- rbind(rate.forecast, list(as.integer(baseYear), baseValue)) # add data from the base year
  rate.forecast <- setDT(rate.forecast, keep.rownames = FALSE)[order(year)] # order by year
  # Third create the forecast ratio, year difference and rate calculations
  rate.forecast[,forecastRatio:=c(tail(rate.forecast[,forecast], -1)/head(rate.forecast[,forecast],-1),0)] # ratio of forecast
  rate.forecast[,yearDiff:=c(tail(rate.forecast[,year], -1) - head(rate.forecast[,year],-1),0)] # difference in years
  rate.forecast[,rate:= (forecastRatio)^(1/yearDiff)-1]
  rate.forecast <- head(rate.forecast,-1)
  # Fourth Loop through all the years to fill in the forecast for the missing years. 
  # In reality, the algorithm calculates the forecasts for all years using the base year value
  for (j in 1:length(forecastYears)){
    yearCurr <- forecastYears[j]
    #print(yearCurr)
    # Find the reference year for this year from the rate.forecast table
    refYear <- max(subset(rate.forecast, year <= eval(yearCurr))$year)
    refRate <- rate.forecast[year==refYear, rate]
    refForecast <- rate.forecast[year==refYear, forecast]
    # Calculate the forecast
    if (yearCurr == baseYear){
      forecast.allYears[1,"forecast":=eval(baseValue)]
      #print("yay")
    }else {
      forecast.allYears[j, "forecast"] = refForecast*(1+refRate)^(yearCurr-refYear)
      #print("yay2")
    }
  }
  
  # return the result
  return(forecast.allYears)
}

##################################################################################################


## FUNCTION #########################################################################################################################
########### QUADRATIC FIT TIME SERIES FORECAST FUNCTION #############################################################################
#### The input data should have columns for Year, Month, Day and Hour and one column with the column name = state.name 
#### (or any other name fed to the function).  #######################
#### The forecast input data set should have Year, state, energy, and peakLoad columns ####

quadraticTimeSeriesForecast <- function(inputData, forecast, state.name, yearCurr, yearForecast){
  demand.working <- inputData[(Year == eval(yearCurr) & !((Month == 2)&(Day==29))),] # Removing February 29th if the current year is leap
  intMult <- nrow(demand.working)/8760 # for intervals other than 1 hour
  #demand.working <- data.table(demandCurr = dataEnergy[(yearCurr == eval(yearCurr) & !((Month == 2)&(Day==29))),get(state.name)]) # Removing February 29th if the current year is leap
  demand.working[, actualInterval:= seq(1,nrow(demand.working))] # actual interval whether hour, 15 minutes, or 5 minutes
  demand.working <- demand.working[order(get(state.name))] # Sort the data by demand
  demand.working[, sortedInterval:= seq(1,nrow(demand.working))] # Provide an ID for the sorted demand
  E.y1 <- sum(demand.working[,get(state.name)]/(8760/nrow(demand.working))) # This demand is in MWH, or whatever units the interval data is in. 8760/nrow ensures that sum is equal to total energy, in case interval is less than an hour.
  E.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), energy]*1000 # Assuming that the energy forecast is in GWh, and peak load is in MWh
  Dmin.y1 <- ifelse(min(demand.working[,get(state.name)])>=0, min(demand.working[,get(state.name)]), 0) # Ensure data is non-negative. Two states (Sikkim and Manipur) has a negative minimum demand - strange!
  ## Peak load for the current year as maximum from the provided data
  Dmax.y1 <- max(demand.working[, get(state.name)])
  ## Peak load for the forecast year, pull from the forecast table
  Dmax.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), peakLoad]
  ## Peak load growth rate
  peakLoadRate <- (Dmax.y2 - Dmax.y1)/Dmax.y1
  
  ## This FOR loop is to reduce the growth rate of the base load gradually to ensure that the coefficients of the quadratic equation are positive.
  ## This will ensure that all the values of the forecast are greater than the base load of the base year.
  ## Soemtimes if the growth rate of the peak load is very high, but the growth rate of energy is low, then the quadratic function will also lead to 
  ## forecast values that are lower than the base year base load. Hence, one cannot assume the base load will grow at the same rate as the peak load
  ## Need to reduce the base load growth rate progressively till we get a reasonable starting point for the quadratic curve that will ensure that all 
  ## forecast values are higher than the base year base load. However, there could be instances where even zero base load growth will lead to forecast
  ## values lower tha base year base load because the quadratic function is just not going to yield an appropriate result (see warning(. The quadratic function does not
  ## ensure that the curve is tangential to the x axis at zero (base load). Perhaps another function such as an exponential function may work. 
  ## For intervals other than 1 hour, need to change the constants, and the equation *****************
  const1 <- 8759
  const2 <- (8759)^2
  const3 <- (8759)^3
  LHS <- matrix(data = c((8759)^3/3, (8759)^2/2, (8759)^2, 8759), nrow=2, ncol=2, byrow=TRUE)
  for (r in c(0.8,0.6, 0.4, 0.2, 0)){
    baseLoadRate <- r*peakLoadRate
    Dmin.y2 <- Dmin.y1*(1+baseLoadRate)
    peakOverBaseInc <- (Dmax.y2 - Dmax.y1) - (Dmin.y2 - Dmin.y1)
    energyOverBaseInc <- (E.y2 - E.y1) - (Dmin.y2 - Dmin.y1)*8760
    RHS <- matrix(data=c(energyOverBaseInc, peakOverBaseInc))
    quadSolution <- solve(LHS, RHS)
    if ((energyOverBaseInc >= 0)&(peakOverBaseInc >= 0)&(quadSolution[1] >= 0)&(quadSolution[2] >= 0)) {
      #print("Quad solution break")
      break
    } #else print("no quad solution break")
    if ((r==0)&(quadSolution[2]<=0)) print("Base load growth rate is zero but quadratic solution coefficient is still negative")
  }  
  ## demand forecast(i) is equal to demandCurrent + base load increase + the quadratic component
  demand.working[,"forecast" := (demand.working[,get(state.name)] + (Dmin.y2 - Dmin.y1) +  (quadSolution[1]*(sortedInterval-1)^2 + quadSolution[2]*(sortedInterval-1)))]
  ## Warning if some of the forecast numbers are below the base load of the base year, in which case the interval (hour) of the base load will be different
  if (demand.working[sortedInterval==1, forecast] != min(demand.working[,forecast])) print("Base load interval has changed")
  ## Sort the demand forecast on the actual interval or hour, and provide the result with only the interval (hour) and the forecast
  demand.result <- demand.working[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", "Hour", "forecast")]
  setnames(demand.result, "forecast", eval(state.name))
  demand.result$Year = eval(yearForecast)
  ## In case the forecast year is a leap year, add 2/28 rows for 2/29 
  if (yearForecast%%4 == 0){
    demand.result.0229 <- demand.result[(Month==2 & Day==28),] # Copy the data from 0228
    demand.result.0229$Day <- 29 # Change the day to Feb 29
    demand.result <- rbind(demand.result, demand.result.0229)
    demand.result <- YMDHtoDateTime(demand.result) # Add the dateTime column, which is helpful for sorting
    demand.result <- demand.result[order(dateTime)] # Sort on the dateTime column, especially important if forecast year is a leap year
    demand.result[,dateTime:=NULL] # Delete the dateTime column, keeping only the Year, Month, Day, Hour columns
  }
  return(demand.result)
  
}

################################################################################################################

## FUNCTION #########################################################################################################################
########### LINEAR FUNCTION FIT FOR TIME SERIES ADJUSTMENT FUNCTION #############################################################################
#### This function can be used for adjusting a base time series given an energy forecast/shortage target value
#### note that the time series at the head end starts at near zero, and linearly goes up so area under the triangle meets the energy target value.
#### The input data should have columns for Year, Month, Day and Hour and one column with the column name = state.name
#### (or any other name fed to the function). Series can be shorter than a year i.e. 8760 intervals. #######################
#### Note that the output data is in the same format as the input data, but with adjusted demand. Demand is only adjusted upward. 

linearTimeSeriesAdjustment <- function(inputData, energyTarget, state.name, timeIntervalColumn){
  ## Provide the actual interval as an ID, and then a sorted interval ID after sorting the data 
  inputData[, actualInterval:= seq(1,nrow(inputData))] # actual interval whether hour, 15 minutes, or 5 minutes
  inputData <- inputData[order(-(get(state.name)))] # Sort the data by demand
  inputData[, sortedInterval:= seq(0,nrow(inputData)-1)] # Provide an ID for the sorted demand
  intMult <- nrow(inputData)/8760 # for intervals other than 1 hour  
  ## set the base target, i.e. vertical side of the triangle i.e. max base load adjustment for the last hour. 
  baseTarget <- energyTarget*1000*2/(nrow(inputData)-1)/intMult ## Divide by the time interval in hours ## For intervals less than 1 h, divide nrow by intMult
  ## Calculate demand adjustment for each time interval
  inputData[,Adjustment := baseTarget*sortedInterval/(nrow(inputData)-1)]
  ## Add the adjusted demand to the original demand
  inputData[,demandAdjusted := get(state.name) + Adjustment]
  ## select the columns in inputData to be written out
  outputData <- inputData[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", eval(timeIntervalColumn), "demandAdjusted")]
  setnames(outputData, "demandAdjusted", eval(state.name))
  return(outputData)
}

################################################################################################################

## FUNCTION #########################################################################################################################
########### EXPONENTIAL FUNCTION FIT FOR TIME SERIES ADJUSTMENT FUNCTION #############################################################################
#### This function can be used for adjusting a base time series given an energy and peak load forecast/shortage target values
#### note that the time series at the tail end starts at near zero, and asymtotically goes to zero - hence the use of the exponential function
#### The input data should have columns for Year, Month, Day and Time Interval and one column with the column name = state.name
#### (or any other name fed to the function). Function can take any interval. #######################
#### The function uses the exponential function for FIT: D = a*e^(-b*interval), where a=Dtarget and b=Dtarget/Etarget. This ensures that the area
#### under the curve is equal to Etarget. A drawback of this method is that the area under the curve between hour 0 (where D is Dtarget)
#### and 8759 (or total intervals in a year) is less than the Etarget by an amount equal to the area under the curve between 8760 (or total intervals in a year)
#### and infinity. But if that difference is less than 1%, then the exponential function FIT may be considered appropriate. 
#### The function will throw a flag if the diff is more than x%.
#### Note that the output data is in the same format as the input data, but with adjusted demand. Demand is only adjusted upward. 

exponentialTimeSeriesAdjustment <- function(inputData, energyTarget, peakTarget, state.name, timeIntervalColumn){
  ## Provide the actual interval as an ID, and then a sorted interval ID after sorting the data 
  inputData[, actualInterval:= seq(1,nrow(inputData))] # actual interval whether hour, 15 minutes, or 5 minutes
  inputData <- inputData[order(-(get(state.name)))] # Sort the data by demand
  inputData[, sortedInterval:= seq(1,nrow(inputData))] # Provide an ID for the sorted demand
  intMult <- nrow(inputData)/8760 # for intervals other than 1 hour
  ## Check whether the ratio of Demand*number of intervals and energy target is less than 5 to check appropriateness of the exponential function.
  if (peakTarget*nrow(inputData)/intMult/(energyTarget*1000) <= 5) {
    print("Ratio of Demand target* number of intervals and energy target is less than 5.") 
  }
  ## coefficients a and b of the exponential decay function
  acoeff <- peakTarget
  bcoeff <- peakTarget/(energyTarget*1000*intMult) # multiply Etarget to convert GWh to MWh. Also multiple by intMult to account for intervals<1h, because area under curve increases.
  ## Apply the exponential function to the data
  inputData[,Adjustment := acoeff * exp(-(bcoeff)*(sortedInterval-1))]
  ## Apply checks to see whether exponential function is appropriate. Check whether the area from 8760 to infinity is more than 1 percent of energy target
  Etail <- acoeff/(bcoeff * exp(nrow(inputData)*bcoeff))/intMult # Divide the area between end of time interval and infinity by intMult to estimate the "energy" in the tail. Otherwise, it is just area.
  if (Etail >= 0.01*energyTarget*1000) {
    print(paste0("Energy in tail of exponential decay function is more than 1 percent of energy target. Adjustment will not meet energy target for ", state.name))
  } else print ("good for exponential fit")
  ## Add the adjusted demand to the original demand
  inputData[,demandAdjusted := get(state.name) + Adjustment]
  ## select the columns in inputData to be written out
  outputData <- inputData[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", eval(timeIntervalColumn), "demandAdjusted")]
  setnames(outputData, "demandAdjusted", eval(state.name))
  return(outputData)
}

################################################################################################################


## FUNCTION #########################################################################################################################
########### ENERGY-WEIGHTED LINEAR AND EXPONENTIAL DECAY FIT TIME SERIES FORECAST FUNCTION ####################################
#### The input data should have columns for Year, Month, Day and Time Interval and one column with the column name = state.name 
#### (or any other name fed to the function).  #######################
#### The forecast input data set should have Year, state, energy, and peakLoad columns ####

linearExpFitTimeSeriesForecast <- function(inputData, forecast, state.name, yearCurr, yearForecast, expAdjustFactor, timeIntervalColumn){
  demand.working <- inputData[(Year == eval(yearCurr) & !((Month == 2)&(Day==29))),] # Removing February 29th if the current year is leap
  #dailyTimeInt.num <- max(as.integer(as.character(demand.working[,get(timeIntervalColumn)]))) # Get the time intervals in a day
  #intMult <- dailyTimeInt.num/24 # for intervals other than 1 hour
  intMult <- nrow(demand.working)/8760 # for intervals other than 1 hour
  #demand.working <- data.table(demandCurr = dataEnergy[(yearCurr == eval(yearCurr) & !((Month == 2)&(Day==29))),get(state.name)]) # Removing February 29th if the current year is leap
  demand.working[, actualInterval:= seq(1,nrow(demand.working))] # actual interval whether hour, 15 minutes, or 5 minutes
  demand.working <- demand.working[order(get(state.name))] # Sort the data by demand
  demand.working[, sortedInterval:= seq(1,nrow(demand.working))] # Provide an ID for the sorted demand
  E.y1 <- sum(demand.working[,get(state.name)]/intMult) # This demand is in MWH, or whatever units the interval data is in. intMult=nrow/8760 ensures that sum is equal to total energy, in case interval is less than an hour.
  E.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), energy]*1000 # Assuming that the energy forecast is in GWh, and peak load is in MWh
  Dmin.y1 <- ifelse(min(demand.working[,get(state.name)])>=0, min(demand.working[,get(state.name)]), 0) # Ensure data is non-negative. Two states (Sikkim and Manipur) have a negative minimum demand - strange!
  ## Peak load for the current year as maximum from the provided data
  Dmax.y1 <- max(demand.working[, get(state.name)])
  ## Peak load for the forecast year, pull from the forecast table
  Dmax.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), peakLoad]
  ## Peak load growth rate
  peakLoadRate <- (Dmax.y2 - Dmax.y1)/Dmax.y1
  energyRate <- (E.y2 - E.y1)/E.y1
  ## If peak load growth rate is GREATER than energy growth rate, apply the hourly linear energy-weighted growth to demand.
  ## Further, apply the exponential decay function for adjusting the demand upward, weighing more towards the peak load hours, using both energy and peak load adjustment.
  ## If peak load growth rate is SMALLER than energy growth rate, apply the hourly linear peak load-weighted growth to demand.
  ## Further, apply the linear adjustment for adjusting demand upward, weighing more towards the base load hours, using only the energy adjustment.
  ## First normalize all the data by total energy, and then multiply by energy or peak load forecast target
  if (peakLoadRate >= energyRate){
    print(paste0("Exponential fit function to adjust peak load for ", yearCurr, " for ", eval(state.name)))
    demand.working[,"forecastLinear" :=  demand.working[,get(state.name)]*(1+energyRate)]
    if((abs(E.y2 - sum(demand.working[,forecastLinear])/intMult)/E.y2) > 0.001) print(paste0("Energy mismatch in linear exponential fit function for ", yearCurr, ". Mismatch error ", ((E.y2 - sum(demand.working[,forecastLinear])/intMult)/E.y2)*100, "%"))
    ## Compute the target energy adjustment using the exponential adjustment factor (see inputs at beginning of the script)
    energy.adjustment <- (Dmax.y2 - max(demand.working[,forecastLinear]))*8760/expAdjustFactor
    ## Reduce all the forecast values by a set value so that the total reduced energy is equal to the energy target 
    demand.working[,"forecastAdj":= demand.working[,forecastLinear]-(energy.adjustment)/8760]
    peakLoad.adjustment <- Dmax.y2 - max(demand.working[,forecastAdj]) ## This target is just for adjusting the peak upwards
    demand.result <- demand.working[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", eval(timeIntervalColumn), "forecastAdj")]
    setnames(demand.result, "forecastAdj", eval(state.name))
    demand.result <- exponentialTimeSeriesAdjustment(demand.result, energy.adjustment/1000, peakLoad.adjustment, eval(state.name), timeIntervalColumn) ## Energy input should be in GWh
    
  } else{
    print(paste0("Linear fit function to adjust energy into baseload for ", yearCurr, " for ", eval(state.name)))
    demand.working[,"forecastLinear" :=  demand.working[,get(state.name)]*(1+peakLoadRate)]
    if((Dmax.y2 - sum(demand.working[,forecastLinear]))/Dmax.y2 > 0.001) print(paste0("Peak demand mismatch in linear exponential fit function for ", yearCurr, ". Mismatch error ", ((Dmax.y2 - sum(demand.working[,forecastLinear]))/Dmax.y2)*100, "%"))
    ## Compute the target energy adjustment as the difference between the forecast and the energy from the linear extrapolation using peak load growth rate
    energy.adjustment <-  (E.y2 - sum(demand.working[,forecastLinear])/intMult) # inteMult accounts for intervals other than 1 hour. 
    demand.result <- demand.working[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", "Interval", "forecastLinear")]
    setnames(demand.result, "forecastLinear", eval(state.name))
    demand.result <- linearTimeSeriesAdjustment(demand.result, energy.adjustment/1000, eval(state.name), timeIntervalColumn) ## Energy input should be in GWh
    print(paste0("Linear fit function to adjust energy into baseload for ", yearCurr, " for ", eval(state.name)))
    
  }
  
  demand.result$Year = eval(yearForecast)
  ## In case the forecast year is a leap year, add 2/28 rows for 2/29 
  if (yearForecast%%4 == 0){
    demand.result.0229 <- demand.result[(Month==2 & Day==28),] # Copy the data from 0228
    demand.result.0229$Day <- 29 # Change the day to Feb 29
    demand.result <- rbind(demand.result, demand.result.0229)
    demand.result <- YMDIntervaltoDateTime(demand.result, eval(timeIntervalColumn)) # Add the dateTime column, which is helpful for sorting
    demand.result <- demand.result[order(dateTime)] # Sort on the dateTime column, especially important if forecast year is a leap year
    demand.result[,dateTime:=NULL] # Delete the dateTime column, keeping only the Year, Month, Day, Hour columns
  }
  return(demand.result)
  
}

################################################################################################################

## FUNCTION #########################################################################################################################
########### ENERGY-WEIGHTED LINEAR TIME SERIES FORECAST FUNCTION ####################################
#### The input data should have columns for Year, Month, Day and Time Interval and one column with the column name = state.name 
#### (or any other name fed to the function).  #######################
#### The forecast input data set should have Year, state, energy, and peakLoad columns ####

linearEnergyTimeSeriesForecast <- function(inputData, forecast, state.name, yearCurr, yearForecast, timeIntervalColumn){
  demand.working <- inputData[(Year == eval(yearCurr) & !((Month == 2)&(Day==29))),] # Removing February 29th if the current year is leap
  intMult <- nrow(demand.working)/8760 # for intervals other than 1 hour
  #demand.working <- data.table(demandCurr = dataEnergy[(yearCurr == eval(yearCurr) & !((Month == 2)&(Day==29))),get(state.name)]) # Removing February 29th if the current year is leap
  demand.working[, actualInterval:= seq(1,nrow(demand.working))] # actual interval whether hour, 15 minutes, or 5 minutes
  demand.working <- demand.working[order(get(state.name))] # Sort the data by demand
  demand.working[, sortedInterval:= seq(1,nrow(demand.working))] # Provide an ID for the sorted demand
  E.y1 <- sum(demand.working[,get(state.name)]/intMult) # This demand is in MWH, or whatever units the interval data is in. intMult=nrow/8760 ensures that sum is equal to total energy, in case interval is less than an hour.
  E.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), energy]*1000 # Assuming that the energy forecast is in GWh, and peak load is in MWh
  Dmin.y1 <- ifelse(min(demand.working[,get(state.name)])>=0, min(demand.working[,get(state.name)]), 0) # Ensure data is non-negative. Two states (Sikkim and Manipur) has a negative minimum demand - strange!
  ## Peak load for the current year as maximum from the provided data
  Dmax.y1 <- max(demand.working[, get(state.name)])
  ## Peak load for the forecast year, pull from the forecast table
  Dmax.y2 <- forecast[(Year==eval(yearForecast) & state == eval(state.name)), peakLoad]
  ## Peak load growth rate
  peakLoadRate <- (Dmax.y2 - Dmax.y1)/Dmax.y1
  ## Apply the hourly linear energy-weighted growth to demand
  ## First normalize all the data by total energy, and then multiply by energy forecast target
  demand.working[,"forecast" :=  demand.working[,get(state.name)]*E.y2/E.y1]
  if((abs(E.y2 - sum(demand.working[,forecast])/intMult)/E.y2) > 0.001) print(paste0("Energy mismatch in linear extrapolation function for ", yearCurr))
  ## Check whether the peak demand of the linearly forecasted data exceeds or falls short of the peak forecast target
  forecastLoadFactor <- sum(demand.working[,forecast])/intMult/(max(demand.working[,forecast])*8760)
  targetLoadFactor <- E.y2/(Dmax.y2*8760)
  if(max(demand.working[, forecast])>=Dmax.y2) {
    print(paste0("Peak demand of linear energy forecast exceeds peak target for ", eval(state.name), " in ", eval(yearForecast), ". ForecastLF ", eval(forecastLoadFactor), "& target LF ", eval(targetLoadFactor)))
  }
  
  ## Sort the demand forecast on the actual interval or hour, and provide the result with only the interval (hour) and the forecast
  demand.result <- demand.working[order(actualInterval)][,.SD, .SDcols = c("Year", "Month", "Day", eval(timeIntervalColumn), "forecast")]
  setnames(demand.result, "forecast", eval(state.name))
  demand.result$Year = eval(yearForecast)
  ## In case the forecast year is a leap year, add 2/28 rows for 2/29 
  if (yearForecast%%4 == 0){
    demand.result.0229 <- demand.result[(Month==2 & Day==28),] # Copy the data from 0228
    demand.result.0229$Day <- 29 # Change the day to Feb 29
    demand.result <- rbind(demand.result, demand.result.0229)
    demand.result <- YMDIntervaltoDateTime(demand.result, eval(timeIntervalColumn)) # Add the dateTime column, which is helpful for sorting
    demand.result <- demand.result[order(dateTime)] # Sort on the dateTime column, especially important if forecast year is a leap year
    demand.result[,dateTime:=NULL] # Delete the dateTime column, keeping only the Year, Month, Day, Hour columns
  }
  return(demand.result)
}

################################################################################################################

