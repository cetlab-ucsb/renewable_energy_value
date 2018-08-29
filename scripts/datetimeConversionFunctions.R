## Author: Ranjit Deshmukh
## 07/16/2016

## This script does the following
## 1) Includes functions to convert between year, month, day, interval to time, and vice versa. 


#################################################################################################################################
#### FUNCTION TO CONVERT "Year", "Month", "Day", "Hour" columns into a "dateTime" column with POSIXct date time class ##########
#### The dateTime column can then be manipulated with the lubridate or other date time package ##################################
YMDHtoDateTime <- function(inputData){
  inputData[,"dateTime"] <- ymd_hms(paste(inputData[,Year],"-",inputData[,Month],"-",inputData[,Day]," ", (as.integer(inputData[,Hour])-1),":00:00",sep=""))
  inputData
}
################################################################################################################################

#################################################################################################################################
#### FUNCTION TO CONVERT "dateTime" column with POSIXct date time class to "Year", "Month", "Day", "Hour" columns ##########
#### The Year, Month, Day, and Hour columns can then be converted into the Plexos format ##################################
#### This function also deletes the dateTime column #############
DateTimetoYMDH <- function(inputData){
  if (class(inputData[,dateTime])!= "POSIXct") {inputData$dateTime <- ymd_hms(inputData$dateTime)}
  inputData[,Year:=year(dateTime)][,Month:=month(dateTime)][,Day:=day(dateTime)][,Hour:=hour(dateTime)+1] ## 1 is added to Hour if hour is 1:24. POSIXct hour is 0:23 
  inputData[,dateTime:=NULL]
  inputData
}
################################################################################################################################

##########################################################################################################################################
#### FUNCTION TO CONVERT "Year", "Month", "Day", "Hour", and one data column to wide Plexos format with each "Hour" as a column ##########
#### The Year, Month, Day, and Hour columns can then be converted into the Plexos format ##################################
YMDHtoPlexos <- function(inputData){
  dataColumnName <- setdiff(colnames(inputData), c("Year", "Month", "Day", "Hour")) 
  outputData <- dcast(inputData, Year + Month + Day ~ Hour, value.var = eval(dataColumnName))
  outputData
}
################################################################################################################################

##########################################################################################################################################
#### FUNCTION TO CONVERT wide Plexos format with each "Interval" as a column to "Year", "Month", "Day", "Interval", and one data column ##########
PlexosToYMDinterval <- function(inputData, timeIntervalColumn, dataColumn){
  #inputData[,eval(dataColumnName):=round(.SD,roundDigits), .SDcols=c(eval(dataColumnName))] # In case you want to round any digits
  outputData <- melt(inputData, id.vars = c("Year", "Month", "Day"), variable.name = eval(timeIntervalColumn), value.name = eval(dataColumn))
  outputData <- outputData[order(Year, Month, Day, get(timeIntervalColumn))]
  outputData[, eval(dataColumn) := as.numeric(get(dataColumn))] # make sure data is numeric and not character
  outputData
}
################################################################################################################################


#################################################################################################################################
#### FUNCTION TO COMPUTE THE NUMBER OF TIME INTERVALS IN A DAY ##################################################################
timeIntervalsDay <- function(inputData, dateTimeColumn){
  timeIntervalDay.num <- nrow(inputData[get(dateTimeColumn) < (inputData[[1,dateTimeColumn]] + 3600*24),])
  return(timeIntervalDay.num)
}
################################################################################################################################

#################################################################################################################################
#### FUNCTION TO CONVERT "Year", "Month", "Day", "Interval" columns into a "dateTime" column with POSIXct date time class ##########
#### The dateTime column can then be manipulated with the lubridate or other date time package ##################################
#### Pass the name of the time interval column to the function, whether it is "Interval" or "Hour" or some other columns name.
#### Year, Month, Day column names need to be labeled as is. The 5*60 was added to shift the datetime index by 5 minutes, otherwise
#### the first interval is 00:00:15, and 4th is 01:00:00, which is considered the next hour. Need first 4 intervals in first hour
#### The function also removes the year, month, day and interval columns
## THIS FUNCTION STARTS TIME AT 00:10:00 I.E. 10 MINUTES
YMDIntervaltoDateTime <- function(inputData, timeIntervalColumn){
  if (class(inputData[, get(timeIntervalColumn)])=="factor"){
    inputData[,eval(timeIntervalColumn)] <- as.integer(as.character(inputData[,get(timeIntervalColumn)]))
  }
  dailyTimeInterval.num <- max(inputData[,get(timeIntervalColumn)])
  hourlyTimeInterval.num <- dailyTimeInterval.num/24
  ## Subtract 5 mins from the time so the hours align to POSIXct, which goes frmo 0-23 hours, which intervals go from 1-24h for an hourly time series
  inputData[,dateTime:= ymd_hms(paste0(inputData[,Year],"-",inputData[,Month],"-",inputData[,Day]," ", (as.integer(inputData[,get(timeIntervalColumn)]%/%(hourlyTimeInterval.num))),":",
                                       (as.integer((60/hourlyTimeInterval.num)*inputData[,get(timeIntervalColumn)]%%hourlyTimeInterval.num)), ":00")) - 5*60]
  inputData[order(dateTime)]
  inputData
}

## THIS FUNCTION STARTS TIME AT 00:00:00 I.E. 0 MINUTES. DATA AVERAGED AT BEGINNING OF HOUR
YMDIntervaltoDateTime.starttime0 <- function(inputData, timeIntervalColumn){
  # dailyTimeInterval.num <- max(as.integer(as.character(inputData[,get(timeIntervalColumn)])))
  dailyTimeInterval.num <- max(inputData[,get(timeIntervalColumn)])
  hourlyTimeInterval.num <- dailyTimeInterval.num/24
  ## Subtract 5 mins from the time so the hours align to POSIXct, which goes frmo 0-23 hours, which intervals go from 1-24h for an hourly time series
  inputData[,dateTime:= ymd_hms(paste0(inputData[,Year],"-",inputData[,Month],"-",inputData[,Day]," ", (as.integer(inputData[,get(timeIntervalColumn)]%/%(hourlyTimeInterval.num))),":",
                                       (as.integer((60/hourlyTimeInterval.num)*inputData[,get(timeIntervalColumn)]%%hourlyTimeInterval.num)), ":00")) - 15*60]
  inputData[order(dateTime)]
  inputData
}
################################################################################################################################

#################################################################################################################################
#### FUNCTION TO CONVERT "dateTime" column with POSIXct date time class to "Year", "Month", "Day", "Interval" columns ##########
#### The Year, Month, Day, and Interval columns can then be converted into the Plexos format ##################################
#### This function also deletes the dateTime column #############
#### This function uses the timeIntervalsDay function to compute the number of time intervals in a day, e.g. 96 for 15 min data, 24 for hourly data ##
DateTimetoYMDinterval <- function(inputData, dateTimeColumn, timeIntDay.fun){
  if (class(inputData[,get(dateTimeColumn)])[1]!= "POSIXct") {inputData[, eval(dateTimecolumn):= ymd_hms(inputData[,eval(dateTimeColumn)])]}
  dailyTimeInt.num <- timeIntDay.fun(inputData, eval(dateTimeColumn)) # Get the time intervals in a day
  inputData[,Year:=year(get(dateTimeColumn))][,Month:=month(get(dateTimeColumn))][,Day:=day(get(dateTimeColumn))][,Interval:=rep(1:dailyTimeInt.num, times=365, each=1)] ## Add the Intervals separate from the date time column values 
  inputData[,(eval(dateTimeColumn)):=NULL]
  inputData
}
################################################################################################################################

#################################################################################################################################
#### A MORE GENERIC FUNCTION TO CONVERT "dateTime" column with POSIXct date time class to "Year", "Month", "Day", "Interval" columns ##########
#### The Year, Month, Day, and Interval columns can then be converted into the Plexos format ##################################
#### This function also deletes the dateTime column #############
#### This function uses the timeIntervalsDay function to compute the number of time intervals in a day, e.g. 96 for 15 min data, 24 for hourly data ##
DateTimetoYMDinterval2 <- function(inputData, dateTimeColumn){
  if (class(inputData[,get(dateTimeColumn)])[1]!= "POSIXct") {inputData[, eval(dateTimecolumn):= ymd_hms(inputData[,eval(dateTimeColumn)])]}
  inputData[,Year:=year(get(dateTimeColumn))][,Month:=month(get(dateTimeColumn))][,Day:=day(get(dateTimeColumn))][,Hour:=hour(get(dateTimeColumn))]
  inputData[,hourRep:=sequence(rle(inputData$Hour)$lengths)] ## Number of repeats of the hour
  maxRep = max(inputData[,hourRep]) # Get the max number of intervals in a hour
  inputData[,Interval:=Hour*maxRep + hourRep] # Calculate the interval
  inputData[,hourRep:=NULL]
  inputData
}
################################################################################################################################

##########################################################################################################################################
#### FUNCTION TO CONVERT "Year", "Month", "Day", "Interval", and one data column to wide Plexos format with each "Interval" as a column ##########
#### The Year, Month, Day, and Interval columns can then be converted into the Plexos format ##################################
#### roundDigits is an integer input for the number of digits after decimal that are to be rounded off ########################
YMDintervaltoPlexos <- function(inputData, timeIntervalColumn, roundDigits){
  dataColumnName <- setdiff(colnames(inputData), c("Year", "Month", "Day", eval(timeIntervalColumn))) 
  inputData[,eval(dataColumnName):=round(.SD,roundDigits), .SDcols=c(eval(dataColumnName))]
  outputData <- dcast(inputData, Year + Month + Day ~ get(timeIntervalColumn), value.var = eval(dataColumnName))
  outputData
}
################################################################################################################################


#### FUNCTION TO ADD ROWS TO PLEXOS DATASETS ESPECIALLY FOR LOOKAHEAD FUNCTION IN PLEXOS ################################################
####Input data should be in Plexos format ##############################################################################################
add.days.to.Plexos <- function(plexosData, numDaysToAdd){
  for (d in 1:numDaysToAdd){
    add.row <- as.data.table(tail(plexosData, 1))
    last.date <- ymd(paste0(add.row[1,Year], "-", add.row[1,Month], "-", add.row[1,Day]))
    new.date <- last.date + days(1)
    plexosData <- rbind(plexosData, add.row[,c("Year", "Month", "Day"):= list(year(new.date), month(new.date), day(new.date))])
  }
  return(plexosData)
}
##########################################################################################################################################

