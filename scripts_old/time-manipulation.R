# time_handling

#' Returns the integer water year given vector of either Date or
#'   POSIXct type
wateryear <- function(dates = Sys.Date()){
  library(lubridate)
  wy <- year(dates)
  wy[month(dates) >= 10] <- wy[month(dates) >= 10] + 1
  wy
}

convertDatesToSameWY <- function(dates, wy = 3001){
  year(dates) <- wy
  year(dates)[month(dates)>=10] <- wy - 1
  dates
}


convertDFToSameWY <- function(inDF,wy = 3001,dateColName = "date"){
  #Given a dataframe with a 'date' column (or other if specified in 'dateColName' argument),
  #  returns the same dataframe with a 'wyDate' column where the dates are converted to
  #  the 3001 water year (or other if specified in 'wy' argument and checks to remove Feb 29 data
  library(lubridate)
  if(all(c("month","day") %in% names(inDF))){
    inDF$wyDate <- as.Date(with(inDF,sprintf("%2g-%2g-%g",month, day, wy)),
                           format="%m-%d-%Y")
  }else{
    inDF$wyDate <- inDF[, dateColName] #copy dates
  }
  
  year(inDF$wyDate) <- wy #make all same year
  inDF <- inDF[!is.na(inDF$wyDate),] #remove bad dates (feb29)
  year(inDF$wyDate[month(inDF$wyDate)>=10]) <- wy-1 #adjust years for months >=Oct
  inDF <- inDF[order(inDF$wyDate),] #sort
  inDF
}

eom <- function(date) {
  #Retrieves the end of month value
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0)
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}


