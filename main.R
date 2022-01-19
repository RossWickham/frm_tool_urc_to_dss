#'
#' Main script to convert from FRM Tool URC Excel tables to a 
#'   DSS time series
#'

#'
#'

#'
#' This script establishes Java settings required to use the dssrip package
#'

# Java base folder relative to main project directory
baseJavaDir = "resources\\java64"

# Configure Java settings for dssrip
options(dss_location=baseJavaDir )
options(dss_jre_location= paste0(baseJavaDir ,"\\java") )
Sys.setenv(JAVA_HOME= paste0(baseJavaDir, "\\java") )

# source("configure_java.R")

# Load packages
# library(dssrip)
library(openxlsx)
library(tidyverse)

# Source utility scripts
dir("scripts", pattern="*.R", full.names = T) %>%
  map(source)

### Config --------------------------------------------------------------------

# Which water year are we using
wy <- wateryear()

# Directory containing URC Excel files
urcDir <- "urc_tables"

# Directory for output DSS
dssDir <- "dss_output"

# Name of the Excel file
excelFileName <- sprintf("%s/%g.xlsx", urcDir, wy)

# Should all month tabs in the Excel be processed?
# If not, default behavior is to process all months prior to now in 
#   the current water year
processAllMonths <- F
# processAllMonths <- T

# Mica storage-elevation curves
mcdbElevStor_NGVD29 <- read_csv("config/mcdb_elev_stor.csv")

### Functions -----------------------------------------------------------------



#' Returns the integer water year given vector of either Date or
#'   POSIXct type
wateryear <- function(dates = Sys.Date()){
  library(lubridate)
  wy <- year(dates)
  wy[month(dates) >= 10] <- wy[month(dates) >= 10] + 1
  wy
}

getMonthsToRead <- function(processAllMonths = F){
  if(processAllMonths){
    processMonths <- 1:12 # Jan-Dec
  }else{
    processMonths <-
      # Get all dates from now to the beginning of the water year
      seq.Date(from = as.Date(sprintf("%g-10-01", wateryear()-1)),
               to = Sys.Date(),
               by = 1) %>%
      # Get the unique months
      month() %>%
      unique()
  }
  # Return the upper-case, three-letter abbreviation of months to read
  month.abb[processMonths] %>%
    toupper()
}

firstRowToNames <- function(inDf){
  names(inDf) <- inDf[1,]
  inDf[-1,]
}

#' Convert the raw URC inputs as read from Excel to data.frame with
#'   columns for date and value
rawURCToDataFrame <- function(rawUrc, wy = wy){
  # Special configuration for URC data
  fwConfig <- fwf_widths(widths = c(5,9,9,rep(8,12)))
  
  rawUrc[,] %>%
    # Interpret as fixed with file
    read_fwf(col_positions = fwConfig) %>%
    t() %>% # transpose rows to columns
    as.data.frame() %>%
    slice(-2) %>%  # remove units row
    firstRowToNames() %>%
    rename(dates = 1) %>% # rename date column
    # Convert all but date column to numeric
    mutate_at(.vars = vars(-("dates")), .funs = as.numeric) %>%
    mutate(
      dates = as.Date(dates, format = "%b %d"),
      dates = convertDatesToSameWY(dates, wy = wy)
    )
}

convertMicaSpaceToElev_NGVD29 <- function(urcDf,
                                          mcdbElevStor_NGVD29,
                                          fullPool_kaf = 20075.475){
  urcDf %>%
    mutate(
      MCDB = fullPool_kaf - MCDB,
      MCDB = approx(x = mcdbElevStor_NGVD29$stor_kaf,
                    y = mcdbElevStor_NGVD29$elev_ngvd29,
                    xout = MCDB)
    )
}

urcDfToTscs <- function(urcDf){
  
}


### Main ----------------------------------------------------------------------


# Determine which month to load given 'processAllMonths' flag, and convert
#   to a string for matching against: needs to be a single element string of
#   month abbreviations separated by pipe operator.  e.g., "DEC|JAN"
monthsMatchPattern <- 
  str_c("(?i)", # (?i) = case insensitive
        getMonthsToRead(processAllMonths = processAllMonths) %>%
          str_c(collapse="|")
  )

# Load DSS file
dssFile <-
  file.path(getwd(), dssDir, "urcs.dss") %>%
  dssrip::readDSS()

if(!dir.exists(dssDir)) dir.create(dssDir)

test <- 
  # Load Excel file
  excelFileName %>%
  openxlsx::loadWorkbook() %>%
  # Load sheet names (three-letter abbreviation of month)
  names() %>% 
  # Restrict only to months of interest
  str_subset(pattern = monthsMatchPattern) %>%
  # Load raw Excel contents to list by month (list by month)
  Map(f = read.xlsx, sheet = ., xlsxFile = list(wb), colNames = list(F)) %>%
  # Convert each raw URC to data.frame
  map(rawURCToDataFrame, wy = wy) %>%
  # Convert Mica space to elevation NGVD29 (list by month)
  map(convertMicaSpaceToElev_NGVD29) %>%
  # Convert data.frame to TimeSeriesContainer
  map(urcDfToTscs, wy = wy) %>%
  # Have a list (months) of lists (TSCs), unnest by one layer
  unlist(recursive = F) %>% 
  # Save to DSS
  map(dssFile$put)




