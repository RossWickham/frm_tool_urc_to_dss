#'
#' Main script to convert from FRM Tool URC Excel tables to a 
#'   DSS time series
#'

#'
#'

suppressPackageStartupMessages({
  # Source utility scripts
  source("configure_dssrip.R")
  dir("scripts", pattern="*.R", full.names = T) %>%
    map(source)
  library(lubridate)
  library(openxlsx)
  library(tidyverse)
})


### Config --------------------------------------------------------------------

# Which water year are we using - must have a corresponding
#   file in the urcDir (e.g., if wy = 2022, file <urcDir>/2022.xlsx
#   must exist)
wy <- wateryear()

# Directory containing URC Excel files
urcDir <- "urc_tables"

# Directory for output DSS
dssDir <- "dss_output"
dssFileName <- "urcs.dss"

# Name of the Excel file
excelFileName <- sprintf("%s/%g.xlsx", urcDir, wy)
# excelFileName <- sprintf("urc_tables/2022_test.xlsx", urcDir, wy)

# Should all month tabs in the Excel be processed?
# If not, default behavior is to process all months prior to now in 
#   the current water year
processAllMonths <- F
# processAllMonths <- T

# Mica storage-elevation curves
mcdbElevStor_NGVD29 <- read_csv("config/mcdb_elev_stor.csv")

### Functions -----------------------------------------------------------------





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

#' Rename columns in dataframe given names and positions
renameCol <- function(inDf, colName, pos){
  names(inDf)[pos] <- colName
  inDf
}

#' Convert the raw URC inputs as read from Excel to data.frame with
#'   columns for date and value
rawURCToDataFrame <- function(rawUrc, wy = wy){
  if(is.null(rawUrc)) return(NULL)
  # Special configuration for URC data
  fwConfig <- fwf_widths(widths = c(5,9,9,rep(8,12)))
  suppressWarnings({
    rawUrc[,] %>%
      # Interpret as fixed with file
      read_fwf(col_positions = fwConfig) %>%
      t() %>% # transpose rows to columns
      as.data.frame() %>%
      slice(-2) %>%  # remove units row
      firstRowToNames() %>%
      renameCol("dates",1) %>% # rename date column
      # Convert all but date column to numeric
      mutate_at(.vars = vars(-("dates")), .funs = as.numeric) %>%
      mutate(
        dates = as.Date(dates, format = "%b %d")
      ) %>%
      mutate(
        
        dates = convertDatesToSameWY(dates, wy = wy)
      )
  })
}

convertMicaSpaceToElev_NGVD29 <- function(urcDf,
                                          mcdbElevStor_NGVD29,
                                          fullPool_kaf = 20075.475){
  urcDf %>%
    mutate(
      MCDB = fullPool_kaf - MCDB
    ) %>%
    mutate(
      MCDB = approx(x = mcdbElevStor_NGVD29$stor_af/1000,
                    y = mcdbElevStor_NGVD29$elev_ft_ngvd29,
                    xout = MCDB)$y
    )
}

#' Form DSS path given the reservoir name and water year and projection month
formDSSPathNames <- function(resvNames, wy, m){
  # Fill the resvName, wy, month into the template
  pathTemplate <- "//%s/ELEV-URC//1DAY/%s-%s/"
  sprintf(pathTemplate, resvNames, wy, m)
}

#'
#' Convert to the URC data.frame to 
#'
urcDfToTscs <- function(urcDf, wy, m){
  # Split each dataframe into a list of data.frames, each with
  #   a column for the date and project URC values
  tryCatch({
    
    resvNames <-  names(urcDf)[-1]
    urcPathNames <- formDSSPathNames(resvNames, wy, m)
    
    # Individual data.frames
    urcDfs <- resvNames %>%
      map(function(x) urcDf[, c("dates", x)])
    
    # Convert to TSCs
    urcTSCs <- Map(dfToURCTimeseries, path = urcPathNames, inDF = urcDfs)
  },
  error = function(e){
    message(e)
    warning(sprintf("\nurcDfToTscs:\tError processing wy %s, month = %s",
                    as.character(wy), as.character(m)))
    return(NULL)
  })
}

dfToURCTimeseries <- function(path, inDF, units = "ft", type = "INST-VAL"){
  # Converts the standard date, event, value
  #   dataframe into a tsc object to save using dssFile$put
  cat("\n",path)
  names(inDF) <- c("date", "value")
  
  inDF <- na.omit(inDF)
  
  # Time needs to be in seconds since some origin (The 1st water year forecast date in this case)
  inDF$date <- as.numeric(inDF$date) # The following lines seem to work best as numerics
  inDF$date <- inDF$date - inDF[1, "date"] + 1 # Index dates to the 1st forecast at +1 (for midnight/24:00)
  inDF$date <- inDF$date*24*60*60 # Multiply increment from days to seconds
  inDF$date <- as.POSIXct(inDF$date, origin = sprintf("%s-10-31", wateryear()-1)) # Convert to datetimes for the current water year 
  
  tsObject <- xts(x = inDF[,c("value")], order.by = inDF$date)
  
  suppressWarnings({
    tsc <- xts.to.tsc(tsObject = tsObject, ePart = "IR-CENTURY") # Changed to IR-CENTURY
  })
  
  pathParts <- unlist(Map(getPathPart,list(path),c("a","b","c","e","f")))
  tsc$watershed <- pathParts[1]
  tsc$location <- pathParts[2]
  tsc$parameter <- pathParts[3]
  pathParts[4] <- "IR-CENTURY" # Override this to be IR-CENTURY from 1DAY
  tsc$version <- pathParts[5]
  tsc$fullName <- sprintf("/%s/%s/%s//%s/%s/",
                          pathParts[1],pathParts[2],pathParts[3],pathParts[4],pathParts[5])
  tsc$type <- type
  tsc$units <- units
  tsc
}

### Main ----------------------------------------------------------------------

cat(sprintf("\n\nLoading URCs from Excel file:\t%s\n\n", excelFileName))

# Determine which month to load given 'processAllMonths' flag, and convert
#   to a string for matching against: needs to be a single element string of
#   month abbreviations separated by pipe operator.  e.g., "DEC|JAN"
monthsMatchPattern <- 
  str_c("(?i)", # (?i) = case insensitive
        getMonthsToRead(processAllMonths = processAllMonths) %>%
          str_c(collapse="|")
  )

# Load DSS file
if(!dir.exists(dssDir)) dir.create(dssDir)
dssFile <-
  file.path(getwd(), dssDir, dssFileName) %>%
  opendss()

tryCatch(
  {
    # Load Excel file
    excelFileName %>%
      openxlsx::loadWorkbook() %>%
      # Load sheet names (three-letter abbreviation of month)
      names() %>% 
      # Restrict only to months of interest
      str_subset(pattern = monthsMatchPattern) %>%
      # Load raw Excel contents to list by month (list by month)
      Map(f = read.xlsx,
          sheet = .,
          xlsxFile = list(excelFileName), colNames = list(F)) %>%
      # Convert each raw URC to data.frame
      map(rawURCToDataFrame, wy = wy) %>%
      compact() %>%
      # Convert Mica space to elevation NGVD29 (list by month)
      map(convertMicaSpaceToElev_NGVD29, mcdbElevStor_NGVD29) %>%
      # Convert data.frame to TimeSeriesContainer
      Map(urcDfToTscs,urcDf = ., wy = list(wy), m = names(.)) %>%
      compact() %>%
      # Have a list (months) of lists (TSCs), unnest by one layer
      unlist(recursive = F) %>% 
      # Save to DSS
      map(dssFile$put)
    NULL
  },
  error = function(e){
    message(e)
    dssFile$done()
  }
)
