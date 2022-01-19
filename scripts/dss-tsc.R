#DSS - Time Series Container functions



### Load ------------------------------------------------------------------------------------------

loadMergedFRADSS <- function(in_paths, dssFileName, compute_type  ){
  #Loads from a merged FRA DSS file, where all data are in one 
  #  DSS file, given the fully qualified path of the file name
  #  e.g., like gatherpaths-extracted
  
  cat(sprintf("\nLoading path(s):\n\t%s",paste0(in_paths,collapse="\n\t")))
  
  #Iterate through paths
  if( toupper(compute_type)=="DETERMINISTIC"){
    pathRegExs <- formQAMetricRegEx(in_paths)
  }else if( toupper(compute_type)=="FRA"){
    pathRegExs <- formFRARegEx(in_paths)
  }else if(toupper(compute_type)=="POR"){
    #Form path regular expression from gatherpaths-like format
    pathRegExs <- formPORPathRegEx(in_paths) #only difference from POR load
  }
  tscList <- lapply(pathRegExs, extractTSCbyRegEx,dssFileName=dssFileName)
  
  #formatting to generic data format used in tool 
  nonNullElement <- sapply(tscList, function(x) !is.null(x))
  if( all(!nonNullElement) ){
    warning(paste0("\nNo data read for provided path expressions in DSS file (%s):\n\t%s",
                   dssFileName, paste0(in_paths,collapse="\n\t")))
    return(NULL)
  }
  in_paths <- in_paths[nonNullElement]
  if( str_detect(tscList[[which(nonNullElement)[1]]]$F[1],"\\d{6}") ){
    tscList <- lapply(tscList,formatMergedDSS)
  }else{
    tscList <- lapply(tscList,formatDSS)
  }
  
  #rename output to match associated name in in_path
  names(tscList) <- in_paths
  
  return(tscList)
}

formatMergedDSS <- function(df){
  #For gatherpaths DSS
  #input is a dataframe as read from the 'extractTSCbyRegEx' function
  #output is a dataframe with path part columns removed (A-F, abc)
  # and a new event column corresponding to the event
  if(is.null(df)) return(NA)
  if(nrow(df)==0 ) return(NA)
  #Convert to Date if 1DAY timesteps
  # if( diff(df$date[1:2]) == 86400) df$date <- as.Date(df$date)
  df$event <- as.numeric(str_extract_all(df$F,"\\d{6}",T))
  df[,c(LETTERS,"abc")] <- NULL
  df <- df[!duplicated(df$date),]
  return(df)
}

formatDSS <- function(df){
  #For QA Metrics and POR datasets
  #input is a dataframe as read from the 'extractTSCbyRegEx' function
  #output is a dataframe with path part columns removed (A-F, abc)
  # and a new event set to NA
  if(is.null(df)) return(NA)
  if(nrow(df)==0 ) return(NA)
  #Convert to Date if 1DAY timesteps
  # if( diff(df$date[1:2]) == 86400) df$date <- as.Date(df$date)
  df$event <- wateryear(df$date) #Map the year to the event
  df[,c(LETTERS,"abc")] <- NULL
  df <- df[!duplicated(df$date),]
  return(df)
}

formPORPathRegEx <- function(in_paths){
  #For reading the DSS files in the POR folders (e.g., POR 29-47)
  #From the gatherpaths-like format of pathnames
  # (e.g., //MCNARY_OUT/FLOW//1DAY/FLOODMODEL1/)
  # converts to a regular expression, that can 
  #  be used to match against POR DSS file paths
  #Example input: "//MCNARY_OUT/FLOW//1DAY/FLOODMODEL1/"
  #Example output: "//MCNARY_OUT/FLOW/*/1DAY/*F1*/"
  fParts <- getPathParts(in_paths,part="F")
  #prefix and suffix F part with wildcard operators for regular expressions
  newFParts <- paste0("*",sapply(fParts,replaceStringFromDict),"*")
  outPaths <- replacePathParts(in_paths,"f",newFParts) #New F parts
  replacePathParts(outPaths,"d","*")                   #D parts to wildcard
}
#These two are the same
formFRARegEx <- formPORPathRegEx


formQAMetricRegEx <- function(paths){
  #format paths with an asterisk in the D part and
  #  a wildcard operator prefix in the F part (e.g., *FLOODMODEL1)
  out <- replacePathParts(paths = paths,part = "D",replacements = "*")
  replacePathParts(paths = out,part = "F",replacements = paste0("*", getPathParts(out,"F")))
}


compileTSC <- function(paths, dssFile,valueColName="value"){
  #Retrieves all of the paths from the dss file object
  #valueColName is the default column name for the column with data
  #Note:
  # > str(dssFile)
  # Formal class 'jobjRef' [package "rJava"] with 2 slots
  # ..@ jobj  :<externalptr> 
  # ..@ jclass: chr "hec/heclib/dss/HecDss"
  library(dssrip)
  library(dplyr)
  pathSplit <- separatePathParts(paths)
  lenPaths <- length(paths)
  cat(sprintf("\nReading %g paths from %s",
              lenPaths,basename(dssFile$getFilename())))
  out <- list()
  for( k in 1:length(paths) ) {
    path <- paths[k]
    if(k %% 1000 == 0) cat(sprintf("\n[%6g/%6g]\t%s", k , lenPaths, path))
    tsc<-try(dssFile$get(path))
    # If not a TimeSeriesContainer, skip and return NULL
    if(!("times" %in% names(tsc)) | class(tsc) == "try-error") {
      warning(sprintf("\nSkipping, bad read or not a TimeSeriesContainer:\t%s", path))
      return(NULL)
    }
    if(length(tsc$times)==0) next #skipping if no data
    tscTimes <- 
      as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC")
    out[[k]] <- data.frame(date = tscTimes,
                           A = pathSplit$A[k],
                           B = pathSplit$B[k],
                           C = pathSplit$C[k],
                           F = pathSplit$F[k],
                           value = tsc$values,
                           stringsAsFactors=F)
  }
  out <- bind_rows(out)
  names(out)[names(out) == "value"] <- valueColName
  out
}



dssDataFromDSSFiles <- function(regExpr, dssFiles){
  #From an individual regular expression, retrieves the
  #  data from each DSS file
  Map(extractTSCbyRegEx, list(regExpr), dssFiles)
}





loadDSSDataFromRegEx <- function(regExprs, dssFiles){
  #wrapper for extractTSCbyRegEx to handle multiple regular expressions
  #  and DSS files.  Note: merges by unique A/B/C parts, so doesn't
  #  handle unique F parts right now, but it could
  #
  #'regExprs' is a string with regular expressions to be used
  #  to extract DSS paths
  #'dssFiles' are fully qualified paths to DSS files to load
  # data from.  Output is a single dataframe with a 'date' column
  #  and one column per regExprs
  require(reshape2)
  #raw read to nested lists of dfs, then merge into list of dfs, then
  rawList <- Map(dssDataFromDSSFiles, list(paste0(regExprs,collapse="|")), list(dssFiles))
  longDF <- bind_rows(lapply(rawList, bind_rows))
  
  #Adding column for the watAlt
  longDF$watAlt <- watAltFromFPart(longDF$F)
  longDF$abcAlt <- sprintf("%s_%s", longDF$abc, longDF$watAlt)
  
  #removing duplicated date/abc combos so dcast works properly
  longDF <- longDF[!duplicated(longDF[, c("date","abcAlt")]),]
  
  #expanding to dataframe
  dcast(data = longDF, formula = "date ~ abcAlt",value.var = "value")
}



#Merges selected paths regular expressions from a DSS file into a
#  long DF with columns for date, DSS path parts, merged A/B/C column,
#  and values
#
#The long DF can be converting to wide dataframe if needed:
# wideDF <- dcast(longDF, date ~ abc, value.var="value")   #if not concerned with F part
# wideDF <- dcast(longDF, date ~ abc+F, value.var="value") #if overlapping dates for some F parts

extractTSCbyRegEx <- function(pathRegEx, dssFileName){
  #dssFileName is fully qualified path to DSS File
  #pathRegEx is a vector of strings with regular expressions
  #  used to extract DSS paths
  
  #Example inputs:
  # dssFileName <- 
  #  "D:\\DSS\\1965W01\\NCS_FullYr_HighPool-1965W01.dss"
  # pathRegEx <- c("/*/*/STAGE/*/*/*/", #pulls all stage data
  #               "/*/*/FLOW/*/*/*/")   #pulls all flow data
  #
  #Example output:
  # > str(longDF)
  # 'data.frame':	3289 obs. of  7 variables:
  # $ date : POSIXct, format: "2007-10-01" "2007-10-02" "2007-10-03" "2007-10-04" ...
  # $ A    : chr  "" "" "" "" ...
  # $ B    : chr  "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" ...
  # $ C    : chr  "ELEV" "ELEV" "ELEV" "ELEV" ...
  # $ F    : chr  "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" ...
  # $ value: num  2064 2064 2064 2064 2064 ...
  # $ abc  : chr  "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" ...
  
  require(dssrip)
  require(purrr)
  require(dplyr)
  require(reshape2)
  
  ### Functions ##############
  
  glob2rx_plus <- function (pattern, trim.head = FALSE, trim.tail = TRUE) 
  {
    #This is a special form of the glob2rx function in the utils package
    #  that treats "+" signs as characters.  There are some instances of "+" signs 
    #  in DSS paths, unfortunately
    if (!length(pattern)) 
      return(character())
    
    p <- gsub("\\.", "\\\\.", paste0("^", pattern, "$"))
    p <- gsub("\\?", ".", gsub("\\*", ".*", p))
    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("\\+", "\\\\+", p)            #added by RSW
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    if (trim.tail) 
      p <- sub("\\.\\*\\$$", "", p)
    if (trim.head) 
      p <- sub("\\^\\.\\*", "", p)
    p
  }
  
  formPaths <- function(a="",b="",c="",d="",e="",f=""){
    #Forms DSS paths from specified character in the form: /A/B/C/D/E/F/
    
    a <- as.character(a)
    b <- as.character(b)
    c <- as.character(c)
    d <- as.character(d)
    e <- as.character(e)
    f <- as.character(f)
    
    #At a minimum, A or B should be specified
    if( all(c(a,b) == "") ) stop("In functiion call: formPaths, need to at least specific an A or B part")
    if( !all(is.character(c(a,b,c,d,e,f))) ) stop("In functiion call: formPaths, all arguments need to be characters or convertible to characters")
    
    return(paste0("/",a,"/",b,"/",c,"/",d,"/",e,"/",f,"/"))
  }
  
  
  compileTSC <- function(paths, dssFile,valueColName="value"){
    #Retrieves all of the paths from the dss file object
    #valueColName is the default column name for the column with data
    #Note:
    # > str(dssFile)
    # Formal class 'jobjRef' [package "rJava"] with 2 slots
    # ..@ jobj  :<externalptr> 
    # ..@ jclass: chr "hec/heclib/dss/HecDss"
    
    pathSplit <- separatePathParts(paths)
    
    lenPaths <- length(paths)
    cat(sprintf("\nReading %g paths from %s",lenPaths,basename(dssFile$getFilename())))
    out <- list()
    for( k in 1:length(paths) ) {
      
      if(k %% 1000 == 0) cat(sprintf("\n[%6g/%6g]", k , lenPaths))
      
      path <- paths[k]
      
      
      tsc<-try(dssFile$get(path))
      
      #skipping if no data or not time series
      if(.jclass(tsc) != "hec.io.TimeSeriesContainer") next
      if(length(tsc$times)==0 ) next 
      
      tscTimes <- as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC")
      
      out[[k]] <- data.frame(date = tscTimes,
                             A = pathSplit$A[k],
                             B = pathSplit$B[k],
                             C = pathSplit$C[k],
                             F = pathSplit$F[k],
                             value = tsc$values,
                             stringsAsFactors=F)
      
      #Correction for daily data
      if(unique(pathSplit$E[k])=="1DAY")
        out[[k]]$date = out[[k]]$date-86400
    }
    
    out <- bind_rows(out)
    
    names(out)[names(out) == "value"] <- valueColName
    
    return(out)
  }
  
  addABCColumn <- function(inDF){
    #Input is a dataframe similar to the one created by
    #  the 'separatePathParts' function in dssrip  package,
    #  with an 'A', 'B', and 'C' column
    #Returns a sdataframe of the merged A, B, and C 
    # parts of the path, separated by '__' and 
    # missing any special characters
    
    inDF$abc <- NA
    noAPart <- inDF$A == ""
    inDF$abc[noAPart] = do.call(paste, c(inDF[noAPart,c("B", "C")], sep = "__"))
    inDF$abc[!noAPart] = do.call(paste, c(inDF[!noAPart,c("A", "B","C")], sep = "__"))
    
    inDF$abc <- gsub("\\*","",inDF$abc)
    
    return(inDF)
  }
  
  ### Main ################
  
  #opening dss and getting all paths
  dssFile <- opendss(dssFileName)
  allPaths <- separatePathParts(getAllPaths(dssFile))
  
  #Retrieving matching paths to read, returns matrix of strings
  selectedPaths <- sapply(pathRegEx,function(x) fullPathByRegex(paths = allPaths$PATH, pattern = glob2rx_plus(x)))
  selectedPaths <- as.vector(unlist(selectedPaths)) #Making vector instead of matrix of strings
  
  if(length(selectedPaths)==0) {
    warning(sprintf("\n\nNo data found in file '%s' for regular expression.", dssFileName))
    dssFile$close()
    return(NULL) #If no data to extract
  }else{
    
    #retrieving data
    longDF <- compileTSC(paths = selectedPaths, dssFile = dssFile)
    dssFile$done() #done with dss file
    
    #Making new column of merged A/B/C compenents, accounting for missing A parts
    longDF <- addABCColumn(longDF)
    
    return(longDF)
  }
  
  #converting to wide dataframe
  # wideDF <- dcast(longDF, date ~ abc, value.var="value")
}



### Save ----------

dfToTSC <- function(path, inDF){
  # Converts the standard date, event, value
  #   dataframe into a tsc object to save using dssFile$put
  cat("\n",path)
  tsObject <- xts(x = inDF[,c("value")], order.by = inDF$date)
  tsc <- xts.to.tsc(tsObject = tsObject)
  pathParts <- unlist(Map(getPathPart,list(path),c("a","b","c","e","f")))
  tsc$watershed <- pathParts[1]
  tsc$location <- pathParts[2]
  tsc$parameter <- pathParts[3]
  tsc$version <- pathParts[5]
  tsc$fullName <- sprintf("/%s/%s/%s//%s/%s/",
                          pathParts[1],pathParts[2],pathParts[3],pathParts[4],pathParts[5])
  tsc
}

saveTSC <- function(tsc, dssFile){
  #dssFile and tsc are the java objects:
  # > str(dssFile)
  # Formal class 'jobjRef' [package "rJava"] with 2 slots
  # ..@ jobj  :<externalptr> 
  #   ..@ jclass: chr "hec/heclib/dss/HecDss"
  # > str(tsc)
  # Formal class 'jobjRef' [package "rJava"] with 2 slots
  # ..@ jobj  :<externalptr> 
  #   ..@ jclass: chr "hec/io/TimeSeriesContainer"
  # tsc can be created from the standard date, event, value dataframe using
  #  the 'dfToTSC' function
  dssFile$put(tsc)
}

save_DSS <- function(file_path, dfList){
  #dfList = allData$data[[alt]] = list of date, event, value datamfraes
  #file_path = fully quialified DSS file's path
  
  dssFile <- opendss(file_path)
  
  connection_name <- conns$connection_name[conns$file_path==file_path]
  #check which paths aren't in the current file connection
  pathsToSave <- names(dfList)[names(dfList) %!in% pathConfig$allPaths[[connection_name]]$paths]
  tscList <- Map(dfToTSC,pathsToSave, dfList[pathsToSave] ) #convert to TSC
  Map(saveTSC, tscList, list(dssFile)) #save
  
  dssFile$done()
}
