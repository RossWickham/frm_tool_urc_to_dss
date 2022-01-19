

### General ###########

"%!in%" <- function(x,y) !(x %in% y) #not in operator

#Extracts from left side of character(similar to Excel function)
left <- function(text, nchars)    substring(text, 1, nchars)

#Extracts from right side of character (similar to Excel function)
right <- function(text, nchars)    substring(text, nchar(text)-nchars+1, nchar(text))

#input is a list of dataframes, output is the same list, with each instance of the
#  dataframes'  columns renamed from 'oldColName' to 'newColName'
#  accepts 'newColName' as a vector
renameColsInDFList <- function(inList,oldColName,newColName){
    Map(renameDFCol, inList, oldColName=oldColName, newColName=newColName)
}
  
#renames a column name in an individual dataframe
renameDFCol <- function(inDF, oldColName,newColName){
  names(inDF)[names(inDF)==oldColName] <- newColName
  return(inDF)
}

getCommonNamesInList <- function(xList, exclude=NULL) {
  #Common names of elements in next list level, excuding those expilcitly defined
  out <- Reduce(intersect,lapply(xList,names))
  out[out %!in% exclude]
}


noPartialMatches <- function(patterns, strings) !grepl(paste0(patterns,collapse = "|"),strings) 

getCommonNamesInList_recursive <- function(xList, exclude=NULL,nLevel=1) {
  #Common names of elements in next list level, excuding those explicitly defined
  xList <- xList[ noPartialMatches(exclude,names(xList)) ]
  nLevel=nLevel-1
  # str(xList)
  if(nLevel==0){
    out <- Reduce(intersect,lapply(xList,names))
    return(out[ noPartialMatches(exclude,out) ])
  }else{
    # return( lapply(unlist(xList,recursive = F),getCommonNamesInList_recursive,exclude=exclude,nLevel=nLevel) )
    return( getCommonNamesInList_recursive(xList=unlist(xList,recursive = F),exclude=exclude,nLevel=nLevel) )
  }
}

getTextBtwnStrings <- function(strings, startStr, endStr){
  #'
  #' Pulls character from a string vector by matching values between
  #'   two string
  #'
  library(purrr)
  library(stringr)
  pattern = sprintf("%s\\s*(.*?)\\s*%s", startStr, endStr)
  rawMatch <- regmatches(strings, regexec(pattern, strings))
  na.omit(rawMatch %>% map_chr(function(x)x[2]))
}

### Colors ##########

colToHex <- function(colName, alpha=0.5) mapToRGBA(col2rgb(col = colName),alpha=alpha)

mapToRGBA <- function(rgbaVector,alpha){
  #input is a vector defining the red, green, blue,
  #  and alpha values (in that order) to be passed
  #  to the rgb function
  rgb(rgbaVector[1]/255, rgbaVector[2]/255, rgbaVector[3]/255, alpha)
}


### Printing ###########

#concatenates strings to a single string using specified separator
charToPrint <- function(strings, sep="\n\t") paste0(strings, collapse=sep)

#converts dataframe to something that is printable
printableDF <- function(df) paste0(apply(df,1,charToPrint,sep="\t"),collapse="\n\t")


### File/Directory Mgmt ###########


recursiveDirName <- function(filePath,nDeep){
  #Retrieve the fully qualified parent directory up 'nDeep' levels
  nDeep = nDeep-1
  dirName <- dirname(filePath)
  ifelse(nDeep==0,
         return(dirName),
         return(recursiveDirName(dirName,nDeep)))
}

#removes file extensions from files
removeFileExtension <- function(fileName) sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(fileName))

### DSS Helpers #######################

getModelName <- function(fParts, extractConfig){
  #'
  #' Retrieves the model name given the F parts without the Catalog ID
  #'
  names(extractConfig$fPartDictionary)[
    match(x = fParts, table=toupper(unlist(extractConfig$fPartDictionary)))
  ]
}

convertPathsToExtractName <- function(paths, extractConfig){
  #'
  #' Given a vector of DSS path strings, converts to the
  #'   proper name for an FRA extract.  This format is 
  #'   used in multiple different tools as a generic ID 
  #'   to compare paths between alternatives.
  #'
  # New F parts, referencing the extract config
  newFParts <- 
    paths %>%
    getPathParts("F") %>%
    stripCatalogIDFromFPart() %>%
    getModelName(extractConfig)
  # Remove D parts, replace with new F parts
  paths %>%
    replacePathParts("D","") %>%
    replacePathParts("F",newFParts)
}

catalogIDFromFPart <- function(fParts){
  #'
  #' Pulls the numeric catalog IDs from F parts
  #'
  library(stringr)
  as.numeric(str_extract(fParts,"\\d{6}"))
}

extractAllPaths <- function(strings){
  #'
  #' Extracts all instances of DSS paths from 
  #'   the input vector
  #'
  library(stringr)
  pattern <- "/.*/.*/.*/.*/.*/.*/"
  str_split(string = strings, pattern = ",") %>% 
    unlist() %>%
    as.character() %>% 
    str_extract(pattern = pattern) %>% 
    na.omit() %>%
    unique()
}

stripCatalogIDFromFPart <- function(fParts) sub(".*\\|","",fParts)

guessModelFromFPart <- function(fParts){
  #'
  #' Guesses what the model is (e.g., "FLOODMODEL1", "FLOODMODEL0", 
  #' "POWERMODEL1") from the F part 
  #' e.g., input = CANLOCUSPA:FRA W FIA:RESSIM-F1CANL_F
  #'       output = FLOODMODEL1
  #'
  library(stringr)
  modelDict <- data.frame( #converting to R dataframe
    fPartDictionary = c('FLOODMODEL0', 'FLOODMODEL1', 'POWERMODEL1',
                        'URC BASE', 'URC ADJ', 'ECC BASE',
                        'URC GCL DWR SHIFT', 'HYDROLOGIC SAMPLER', 'HYDRO SAMPLE',
                        'YAKIMAMODEL', 'UPPERSNAKEMODEL', 'FIA_MODEL',
                        "FIA_SPRING", "FIA_WINTER",'KNNMODEL',
                        "BASIC_FLOOD", "CANADIAN_OPS", "DAILY_OPS"),
    watFPart = c("F0","F1","P1",
                 "URC-BASE", NA, "ECC BASE",
                 "URC-GCL DWR SHIFT", NA, NA, #URC GCL, Hydro Samplers
                 NA, NA, NA, #YAKIMA, Upper Snake
                 NA, NA ,NA, #FIA, KNN
                 "Fx0", "C1", "D1"),
    stringsAsFactors=F
  )
  modelNames <- str_extract(".*:$",fParts)
  pmatches <- Map(pmatch, modelDict$watFPart, list(fParts))
  

  
}

watAltFromFPart <- function(fParts){
  #output is the WAT alternative name from the F Part of the DSS path
  #e.g., fParts = 'JM_COE_NP:POR 65-76:RESSIM-F0COEFRM'
  #     output = 'JM_COE_NP'
  substring( fParts, 1,regexpr(":|~",fParts)-1)
}

extractPathParts <- function(inPaths, pathsToExtract=paste0(LETTERS[1:6], collapse="") ){
  if(is.null(inPaths)) return(NULL)
  if(length(pathsToExtract) != 1)
    stop("Function call: extractPathParts, pathsToExtract is a character string of length one (e.g., 'ABD' or 'abd') specifying DSS path parts")
  if( !is.character(c(pathsToExtract, inPaths)) )
    stop("Function call: extractPathParts, inPaths (vector) and pathsToExtract (atomic vector) arguments must be characters")
  
  #The corresponding numbers of the path parts as specified from a strsplit call
  whichParts <- unique(c(which(LETTERS[1:6] %in% strsplit(pathsToExtract,"")[[1]]) + 1, #Checking upper and lower case
                         which(letters[1:6] %in% strsplit(pathsToExtract,"")[[1]]) + 1 ))
  
  #Input is vector of separated path parts and numbers identifying which path parts to return from strsplit call
  replacePaths <- function(splitPaths, whichParts){
    if(length(splitPaths) != 7) stop("Function call: extractPathParts, inPaths argument must be a vector of full DSS paths: '/A/B/C/D/E/F/'")
    outPath <- rep("",6) #intializing a blank template to add data to
    outPath[whichParts-1] <- splitPaths[whichParts]
    return( toupper(paste0("/",paste0(c(outPath), collapse="/"),"/")) )
  }
  return( sapply( strsplit(inPaths,"/"), replacePaths, whichParts=whichParts, simplify = T) )
}


getPathPart <- function(path,part){ #gets A-F part from DSS path
  partIndex <- which(toupper(part) == LETTERS)
  strsplit(path,"/")[[1]][partIndex+1]
}

#vectorized version of function
getPathParts <- function(paths,part) sapply(paths, getPathPart,part=part,simplify = T)

formPathStringVector <- function(strings){
  #Given character vector of length 6, representing DSS path parts
  #  A-F, returns an atomic character with fully formed DSS path,
  #  separated by "/"
  paste0("/",paste0(strings,collapse="/"),"/")
}

replacePathParts <- function(paths,part,replacements){
  partIndex <- which(toupper(part) == LETTERS) #finding which part to change
  splitPaths <- strsplit(paths,"/") #splitting paths to a nested list
  #Mapping new F parts to old, returning as character of formed DSS paths
  as.character(Map(f = function(x,y) {x[partIndex+1] <- y; return(formPathStringVector(x[2:7]))},
                     splitPaths,replacements))
}

glob2rx_plus <- function (pattern, trim.head = FALSE, trim.tail = TRUE) {
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


### Metadata ############

#Adding on base year information
mapBaseYr <- function(events, eventYr){
  #maps the base year to the provided events given the
  #  event-year pairings from the hydrologic sampler
  if(max(events) == 5000){
    return(eventYr$fiveK$base_year[match(events, eventYr$fiveK$event)])
  }else{
    return(eventYr$fiftyK$base_year[match(events, eventYr$fiftyK$event)])
  }
}
