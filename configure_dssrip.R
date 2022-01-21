#'
#' This script establishes Java settings required to use the dssrip package
#'

library(rjson)
library(tidyverse)

# Unset or remove some options that might remain 
Sys.unsetenv("JAVA_HOME")
if(exists("java32Dir")) rm("java32Dir")
if(exists("java64Dir")) rm("java64Dir")

# Establish path to Java in HEC-DSSVue version
baseJavaDir <- 
  file.path(getwd(),"resources","HEC-DSSVue-v3.0.00.212") %>%
  str_replace_all("/","\\\\")

# Load config file, esnuring the dss_location field is set properly
configFileName <- "config\\jar_config.json"
javaConfig <- rjson::fromJSON(file = configFileName)
javaConfig$configs[[1]]$dss_location <- baseJavaDir
# Convert to string and save
toJSON(javaConfig) %>%
  write(file = configFileName)

# Increasing Java memory allocation
options(java.parameters = "-Xms64m")

# Set DSS options
options(dss_location=baseJavaDir)
options(dss_jre_location=file.path(baseJavaDir,"java"))

# Load package
library(dssrip)
