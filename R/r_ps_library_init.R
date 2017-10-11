#load libraries
#library(httr)
#library(jsonlite)
#library(lubridate)
library(tidyverse)
library(stringr)
library(readxl) #read excel file
library(dplyr)
library(ggplot2)
library(openxlsx) #'write xlsx'
#library(data.table)
#library(rgdal)

#load file r_kobo_utils.R file first
options(java.parameters = "-Xmx6000m")
options(stringsAsFactors = FALSE)
#language setting
Sys.setlocale(category = "LC_ALL",locale = "arabic")
Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
Sys.getenv("R_ZIPCMD","zip")


path <- Sys.getenv("PATH")
Sys.setenv("PATH" = paste(path, "C:/Rtools/bin", sep = ";"))

tempfile(tmpdir="./Data/temp")



