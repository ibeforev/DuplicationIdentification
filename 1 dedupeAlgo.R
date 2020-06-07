rm(list = setdiff(ls(), "zzCon"))
gc()

# Set up environment #
require("data.table")
require("dplyr")
require("lubridate")
require("openxlsx")
require("reshape2")
require("odbc")
require("DBI")
require("stringdist")

options(scipen = 999)
Sys.setenv(TZ = "GMT")
Sys.setenv("R_ZIPCMD" = "D:/Rtools/bin/zip.exe")

# Connect to FRED #
zzCon <- dbConnect(odbc(), 
                   dsn = "", 
                   uid = "", 
                   pwd = "")

# Parameters #
matchKeep <- .90
firstDistance <- 0
lastDistance <- 0
addressDistance <- 2

# Scoring values # 
dobMatch <- 2.1/7
nameMatch <- 1.5/7
genderMatch <- 1.2/7
countryMatch <- 1/7
addressMatch <- .6/7
fullExactMatch <- .3/7
addressExactMatch <- .3/7

# Designate if testing and sample size #
isTest <- "Yes"
sampSize <- 100000

# Run algo #
source("2 source.R")
