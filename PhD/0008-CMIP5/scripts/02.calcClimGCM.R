#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#1. Calculate monthly climatological means for pr, tas, and dtr

#Get CMIP5 weather data
library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

yi <- 1961
yf <- 2005

#get the list of unprocessed GCMs
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
gcmList <- unique(gcmChars$GCM)

mList <- 1:length(gcmList)





