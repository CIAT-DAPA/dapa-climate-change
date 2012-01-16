#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

#load libraries
library(raster)

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/interpolate-functions.R",sep=""))

#defining basic stuff
anuDir <- "C:/anu/Anuspl43/bin"
inDir <- "D:/CIAT_work/crop-modelling/climate-data"
gdir <- paste(inDir,"/gsod-daily",sep="")
hdir <- paste(inDir,"/ghcn-daily",sep="")
ye <- 1960; re <- "afr"

#read in the data
goData <- read.csv(paste(gdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
goData$USAF <- NULL; goData$WBAN <- NULL #remove extra fields
ghData <- read.csv(paste(hdir,"/grouped_output-",re,"/",ye,".csv",sep=""))

#merge both datasets
gaData <- rbind(goData,ghData)
nd <- leap(ye) #check whether leap year so to remove day 366 if needed
if (nd==365) {gData$X366 <- NULL}

#interpolations directory
iDir <- paste(inDir,"/interpolations",sep=""); if (!file.exists(iDir)) {dir.create(iDir)}

for (i in 1:nd) {
  cat("Processing",i,"\n")
  interpolateDay(i,gaData,iDir,ye,re)
}
