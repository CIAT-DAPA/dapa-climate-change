#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("Error: do not run whole thing \n")

library(raster); library(rgdal)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/interpolate-functions.R",sep=""))

bDir <- "E:/PhD-work/crop-modelling/climate-data"
iDir <- paste(bDir,"/daily-interpolations",sep="")

ye <- 1960

yd_afr <- paste(iDir,"/",ye,"-afr",sep="")
yd_sas <- paste(iDir,"/",ye,"-sas",sep="")

dList <- as.numeric(list.files(yd_afr))
dList <- min(dList):max(dList)

#run function for selected areas
eaf <- extractRainfall(yd_afr,"rain_eaf",lon=38.7266,lat=9.0079,dList)
waf <- extractRainfall(yd_afr,"rain_waf",lon=-0.8987,lat=9.8114,dList)
igp <- extractRainfall(yd_sas,"rain_igp",lon=38.7266,lat=9.0079,dList)

plot(waf$DOY,waf$RAIN,type="l")
