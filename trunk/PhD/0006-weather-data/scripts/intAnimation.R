#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("Error: do not run whole thing \n")

library(raster); library(maptools); data(wrld_simpl); library(rgdal)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/interpolate-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/climate-data"
iDir <- paste(bDir,"/daily-interpolations",sep="")

re <- "afr"

for (ye in 1960:2010) {
  yd <- paste(iDir,"/",ye,"-",re,sep="")
  
  dList <- as.numeric(list.files(yd))
  dList <- min(dList):max(dList)
  
  imgDir <- paste(iDir,"/",ye,"-",re,"-img",sep="")
  if (!file.exists(imgDir)) {dir.create(imgDir)}
  
  setwd(imgDir)
  
  # creating the PNGs
  if (re=="afr") {
    #plot east africa rainfall
    if (!file.exists("rain_eaf.gif")) {eaf <- createGIF(yd,"rain_eaf",550,700,dList)}
    if (!file.exists("rain_waf.gif")) {waf <- createGIF(yd,"rain_waf",700,500,dList)}
  } else {
    if (!file.exists("rain_igp.gif")) {igp <- createGIF("rain_igp")}
  }
}
