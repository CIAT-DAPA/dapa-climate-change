#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

library(raster); library(maptools); data(wrld_simpl); library(rgdal)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/interpolate-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/climate-data"
iDir <- paste(bDir,"/daily-interpolations",sep="")

re <- "sas"
dFile_igp <- paste(iDir,"/0_files/rain_igp_dummy.asc",sep="")
dFile_eaf <- paste(iDir,"/0_files/rain_eaf_dummy.asc",sep="")
dFile_waf <- paste(iDir,"/0_files/rain_waf_dummy.asc",sep="")

for (ye in 1960:2010) {
  cat("Processing year",ye,"\n")
  yd <- paste(iDir,"/",ye,"-",re,sep="")
  
  dList <- as.numeric(list.files(yd))
  dList <- min(dList):max(dList)
  
  imgDir <- paste(iDir,"/",ye,"-",re,"-img",sep="")
  if (!file.exists(imgDir)) {dir.create(imgDir)}
  setwd(imgDir)
  
  # creating the PNGs
  if (re=="afr") {
    #plot east africa rainfall
    if (!file.exists("rain_eaf.gif")) {eaf <- createGIF(yd,"rain_eaf",550,700,dList,dFile_eaf)}
    if (!file.exists("rain_waf.gif")) {waf <- createGIF(yd,"rain_waf",700,500,dList,dFile_waf)}
  } else {
    if (!file.exists("rain_igp.gif")) {igp <- createGIF(yd,"rain_igp",700,600,dList,dFile_igp)}
  }
}
