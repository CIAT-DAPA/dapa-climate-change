# Download geographic data and return as R object
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1
# October 2008

#http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1
#&lat=3&lon=75&ms=1&ds=1&ys=1997&me=9&de=1&ye=2009&submit=Yes&p=RAIN

require(XML)
require(raster)

#Basic data
bDir <- "F:/PhD-work/crop-modelling/climate-data"
powerDir <- paste(bDir,"/POWER-daily",sep="")
reg <- "eaf"

#loading mask to get XYs
msk <- raster(paste(powerDir,"/0_files/rain_",reg,"_dummy.asc",sep=""))
xy <- as.data.frame(xyFromCell(msk,1:ncell(msk)))
xy$CELL <- 1:ncell(msk)
if (!file.exists(paste(powerDir,"/0_files",sep=""))) {dir.create(paste(powerDir,"/0_files",sep=""))}

#Regional output folder
od <- paste(powerDir,"/data-",reg,sep="")
if (!file.exists(od)) {dir.create(od)}

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=8) #initiate cluster

#export functions and data
sfExport("getPOWER")
sfExport("xy")
sfExport("od")

controlDownload <- function(i) {
  require(XML); require(raster)
  cat("Downloading",i,"of",nrow(xy),"\n")
  #cell output folder
  cd <- paste(od,"/cell-",i,sep="")
  if (!file.exists(cd)) {dir.create(cd)}
  if (!file.exists(paste(cd,"/data.csv",sep=""))) {
    getPOWER(lat=xy$x[i],lon=xy$y[i],cd)
  } else {
    cat("Cell data already downloaded \n")
  }
}

system.time(sfSapply(as.vector(1960:2009), controlCompare))


###################################################
#Function to get the NASA POWER data
getPOWER <- function(lat,lon,outDir) {
  setwd(outDir)
  baseURL <- "http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat="
  theurl <- paste(baseURL,lat,"&lon=",lon,"&ms=1&ds=1&ys=1997&me=9&de=1&ye=2009&submit=Yes&p=RAIN",sep="")
  doc <- htmlTreeParse(theurl,useInternalNodes=T)
  x <- xpathApply(doc, "//body", xmlValue)
  x <- x[[1]]; x <- substring(x,1)
  fx <- file("temp.wth","w")
  writeLines(x,fx)
  close(fx)
  y <- read.fortran("test.wth",skip=26,format=c("I6","I5","8F7"))
  names(y) <- c("WEYR","WEDAY","SRAD","TMAX","TMIN","RAIN","WIND","DEW","T2M","RH2M")
  write.csv(y[1:(nrow(y)-1),],"data.csv",quote=F,row.names=F)
}

