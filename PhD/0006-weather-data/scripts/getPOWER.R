#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

#http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1
#&lat=3&lon=75&ms=1&ds=1&ys=1997&me=9&de=1&ye=2009&submit=Yes&p=RAIN

require(XML)
require(raster)

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/getPOWER-functions.R",sep=""))

#Basic data
bDir <- "D:/CIAT_work/crop-modelling/climate-data"
powerDir <- paste(bDir,"/POWER-daily",sep="")
reg <- "igp"

#loading mask to get XYs
msk <- raster(paste(powerDir,"/0_files/rain_",reg,"_dummy.asc",sep=""))
xy <- as.data.frame(xyFromCell(msk,1:ncell(msk)))
xy$CELL <- 1:ncell(msk)
if (!file.exists(paste(powerDir,"/0_files",sep=""))) {dir.create(paste(powerDir,"/0_files",sep=""))}

#Regional output folder
od <- paste(powerDir,"/data-",reg,sep="")
if (!file.exists(od)) {dir.create(od)}

#loading mask to get XYs
msk <- raster(paste(powerDir,"/0_files/rain_",reg,"_dummy.asc",sep=""))
xy <- as.data.frame(xyFromCell(msk,1:ncell(msk)))
xy$CELL <- 1:ncell(msk)

#Regional output folder
od <- paste(powerDir,"/data-",reg,sep="")
if (!file.exists(od)) {dir.create(od)}

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=4) #initiate cluster

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
  if (!file.exists(paste(cd,"/data_1983-1996.csv",sep=""))) {
    getPOWER(lat=xy$y[i],lon=xy$x[i],cd,1983,1996)
  } else {
    cat("Cell data already downloaded \n")
  }
}

#run the control function

system.time(sfSapply(as.vector(1:nrow(xy)), controlDownload))

#stop the cluster
sfStop()

