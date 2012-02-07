#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

require(raster); require(rgdal)

#sourcing functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/getPOWER-functions.R",sep=""))

#Basic data
bDir <- "F:/PhD-work/crop-modelling/climate-data"
powerDir <- paste(bDir,"/POWER-daily",sep="")
reg <- "eaf"

#loading mask to get XYs
msk <- raster(paste(powerDir,"/0_files/rain_",reg,"_dummy.asc",sep=""))
xy <- as.data.frame(xyFromCell(msk,1:ncell(msk)))
xy$CELL <- 1:ncell(msk)

dataDir <- paste(powerDir,"/data-",reg,sep="")
rsDir <- paste(powerDir,"/raster-",reg,sep="")
if (!file.exists(rsDir)) {dir.create(rsDir)}

#read dummy data for the loop
dummy <- read.csv(paste(dataDir,"/cell-1/data.csv",sep=""))

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=6) #initiate cluster

#export functions
sfExport("getDataDay")

#export variables
sfExport("dummy")
sfExport("dataDir")
sfExport("rsDir")
sfExport("msk")
sfExport("reg")

#function to control the raster creation
controlCreateRaster <- function(j) {
  library(raster)
  toSkip <- j-1
  year <- dummy$WEYR[j]
  doy <- dummy$WEDAY[j]
  
  cat("Processing day",doy)
  
  #create output folder for year
  yoDir <- paste(rsDir,"/",year,sep="")
  if (!file.exists(yoDir)) {dir.create(yoDir)}
  
  if (!file.exists(paste(yoDir,"/rain-",j,".asc",sep=""))) {
    x <- sapply(1:ncell(msk),getDataDay,toSkip,dataDir)
    rs <- raster(msk); rs[] <- x
    #plot(rs,useRaster=F)
    rs <- writeRaster(rs,paste(yoDir,"/rain-",j,".asc",sep=""),format="ascii",overwrite=T)
    rm(rs); g=gc()
    cat(" done\n")
  } else {
    cat(" already exists\n")
  }
}

#run the control function
system.time(sfSapply(as.vector(1:nrow(dummy)), controlCreateRaster))

#stop the cluster
sfStop()


