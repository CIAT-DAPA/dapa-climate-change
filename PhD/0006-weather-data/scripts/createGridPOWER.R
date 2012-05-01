#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

require(raster); require(rgdal)

#sourcing functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/getPOWER-functions.R",sep=""))

#Basic data
bDir <- "E:/PhD-work/crop-modelling/climate-data"
powerDir <- paste(bDir,"/POWER-daily",sep="")
yearIni <- 1997; yearFin <- 2009; wVar <- "SRAD"

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=3) #initiate cluster

for (reg in c("waf")) {#c("eaf","sas","waf")) {
  #reg <- "eaf"
  
  #loading mask to get XYs
  msk <- raster(paste(powerDir,"/0_files/",reg,"_dummy.asc",sep=""))
  xy <- as.data.frame(xyFromCell(msk,1:ncell(msk)))
  xy$CELL <- 1:ncell(msk)
  
  dataDir <- paste(powerDir,"/data-",reg,sep="")
  rsDir <- paste(powerDir,"/raster-",reg,sep="")
  if (!file.exists(rsDir)) {dir.create(rsDir)}
  
  orsDir <- paste(rsDir,"/",tolower(wVar),sep="")
  if (!file.exists(orsDir)) {dir.create(orsDir)}
  
  #read dummy data for the loop
  dummy <- read.csv(paste(dataDir,"/cell-1/data_",yearIni,"-",yearFin,".csv",sep=""))
  
  #export functions
  sfExport("getDataDay")
  
  #export variables
  sfExport("dummy")
  sfExport("yearIni")
  sfExport("yearFin")
  sfExport("wVar")
  sfExport("dataDir")
  sfExport("rsDir")
  sfExport("orsDir")
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
    yoDir <- paste(orsDir,"/",year,sep="")
    if (!file.exists(yoDir)) {dir.create(yoDir)}
    
    if (!file.exists(paste(yoDir,"/",tolower(wVar),"-",doy,".asc",sep=""))) {
      x <- sapply(1:ncell(msk),getDataDay,toSkip,dataDir,yearIni,yearFin,toupper(wVar))
      x[which(x == -99)] <- NA
      rs <- raster(msk); rs[] <- x
      #rs[which(rs[] == -99)] <- NA
      #plot(rs,useRaster=F)
      rs <- writeRaster(rs,paste(yoDir,"/",tolower(wVar),"-",doy,".asc",sep=""),format="ascii",overwrite=T)
      rm(rs); g=gc()
      cat(" done\n")
    } else {
      cat(" already exists\n")
    }
  }
  
  #run the control function
  system.time(sfSapply(as.vector(1:nrow(dummy)), controlCreateRaster))
}

#stop the cluster
sfStop()

#read some data
# stkp <- stack(paste(orsDir,"/1985/srad-",1:365,".asc",sep=""))
# vp <- extract(stkp,cbind(X=40,Y=5))

