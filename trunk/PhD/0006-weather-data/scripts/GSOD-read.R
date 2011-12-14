#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011
stop("error")

source("D:/CIAT_work/crop-modelling/GHCND-GSOD-functions.R")

bDir <- "D:/CIAT_work/crop-modelling/climate-data"; setwd(bDir)
gsodDir <- paste(bDir,"/gsod-daily",sep="")

#gsod stations
stations.gsod <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
stations.gsod$LON <- stations.gsod$LON/1000; stations.gsod$LAT <- stations.gsod$LAT/1000
stations.gsod$ELEV..1M. <- stations.gsod$ELEV..1M./10

#1. create extents (only for interpolation, but use only two extents for fitting: AFRICA & IGP)
#2. define working gridcell size (probably 1 degree)
#3. make inventory of data (count number of data points per day per fitting region)
#4. interpolate to target cellsize all daily data between 1960-2011


#1. create extents
require(raster); require(maptools); require(rgdal)

#projection extents
waf.xt <- extent(-20,20,1,30)
eaf.xt <- extent(25,50,-15,20)
igp.xt <- extent(65,100,5,40)

#fitting extents
afr.xt <- extent(-20,55,-40,30)
sas.xt <- igp.xt

#plot the extents (for reference -commented!)
#rs <- raster(); rs[] <- rnorm(1:ncell(rs))
#data(wrld_simpl)
#plot(rs,col=colorRampPalette(c("grey10","grey90"))(100)); plot(wrld_simpl,add=T,col='white')
#plot(waf.xt,add=T,col='red'); plot(eaf.xt,add=T,col='blue'); plot(igp.xt,add=T,col='orange')
#plot(afr.xt,add=T,col='black',lty=1); plot(sas.xt,add=T,col='black',lty=2)

#2. define working gridcell
cellSize <- 1

#3. Make inventory of data (points / day / fit region)
#define initial and final year
yearSeries <- c(1960:2010)

#select stations within 3+degree of interpolation extents
gsod.afr <- stations.gsod[which(stations.gsod$LON>=(afr.xt@xmin-3) & stations.gsod$LON<=(afr.xt@xmax+3)
                          & stations.gsod$LAT>=(afr.xt@ymin-3) & stations.gsod$LAT<=(afr.xt@ymax+3)),]
gsod.sas <- stations.gsod[which(stations.gsod$LON>=(sas.xt@xmin-3) & stations.gsod$LON<=(sas.xt@xmax+3)
                          & stations.gsod$LAT>=(sas.xt@ymin-3) & stations.gsod$LAT<=(sas.xt@ymax+3)),]

#clean gsod stations of Africa
gsod.afr <- gsod.afr[-which(gsod.afr$LON == 0 | gsod.afr$LAT == 0),]

#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=5) #initiate cluster

#export functions
sfExport("convertGSOD")
sfExport("createDateGrid")
sfExport("leap")

#export variables
sfExport("bDir")

IDs <- paste("USAF",gsod.afr$USAF,"_WBAN",gsod.afr$WBAN,sep="")

count <- 1
for (yr in yearSeries[1:5]) {
  cat(yr,paste("(",count," out of ",length(yearSeries),")",sep=""),"\n")
  gdir <- paste(gsodDir,"/gsod_",yr,sep="")
  ogdir <- paste(gsodDir,"/gsod_",yr,"_out",sep=""); if (!file.exists(ogdir)) {dir.create(ogdir)}
  controlConvert <- function(i) { #define a new function
    convertGSOD(i,yr,gdir,ogdir)
  }
  sfExport("yr"); sfExport("gdir"); sfExport("ogdir")
  system.time(sfSapply(as.vector(IDs), controlConvert))
  count <- count+1
}


