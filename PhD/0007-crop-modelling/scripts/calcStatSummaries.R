#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#sourcing important functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
#src.dir <- "/home/jramirez/dapa-climate-change/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep=""))

src.dir2<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "/home/jramirez/dapa-climate-change/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/detrender-functions.R",sep=""))

#set the working folder
bDir <- "H:/" #"F:/PhD-work/crop-modelling/GLAM/climate-signals-yield"
#bDir <- "/andromeda_data1/jramirez/crop-modelling/GLAM/climate-signals-yield"
cropName <- "gnut"
cd <- paste(bDir,"/",toupper(cropName),sep="")

#load shapefile
shp <- paste(cd,"/shp/IND2-",tolower(cropName),".shp",sep="")
shp <- readShapePoly(shp)

#load detrended data
rawData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-raw.csv",sep=""))
loeData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-loess.csv",sep=""))
linData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-linear.csv",sep=""))
quaData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-quadratic.csv",sep=""))
fouData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-fourier.csv",sep=""))

#Calculate or load the summary data
if (!file.exists(paste(cd,"/data/detrended-IND2-gnut-summary.csv",sep=""))) {
  sData <- calcSummary(yieldData,"DISID",yfds,hfds,pfds,1900+(iyr:fyr),cd)
} else {
  sData <- read.csv(paste(cd,"/data/detrended-IND2-gnut-summary.csv",sep=""))
}

##################################################################################
##################################################################################
#2. For each of the years create a 1x1 degree grid with the detrended series by
#   2.1. Create a 1x1 min resolution raster with the districts

#convert shape to raster (if not exists) for assigning values and then plot
resol <- 1/60

#clean mask raster
if (!file.exists(paste(cd,"/raster/india-1min-msk.asc",sep=""))) {
  rs <- createMask(shp,resol)
  rs <- writeRaster(rs,paste(cd,"/raster/india-1min-msk.asc",sep=""),format='ascii')
} else {
  rs <- raster(paste(cd,"/raster/india-1min-msk.asc",sep=""))
}

#raster with district IDs
if (!file.exists(paste(cd,"/raster/india-1min-disid.asc",sep=""))) {
  rk <- rasterize(shp,rs,field="DISID")
  rk <- writeRaster(rk,paste(cd,"/raster/india-1min-disid.asc",sep=""),format='ascii')
} else {
  rk <- raster(paste(cd,"/raster/india-1min-disid.asc",sep=""))
  rk <- readAll(rk)
}

x <- createSummaryRasters(rs,rk,sData,"DISID",cd)


#plot the summary rasters
sumDir <- paste(cd,"/raster/summaries",sep="")
ascList <- list.files(sumDir)

for (asc in ascList) {
  #loading raster
  cat("\nLoading raster",asc,"\n")
  rs <- raster(paste(sumDir,"/",asc,sep=""))
  tiffName <- paste(sumDir,"/",strsplit(asc,".",fixed=T)[[1]][1],".tif",sep="")
  ht <- 1000
  fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
  wt <- ht*(fct-.1)
  
  #get colors
  cat("Get legend stuff \n")
  brks <- unique(quantile(rs[],na.rm=T,probs=seq(0,1,by=0.05)))
  brks.lab <- round(brks,0)
  cols <- c("grey 70",colorRampPalette(c("pink","red"))(length(brks)))
  
  #create the tiff
  cat("Now the plot \n")
  tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
  par(mar=c(3,3,1,3.5))
  plot(rs,
       col=cols,
       breaks=brks,lab.breaks=brks.lab,
       useRaster=T,
       horizontal=T,
       legend.shrink=0.95)
  plot(shp,add=T,border="white",lwd=0.2)
  plot(wrld_simpl,add=T,lwd=0.8)
  dev.off()
}

