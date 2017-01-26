#Julian Ramirez-Villegas, based on obs_yield_plots.R (from robustness)
#CIAT / CCAFS
#Jan 2017
stop("!")
#read in Iizumi et al. (2013) yield data
#Rojas et al. used 1986-2005 baseline

#1. create global yield rasters for the 4 crops, for all crop seasons
#2. create yield rasters for period 1985-2006 at resolution of Rojas et al. study (1x1 degree)

#packages
library(sp); library(raster); library(rgdal); library(maptools); library(rasterVis)
data(wrld_simpl)

#settings
wd <- "~/Leeds-work"
dataDir <- paste(wd,"/datasets",sep="")
toeDir <- paste(wd,"/ToE-global-crops-seasons",sep="")
outyDir <- paste(toeDir,"/yield_responses/yield_data",sep="")
if (!file.exists(outyDir)) {dir.create(outyDir)}
cropname <- "soybean"
yieldDir <- paste(dataDir,"/yield_data/gdhy/gdhy_2013JAN29_",cropname,sep="")

if (cropname %in% c("maize","rice")) {
  seasons <- c("_","_major_","_second_")
} else if (cropname %in% c("soybean")) {
  seasons <- c("_")
} else {
  seasons <- c("_","_spring_","_winter_")
}
years <- c(1982:2006)

###
#1. create global yield rasters for the 4 crops, for all crop seasons
#choose / loop season
for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas," for crop:",cropname,"\n",sep="")
  
  #output raster dir
  orsDir <- paste(yieldDir,"/raster",seas,cropname,sep="")
  if (!file.exists(orsDir)) {dir.create(orsDir)}
  
  if ((length(list.dirs(orsDir))-1) != length(years)) {
    #choose / loop year
    for (yr in years) {
      #yr <- years[1]
      cat("\n...processing year: ",yr,"\n",sep="")
      
      #output year dir
      orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
      if (!file.exists(orsyrDir)) {dir.create(orsyrDir)}
      
      #read table
      ydata <- read.table(paste(yieldDir,"/gdhy_2013JAN29_",cropname,seas,yr,".dat",sep=""),sep="",header=T)
      
      #get grid names to info
      grdnames <- names(ydata)[5:ncol(ydata)]
      grdnames_f <- gsub("t.ha.","",grdnames)
      grdnames_f <- gsub("\\.","",grdnames_f)
      
      #get lats and lons for grid creation
      longs <- unique(ydata$Long.deg..)
      lats <- unique(ydata$Lati.deg..)
      
      #create raster and put data into it (lon,lat are center)
      rs <- raster(nrows=length(lats), ncols=length(longs), xmn=-0.5625, xmx=359.4375, ymn=-90, ymx=90)
      #rs2 <- raster(nrows=length(lats), ncols=length(longs), xmn=0, xmx=360, ymn=-90, ymx=90)
      ydata$CELL <- cellFromXY(rs, xy=cbind(x=ydata$Long.deg..,y=ydata$Lati.deg..))
      
      for (gn in 1:length(grdnames)) {
        #gn <- 1
        cat("processing",grdnames_f[gn],"\n")
        if (!file.exists(paste(orsyrDir,"/gdhy_2013JAN29_",cropname,seas,grdnames_f[gn],"_",yr,".tif",sep=""))) {
          trs <- rs
          trs[ydata$CELL] <- ydata[,grdnames[gn]]
          trs[which(trs[] == -999)] <- NA
          if (gn == 1) {trs[which(trs[] == 0)] <- NA}
          trs <- rotate(trs)
          trs <- writeRaster(trs, paste(orsyrDir,"/gdhy_2013JAN29_",cropname,seas,grdnames_f[gn],"_",yr,".tif",sep=""),format="GTiff")
          rm(trs)
        }
      }
    }
  }
}


###
#2. create yield rasters for period 1985-2006 at resolution of Rojas et al. study (1x1 degree)

msk <- raster(paste(toeDir,"/data/mask_4TOE.nc",sep=""))
msk[which(!is.na(msk[]))] <- 1

#choose / loop season
for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas," for crop:",cropname,"\n",sep="")
  
  #output raster dir
  oyldDir <- paste(outyDir,"/yield_data",seas,cropname,sep="")
  if (!file.exists(oyldDir)) {dir.create(oyldDir)}
  
  #where data was saved above
  orsDir <- paste(yieldDir,"/raster",seas,cropname,sep="")
  
  #choose / loop year
  for (yr in years) {
    #yr <- years[1]
    cat("\n...processing year: ",yr,"\n",sep="")
    
    #output year dir
    if (!file.exists(paste(oyldDir,"/gdhy_2013JAN29_",cropname,seas,"ModelYld500_",yr,".tif",sep=""))) {
      orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
      trs <- raster(paste(orsyrDir,"/gdhy_2013JAN29_",cropname,seas,"ModelYld500_",yr,".tif",sep=""))
      trs <- resample(trs, msk, method="bilinear")
      trs <- writeRaster(trs, paste(oyldDir,"/gdhy_2013JAN29_",cropname,seas,"ModelYld500_",yr,".tif",sep=""),format="GTiff")
    }
  }
}


