#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014
stop("!")

#calculate tmin and tmax from WFDEI Tair data using cdo
#commands as follows:

#cdo daymax Tair_WFDEI_201212.nc Tmax_daily_WFDEI_201212.nc
#cdo daymin Tair_WFDEI_201212.nc Tmin_daily_WFDEI_201212.nc

#all data at: /nfs/a101/eejarv/quest-for-robustness/data/meteorology/wfd_data
#Tair: Tair_WFDEI
#Tmax: Tmax_daily_WFDEI (will be created)
#Tmin: Tmin_daily_WFDEI (will be created)

#1. gunzip the file
#2. create the two files
#3. gzip the file again

#data format: Tair_WFDEI_197901.nc.gz through to Tair_WFDEI_201212.nc.gz
#ALSO: do the same process to WFD / WFDEI data than to ISIMIP weather (i.e. remapcon2 and sellonlatbox)

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
wd <- "/nfs/a101/eejarv/quest-for-robustness"
#metDir <- paste(wd,"/data/meteorology/ISIMIP_wth",sep="")
metDir <- paste(wd,"/data/meteorology/wfd_data",sep="")

#output directories
taDir <- paste(metDir,"/Tair_WFDEI",sep="")
txDir <- paste(metDir,"/Tmax_daily_WFDEI",sep=""); if (!file.exists(txDir)) {dir.create(txDir)}
tnDir <- paste(metDir,"/Tmin_daily_WFDEI",sep=""); if (!file.exists(tnDir)) {dir.create(tnDir)}

#loop years and months
for (yr in 1979:2012) {
  #yr <- 1979
  for (mth in 1:12) {
    #mth <- 1
    cat("...processing year=",yr,"and month=",mth,"\n")
    tmth <- sprintf("%1$02d",mth)
    
    inFile <- paste("Tair_WFDEI_",yr,tmth,".nc",sep="")
    txFile <- paste("Tmax_daily_WFDEI_",yr,tmth,".nc",sep="")
    tnFile <- paste("Tmin_daily_WFDEI_",yr,tmth,".nc",sep="")
    
    if (!file.exists(paste(tnDir,"/",tnFile,sep=""))) {
      #gunzip the file
      setwd(taDir)
      system(paste("gunzip ",inFile,".gz",sep=""))
      
      #create the two files
      system(paste("cdo daymax ",inFile," ",txDir,"/",txFile,sep=""))
      system(paste("cdo daymin ",inFile," ",tnDir,"/",tnFile,sep=""))
      
      #gzip the file again
      system(paste("gzip ",inFile,sep=""))
      setwd(wd)
    }
  }
}



