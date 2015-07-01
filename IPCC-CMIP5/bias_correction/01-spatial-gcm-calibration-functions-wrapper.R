## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=10) #initiate cluster

stop("error")
source("01-gcm-calibration-functions.R")

#Set parameters 
rcp="historical"
ts="1970_2000"
bbox=c(-89.40,-82.52,8,16.04)
reg_suf="cam"
resolution=0.04
gcmDir="/mnt/data_cluster_2/gcm/cmip5/raw/daily"
# gcmDir="T:/gcm/cmip5/raw/daily"
dirout="/home/cnavarro/Request_oovalle"
# dirout="D:/cenavarro/Request_oovalle"

gcmDir <- paste0(gcmDir, "/", rcp)

# Export functions
sfExport("GCMDailyResample")

#export variables
sfExport("rcp")
sfExport("ts")
sfExport("bbox")
sfExport("reg_suf")
sfExport("resolution")
sfExport("gcmDir")
sfExport("dirout")

gcmList <- list.dirs(gcmDir, recursive = FALSE, full.names = FALSE)

for (i in 1:length(gcmList)){
  
  # gcm <- paste(as.matrix(gcmList[i]))
  gcm <- gcmList[i]
  sfExport("gcm")    
  
  control <- function(i) { #define a new function
    
    library(ncdf)
    library(raster)
    library(lubridate)
    library(rgdal)
    
    cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
    GCMDailyResample(rcp, ts, bbox, reg_suf, resolution, i, gcmDir, dirout)
    
  }
  
  system.time(sfSapply(as.vector(paste(as.matrix(gcmList)[1:length(gcmList)])), control))
  
}


#stop the cluster calculation
sfStop()

cat("GCM Resample done!")
