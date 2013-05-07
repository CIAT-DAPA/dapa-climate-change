#---------------------------------------------------------------------------------------------------------
# Description: This script is to calcute tmax or tmin if it is possible
# Author: Carlos Navarro
# Date: 07/05/13
#---------------------------------------------------------------------------------------------------------

require(raster)
require(ncdf)
# rcp <- "historical"

GCMTmpCalc <- function(rcp='historical') {
  
  rcp <- "historical"
  baseDir <- "T:/gcm/cmip5/raw/monthly"
  gcmStats <- read.table(paste(baseDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  
  for (i in 1:nrow(gcmStats)){
    
    if(paste(as.matrix(gcmStats)[i,10]) == "tmax") {
    
      gcm <- paste(as.matrix(gcmStats)[i,2])
      ens <- paste(as.matrix(gcmStats)[i,3])
      ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
      mthDir <- paste(ensDir, "/monthly-files", sep="")
      
      cat(paste("\n ->. Processing ", rcp, " ", gcm, " ", ens,  sep=""))
      
      yearDirs <- list.dirs(mthDir, recursive = FALSE, full.names = FALSE)
      for (yearDir in yearDirs) {
        
        for (mth in monthList) {
          
          tminRaster <- raster(paste(yearDir, "/tmin_", mth, ".nc", sep=""))
          tmeanRaster <- raster(paste(yearDir, "/tmean_", mth, ".nc", sep=""))
          tmaxRaster <- (2 * tmeanRaster) - tminRaster
          tmaxRaster <- writeRaster(tmaxRaster, paste(yearDir, "/tmax_", mth, ".nc", sep=""), format="CDF", overwrite=T)
          cat(paste("\n\t\t ->. ", paste(basename(yearDir)), " tmax_", mth, ".nc", sep=""))
          
        }
      }    
    }
    
    if(paste(as.matrix(gcmStats)[i,10]) == "tmin") {
      
      gcm <- paste(as.matrix(gcmStats)[i,2])
      ens <- paste(as.matrix(gcmStats)[i,3])
      ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
      mthDir <- paste(ensDir, "/monthly-files", sep="")
      
      cat(paste("\n ->. Processing ", rcp, " ", gcm, " ", ens,  sep=""))
      
      yearDirs <- list.dirs(mthDir, recursive = FALSE, full.names = FALSE)
      for (yearDir in yearDirs) {
        
        for (mth in monthList) {
          
          tmaxRaster <- raster(paste(yearDir, "/tmax_", mth, ".nc", sep=""))
          tmeanRaster <- raster(paste(yearDir, "/tmean_", mth, ".nc", sep=""))
          tminRaster <- (2 * tmeanRaster) - tmaxRaster
          tminRaster <- writeRaster(tminRaster, paste(yearDir, "/tmin_", mth, ".nc", sep=""), format="CDF", overwrite=T)
          cat(paste("\n\t\t ->. ", paste(basename(yearDir)), " tmin_", mth, ".nc", sep=""))
          
        }
      }    
    }
    
    
  }

}