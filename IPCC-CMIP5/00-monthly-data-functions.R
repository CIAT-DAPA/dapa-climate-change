#-----------------------------------------------------------------------
# Description: This script is to calcute tmax or tmin if it is possible
# Author: Carlos Navarro
# Date: 07/05/13
#-----------------------------------------------------------------------

require(raster)
require(ncdf)
# rcp <- "historical"

GCMTmpCalc <- function(rcp='historical', baseDir="T:/data/gcm/cmip5/raw/monthly") {
  # rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  # for (rcp in rcpList) {
  
  
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
        
        if (paste(basename(yearDir)) < 2100) {
          
          for (mth in monthList) {
            
            tminRaster <- raster(paste(yearDir, "/tmin_", mth, ".nc", sep=""))
            tmeanRaster <- raster(paste(yearDir, "/tmean_", mth, ".nc", sep=""))
            tmaxRaster <- (2 * tmeanRaster) - tminRaster
            tmaxRaster <- writeRaster(tmaxRaster, paste(yearDir, "/tmax_", mth, ".nc", sep=""), format="CDF", overwrite=T)
            cat(paste("\n\t\t ->. ", rcp, " ", paste(basename(yearDir)), " tmax_", mth, ".nc", sep=""))
            
          }
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
        
        if (paste(basename(yearDir)) < 2100) {
          
          for (mth in monthList) {
            
            tmaxRaster <- raster(paste(yearDir, "/tmax_", mth, ".nc", sep=""))
            tmeanRaster <- raster(paste(yearDir, "/tmean_", mth, ".nc", sep=""))
            tminRaster <- (2 * tmeanRaster) - tmaxRaster
            tminRaster <- writeRaster(tminRaster, paste(yearDir, "/tmin_", mth, ".nc", sep=""), format="CDF", overwrite=T)
            cat(paste("\n\t\t ->. ", rcp, " ", paste(basename(yearDir)), " tmin_", mth, ".nc", sep=""))
            
          }
        }
      }    
    }
    
    
  }
  
}




#---------------------------------------------------------------------------------------------------------
# Description: This script is to calculate the averaging surfaces of the CMIP5 monhtly climate data for 
#              precipitation, maximum temperature and minimum temperature
# Author: Carlos Navarro
# Date: 29/04/13
#---------------------------------------------------------------------------------------------------------

require(raster)
require(ncdf)
# rcp <- "historical"

GCMAverage <- function(rcp='historical') {
  
  monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  varList <- c("prec", "tmax", "tmin")
  
  baseDir <- "T:/gcm/cmip5/raw/monthly"
  rcpDir <- paste(baseDir, "/", rcp, sep="")
  
  modelDirs <- list.dirs(rcpDir, recursive = FALSE, full.names = FALSE)
  for (modelDir in modelDirs) {
    
    
    ensList <- list.dirs(modelDir, recursive = FALSE, full.names = FALSE)
    for (ensDir in ensDirs) {
      
      ensDir <- "T:/gcm/cmip5/raw/monthly/historical/bcc_csm1_1/r1i1p1"
      mthDir <- paste(ensDir, "/monthly-files", sep="")
      
      avgDir <- paste(ensDir, "/average", sep="")
      if (!file.exists(avgDir)) {dir.create(avgDir)}
      
      for (var in varList) {
        
        for (mth in monthList) {
          
          var <- "prec"
          mth <- "01"
          
          if (rcp == "historical"){
            
            cat("Average over ", rcp, " ", model, " ", ensemble, " 1991_1990\n"))

avgDir1975 <- paste(avgDir, "/1961_1990", sep="")            
if (!file.exists(avgDir1975)) {dir.create(avgDir1975)}
mthNc1975 <- lapply(paste(mthDir, "/", 1961:1990, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
mthNcAvg1975 <- writeRaster(rotate(mean(stack(mthNc1975))), paste(avgDir1975, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)

avgDir1985 <- paste(avgDir, "/1971_2000", sep="")            
if (!file.exists(avgDir1985)) {dir.create(avgDir1985)}
mthNc1975 <- lapply(paste(mthDir, "/", 1961:1990, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
mthNcAvg1975 <- writeRaster(rotate(mean(stack(mthNc1975))), paste(avgDir1985, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)     

          } else {
            
            mthNc2030 <- lapply(paste(mthDir, "/", 2020:2049, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
            mthNcAvg2030 <- rotate(mean(stack(mthNc2030)))
            mthNcAvg2030 <- writeRaster(mthNcAvg2030, paste(avgDir, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
            
            mthNc2050 <- lapply(paste(mthDir, "/", 2040:2069, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
            mthNcAvg2050 <- rotate(mean(stack(mthNc2050)))
            mthNcAvg2050 <- writeRaster(mthNcAvg2050, paste(avgDir, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
            
            mthNc2070 <- lapply(paste(mthDir, "/", 2060:2089, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
            mthNcAvg2070 <- rotate(mean(stack(mthNc2070)))
            mthNcAvg2070 <- writeRaster(mthNcAvg2070, paste(avgDir, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
            
            mthNc2090 <- lapply(paste(mthDir, "/", 2080:2099, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
            mthNcAvg2090 <- rotate(mean(stack(mthNc2090)))
            mthNcAvg2090 <- writeRaster(mthNcAvg2090, paste(avgDir, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
            
          }



        }
      }
    }
  }
}



rs <- raster(inFile)
rs <- readAll(rs)

if (xres(rs) == xres(rsum)) {
  cat("\n", "Not resampling ", "\n")
  
  rsum <- rsum + rs
  m <- m+1
} else {
  
  dfr <- data.frame(raster=c("rs", "rsum"), resol=c(xres(rs), xres(rsum)))
  mnResRaster <- trim(dfr[which(dfr[,2] == min(dfr[,2])),1])
  mxResRaster <- trim(dfr[which(dfr[,2] == max(dfr[,2])),1])
  
  nCols <- ncol(get(paste(mnResRaster)))
  nRows <- nrow(get(paste(mnResRaster)))
  
  xMin <- xmin(get(paste(mnResRaster)))
  xMax <- xmax(get(paste(mnResRaster)))
  yMin <- ymin(get(paste(mnResRaster)))
  yMax <- ymax(get(paste(mnResRaster)))
  
  nwrs <- raster(get(paste(mnResRaster)))
  
  cat(ncol(get(paste(mnResRaster))), nrow(get(paste(mnResRaster))),"\n")
  cat(ncol(get(paste(mxResRaster))), nrow(get(paste(mxResRaster))),"\n")
  cat(ncol(nwrs), nrow(nwrs), "\n")
  
  rm(dfr)
  
  cat("\n", "Resampling ", mxResRaster, " with ", mnResRaster, "\n")
  
  nwrs <- resample(get(paste(mxResRaster)), nwrs, method='ngb')
  rsum <- rsum + nwrs
  rm(nwrs)
  
  m <- m+1
}
}
}
}

cat("Average over ", m, " models", "\n")

rsum <- rsum / m
rsum <- writeRaster(rsum, outFile, format='ascii', overwrite=TRUE)

}

} 
}
}
setwd("F://climate_change//_scripts")
return("Done!")
}


