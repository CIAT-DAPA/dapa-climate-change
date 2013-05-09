#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
#-----------------------------------------------------------------------

require(raster)
require(ncdf)
# rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")


#####################################################################################################
# Description: This function is to calcute tmax or tmin if it is possible
#####################################################################################################


GCMTmpCalc <- function(rcp='historical', baseDir="T:/data/gcm/cmip5/raw/monthly") {
  
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
            
            if (!file.exists(paste(yearDir, "/tmax_", mth, ".nc", sep=""))) {
            
              tminRaster <- raster(paste(yearDir, "/tmin_", mth, ".nc", sep=""))
              tmeanRaster <- raster(paste(yearDir, "/tmean_", mth, ".nc", sep=""))
              tmaxRaster <- (2 * tmeanRaster) - tminRaster
              tmaxRaster <- writeRaster(tmaxRaster, paste(yearDir, "/tmax_", mth, ".nc", sep=""), format="CDF", overwrite=T)
              cat(paste("\n\t\t ->. ", rcp, " ", paste(basename(yearDir)), " tmax_", mth, ".nc", sep=""))
            
            }
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
            
            if (!file.exists(paste(yearDir, "/tmin_", mth, ".nc", sep=""))) {
              
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
  
  return("Temperature Calcs Process Done!")
}


#####################################################################################################
# Description: This function is to calculate the averaging surfaces of the CMIP5 monhtly climate data
#####################################################################################################


GCMAverage <- function(rcp='historical', baseDir <- "T:/gcm/cmip5/raw/monthly") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  gcmStats <- read.table(paste(baseDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  varList <- c("prec", "tmax", "tmin")
  
  for (i in 1:nrow(gcmStats)){
    
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var") {
      
      gcm <- paste(as.matrix(gcmStats)[i,2])
      ens <- paste(as.matrix(gcmStats)[i,3])
      ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
      mthDir <- paste(ensDir, "/monthly-files", sep="")
      
      avgDir <- paste(ensDir, "/average", sep="")
      if (!file.exists(avgDir)) {dir.create(avgDir)}
      

      if (rcp == "historical"){
        
        periodList <- c("1960", "1970")
        
        for (period in periodList) {
          
          staYear <- period
          endYear <- period + 29
          
          cat("Average over ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear)," \n"))

          for (var in varList) {
            
            for (mth in monthList) {
              
              if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, sep=""))) 
              {dir.create(paste(avgDir, "/", staYear, "_", endYear, sep=""))}
              
              mthNc <- lapply(paste(mthDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
              mthNcAvg <- rotate(mean(stack(mthNc)))
              mthNcAvg <- writeRaster(mthNcAvg, paste(avgDir, "/", staYear, "_", endYear, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
              
              cat("\t", gcm, "\t", ens, "\t", paste(staYear, "_", endYear), "\t", var, "_", mth," \n"))
              
              }
            }
          } 

        } else {

          periodList <- c("2020", "2040", "2060", "2080")
          
          for (period in periodList) {

            staYear <- period
            endYear <- period + 29
            
            cat("Average over ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear)," \n"))
            
            for (var in varList) {
              
              for (mth in monthList) {
                
                if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, sep=""))) 
                  {dir.create(paste(avgDir, "/", staYear, "_", endYear, sep=""))}
                
                mthNc <- lapply(paste(mthDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
                mthNcAvg <- rotate(mean(stack(mthNc)))
                mthNcAvg <- writeRaster(mthNcAvg, paste(avgDir, "/", staYear, "_", endYear, "/", var, "_", mth, ".nc", sep=""), format='CDF', overwrite=T)
                
                cat("\t", gcm, "\t", ens, "\t", paste(staYear, "_", endYear), "\t", var, "_", mth," done\n"))
              }
            }
          }        
        }
      }
    }

  return("GCM Average Process Done!")
  
  } 


GCMResample <- function(rcp='historical', baseDir <- "T:/gcm/cmip5/raw/monthly") {
  
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

  cat("Average over ", m, " models", "\n")
  
  rsum <- rsum / m
  rsum <- writeRaster(rsum, outFile, format='ascii', overwrite=TRUE)
  
  return("GCM Resample Process Done!")
  
  }






