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


GCMAverage <- function(rcp='rcp26', baseDir="T:/gcm/cmip5/raw/monthly") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # Read gcm characteristics table
  gcmStats <- read.table(paste(baseDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  
  # Set number of days by month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")

  # List of variables to average
  varList <- c("prec", "tmax", "tmin")
  
  # Get gcm statistics
  dataMatrix <- c("rcp", "model", "xRes", "yRes", "nCols", "nRows", "xMin", "xMax", "yMin", "yMax")
  
  # Loop around gcms and ensembles
  for (i in 1:nrow(gcmStats)){
  
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStats)[i,10]) == "ins-yr"){
        
        # Get gcm and ensemble names
        gcm <- paste(as.matrix(gcmStats)[i,2])
        ens <- paste(as.matrix(gcmStats)[i,3])
        
        # Path of each ensemble
        ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
        
        # Directory with monthly splitted files
        mthDir <- paste(ensDir, "/monthly-files", sep="")
        
        # Create output average directory
        avgDir <- paste(ensDir, "/average", sep="")
        if (!file.exists(avgDir)) {dir.create(avgDir)}
        
        # Period list for historical and future pathways
        if (rcp == "historical"){
          
          periodList <- c("1961", "1971")
        
        } else {
          
          periodList <- c("2020", "2030", "2040", "2050", "2060", "2070")
          
        }
        
        # Loop around periods
        for (period in periodList) {
          
          # Define start and end year
          staYear <- as.integer(period)
          endYear <- as.integer(period) + 29
          
          cat("\nAverage over: ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
  
          # Loop around variables
          for (var in varList) {
            
            # Loop around months
            for (mth in monthList) {
              
              if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, sep=""))) 
              {dir.create(paste(avgDir, "/", staYear, "_", endYear, sep=""))}
              
              # Define month without 0 in one digit number
              mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
              outNcAvg <- paste(avgDir, "/", staYear, "_", endYear, "/", var, "_", mthMod, ".nc", sep="")
              
              if (!file.exists(outNcAvg)){
                
                # List of NetCDF files by month for all 30yr period
                mthNc <- lapply(paste(mthDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
                
                # Create a stack of list of NC, rotate and convert units in mm/monnth and deg celsious
                if (var == "prec"){
                  
                  daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
                  mthNcAvg <- rotate(mean(stack(mthNc))) * 86400 * (daysmth)
                  
                } else {
                  
                  mthNcAvg <- rotate(mean(stack(mthNc))) - 272.15
                }
                
                # Write output average NetCDF file
                mthNcAvg <- writeRaster(mthNcAvg, outNcAvg, format='CDF', overwrite=T)
              
                cat(" .> ", paste(var, "_", mthMod, sep=""), "\tdone!\n")
              
              } else {cat(" .>", paste(var, "_", mthMod, sep=""), "\tdone!\n")}
              
              }
            }
          
          if(ens == "r1i1p1") {
          
            # Get a table with resolution and extent by model
            exNc <- raster(paste(avgDir, "/", staYear, "_", endYear, "/prec_1.nc", sep=""))
            
            xRes <- xres(exNc)
            yRes <- yres(exNc)
            nCols <- ncol(exNc)
            nRows <- nrow(exNc)
            xMin <- xmin(exNc)
            xMax <- xmax(exNc)
            yMin <- ymin(exNc)
            yMax <- ymax(exNc)
            
#             gcmChart <- cbind(rcp, gcm, xRes, yRes, nCols, nRows, xMin, xMax, yMin, yMax)
            
            dataMatrix <- rbind(dataMatrix,c(rcp, gcm, xRes, yRes, nCols, nRows, xMin, xMax, yMin, yMax))
            
            }
          }
        }
      }
    
      
    }
  
  write.csv(dataMatrix, paste(baseDir, "/", rcp, "-gcm-chart.csv", sep=""), row.names=F)
  cat("GCM Average Process Done!")
  
  }


#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################
GCMAnomalies <- function(rcp='rcp26', baseDir="T:/gcm/cmip5/raw/monthly", ens="r1i1p1", basePer="1961_1990") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXX GCM ANOMALIES CALCULATION XXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")

  # List of variables and months
  varList <- c("prec", "tmax", "tmin")
  monthList <- c(1:12)
  
  curDir <- paste(baseDir, "/historical", sep="")
  futDir <- paste(baseDir, "/", rcp, sep="")
  
  gcmList <- list.dirs(curDir, recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {
    
    # Get gcm names    
    gcm <- basename(gcm)

    # Path of each ensemble
    curEnsDir <- paste(curDir, "/", gcm, "/", ens, sep="")
    
    # Average directory
    curAvgDir <- paste(curEnsDir, "/average/", basePer, sep="")
    
    periodList <- c("2020", "2030", "2040", "2050", "2060", "2070")
    
    for (period in periodList) {
      
      # Define start and end year
      staYear <- as.integer(period)
      endYear <- as.integer(period) + 29
    
      futAvgDir <- paste(futDir, "/", gcm, "/", ens, "/average/", staYear, "_", endYear, sep="")
        
      if (file.exists(futAvgDir)){
        
        if (file.exists(curAvgDir)){
          
          cat("\t Anomalies over: ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
          
          # Create anomalies output directory 
          if (basePer == "1961_1990"){

            anomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1975s", sep="")
            anomPerDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1975s/", staYear, "_", endYear, sep="")  
            
          } else if (basePer == "1971_2000") {
            
            anomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1985s", sep="")
            anomPerDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1985s/", staYear, "_", endYear, sep="")
            
          }
          
          if (!file.exists(anomDir)) {dir.create(anomDir)}
          if (!file.exists(anomPerDir)) {dir.create(anomPerDir)}
          
          # Loop around variables
          for (var in varList) {
            
            # Loop around months
            for (mth in monthList) {
              
              curAvgNc <- raster(paste(curAvgDir, "/", var, "_", mth, ".nc", sep=""))
              futAvgNc <- raster(paste(futAvgDir, "/", var, "_", mth, ".nc", sep=""))
              
              anomNc <- futAvgNc - curAvgNc
              # resAnomNc  <- resample(anomNc, rs, method='ngb')
              
              rs <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90)
              anomNcExt <- setExtent(anomNc, extent(rs), keepres=FALSE, snap=FALSE)
              resAnomNcExt  <- resample(anomNcExt, rs, method='ngb')              

              outNc <- paste(anomPerDir, "/", var, "_", mth, ".nc", sep="")
              anomNc <- writeRaster(resAnomNcExt, outNc, format='ascii', overwrite=TRUE)
              
              cat(" .> Anomalies ", paste("\t ", var, "_", mth, sep=""), "\tdone!\n")
              
            }    
          } 
        }  
      }  
    }
  }
  cat("GCM Anomalies Process Done!")
}
