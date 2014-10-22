#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
#-----------------------------------------------------------------------

require(maptools)
require(raster)
require(ncdf)
require(rgdal)
require(sp)


#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################
GCMAnomaliesYearly <- function(rcp="rcp60", gcm="bcc_csm1_1", ens="r1i1p1", year="2006", futDir="T:/gcm/cmip5/raw/monthly/rcp60", curAvgDir="T:/gcm/cmip5/raw/monthly/historical/bcc_csm1_1/r1i1p1/average/1961_1990", basePer="1961_1990", outDir="D:/CIAT/Workspace/urippke") {
  
  futMthDir <- paste(futDir, "/", gcm, "/", ens, "/monthly-files/", year, sep="")
  
  # Define start and end year
  #staYear <- as.integer(period)
  #endYear <- as.integer(period) + 29  
  
  if (file.exists(futMthDir)){
    
    if (file.exists(curAvgDir)){
      
      cat("\t Anomalies over: ", rcp, " ", gcm, " ", ens, " ", year," \n\n")
      
      # Create anomalies output directory 
      if (basePer == "1961_1990"){
        
        anomDir <- paste(outDir, "/anomalies_1975s_yearly/", rcp, "/", gcm, "/", ens, "/", year, sep="")
        
      } else if (basePer == "1971_2000") {
        
        anomDir <- paste(outDir, "/anomalies/", rcp, "/", gcm, "/", ens, "/", year, sep="")
        
      }
      
      
      if (!file.exists(anomDir)) {dir.create(anomDir, recursive = TRUE)}
      
      
      # List of variables and months
      varList <- c("prec", "tmax", "tmin")
      
      
      # Get a list of month with and withour 0 in one digit numbers
      monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
      monthListMod <- c(1:12)
      
      # Set number of days by month
      ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      
      # Combirn number of month and days in one single data frame
      ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
      names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
      
      
      # Loop around variables
      for (var in varList) {
        
        # Loop around months
        for (mth in monthList) {
          
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          
          outNc <- paste(anomDir, "/", var, "_", mthMod, ".nc", sep="")
          if (!file.exists(outNc)) {
            
            curAvgNc <- raster(paste(curAvgDir, "/", var, "_", mthMod, ".nc", sep="")) ##variables in mm and deg
            futMthNc <- raster(paste(futMthDir, "/", var, "_", mth, ".nc", sep=""))
            
            # Create a stack of list of NC, rotate and convert units in mm/monnth and deg celsious
            if (var == "prec"){
              
              daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
              futMthNc <- rotate(futMthNc) * 86400 * (daysmth)
              
            } else {
              
              futMthNc <- rotate(futMthNc) - 272.15
            }
            
            
            anomNc <- futMthNc - curAvgNc
            
            # resAnomNc  <- resample(anomNc, rs, method='ngb')                
            
            rs <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, ncols=720, nrows=360)
            anomNcExt <- setExtent(anomNc, extent(rs), keepres=FALSE, snap=TRUE)
            resAnomNcExt  <- resample(anomNcExt, rs, method='ngb')
            
            anomNc <- writeRaster(resAnomNcExt, outNc, format='CDF', overwrite=TRUE)
            
            
            cat(" .> ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")
            
          } else {cat(" .> ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")}
          
        }    
      } 
    }
  }
}

#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################
GCMCalcFutureYearly <- function(rcp='rcp26', baseDir="L:/gcm/cmip5/raw/monthly", ens="r1i1p1", basePer="1971_2000", outDir="G:/cenavarro/Request/urippke", cruDir="S:/data/observed/gridded_products/cru-ts-v3-21") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXX GCM FUTURE CALC YEARLY CALCULATION XXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # List of variables and months
  varList <- c("prec", "tmax", "tmin")
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  
  # Set number of days by month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  gcmStats <- read.table(paste("G:/_scripts/dapa-climate-change/IPCC-CMIP5", "/data/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  # Loop around gcms and ensembles
  for (i in 1:nrow(gcmStats)){
    
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var"){
      
      if(!paste(as.matrix(gcmStats)[i,10]) == "ins-yr"){
        
        if(paste(as.matrix(gcmStats)[i,3]) == "r1i1p1"){
          # Get gcm and ensemble names
          gcm <- paste(as.matrix(gcmStats)[i,2])
          
          cat("\tFuture Calcs over: ", rcp, " ", gcm, " ", ens, " \n\n")
          
          # Loop around months
          for (mth in monthList) {
            
            mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
            
            # Loop around variables
            for (var in varList) {
              
              if (!file.exists(paste(outDir, "/africa_cmip5_30min_yearly/", rcp, "/", gcm, "/", ens, "/", 2099, "/", var, "_", mthMod, ".tif", sep=""))){
                
                year <- 2006:2099
                cruAsc <- raster(paste(cruDir, "/30yr_averages/", basePer, "/", var, "_", mthMod, ".asc", sep=""))
                anomDir <- paste(outDir, "/world_anomalies_cmip5_raw_resolution/", rcp, "/", gcm, "/", ens, sep="")
                anomNc <- lapply(paste(anomDir, "/", year, "/", var, "_", mthMod, ".nc", sep=""),FUN=raster)
                anomNc <- stack(anomNc)
                outFut <- cruAsc + anomNc
                ext <- extent(-26, 64, -47, 38)
                outFut <- crop(outFut, ext)
                
                
                if (var == "prec"){outFut[][outFut[]<0]=0}
                
                for (i in 1:dim(outFut)[[3]]){
                  
                  futDir <- paste(outDir, "/africa_cmip5_30min_yearly/", rcp, "/", gcm, "/", ens, "/", year[i], sep="")
                  if (!file.exists(futDir)) {dir.create(futDir, recursive = TRUE)}
                  
                  futTif <- paste(futDir, "/", var, "_", mthMod, ".tif", sep="")
                  
                  
                  if (!file.exists(futTif)) {
                    
                    outYrAsc <- writeRaster(outFut[[i]], futTif, format='GTiff', overwrite=FALSE)
                    
                    cat(" .> ", paste("\t ", var, "_", mthMod, " ", year[i], sep=""), "\tdone!\n")
                  }  else {cat(" .> ", paste("\t ", var, " ", mthMod, " ", year[i], sep=""), "\tdone!\n")}
                  
                }
              }
            }
          }  
        }
      }
    }
  } 
  cat("GCM Future Calcs Process Done!")
}
