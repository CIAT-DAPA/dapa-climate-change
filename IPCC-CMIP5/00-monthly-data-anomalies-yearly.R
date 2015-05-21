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
#       varList <- c("prec", "tmax", "tmin")
      var <- "prec"
      
      # Get a list of month with and withour 0 in one digit numbers
      monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
      monthListMod <- c(1:12)
      
      # Set number of days by month
      ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      
      # Combirn number of month and days in one single data frame
      ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
      names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
      
      
      # Loop around variables
#       for (var in varList) {
        
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
              futMthNc <- rotate(futMthNc * 86400 * (daysmth))
              
            } else {
              
              futMthNc <- rotate(futMthNc) - 272.15
            }
            
            if (var == "prec" || var == "rsds"){
              anomNc <- (futMthNc - curAvgNc) / curAvgNc
            } else {
              anomNc <- futMthNc - curAvgNc  
            }
            
            
            # resAnomNc  <- resample(anomNc, rs, method='ngb')                
            
            rs <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, ncols=720, nrows=360)
            anomNcExt <- setExtent(anomNc, extent(rs), keepres=FALSE, snap=TRUE)
            resAnomNcExt  <- resample(anomNcExt, rs, method='ngb')
            
            anomNc <- writeRaster(resAnomNcExt, outNc, format='CDF', overwrite=TRUE)
            
            
            cat(" .> ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")
            
          } else {cat(" .> ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")}
          
        }    
#       } 
    }
  }
}

#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################
GCMCalcFutureYearly <- function(rcp='rcp26', gcm="bcc_csm1_1", ens="r1i1p1", outDir="D:/CIAT/Workspace/urippke", obsDir="S:/observed/gridded_products/worldclim/Global_30min/_asciis", dataset="wcl") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXX GCM FUTURE CALC YEARLY CALCULATION XXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # List of variables and months
  varList <- c("prec", "tmax", "tmin")
#   var <- "prec"
  
  # Loop around months
  for (mth in 1:12) {
    
    # Loop around variables
    for (var in varList) {
      
      if (!file.exists(paste(outDir, "/diss_africa_1975s_yearly_", dataset, "/", rcp, "/", gcm, "/", ens, "/", 2099, "/", var, "_", mth, ".tif", sep=""))){
        
        year <- 2006:2099
        
        obsAsc <- raster(paste(obsDir, "/", var, "_", mth, ".asc", sep=""))
                
        anomDir <- paste(outDir, "/anomalies_1975s_yearly/", rcp, "/", gcm, "/", ens, sep="")
        anomNc <- lapply(paste(anomDir, "/", year, "/", var, "_", mth, ".nc", sep=""),FUN=raster)
        anomNc <- stack(anomNc)
       
        
        if (var == "prec" || var == "rsds"){
          outFut <- obsAsc * (1 + anomNc)
          
        } else {
          outFut <- obsAsc + anomNc
        }
        
        
        ext <- extent(-26, 64, -47, 38)
        outFut <- crop(outFut, ext)
        crs(outFut) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        
        for (i in 1:dim(outFut)[[3]]){
          
          futDir <- paste(outDir, "/diss_africa_1975s_yearly_", dataset, "/", rcp, "/", gcm, "/", ens, "/", year[i], sep="")
          if (!file.exists(futDir)) {dir.create(futDir, recursive = TRUE)}
          
          futTif <- paste(futDir, "/", var, "_", mth, ".tif", sep="")
          
          if (!file.exists(futTif)) {

            
            if (var == "prec"){outFut[[i]][outFut[[i]]<0]=0}
            
            outYrAsc <- writeRaster(outFut[[i]], futTif, format='GTiff', overwrite=TRUE)
            
            
          }
        }
      }
    } 

  }

  if (!file.exists(paste(outDir, "/diss_africa_1975s_yearly_", dataset, "/", rcp, "/", gcm, "/", ens, "/", 2099, "/tmean_12.tif", sep=""))){
    
    for (yr in 2006:2099){
      
      futDir <- paste(outDir, "/diss_africa_1975s_yearly_", dataset, "/", rcp, "/", gcm, "/", ens, "/", yr, sep="")
      
      for (mth in 1:12){
        
        outTmin <- paste(futDir, "/tmin_", mth, ".tif", sep="")  
        outTmax <- paste(futDir, "/tmax_", mth, ".tif", sep="")
        outTmean <- paste(futDir, "/tmean_", mth, ".tif", sep="")
        
        if (!file.exists(outTmean)) {
          
          tmean <- ( raster(outTmax) + raster(outTmin) ) / 2
          
          outRs <- writeRaster(tmean, outTmean, format='GTiff', overwrite=FALSE)
         
        }
        
      }
      
    }
    
  }
  
  cat("GCM Future Calcs Process Done!")
}
