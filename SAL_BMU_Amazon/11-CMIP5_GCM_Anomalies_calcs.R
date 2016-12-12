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
# rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")


#####################################################################################################
# Description: This function is to calculate the averaging surfaces of the CMIP5 monhtly climate data
#####################################################################################################

GCMAverage <- function(rcp='rcp26', baseDir="T:/gcm/cmip5/raw/monthly", scrDir="D:/_scripts/dapa-climate-change/IPCC-CMIP5/data") {
  
  require(raster)
  require(ncdf)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # Read gcm characteristics table
  gcmStats <- read.table(paste(scrDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
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
          
          #           periodList <- c("1961", "1971", "1981")
          period <- "1986"
        } else {
          
          periodList <- c("2016", "2046", "2081")
          
        }
        
        # Loop around periods
        for (period in periodList) {
        
        # Define start and end year
        staYear <- as.integer(period)
        endYear <- as.integer(period) + 19
        
        cat("\nAverage over: ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
        
        if (gcm != "lasg_fgoals_g2" && gcm != "gfdl_esm2g" && gcm != "gfdl_esm2m"){
          
          if (ens == "r1i1p1"){
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
          }
          
        }
        
        #           
        #           if(ens == "r1i1p1") {
        #           
        #             # Get a table with resolution and extent by model
        #             exNc <- raster(paste(avgDir, "/", staYear, "_", endYear, "/prec_1.nc", sep=""))
        #             
        #             xRes <- xres(exNc)
        #             yRes <- yres(exNc)
        #             nCols <- ncol(exNc)
        #             nRows <- nrow(exNc)
        #             xMin <- xmin(exNc)
        #             xMax <- xmax(exNc)
        #             yMin <- ymin(exNc)
        #             yMax <- ymax(exNc)
        #             
        # #             gcmChart <- cbind(rcp, gcm, xRes, yRes, nCols, nRows, xMin, xMax, yMin, yMax)
        #             
        #             dataMatrix <- rbind(dataMatrix,c(rcp, gcm, xRes, yRes, nCols, nRows, xMin, xMax, yMin, yMax))
        #             
        #             }
                  }
      }
    }
    
    
  }
  
  #   write.csv(dataMatrix, paste(baseDir, "/", rcp, "-gcm-chart.csv", sep=""), row.names=F)
  cat("GCM Average Process Done!")
  
}

#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################

GCMAnomalies <- function(rcp='rcp26', baseDir="T:/gcm/cmip5/raw/monthly", ens="r1i1p1", basePer="1961_1990", oDir="D:/CIAT/ecu-hidroelectrica/03_future_climate_data/anomalies_cmip5", bbox="//nina/cenavarro/ecu-hidroelectrica/02_baseline/_region/alt-prj-ecu.asc.asc") {
  
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
  
  extbbox <- extent(bbox)
  
  gcmList <- list.dirs(curDir, recursive = FALSE, full.names = FALSE)
  
  #   dataMatrix <- c("gcm", "period", "var_mth", "value_st_1")
  
  for (gcm in gcmList) {
    
    # Get gcm names    
    gcm <- basename(gcm)
    
    if (gcm != "lasg_fgoals_g2" && gcm != "gfdl_esm2g" && gcm != "gfdl_esm2m"){
      
      # Path of each ensemble
      curEnsDir <- paste(curDir, "/", gcm, "/", ens, sep="")
      
      # Average directory
      curAvgDir <- paste(curEnsDir, "/average/", basePer, sep="")
      
      periodList <- c("2020", "2040", "2070")
      
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
              
            } else if (basePer == "1986_2005") {
              
              anomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1995s", sep="")
              anomPerDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1995s/", staYear, "_", endYear, sep="")
              oDirPer <- paste(oDir, "_raw/", rcp, "/", gcm, "/", staYear, "_", endYear, sep="")
              oDirPerRes <- paste(oDir, "_2_5min/", rcp, "/", gcm, "/", staYear, "_", endYear, sep="")
            }
            
            
            #           if (!file.exists(anomDir)) {dir.create(anomDir)}
            #           if (!file.exists(anomPerDir)) {dir.create(anomPerDir)}
            if (!file.exists(oDirPer)) {dir.create(oDirPer, recursive=T)}
            if (!file.exists(oDirPerRes)) {dir.create(oDirPerRes, recursive=T)}
            
            # Loop around variables
            for (var in varList) {
              
              # Loop around months
              for (mth in monthList) {
                
                
                outNc <- paste(oDirPerRes, "/", var, "_", mth, "_tmp.nc", sep="")
                if (!file.exists(outNc)) {
                  
                  curAvgNc <- raster(paste(curAvgDir, "/", var, "_", mth, ".nc", sep=""))
                  futAvgNc <- raster(paste(futAvgDir, "/", var, "_", mth, ".nc", sep=""))
                  
                  if (var == "prec" || var == "rsds"){
                    anomNc <- (futAvgNc - curAvgNc) / (curAvgNc + 0.5)
                  } else {
                    anomNc <- futAvgNc - curAvgNc  
                  }
                  
                  anomNc <- writeRaster(anomNc, outNc, format='CDF', overwrite=FALSE)
                }
                
                
                outNcRes <- paste(oDirPerRes, "/", var, "_", mth, ".nc", sep="")
                if (!file.exists(outNcRes)) {
                  
                  system(paste("cdo sellonlatbox,",extbbox@xmin-5,",",extbbox@xmax+5,",",extbbox@ymin-5,",",extbbox@ymax+5," ", outNc, " ", oDirPer, "/", var, "_", mth, ".nc", sep=""))
                  unlink(outNc, recursive = TRUE)
                  
                  anomNc <- raster(paste(oDirPer, "/", var, "_", mth, ".nc", sep=""))
                  
                  # resAnomNc  <- resample(anomNc, rs, method='ngb')
                  # anomNcExt <- setExtent(anomNc, extbbox, keepres=TRUE, snap=FALSE)
                  resAnomNcExt  <- resample(anomNc, bbox, method='bilinear')
                  resAnomNcExt <- writeRaster(resAnomNcExt, outNcRes, format='CDF', overwrite=FALSE)
                  
                }
              }    
            } 
          }  
        }  
      }
    }}
  cat("GCM Anomalies Process Done!")
}


### Parameters ###

# rcp <- "rcp26"
# rcp <- "rcp45"
# rcp <- "rcp60"
# rcp <- "rcp85"
# ens <- "r1i1p1"
# basePer <- "1961_1990"

# 
# otp <- GCMTmpCalc(rcp, baseDir)
# otp <- GCMAverage(rcp, baseDir)
# otp <- GCMAnomalies(rcp, baseDir, ens, basePer)
# otp <- GCMSummary(baseDir, ens)
# 
# basePer <- "1975s"
# basePer <- "1985s"
# otp <- GCMEnsembleAnom(baseDir, ens, basePer)
# 
# imageDir <- "T:/gcm/cmip5/baseinfo/inventory"
# baseDir <- "T:/gcm/cmip5/raw/monthly"
# ens <- "r1i1p1"
# otp <- GCMVerification(baseDir, ens, imageDir)
# otp <- GCMAnomaliesYearly(rcp, baseDir, ens, basePer, outDir)


# rcp <- "rcp85"
# scrDir <- "D:/_scripts/dapa-climate-change/IPCC-CMIP5/data"
# baseDir <- "T:/gcm/cmip5/raw/monthly"
# otp <- GCMAverage(rcp, baseDir, scrDir)


# source("11-CMIP5_GCM_Anomalies_calcs.R")
# rcp <- "rcp26"
# rcp <- "rcp45"
rcp <- "rcp85"
baseDir="T:/gcm/cmip5/raw/monthly"
ens="r1i1p1"
basePer <- "1986_2005"
oDir="Z:/DATA/WP2/03_Future_data/anomalies"
bbox <- raster(extent(-80.00833, -72.00833, -12, 3.000001), res=2.5/60) #2.5 min res
otp <- GCMAnomalies(rcp, baseDir, ens, basePer, oDir, bbox)



