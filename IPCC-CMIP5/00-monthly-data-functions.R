#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
#-----------------------------------------------------------------------

require(raster)
require(ncdf)
require(rgdal)
require(sp)
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

# rcp <- "rcp26"
# baseDir <- "T:/gcm/cmip5/raw/monthly"
# ens <- "r1i1p1"
# basePer <- "1961_1990"

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
              
              outAsc <- paste(anomPerDir, "/", var, "_", mth, ".asc", sep="")
              
              if (!file.exists(outAsc)) {
                
                curAvgNc <- raster(paste(curAvgDir, "/", var, "_", mth, ".nc", sep=""))
                futAvgNc <- raster(paste(futAvgDir, "/", var, "_", mth, ".nc", sep=""))
                
                anomNc <- futAvgNc - curAvgNc
                # resAnomNc  <- resample(anomNc, rs, method='ngb')
                
                rs <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90)
                anomNcExt <- setExtent(anomNc, extent(rs), keepres=TRUE, snap=FALSE)
                # resAnomNcExt  <- resample(anomNcExt, rs, method='ngb')              
  
                
                anomNc <- writeRaster(anomNc, outAsc, format='ascii', overwrite=FALSE)
                
                cat(" .> Anomalies ", paste("\t ", var, "_", mth, sep=""), "\tdone!\n")
                
              } else {cat(" .> Anomalies ", paste("\t ", var, "_", mth, sep=""), "\tdone!\n")}
              
            }    
          } 
        }  
      }  
    }
  }
  cat("GCM Anomalies Process Done!")
}



###########################################################
# Description: Summary of availables GCMs for downscaling 
###########################################################
GCMSummary <- function(baseDir="T:/gcm/cmip5/raw/monthly", ens="r1i1p1") {
  
  dataMatrix <- c("model", "ens", "rcp")
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  
  for (rcp in rcpList) {
      
    futDir <- paste(baseDir, "/", rcp, sep="")
    
    gcmList <- list.dirs(futDir, recursive = FALSE, full.names = FALSE)
    
    for (gcm in gcmList) {
      
      gcm <- basename(gcm)
      futAnomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1975s", sep="")
        
        if (file.exists(futAnomDir)){
          
          dataMatrix <- rbind(dataMatrix,c(gcm, ens, rcp))
          
        }  
      }
    }
  
  write.csv(dataMatrix, paste(baseDir, "/availability-gcm-", ens,".csv", sep=""), row.names=F)
  cat("GCM Summary Done!")
}

#####################################################################
# Description: This function is to calculate an ensemble of anomalies
#####################################################################
GCMEnsembleAnom <- function(baseDir="T:/data/gcm/cmip5/raw/monthly", ens="r1i1p1", basePer="1975s") {
  
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  
  # List of variables and months
  varList <- c("prec", "tmax", "tmin")
  mthList <- c(1:12)
 
  for (rcp in rcpList){
      
    gcmSum <- data.frame(read.csv(paste(baseDir, "/availability-gcm-", ens,".csv", sep="")))
    gcmSum <- gcmSum[gcmSum$rcp == rcp, ]
    gcmList <- gcmSum$model
    
    futDir <- paste(baseDir, "/", rcp, sep="")
    periodList <- c("2020", "2040", "2060")
    
    ensDir <- paste(baseDir, "/ensemble_anomalies", "/", basePer, "/", rcp, sep="")
    if (!file.exists(ensDir)) {
      dir.create(paste(baseDir, "/ensemble_anomalies", sep=""))
      dir.create(paste(baseDir, "/ensemble_anomalies/", basePer, sep=""))
      dir.create(ensDir)
    }
    
    for (period in periodList) {

      # Define start and end year
      staYear <- as.integer(period)
      endYear <- as.integer(period) + 29
      
      gcmMatrix <- matrix(ncol=0, nrow=length(gcmList))
      varNames <- c()
      
      ensDirPer <- paste(ensDir, "/", staYear, "_", endYear, sep="")
      if (!file.exists(ensDirPer)) {dir.create(ensDirPer)}
      
      csvStats <- paste(ensDir, "/anomalies-", rcp, "-", ens, "-", staYear, "_", endYear, ".csv", sep="")
      
      if (!file.exists(csvStats)){
        
        cat("Ensemble over: ", rcp, " ", ens, " ", paste(staYear, "_", endYear, " ", basePer, sep="")," \n\n")
        
        for (var in varList){
          
          for (mth in mthList){
            
            fileList <- c()
    
            for (gcm in gcmList){
              
              futAnomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_", basePer, "/", staYear, "_", endYear, sep="")
              futAnomPath <- paste(futAnomDir, "/", var, "_", mth, ".asc", sep="")
  
              fileList <- c(fileList, futAnomPath)
              
            }
          
            gcmStack <- stack(lapply(fileList, FUN=raster))
            
            cat(" .> Calculating..  ", paste("\t ", var, "_", mth, sep=""))
            
            if (!file.exists(paste(ensDirPer, "/", var, "_", mth, ".asc", sep=""))){
              
              gcmMean <- mean(gcmStack)
              fun <- function(x) { sd(x) }
              gcmStd <- calc(gcmStack, fun)
              # gcmRange <- range(gcmStack, na.rm = TRUE)
              
              gcmMean <- writeRaster(gcmMean, paste(ensDirPer, "/", var, "_", mth, ".asc", sep=""), format='ascii', overwrite=FALSE)
              gcmStd <- writeRaster(gcmStd, paste(ensDirPer, "/", var, "_", mth, "_sd.asc", sep=""), format='ascii', overwrite=FALSE)
              # gcmRange <- writeRaster(gcmRange, paste(ensDirPer, "/", var, "_", mth, "_range.asc", sep=""), format='ascii', overwrite=FALSE)
            }
          
            stat <- cellStats(gcmStack, mean)
            
            gcmMatrix <- data.frame(cbind(as.data.frame(gcmMatrix), as.numeric(stat)))
            varNames <- cbind((varNames), paste(var, "_", mth, sep=""))
            
            
            cat(" .. done!\n")
            
          }
        }  
        
        colnames(gcmMatrix) <- varNames
        gcmMatrix <- cbind(as.data.frame(gcmList), gcmMatrix)
        colnames(gcmMatrix)[1] <- "model"
        
        cat("\n .> Writting stats csv file\n")
        
        write.csv(gcmMatrix, csvStats, row.names=F)
        
        cat("Ensemble over: ", rcp, " ", ens, " ", paste(staYear, "_", endYear, " ", basePer, sep="")," .. done!\n\n")
        
      } else {
        
        cat("Ensemble over: ", rcp, " ", ens, " ", paste(staYear, "_", endYear, " ", basePer, sep="")," .. done!\n\n")
        
      }
      
    }
    
    cat("GCM Ensemble Anomalies Process Done!")
  }
}


############################################################################################
# Description: Script to create images of the differences of ensembles anomalies of GCM data
############################################################################################
GCMEnsembleAnomDiff <- function(baseDir="T:/gcm/cmip5/raw/monthly", ens="r1i1p1") {
  
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  for (rcp in rcpList){
    
    # List of variables and months
    varList <- c("prec", "tmax", "tmin")
    mthList <- c(1:12)
    
    periodList <- c("2020", "2040", "2060")
    for (period in periodList) {
      
      # Define start and end year
      staYear <- as.integer(period)
      endYear <- as.integer(period) + 29
      
      ensDir1975s <- paste(baseDir, "/ensemble_anomalies", "/1975s/", rcp, "/", staYear, "_", endYear, sep="")
      ensDir1985s <- paste(baseDir, "/ensemble_anomalies", "/1985s/", rcp, "/", staYear, "_", endYear, sep="")
      endDirDiff <- paste(baseDir, "/ensemble_anomalies/diff/", rcp, "/", staYear, "_", endYear, sep="")
      
      if (!file.exists(endDirDiff)) {
        dir.create(paste(baseDir, "/ensemble_anomalies", "/diff", sep=""))
        dir.create(paste(baseDir, "/ensemble_anomalies", "/diff/", rcp, sep=""))
        dir.create(endDirDiff)
      }
      
      cat("Ensemble Difference over: ", rcp, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
      
      for (var in varList){
        
        for (mth in mthList){
          
          if (!file.exists(paste(endDirDiff, "/", var, "_", mth, ".jpg", sep=""))){
            
            cat(" .> Calculating..  ", paste("\t ", var, "_", mth, sep=""))
            
            asc1975 <- raster(paste(ensDir1975s, "/", var, "_", mth, ".asc", sep=""))
            asc1985 <- raster(paste(ensDir1985s, "/", var, "_", mth, ".asc", sep=""))
            
            diffAsc <- asc1975 - asc1985
            
            jpeg(paste(endDirDiff, "/", var, "_", mth, ".jpg", sep=""))
            plot(diffAsc)
            dev.off()
            
            # diffAsc <- writeRaster(diffAsc, paste(endDirDiff, "/", var, "_", mth, ".asc", sep=""), format='ascii', overwrite=FALSE)
            
            cat(" .. done!\n")
            
          } else { cat(" .> Calculating..  ", paste("\t ", var, "_", mth, " .. done!\n", sep="")) }    
        }
      }
    }
    
    cat("GCM Ensemble Differences Anomalies Process Done!")
  }
}  



############################################################################################
# Description: Script to create images from GCM data anomalies and future
############################################################################################
GCMVerification <- function(baseDir="T:/data/gcm/cmip5/raw/monthly", ens="r1i1p1", imageDir="T:/gcm/cmip5/baseinfo/inventory") {
  
  baseDir <- "T:/gcm/cmip5/raw/monthly"
  ens <- "r1i1p1"
  imageDir <- "T:/gcm/cmip5/baseinfo/inventory"
  
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  periodList <- c("2020_2049", "2040_2069", "2060_2089", "2070_2099")
  
  for (rcp in rcpList) {
    
    futDir <- paste(baseDir, "/", rcp, sep="")
    
    gcmList <- list.dirs(futDir, recursive = FALSE, full.names = FALSE)
    
    for (gcm in gcmList) {
      
      gcm <- basename(gcm)
      
      for (period in periodList) {
      
        futAnomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1975s/", period, sep="")
        if (file.exists(futAnomDir)){
        
          outImageDir <- paste(imageDir, "/", rcp, "/", gcm, "/", period, sep="")
          if (!file.exists(outImageDir)) {dir.create(outImageDir, recursive = TRUE)}
  
          cat(paste("Processing ", rcp, " ", period, " ", gcm, "\n"))
      
          ascList <- list.files(futAnomDir, pattern="*.asc")
          setwd(futAnomDir)
                
          for (asc in ascList) {
            
            cat(paste("\t..creating an image for ", asc),"\n")
            
            # Creating an image per timeslice and model
            paste(unlist(strsplit(asc, ".", fixed=T))[1], ".asc", sep="")
            
            if (!file.exists(paste(outImageDir, "/", unlist(strsplit(asc, ".", fixed=T))[1], ".jpg", sep=""))){
              rs <- raster(asc)
              jpeg(paste(outImageDir, "/", unlist(strsplit(asc, ".", fixed=T))[1], ".jpg", sep=""))
              plot(rs)
              dev.off()
            }
          }
        }
      }
      
    }
  }
  setwd("D:/CIAT/_tools/dapa-climate-change/IPCC-CMIP5")
  return("GCM Verification done!")
}
