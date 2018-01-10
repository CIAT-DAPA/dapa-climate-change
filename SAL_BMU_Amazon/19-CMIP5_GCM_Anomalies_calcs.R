#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
#-----------------------------------------------------------------------

require(maptools)
require(raster)
require(rgdal)
require(sp)
require(ncdf4)
# rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")


rcp='historical'
rcpf ='rcp85'
staYear=1981
endYear=2010
baseDir="T:/gcm/cmip5/raw/monthly"
scrDir="D:/_scripts/dapa-climate-change/IPCC-CMIP5/data"
otp <- GCMAverageHist(rcp, rcpf, staYear, endYear, baseDir, scrDir)

rcp <- "rcp45"
baseDir="T:/gcm/cmip5/raw/monthly"
ens="r1i1p1"
basePer <- "1981_2010"
oDir="Z:/DATA/WP2/03_Future_data/anomalies"
bbox <- raster(extent(-80, -66, -16, 5), res=2.5/60) #2.5 min res
otp <- GCMAnomalies(rcp, baseDir, ens, basePer, oDir, bbox)


rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
baseDir <- "Z:/DATA/WP2/03_Future_data/anomalies_2_5min"
perList <- c("2020_2049", "2040_2069", "2070_2099")
for (rcp in rcpList){
  otp <- GCMAnomaliesEns(rcp, baseDir, perList)   
}


#####################################################################################################
# Description: This function is to calculate the averaging surfaces of the CMIP5 monhtly climate data
#####################################################################################################

GCMAverageHist <- function(rcp='historical', rcpf ='rcp26', staYear=1981, endYear=2010, baseDir="T:/gcm/cmip5/raw/monthly", scrDir="D:/_scripts/dapa-climate-change/IPCC-CMIP5/data") {
  
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
        
        if (gcm %in% c("bcc_csm1_1", "bcc_csm1_1_m", "cesm1_cam5", "csiro_mk3_6_0", "fio_esm", 
                       "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_r", "ipsl_cm5a_lr", 
                       "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", 
                       "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")) {
          
          
          ens <- paste(as.matrix(gcmStats)[i,3])
          
          # Path of each ensemble
          ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
          
          # Directory with monthly splitted files
          mthDir <- paste(ensDir, "/monthly-files", sep="")
          mthfDir <- paste(baseDir, "/", rcpf, "/", gcm, "/", ens, "/monthly-files", sep="")
          
          # Create output average directory
          avgDir <- paste(ensDir, "/average", sep="")
          if (!file.exists(avgDir)) {dir.create(avgDir)}
          
          # Period list for historical and future pathways
          cat("\nAverage over: ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
          
          if (gcm != "lasg_fgoals_g2" && gcm != "gfdl_esm2g" && gcm != "gfdl_esm2m"){
            
            if (ens == "r1i1p1"){
              
              # Loop around variables
              for (var in varList) {
                
                # Loop around months
                for (mth in monthList) {
                  
                  if (file.exists(paste(mthfDir, "/2015/", var, "_", mth, ".nc", sep=""))){
                    
                    oDir <- paste(avgDir, "/", staYear, "_", endYear, "_", rcpf, sep="")
                    if (!file.exists(oDir)) {dir.create(oDir)}
                    
                    # Define month without 0 in one digit number
                    mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
                    outNcAvg <- paste(oDir, "/", var, "_", mthMod, ".nc", sep="")
                    
                    if (!file.exists(outNcAvg)){
                      
                      yrLs <- list.dirs(mthDir, full.names = FALSE, recursive = TRUE)
                      
                      if (as.integer(yrLs[length(yrLs)]) >= endYear){
                        
                        # List of NetCDF files by month for all 30yr period
                        mthNc <- lapply(paste(mthDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
                        
                      } else {
                        
                        mthNc <- paste(mthDir, "/", staYear:(yrLs[length(yrLs)]), "/", var, "_", mth, ".nc", sep="")
                        mthNc <- c(mthNc, paste(mthfDir, "/", (as.integer(yrLs[length(yrLs)]) + 1):endYear, "/", var, "_", mth, ".nc", sep=""))
                        mthNc <- lapply(mthNc, FUN=raster)
                        
                      }
                      
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
            
          }
          
        }

      }
      
    }
    
  }
  
  cat("GCM Average Process Done!")
  
}


GCMAverageFut <- function(rcp='historical', baseDir="T:/gcm/cmip5/raw/monthly", scrDir="D:/_scripts/dapa-climate-change/IPCC-CMIP5/data") {
  
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
        
        
        
        if (gcm %in% c("bcc_csm1_1", "bcc_csm1_1_m", "cesm1_cam5", "csiro_mk3_6_0", "fio_esm", 
                       "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_r", "ipsl_cm5a_lr", 
                       "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", 
                       "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")) {
          
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
            
            periodList <- c("2010", "2030", "2081")
            
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
          }
        }
        

      }
    }
    
  }
  
  cat("GCM Average Process Done!")
  
}


#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################

GCMAnomalies <- function(rcp='rcp26', baseDir="T:/gcm/cmip5/raw/monthly", ens="r1i1p1", basePer="1961_1990", oDir="D:/CIAT/ecu-hidroelectrica/03_future_climate_data/anomalies_cmip5", bbox="//nina/cenavarro/ecu-hidroelectrica/02_baseline/_region/alt-prj-ecu.asc.asc") {
  
  require(maptools)
  require(raster)
  require(rgdal)
  require(sp)
  require(ncdf4)
    
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
    
    
    if (gcm %in% c("bcc_csm1_1", "bcc_csm1_1_m", "cesm1_cam5", "csiro_mk3_6_0", "fio_esm", 
                   "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_r", "ipsl_cm5a_lr", 
                   "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", 
                   "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")) {
      
      if (gcm != "lasg_fgoals_g2" && gcm != "gfdl_esm2g" && gcm != "gfdl_esm2m"){ # Cells are not equally spaced
        
        # Path of each ensemble
        curEnsDir <- paste(curDir, "/", gcm, "/", ens, sep="")
        
        # Average directory
        curAvgDir <- paste(curEnsDir, "/average/", basePer, "_", rcp, sep="")
        
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
                
              } else if (basePer == "1981_2010") {
                
                #               anomDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1995s", sep="")
                #               anomPerDir <- paste(futDir, "/", gcm, "/", ens, "/anomalies_1995s/", staYear, "_", endYear, sep="")
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
      }
    }
    
    
  }
  cat("GCM Anomalies Process Done!")
}

GCMAnomaliesEns <- function(rcp='rcp26', baseDir="Z:/DATA/WP2/03_Future_data/anomalies_2_5min", perList=c("2020_2049", "2040_2069", "2070_2099")) {
  
  ### Author: Julián Ardila & Carlos Navarro c.e.navarro@cgiar.org
  ### Date: February 2016
  
  require(raster)
  require(ncdf)
  require(maptools)
  require(rgdal)
  
  varList <- c("prec", "tmin", "tmax")
  
  # Get a list of GCMs
  gcmList <- list.dirs(paste0(baseDir, "/", rcp), recursive = FALSE, full.names = FALSE)
  
  gcmList <- setdiff(gcmList, "ensemble")
  
  cat("Anomalies Ensemble over: ", rcp, "\n")
  
  for (period in perList) {
    
    oDirEns <- paste0(baseDir, "/", rcp, "/ensemble/", period)
    if (!file.exists(oDirEns)) {dir.create(oDirEns, recursive=T)}
    
    setwd(paste(baseDir, "/", rcp, sep=""))
    
    for (var in varList){
      
    if (!file.exists(paste(oDirEns, "/", var, "_12.tif", sep=""))){
        
        for (mth in 1:12){
          
          gcmStack <- stack(lapply(paste0(gcmList, "/", period, "/", var, "_", mth, ".nc"),FUN=raster))
          
          gcmMean <- mean(gcmStack)
          fun <- function(x) { sd(x) }
          gcmStd <- calc(gcmStack, fun)
          
#           gcmMean <- trunc(gcmMean)
#           gcmStd <- trunc(gcmStd)
          
          if (var == "prec"){
            gcmMean <- gcmMean * 100
            gcmStd <- gcmStd * 100
          } else {
            gcmMean <- gcmMean * 10
            gcmStd <- gcmStd * 10
          }
          
          gcmMean <- writeRaster(gcmMean, paste(oDirEns, "/", var, "_", mth, '.tif',sep=''), format="GTiff", overwrite=T, datatype='INT2S')
          gcmStd <- writeRaster(gcmStd, paste(oDirEns, "/", var, "_", mth, "_sd.tif", sep=""), format="GTiff", overwrite=T, datatype='INT2S')
        }
        
      }
    }
    
  }
  
  
  
  cat("Seasonal Calcs anomalies ensemble over: ", rcp, "\n")
  
  # List of seasons
  seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
  
  for (period in perList) {
    
    oDirEns <- paste0(baseDir, "/", rcp, "/ensemble/", period)
    
    for (var in varList){
      
      # Load averages files 
      iAvg <- stack(paste(oDirEns,'/', var, "_", 1:12, ".tif",sep=''))
      
      # Loop throught seasons
      for (i in 1:length(seasons)){
        
        if (!file.exists(paste(oDirEns,'/', var, "_", names(seasons[i]), '.tif',sep=''))){ 
          
          cat("Calcs ", var, names(seasons[i]), "\n")
          sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
          
          writeRaster(sAvg, paste(oDirEns,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
          
        }
        
      } 
      
    }
    
  }
  
}
  



