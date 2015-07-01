######################################################################################################################
#### Author : Carlos Navarro
#### Date   : May 2015
#### Contact: c.e.navarro@cgiar.org
######################################################################################################################

######################################################################################################################
########################################### BIAS CORRECTION METHODOLOGIES ############################################
######################################################################################################################

######################################################################################################################
### This script is to calibrate daily observations (Reanalysis) and GCM projections using 'nudging' (bias correction) 
### approache. This types of approach could be more widely adopted for assessing
### calibration methodologies for crop modelling. 
### Reference : Hawkins E, Osborne TM, Ho CK, Challinor AJ. 2013. Calibration and bias correction of climate 
### projections for crop modelling: An idealised case study over Europe. Agricultural and Forest Meteorology 170:19-31
######################################################################################################################

## 1- Average values from Observations to create climatology
OBSAverage <- function(var="tmax",dataset="wfd", ts="1970_2000", bbox=extent(-89.40,-82.52,8,16.04), reg_suf="cam", dirbase="/mnt/data_cluster_4/observed/gridded_products/wfd/daily/nc-files", oDir="/home/cnavarro/Request_oovalle") {
  
  yi <- substr(ts, 1, 4)
  yf <- substr(ts, 6, 9)
  
  bbox <- extent(bbox)
  
  if(dataset == "wfd"){period <- "1950_2001"}
  
  oDir <- paste0(oDir, "/obs")
  if (!file.exists(dirout)) {dir.create(dirout)}
  
  obs <- paste0(dirbase, "/", var, "_daily_ts_", dataset, "_", period, ".nc")
  obsTs <- paste0(oDir, "/", var, "_daily_ts_", dataset, "_", period, ".nc")
  obsTsReg <- paste0(oDir, "/", var, "_daily_ts_", dataset, "_", period, "_", reg_suf)
  
  ## Select dates and cut region
  
  if (!file.exists(paste0(obsTsReg, "_std.nc"))) {
    
    system(paste0("cdo seldate,", yi, "-01-01,", yf, "-12-31 ", obs, " ", obsTs))
    system(paste0("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", obs, " ", obsTsReg, ".nc"))
    system(paste0("cdo ymonavg ", obsTsReg, ".nc", " ",  obsTsReg, "_avg.nc"))
    system(paste0("cdo ymonstd ", obsTsReg, ".nc", " ",  obsTsReg, "_std.nc"))
    
    file.remove(obsTs)
    file.remove(paste0(obsTsReg, ".nc"))
    
  }
  
  
  
}

## 2- Cut GCM Daily
GCMDailyResample <- function(rcp="historical", ts="1970_2000", bbox=extent(-89.40,-82.52,8,16.04), reg_suf="cam", resolution=0.04, gcm="bcc_csm1_1", gcmDir="T:/gcm/cmip5/raw/daily",  dirout="D:/cenavarro/Request_oovalle"){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  diroutraw <- paste0(dirout, "/gcm_raw")
  diroutres <- paste0(dirout, "/gcm_res_", reg_suf)
  
  bbox <- extent(bbox)
  
  varlist <- c("pr", "rsds", "sfcWind", "tas")
  #   varlist <- c("tasmax", "tasmin")  
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  yi <- substr(ts, 1, 4)
  yf <- substr(ts, 6, 9)
  
  
  # Create output directories
  oDir <- paste(diroutraw, "/", basename(gcm), "/", ts, sep="")
  oDirRes <- paste(diroutres, "/", basename(gcm), "/", ts, sep="")
  if (!file.exists(paste(oDir, "/by-month", sep=""))) {dir.create(paste(oDir, "/by-month", sep=""), recursive=T)}
  if (!file.exists(paste(oDirRes, "/by-month", sep=""))) {dir.create(paste(oDirRes, "/by-month", sep=""), recursive=T)}
  
  ####### Process GCM  #######
  
  cat(" Cutting : ", "historical ", basename(gcm), " \n")
  
  ## Select dates and cut region
  for (var in varlist){
    
    if (!file.exists(paste(oDir, "/", var, "_", ts, "_day_", reg_suf, ".nc", sep=""))) {
      
      ncList <- list.files(path=paste(gcmDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
      
      if (!file.exists(paste(oDir, "/", var, "_", ts, "_day.nc", sep=""))) {
        system(paste("cdo seldate,", yi, "-01-01,", yf, "-12-31 ", ncList[1], " ", oDir, "/", var, "_", ts, "_day.nc", sep=""))
      }
      
      system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", oDir, "/", var, "_", ts, "_day.nc ", oDir, "/", var, "_", ts, "_day_", reg_suf, ".nc",sep=""))
    }
    
    
    ## Split in years and then in months
    if (!file.exists(paste(oDir, "/by-month/", var, "_", yf, "_12.nc", sep=""))) {
      
      system(paste("cdo splityear ", oDir, "/", var, "_", ts, "_day_", reg_suf, ".nc ", oDir, "/by-month/", var, "_", sep=""))
      
      for (yr in yi:yf){
        
        system(paste("cdo splitmon ", oDir, "/by-month/", var, "_", yr, ".nc ", oDir, "/by-month/", var, "_", yr, "_", sep=""))
        
        file.remove(paste(oDir, "/by-month/", var, "_", yr, ".nc", sep=""))
        
      }
      
    }
  }
  
  ####### Resample GCMs  ######
  
  for (var in varlist){
    
    if (var == "tasmax"){varmod <- "tmax"}
    if (var == "tasmin"){varmod <- "tmin"}
    if (var == "pr"){varmod <- "prec"}
    if (var == "rsds"){varmod <- "rsds"}
    if (var == "sfcWind"){varmod <- "wsmean"}
    if (var == "tas"){varmod <- "tmean"}
    
    for (mth in mthList) {
      
      mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
      ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
      
      if (!file.exists(paste(oDirRes, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""))) {
        
        for (yr in yi:yf){
          
          cat(" Resample daily: historical ", varmod, "_", yr, " ", mth, "\n")
          
          if (!file.exists(paste(oDirRes, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))) {
            
            rs <- stack(paste(oDir, "/by-month/", var, "_", yr, "_", mth, ".nc", sep=""))
            dayNcStackRes <- resample(rs, raster(nrows=nrow(raster(bbox, res=resolution)), ncols=ncol(raster(bbox, res=resolution)), xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax), method='bilinear')
            
            xmin(dayNcStackRes) <- xmin(dayNcStackRes)-360
            xmax(dayNcStackRes) <- xmax(dayNcStackRes)-360
            
            if (varmod == "tmax"){dayNcStackRes <- dayNcStackRes - 273.15}
            if (varmod == "tmin"){dayNcStackRes <- dayNcStackRes - 273.15}
            if (varmod == "prec"){dayNcStackRes <- dayNcStackRes * 86400}
            
            dayNcStackRes <- writeRaster(dayNcStackRes, paste(oDirRes, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""), format="CDF", overwrite=T)
            
          }
          
          cat(" Calculating avg and std daily: historical ", basename(gcm), " ", varmod, "_", yr, " ", mth, "\n")
          system(paste("cdo -s dayavg ", oDirRes, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  oDirRes, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
          system(paste("cdo -s daystd ", oDirRes, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  oDirRes, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
          
        }
        
        if (!file.exists(paste(oDirRes, "/", varmod, "_", ts, "_", mth, "_avg.nc", sep=""))) {
          avgNcList <- paste(oDirRes, "/by-month/", varmod, "_", yi:yf, "_", mth, "_avg.nc", sep="")
          avgNcStack <- mean(stack(avgNcList))
          avgNcStack <- writeRaster(avgNcStack, paste(oDirRes, "/", varmod, "_", ts, "_", mth, "_avg.nc", sep=""), format="CDF", overwrite=T)
        }
        
        if (!file.exists(paste(oDirRes, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""))) {
          stdNcList <- paste(oDirRes, "/by-month/", varmod, "_", yi:yf, "_", mth, "_std.nc", sep="")
          stdNcStack <- mean(stack(stdNcList))
          stdNcStack <- writeRaster(stdNcStack, paste(oDirRes, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""), format="CDF", overwrite=T)
        }
        
        for (nc in avgNcList){
          file.remove(paste(nc))
        }
        
        for (nc in stdNcList){
          file.remove(paste(nc))
        }
        
      }      
    }
  }  
  
}

## 4-  BC Calculation
GCM_BC_DailyCalcs <- function(rcp="historical", ts="1970_2000", bbox=extent(-89.40,-82.52,8,16.04), reg_suf="cam", resolution=0.04, dataset="wfd", dirobs="S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat", dirgcm="W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat", dirout="W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"){
  
  require(raster)
  require(ncdf)
  
  maskobs <- raster(paste("S:/observed/gridded_products/wfd/raw/mask_wfd_lat.nc"))
  
  extlat <- extent(-120,-30,-56,33)
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  #   varlist <- c("prec", "tmax", "tmin", "rsds")
  #   varlist <- c("tmax", "tmin")
  
  if (!file.exists(dirout)) {dir.create(dirout)}
  gcmList <- list.dirs(dirgcm, recursive = FALSE, full.names = FALSE)
  
  yi <- substr(ts, 1, 4)
  yf <- substr(ts, 6, 9)
  
  for (gcm in gcmList[startModel:endModel]){
    
    dir.out.bc <- paste(dirout, "/", basename(gcm), "/", ts, "/by_month", sep="")
    if (!file.exists(dir.out.bc)) {dir.create(dir.out.bc, recursive=T)}
    
    for (var in varlist){
      
      if (!file.exists(paste(dir.out.bc, "/", var, "_", yf, "_12.nc", sep=""))){
        
        obs.his.avg <- stack(paste0(dirobs, "/", var, "_daily_ts_", dataset, "_", ts, "_", reg_suf, "_avg.nc"))
        obs.his.std <- stack(paste0(dirobs, "/", var, "_daily_ts_", dataset, "_", ts, "_", reg_suf, "_std.nc"))
        
        for (mth in monthList){
          
          gcm.avg <- raster(paste(dirgcm, "/", basename(gcm), "/", ts, "/", var, "_", ts, "_", mth, "_avg.nc", sep=""))
          gcm.std <- raster(paste(dirgcm, "/", basename(gcm), "/", ts, "/", var, "_", ts, "_", mth, "_std.nc", sep=""))
          
          gcm.avg <- mask(gcm.avg, maskobsLat)
          gcm.std <- mask(gcm.std, maskobsLat)
          
          ## Convert units
          if (var == "prec"){
            obs.his.avg <- obs.his.avg * 86400
            obs.his.std <- obs.his.std * 86400 
          } 
          
          if (var == "tmin"){
            obs.his.avg <- obs.his.avg - 273.15
          }
          
          if (var == "tmax"){
            obs.his.avg <- obs.his.avg - 273.15
          }
          
          
          for (yr in yi:yf){
            
            gcm.bc.out <- paste(dir.out.bc, "/", var, "_", yr, "_", mth, ".nc", sep="")
            
            if (!file.exists(gcm.his.bc.out)) {
              
              cat(" BC Calcs: ", basename(gcm), " ", var, " ", yr, " ", mth, " ")
              
              gcm.day.stack <- stack(paste0(dirgcm, "/", basename(gcm), "/", ts, "/by-month/", var, "_", yr, "_", mth, ".nc"))
              gcm.day.stack <- mask(gcm.his.day.stack, maskobsLat)
              
              if (var == "prec" || var == "rsds"){
                gcm.bc <- gcm.day.stack *  (1 + ((obs.his.avg[mth] - gcm.avg) / obs.his.avg[mth]))
              } else {
                gcm.bc <- obs.his.avg[mth] + ( (obs.his.std[mth] / gcm.std) * (gcm.day.stack - gcm.avg) )  
              }
              
              
              gcm.bc <- writeRaster(gcm.bc, paste(dir.out.bc, "/", var, "_", yr, "_", mth, "_temp.nc", sep=""), format="CDF", overwrite=T)       
              system(paste("cdo -settaxis,", yr, "-", mth, "-01,00:00:00,1day ", dir.out.bc, "/", var, "_", yr, "_", mth, "_temp.nc ", gcm.bc.out, sep=""))
              
              
              file.remove(paste(dir.out.bc, "/", var, "_", yr, "_", mth, "_temp.nc", sep=""))
              
              cat(" Done! \n")
              
            } else {cat(" BC Calcs: ", basename(gcm), " ", var, " ", yr, " ", mth, " Done! \n")}
            
          }
        }
      }
    }
  }
}
