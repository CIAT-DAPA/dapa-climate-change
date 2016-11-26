### Clipping GCM Historical information
### C. Navarro - H. Achicanoy
### CIAT, 2016

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### Processing GCMs information
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

### 1.1 GCM extract daily information process for historical data (Done)
gcmDailyProcess <- function(){
  
  require(raster)
  require(ncdf)
  require(ncdf4)
  require(rgdal)
  
  # Change according to period of analysis
  gcmDir <- "/mnt/data_cluster_2/gcm/cmip5/raw/daily/historical"
  # Create folder to save raw information
  dirout <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_raw_res"
  # Create folder to save resampled information
  diroutcut <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat"
  
  # List of GCMs to extract climatic information
  gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")
  # List of climatic variables to be extracted
  varlist <- c("tasmax", "tasmin", "pr", "rsds")
  # List of months
  mthList <- c(paste(0, c(1:9), sep=""), paste(c(10:12)))
  # List of metrics to be calculated (average and standar deviation)
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  # Numbers of days per month
  ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  # Extent of Kenya raster
  bbox <- extent(33.9, 41.9, -4.700001, 5.449999)
  
  ## Loop through list of GCMs
  for(gcm in gcmList)
  {
    # Create folder for period to analyse - raw information
    diroutgcmhis <- paste(dirout, "/", basename(gcm), "/1971_2000", sep="")
    # Create folder for period to analyse - resampled information
    diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/1971_2000", sep="")
    
    cat(" Cutting:", "Historical ", basename(gcm), "\n")
    
    if (!dir.exists(diroutgcmhis)) {dir.create(diroutgcmhis, recursive=T)}
    if (!dir.exists(paste(diroutgcmhis, "/by-month", sep=""))) {dir.create(paste(diroutgcmhis, "/by-month", sep=""), recursive=T)}
    
    if (!dir.exists(diroutgcmhiscut)) {dir.create(diroutgcmhiscut, recursive=T)}
    if (!dir.exists(paste(diroutgcmhiscut, "/by-month", sep=""))) {dir.create(paste(diroutgcmhiscut, "/by-month", sep=""), recursive=T)}
    
    ## Loop through list of variables
    for(var in varlist)
    {
      if(!file.exists(paste(diroutgcmhis, "/", var, "_1971_2000_day_lat.nc", sep="")))
      {
        ncList <- list.files(path=paste(gcmDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
        if(!file.exists(paste(diroutgcmhis, "/", var, "_1971_2000_day.nc", sep="")))
        {
          system(paste("cdo seldate,1971-01-01,2000-12-31 ", ncList[1], " ", diroutgcmhis, "/", var, "_1971_2000_day.nc", sep=""))
        }
        system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", diroutgcmhis, "/", var, "_1971_2000_day.nc ", diroutgcmhis, "/", var, "_1971_2000_day_lat.nc",sep=""))
      }
      if(!file.exists(paste(diroutgcmhis, "/by-month/", var, "_2000_12.nc", sep="")))
      {
        system(paste("cdo splityear ", diroutgcmhis, "/", var, "_1971_2000_day_lat.nc ", diroutgcmhis, "/by-month/", var, "_", sep=""))
        for(yr in 1971:2000)
        {
          system(paste("cdo splitmon ", diroutgcmhis, "/by-month/", var, "_", yr, ".nc ", diroutgcmhis, "/by-month/", var, "_", yr, "_", sep=""))
          file.remove(paste(diroutgcmhis, "/by-month/", var, "_", yr, ".nc", sep=""))
        }
      }
    }
  }
}
gcmDailyProcess()

### 2.1 GCM extract daily information process for future data (Done)
gcmDailyFutureProcess <- function(rcp='rcp26', period='2021_2045'){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  # Change according to period of analysis
  gcmDir <- paste("/mnt/data_cluster_2/gcm/cmip5/raw/daily/", rcp, sep="")
  # Create folder to save raw information
  dirout <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_raw_res"
  # Create folder to save resampled information
  diroutcut <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat"
  
  if(period=='2021_2045')
  {
    inYr <- 2021; endYr <- 2045
  } else {
    if(period=='2041_2065')
    {
      inYr <- 2041; endYr <- 2065
    }
  }
  
  # List of GCMs to extract climatic information
  gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")
  # List of climatic variables to be extracted
  varlist <- c("tasmax", "tasmin", "pr", "rsds")
  # List of months
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  # List of metrics to be calculated (average and standar deviation)
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  # Numbers of days per month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  # Extent of Kenya raster
  bbox <- extent(33.9, 41.9, -4.700001, 5.449999)
  
  ## Loop through list of GCMs
  for(gcm in gcmList)
  {
    # Create folders according to period
    if(period=='2021_2045'){
      # Create folder for period to analyse - raw information
      diroutgcmhis <- paste(dirout, "/", basename(gcm), "/2021_2045/", basename(rcp), sep="")
      # Create folder for period to analyse - resampled information
      diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/2021_2045/", basename(rcp), sep="")
    } else {
      if(period=='2041_2065'){
        # Create folder for period to analyse - raw information
        diroutgcmhis <- paste(dirout, "/", basename(gcm), "/2041_2065/", basename(rcp), sep="")
        # Create folder for period to analyse - resampled information
        diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/2041_2065/", basename(rcp), sep="")
      }
    }
    
    cat(" Cutting:", "Future ", basename(gcm), "\n")
    
    if (!file.exists(diroutgcmhis)) {dir.create(diroutgcmhis, recursive=T)}
    if (!file.exists(paste(diroutgcmhis, "/by-month", sep=""))) {dir.create(paste(diroutgcmhis, "/by-month", sep=""), recursive=T)}
    
    if (!file.exists(diroutgcmhiscut)) {dir.create(diroutgcmhiscut, recursive=T)}
    if (!file.exists(paste(diroutgcmhiscut, "/by-month", sep=""))) {dir.create(paste(diroutgcmhiscut, "/by-month", sep=""), recursive=T)}
    
    ## Loop through list of variables
    for(var in varlist)
    {
      
      if(period=='2021_2045')
      {
        if(!file.exists(paste(diroutgcmhis, "/", var, "_2021_2045_day_lat.nc", sep="")))
        {
          ncList <- list.files(path=paste(gcmDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
          if(!file.exists(paste(diroutgcmhis, "/", var, "_2021_2045_day.nc", sep="")))
          {
            system(paste("cdo seldate,2021-01-01,2045-12-31 ", ncList[1], " ", diroutgcmhis, "/", var, "_2021_2045_day.nc", sep=""))
          }
          system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", diroutgcmhis, "/", var, "_2021_2045_day.nc ", diroutgcmhis, "/", var, "_2021_2045_day_lat.nc",sep=""))
        }
        if(!file.exists(paste(diroutgcmhis, "/by-month/", var, "_2045_12.nc", sep="")))
        {
          system(paste("cdo splityear ", diroutgcmhis, "/", var, "_2021_2045_day_lat.nc ", diroutgcmhis, "/by-month/", var, "_", sep=""))
          for(yr in 2021:2045)
          {
            system(paste("cdo splitmon ", diroutgcmhis, "/by-month/", var, "_", yr, ".nc ", diroutgcmhis, "/by-month/", var, "_", yr, "_", sep=""))
            file.remove(paste(diroutgcmhis, "/by-month/", var, "_", yr, ".nc", sep=""))
          }
        }
      } else {
        if(period=='2041_2065')
        {
          if(!file.exists(paste(diroutgcmhis, "/", var, "_2041_2065_day_lat.nc", sep="")))
          {
            ncList <- list.files(path=paste(gcmDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
            if(!file.exists(paste(diroutgcmhis, "/", var, "_2041_2065_day.nc", sep="")))
            {
              system(paste("cdo seldate,2041-01-01,2065-12-31 ", ncList[1], " ", diroutgcmhis, "/", var, "_2041_2065_day.nc", sep=""))
            }
            system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", diroutgcmhis, "/", var, "_2041_2065_day.nc ", diroutgcmhis, "/", var, "_2041_2065_day_lat.nc",sep=""))
          }
          if(!file.exists(paste(diroutgcmhis, "/by-month/", var, "_2065_12.nc", sep="")))
          {
            system(paste("cdo splityear ", diroutgcmhis, "/", var, "_2041_2065_day_lat.nc ", diroutgcmhis, "/by-month/", var, "_", sep=""))
            for(yr in 2041:2065)
            {
              system(paste("cdo splitmon ", diroutgcmhis, "/by-month/", var, "_", yr, ".nc ", diroutgcmhis, "/by-month/", var, "_", yr, "_", sep=""))
              file.remove(paste(diroutgcmhis, "/by-month/", var, "_", yr, ".nc", sep=""))
            }
          }
        }
      }
    }
  }
}
periodList <- c("2021_2045", "2041_2065")
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
lapply(1:length(periodList), function(i){
  cat('Processing period:', periodList[[i]],'\n')
  library(parallel)
  mclapply(1:length(rcpList), function(j){
    cat('Processing RCP:', rcpList[[j]],'\n')
    gcmDailyFutureProcess(rcp=rcpList[[j]], period=periodList[[i]])
    return(cat('Process done for RCP:', rcpList[[j]],'\n'))
  }, mc.cores=4)
  return(cat('Process done for period:', periodList[[i]],'\n'))
})

### 1.2 GCM resample process for historical data (Running)
gcmDailyResample <- function(){
  
  require(raster)
  require(ncdf4)
  require(rgdal)
  
  dirout <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_raw_res"
  diroutcut <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat"
  inYr <- 1971
  endYr <- 2000
  
  # gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")
  gcmList <- c("csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")
  varlist <- c("tasmax", "tasmin", "pr", "rsds")
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  bbox <- extent(33.9, 41.9, -4.700001, 5.449999)
  rows=203
  cols=160
  
  ## Reggrid GCM Historical
  for(gcm in gcmList){
    for(var in varlist){
      
      if(var == "tasmax"){varmod <- "tmax"}
      if(var == "tasmin"){varmod <- "tmin"}
      if(var == "pr"){varmod <- "prec"}
      if(var == "rsds"){varmod <- "rsds"}
      
      diroutgcmhis <- paste(dirout, "/", basename(gcm), "/", inYr, "_", endYr, sep="")
      diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/", inYr, "_", endYr, sep="")
      
      for(mth in mthList){
        
        mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
        ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
        
        if(!file.exists(paste(diroutgcmhiscut, "/", varmod, "_", inYr, "_", endYr, "_", mth, "_std.nc", sep=""))){
          
          for(yr in inYr:endYr){
            
            for(i in 1:31){
              assign(paste("d", i, sep=""), raster())
            }
            
            cat(" Resample daily: historical", varmod, "_", yr, " ", mth, "\n")
            
            if(!file.exists(paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))){
              
              f <- paste(diroutgcmhis, "/by-month/", var, "_", yr, "_", mth, ".nc", sep="")
              rx <- raster(f)
              
              for(i in 1:rx@file@nbands){
                assign(paste("d", i, sep=""), raster(f, band=i))
              }
              
              dList <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31)
              
              dayNcStack <- stack(dList[1:ndayMth])
              dayNcStackRes <- resample(dayNcStack, raster(nrows=rows, ncols=cols, xmn=bbox@xmin, xmx=bbox@xmax, ymn=bbox@ymin, ymx=bbox@ymax, resolution=0.05), method='bilinear')
              
              # dayNcStackRes <- resample(dayNcStack, raster(nrows=rows, ncols=cols, xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax, resolution=0.05), method='bilinear')
              # xmin(dayNcStackRes) <- xmin(dayNcStackRes)-360
              # xmax(dayNcStackRes) <- xmax(dayNcStackRes)-360
              
              if(varmod == "tmax"){dayNcStackRes <- dayNcStackRes - 273.15}
              if(varmod == "tmin"){dayNcStackRes <- dayNcStackRes - 273.15}
              if(varmod == "prec"){dayNcStackRes <- dayNcStackRes * 86400}
              
              dayNcStackRes <- writeRaster(dayNcStackRes, paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""), format="CDF", overwrite=T)
              
            }
            
            cat(" Calculating avg and std daily: historical", basename(gcm), " ", varmod, "_", yr, " ", mth, "\n")
            system(paste("cdo dayavg ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
            system(paste("cdo daystd ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
            
            # system(paste("D:/jetarapues/cdo/cdo.exe -s dayavg ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
            # system(paste("D:/jetarapues/cdo/cdo.exe -s daystd ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
            
          }
          
          avgNcList <- paste(diroutgcmhiscut, "/by-month/", varmod, "_", inYr:endYr, "_", mth, "_avg.nc", sep="")
          avgNcStack <- mean(stack(avgNcList))
          avgNcStack <- writeRaster(avgNcStack, paste(diroutgcmhiscut, "/", varmod, "_1971_2000_", mth, "_avg.nc", sep=""), format="CDF", overwrite=T)
          
          stdNcList <- paste(diroutgcmhiscut, "/by-month/", varmod, "_", inYr:endYr, "_", mth, "_std.nc", sep="")
          stdNcStack <- mean(stack(stdNcList))
          stdNcStack <- writeRaster(stdNcStack, paste(diroutgcmhiscut, "/", varmod, "_1971_2000_", mth, "_std.nc", sep=""), format="CDF", overwrite=T)
          
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
}
gcmDailyResample()

### 2.2 GCM resample process for future data (Ready to run)
gcmDailyFutureResample <- function(rcp='rcp26', period='2021_2045'){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  dirout <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_raw_res"
  diroutcut <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat"
  if(period=='2021_2045')
  {
    inYr <- 2021; endYr <- 2045
  } else {
    if(period=='2041_2065')
    {
      inYr <- 2041; endYr <- 2065
    }
  }
  
  gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")
  varlist <- c("tasmax", "tasmin", "pr", "rsds")
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  bbox <- extent(33.9, 41.9, -4.700001, 5.449999)
  rows=203
  cols=160
  
  ## Reggrid GCM future data
  for(gcm in gcmList){
    for(var in varlist){
      
      if (var == "tasmax"){varmod <- "tmax"}
      if (var == "tasmin"){varmod <- "tmin"}
      if (var == "pr"){varmod <- "prec"}
      if (var == "rsds"){varmod <- "rsds"}
      
      # diroutgcmhis <- paste(dirout, "/", basename(gcm), "/", inYr, "_", endYr, sep="")
      # diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/", inYr, "_", endYr, sep="")
      
      if(period=='2021_2045'){
        diroutgcmhis <- paste(dirout, "/", basename(gcm), "/2021_2045/", basename(rcp), sep="")
        diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/2021_2045/", basename(rcp), sep="")
      } else {
        if(period=='2041_2065'){
          diroutgcmhis <- paste(dirout, "/", basename(gcm), "/2041_2065/", basename(rcp), sep="")
          diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/2041_2065/", basename(rcp), sep="")
        }
      }
      
      for(mth in mthList){
        
        mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
        ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
        
        if(!file.exists(paste(diroutgcmhiscut, "/", varmod, "_", inYr, "_", endYr, "_", mth, "_std.nc", sep=""))){
          
          for(yr in inYr:endYr){
            
            for(i in 1:31){
              assign(paste("d", i, sep=""), raster())
            }
            
            cat(" Resample daily: future", varmod, "_", yr, " ", mth, "\n")
            
            if(!file.exists(paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))){
              
              f <- paste(diroutgcmhis, "/by-month/", var, "_", yr, "_", mth, ".nc", sep="")
              rx <- raster(f)
              
              for(i in 1:rx@file@nbands){
                assign(paste("d", i, sep=""), raster(f, band=i))
              }
              
              dList <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31)
              
              dayNcStack <- stack(dList[1:ndayMth])
              dayNcStackRes <- resample(dayNcStack, raster(nrows=rows, ncols=cols, xmn=bbox@xmin, xmx=bbox@xmax, ymn=bbox@ymin, ymx=bbox@ymax, resolution=0.05), method='bilinear')
              
              # dayNcStackRes <- resample(dayNcStack, raster(nrows=rows, ncols=cols, xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax, resolution=0.05), method='bilinear')
              # xmin(dayNcStackRes) <- xmin(dayNcStackRes)-360
              # xmax(dayNcStackRes) <- xmax(dayNcStackRes)-360
              
              if (varmod == "tmax"){dayNcStackRes <- dayNcStackRes - 273.15}
              if (varmod == "tmin"){dayNcStackRes <- dayNcStackRes - 273.15}
              if (varmod == "prec"){dayNcStackRes <- dayNcStackRes * 86400}
              
              dayNcStackRes <- writeRaster(dayNcStackRes, paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""), format="CDF", overwrite=T)
              
            }
            
            cat(" Calculating avg and std daily: historical", basename(gcm), " ", varmod, "_", yr, " ", mth, "\n")
            system(paste("cdo dayavg ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
            system(paste("cdo daystd ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
            
            # system(paste("D:/jetarapues/cdo/cdo.exe -s dayavg ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
            # system(paste("D:/jetarapues/cdo/cdo.exe -s daystd ", diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
            
          }
          
          avgNcList <- paste(diroutgcmhiscut, "/by-month/", varmod, "_", inYr:endYr, "_", mth, "_avg.nc", sep="")
          avgNcStack <- mean(stack(avgNcList))
          avgNcStack <- writeRaster(avgNcStack, paste(diroutgcmhiscut, "/", varmod, "_1971_2000_", mth, "_avg.nc", sep=""), format="CDF", overwrite=T)
          
          stdNcList <- paste(diroutgcmhiscut, "/by-month/", varmod, "_", inYr:endYr, "_", mth, "_std.nc", sep="")
          stdNcStack <- mean(stack(stdNcList))
          stdNcStack <- writeRaster(stdNcStack, paste(diroutgcmhiscut, "/", varmod, "_1971_2000_", mth, "_std.nc", sep=""), format="CDF", overwrite=T)
          
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
}
periodList <- c("2021_2045", "2041_2065")
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
lapply(1:length(periodList), function(i){
  cat('Processing period:', periodList[[i]],'\n')
  library(parallel)
  mclapply(1:length(rcpList), function(j){
    cat('Processing RCP:', rcpList[[j]],'\n')
    gcmDailyFutureResample(rcp=rcpList[[j]], period=periodList[[i]])
    return(cat('Process done for RCP:', rcpList[[j]],'\n'))
  }, mc.cores=4)
  return(cat('Process done for period:', periodList[[i]],'\n'))
})
