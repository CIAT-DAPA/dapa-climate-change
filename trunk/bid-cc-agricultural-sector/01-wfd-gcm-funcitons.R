# cd G:\_scripts\dapa-climate-change\bid-cc-agricultural-sector
# source("01-wfd-gcm-funcitons.R")
# GCMDailyProcessHistorical(1,4)

### 0 - Obtain lat, lon, alt & alt strm from wfd coordinates
WFDMask <- function(dirbase="S:/observed/gridded_products/wfd"){
  
  require(maptools)
  require(raster)
  
  latlon <- open.ncdf(paste(dirbase, "/raw/WFD-land-lat-long-z.nc", sep=""), write=FALSE)
  lon <- get.var.ncdf(latlon, "Longitude")
  lat <- get.var.ncdf(latlon, "Latitude")
  z <- get.var.ncdf(latlon, "Z")
  coords <- data.frame(lon, lat)
  close.ncdf(latlon)
  
  alt_30s <- "S:/observed/gridded_products/srtm/Altitude_30s/alt"
  alt <- extract(raster(alt_30s), coords)
  
  matrix <- cbind.data.frame(1:dim(coords)[1], coords[,2], coords[,1], alt, z)
  colnames(matrix) <- c("id", "lat", "lon", "alt", "alt_wfd")
  write.table(matrix, paste("D:/CIAT/Workspace/bid/lat-lon-z.txt", sep=""), row.names=F, sep="\t")
  
  extlat <- extent(-120,-30,-56,33)
  matrixlat <- matrix[which(matrix$lon>=extlat@xmin & matrix$lon<=extlat@xmax & matrix$lat>=extlat@ymin & matrix$lat<=extlat@ymax),]
  write.table(matrixlat, paste("D:/CIAT/Workspace/bid/lat-lon-z_lat.txt", sep=""), row.names=F, sep="\t")
  
  # plot(coords, add=T)
  # dim(coords)
  
  # data(wrld_simpl)
  # plot(wrld_simpl, add=T)
}

## 1- Prepare WFD Region and WCL climatology 0.5 deg
WCLClimatology <- function(){
    
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  
  dirbase <- "S:/observed/gridded_products/wfd"
  dirwclglobe <- "S:/observed/gridded_products/worldclim/Lat_30s"
  dirwcl <- "S:/observed/gridded_products/worldclim/Lat_30s"
  dirout <- "D:/CIAT/Workspace/bid"
  alt_30s <- "S:/observed/gridded_products/srtm/Altitude_30s/alt"
  
  maskWFD <- raster(paste(dirbase, "/raw/mask_wfd.nc", sep=""))
  maskWFDLat <- crop(maskWFD, extent(-120, -30, -56, 33))
  maskWFDLat <- writeRaster(maskWFDLat, paste(dirbase, "/raw/mask_wfd_lat.nc", sep=""), overwrite=TRUE)
  
  coords <- read.table(paste(dirout, "/lat-lon-z_lat.txt", sep=""), header=T)
  
  # wclExt <- setExtent(raster("D:/CIAT/climate_change/worldclim_2_5min/bio_1.asc"), extent(maskWFD), keepres=FALSE, snap=FALSE)
  varlist <- c("tmax", "tmin", "prec")
  
  if (!file.exists(paste(dirout, "/wcl_0_5_deg", sep=""))) {dir.create(paste(dirout, "/wcl_0_5_deg", sep=""), recursive=TRUE)}
  
  for (mth in 1:12) {
    
    for (var in varlist){
        
      cat(" Cut and resample wcl: ", var, "_", mth, " \n\n")
      
      if (!file.exists(paste(dirout, "/wcl_0_5_deg", "/", var, "_", mth, ".nc", sep=""))) {
        
        wcl <- raster(paste(dirwcl, "/", var, "_", mth, ".asc", sep=""))
        resWcl <- resample(wcl, maskWFDLat, method='bilinear')
        cutResWcl <- mask(resWcl, maskWFDLat)
        
        cutResWcl <- writeRaster(cutResWcl, paste(dirout, "/wcl_0_5_deg", "/", var, "_", mth, ".nc", sep=""))
      }
      
      
      if (paste(var, "_", mth, sep="") == "tmax_1") {
        
        resWcl <- raster(paste(dirout, "/wcl_0_5_deg", "/", var, "_", mth, ".nc", sep=""))
  #       coords <- coordinates(resWcl)
        
        alt <- extract(raster(alt_30s), coords[3:2])
        resWclVal <- extract(resWcl, coords[3:2])
  #       matrix <- cbind.data.frame(1:dim(coords)[1], coords[,2], coords[,1], alt, resWcl[])
  
        if (!var == "prec"){resWclVal <- resWclVal/10}
  
        matrix <- cbind.data.frame(coords[1:3], alt, resWclVal)
        matrix_names <- cbind(paste(var, "_", mth, sep=""))
        
      } else{
        
        resWcl <- raster(paste(dirout, "/wcl_0_5_deg", "/", var, "_", mth, ".nc", sep=""))
        resWclVal <- extract(resWcl, coords[3:2])
        
        #       matrix <- cbind.data.frame(matrix, resWcl[])      
        
        if (!var == "prec"){resWclVal <- resWclVal/10}
  
        matrix <- cbind.data.frame(matrix, resWclVal)
        matrix_names <- cbind(matrix_names, paste(var, "_", mth, sep=""))
        
      }
          
    }
  }
  
  colnames(matrix) <- c("id", "lat", "lon", "alt", matrix_names)
  
  cat(" Writting output tab delimited file\n\n")
  write.table(matrix, paste(dirout, "/wcl_0_5_deg_lat.txt", sep=""), row.names=F, sep="\t")
  write.table(na.omit(matrix), paste(dirout, "/wcl_0_5_deg_na_omit_lat.txt", sep=""), row.names=F, sep="\t")
}

## 2- Extract values from WFD nc values
WFDExtractValues <- function(dirbase="S:/observed/gridded_products/wfd", dirout=paste(dirbase, "/csv-files", sep="")){
    
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
    
  latlon_wfd <- open.ncdf(paste(dirbase, "/raw/WFD-land-lat-long-z.nc", sep=""), write=FALSE)
  lon <- get.var.ncdf(latlon_wfd, "Longitude")
  lat <- get.var.ncdf(latlon_wfd, "Latitude")
  
  varlist <- c("Rainf", "Tmax", "Tmin")
  
  for (var in varlist){
    
    if (var == "Rainf"){
      ncList <- list.files(paste(dirbase, "/raw/", var, "_daily_WFD_GPCC", sep=""), full.names = TRUE)
    } else {
      ncList <- list.files(paste(dirbase, "/raw/", var, "_daily_WFD", sep=""), full.names = TRUE)
    }
    
    diroutvar <- paste(dirout, "/", tolower(var), "-daily", sep="")
    if (!file.exists(diroutvar)) {dir.create(diroutvar)}
    
    for (nc in ncList){
      
      ## Estract year and month from nc name
      if (var == "Rainf"){
        
        year <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 5), 1, 4)
        mth <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 5), 5, 6)
        
      } else {
        
        year <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 4), 1, 4)
        mth <- substr(sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 4), 5, 6)
        
      }
      
      if (!file.exists(paste(diroutvar, "/", tolower(var), "-", year, mth, ".csv", sep=""))) {
  
        ## Extract values from nc file
        cat(paste("\n ->.  Reading in NetCDF file ", tolower(var), " ", year, mth))
        nc <- open.ncdf(nc, write=FALSE)
        val <- get.var.ncdf(nc, var) 
        days <- get.var.ncdf(nc, "tstep")
              
        ## Conver units from kg/m2/s to mm/day and K to deg C
        if (var == "Rainf"){
          val <- val * 86400
        } else {
          val <- val - 273.15
        }
        
        ## Array coordinates, values and dates
        datedays <- c(paste(year, mth, "0", days[1:9], sep=""), paste(year, mth, days[10:length(days)], sep=""))
        val.array <- cbind.data.frame(lon,lat, val)
        names(val.array) <- c("lat", "lon", datedays)
        
        ## Write csv output file
        cat(paste("\n ->.  Writing out csv file ", tolower(var), " ", year, mth))
        write.csv(val.array, paste(diroutvar, "/", tolower(var), "-", year, mth, ".csv", sep=""), row.names=F)
      }     
    }
  }
}

## 3- Average values from WFD to create the 1950-2000 climatology
WFDAverage <- function(dirbase="S:/observed/gridded_products/wfd/csv-files", dirout="G:/cenavarro/bid/wfd_0_5_deg") {
  
  require(raster)
    
  if (!file.exists(dirout)) {dir.create(dirout)}
  varlist <- c("Rainf", "Tmax", "Tmin")
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  for (var in varlist){   
    
    if  (var == "Rainf"){varmod <- "prec"} else {varmod <- var}
    
    diroutvar <- paste(dirbase, "/", tolower(var), "-daily", sep="")
    
    for (mth in mthList){
      
      if (!file.exists(paste(dirout, "/", tolower(varmod), "_", mth, ".csv", sep=""))) {
        
        base.array <- read.csv(paste(diroutvar, "/", tolower(var), "-", 1950, mth, ".csv", sep=""))
        
        for (i in 1951:2000){
          
          if (mth == "02"){
            
            cat(" Sum : ", var, " ", i, " ", mth, " \n")
            next.array <- read.csv(paste(diroutvar, "/", tolower(var), "-", i, mth, ".csv", sep=""))
            base.array <- base.array[1:30] + next.array[1:30]        
            
          } else{
            
            cat(" Sum : ", var, " ", i, " ", mth, " \n")
            next.array <- read.csv(paste(diroutvar, "/", tolower(var), "-", i, mth, ".csv", sep=""))
            base.array <- base.array + next.array
            
          }
          
        }
        
        result.array <- base.array/51
        write.csv(result.array, paste(dirout, "/", tolower(varmod), "_", mth, ".csv", sep=""), row.names=F)
        
      } 
      
      wfdAvg <-  read.csv(paste(dirout, "/", tolower(varmod), "_", mth, ".csv", sep=""), header=T)
      matrix <- cbind.data.frame(1:dim(wfdAvg)[1], wfdAvg)
      
      extlat <- extent(-120,-30,-56,33)
      matrixlat <- matrix[which(matrix$lat>=extlat@xmin & matrix$lat<=extlat@xmax & matrix$lon>=extlat@ymin & matrix$lon<=extlat@ymax),]
      
      names(matrixlat) <- c("id", "lat", "lon", 1:(dim(wfdAvg)[2]-2))
      write.table(matrixlat, paste(dirout, "_lat/", tolower(varmod), "_", mth, ".txt", sep=""), row.names=F, sep="\t")
      
    }
    
  }

}
  
## 4- GCM Daily process Historical
GCMDailyProcessHistorical <- function(startModel=1, endModel=2){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  dirbase <- "S:/observed/gridded_products/wfd"
  gcmHisDir <- "T:/gcm/cmip5/raw/daily/historical"
  gcmFutDir <- "T:/gcm/cmip5/raw/daily/rcp_45"
  dirout <- "G:/cenavarro/bid/gcm_raw_res"
#   diroutcut <- "G:/cenavarro/bid/gcm_0_5deg_lat"
  diroutcut <- "W:/bid/gcm_0_5deg_lat"
  
  gcmList <- list.dirs(dirout, recursive = FALSE, full.names = FALSE)
  varlist <- c("tasmax", "tasmin", "pr")
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  metList <- c("avg", "std")
    
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
    
  bbox <- extent(-120,-30,-56,33)

  ## Process GCM Historical
  for (gcm in gcmList[startModel:endModel]){
    
    diroutgcmhis <- paste(dirout, "/", basename(gcm), "/1960_1990", sep="")
    diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/1960_1990", sep="")
    
    if (!file.exists(paste(diroutgcmhiscut, "/by-month/prec_1990_12.nc", sep=""))){
      
      cat(" Cutting : ", "historical ", basename(gcm), " \n")
      
      if (!file.exists(diroutgcmhis)) {dir.create(diroutgcmhis, recursive=T)}
      if (!file.exists(paste(diroutgcmhis, "/by-month", sep=""))) {dir.create(paste(diroutgcmhis, "/by-month", sep=""), recursive=T)}
        
      if (!file.exists(diroutgcmhiscut)) {dir.create(diroutgcmhiscut, recursive=T)}
      if (!file.exists(paste(diroutgcmhiscut, "/by-month", sep=""))) {dir.create(paste(diroutgcmhiscut, "/by-month", sep=""), recursive=T)}
      
      
      ##Historical
      for (var in varlist){
          
        if (!file.exists(paste(diroutgcmhis, "/", var, "_1960_1990_day_lat.nc", sep=""))) {
          
          ncList <- list.files(path=paste(gcmHisDir, "/", basename(gcm), "/r2i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
          
          if (!file.exists(paste(diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))) {
            system(paste("cdo seldate,1980-01-01,1990-12-31 ", ncList[1], " ", diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))
          }
          
          system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", diroutgcmhis, "/", var, "_1960_1990_day.nc ", diroutgcmhis, "/", var, "_1960_1990_day_lat.nc",sep=""))
          file.remove(paste(diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))
          
        }
  
        if (!file.exists(paste(diroutgcmhis, "/by-month/", var, "_1990_12.nc", sep=""))) {
          
          system(paste("cdo splityear ", diroutgcmhis, "/", var, "_1960_1990_day_lat.nc ", diroutgcmhis, "/by-month/", var, "_", sep=""))
          
          for (yr in 1960:1990){
            system(paste("cdo splitmon ", diroutgcmhis, "/by-month/", var, "_", yr, ".nc ", diroutgcmhis, "/by-month/", var, "_", yr, "_", sep=""))
            file.remove(paste(diroutgcmhis, "/by-month/", var, "_", yr, ".nc", sep=""))
          }
        }
        
            
        if (!file.exists(paste(diroutgcmhis, "/", var, "_1960_1990_std_day.nc", sep=""))) {
          
          system(paste("cdo ymonavg ", diroutgcmhis, "/", var, "_1960_1990_day_lat.nc", " ",  diroutgcmhis, "/", var, "_1960_1990_avg_day.nc", sep=""))
          system(paste("cdo ymonstd ", diroutgcmhis, "/", var, "_1960_1990_day_lat.nc", " ",  diroutgcmhis, "/", var, "_1960_1990_std_day.nc", sep=""))
          
        }
      }
  #   }
  #     
  #   ## Reggrid GCM Historical
  #   for (gcm in gcmList[startModel:endModel]){
      
      for (var in varlist){
        
        if (var == "tasmax"){varmod <- "tmax"}
        if (var == "tasmin"){varmod <- "tmin"}
        if (var == "pr"){varmod <- "prec"}
        
        for (met in metList){
        
          if (!file.exists(paste(diroutgcmhiscut, "/", varmod, "_1960_1990_", met, "_day.nc", sep=""))) {
 
            cat(" Resampling : ", "historical ", basename(gcm), " ", varmod, " 1960_1990 ", met, " \n")
            
            m <- paste(diroutgcmhis, "/", var, "_1960_1990_", met, "_day.nc", sep="")
            mx <- raster(m)
            
            for( i in 1:mx@file@nbands){
              assign(paste("m", i, sep=""), raster(m, band=i))
            }
            
            mList <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
            
            mthNcStack <- stack(mList[1:12])
            mthNcStackRes <- resample(mthNcStack, raster(nrows=178, ncols=180, xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax), method='bilinear')
            
            xmin(mthNcStackRes) <- xmin(mthNcStackRes)-360
            xmax(mthNcStackRes) <- xmax(mthNcStackRes)-360
            
            if (varmod == "prec"){
              mthNcStackRes <- mthNcStackRes * 86400
            } else {
              mthNcStackRes <- mthNcStackRes - 273.15
            }
            
            mthNcStackRes <- writeRaster(mthNcStackRes, paste(diroutgcmhiscut, "/", varmod, "_1960_1990_", met, "_day.nc", sep=""), format="CDF", overwrite=T)
            
            
          }    
        }
      }
         
      for (var in varlist){
        
        for (yr in 1960:1990){
                  
          for (mth in mthList) {
  
            for(i in 1:31){
              assign(paste("d", i, sep=""), raster())
            }
  
            mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
            ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
            
            if  (var == "tasmax"){varmod <- "tmax"}
            if (var == "tasmin"){varmod <- "tmin"}
            if (var == "pr"){varmod <- "prec"}
            
            cat(" Resample daily: historical ", varmod, "_", yr, " ", mth, "\n")
            
            if (!file.exists(paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))) {
              
              f <- paste(diroutgcmhis, "/by-month/", var, "_", yr, "_", mth, ".nc", sep="")
              rx <- raster(f)
              
              for( i in 1:rx@file@nbands ){
                assign(paste("d", i, sep=""), raster(f, band=i))
              }
              
              dList <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31)
              
              dayNcStack <- stack(dList[1:ndayMth])
              dayNcStackRes <- resample(dayNcStack, raster(nrows=178, ncols=180, xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax), method='bilinear')
              
              xmin(dayNcStackRes) <- xmin(dayNcStackRes)-360
              xmax(dayNcStackRes) <- xmax(dayNcStackRes)-360
              
              if (varmod == "prec"){
                dayNcStackRes <- dayNcStackRes * 86400
              } else {
                dayNcStackRes <- dayNcStackRes - 273.15
              }
              
              dayNcStackRes <- writeRaster(dayNcStackRes, paste(diroutgcmhiscut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""), format="CDF", overwrite=T)
  
            }
          }
        }  
      }
      
      if (file.exists(paste(diroutgcmhis, "/by-month", sep=""))) {
        system(paste("rmdir /s /q ", diroutgcmhis, "/by-month", sep=""))
      }
    }
  }
}

## 5- GCM Daily process Future
GCMDailyProcessFuture <- function(startModel=1, endModel=2){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  dirbase <- "S:/observed/gridded_products/wfd"
  gcmHisDir <- "T:/gcm/cmip5/raw/daily/historical"
  gcmFutDir <- "T:/gcm/cmip5/raw/daily/rcp_45"
  dirout <- "G:/cenavarro/bid/gcm_raw_res"
  diroutcut <- "G:/cenavarro/bid/gcm_0_5deg_lat"
  
  gcmList <- list.dirs(dirout, recursive = FALSE, full.names = FALSE)
  varlist <- c("tasmax", "tasmin", "pr")
  mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  
  metList <- c("avg", "std")
  
  # Get a list of month with and withour 0 in one digit numbers
  mthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
  names(mthMat) <- c("Mth", "MthMod", "Ndays")
  
  bbox <- extent(-120,-30,-56,33)
  
  ## Process GCM Future
  for (gcm in gcmList){
    
    cat(" Cutting : ", "future ", basename(gcm), " \n")
    
    ##Future
    
    for (var in varlist){
      
      diroutgcmfut <- paste(dirout, "/", basename(gcm), "/2020_2049", sep="")
      if (!file.exists(diroutgcmfut)) {dir.create(diroutgcmfut, recursive=T)}
      if (!file.exists(paste(diroutgcmfut, "/by-month", sep=""))) {dir.create(paste(diroutgcmfut, "/by-month", sep=""), recursive=T)}
      
      ncList <- list.files(path=paste(gcmFutDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
      
      if (!file.exists(paste(diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))) {
        
        system(paste("cdo -s seldate,2020-01-01,2049-12-31 ", ncList[1], " ", diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))
        
      }
      
      if (!file.exists(paste(diroutgcmfut, "/by-month/", var, "_2049_12.nc", sep=""))) {
        system(paste("cdo -s splityear ", diroutgcmfut, "/", var, "_2020_2049_day.nc ", diroutgcmfut, "/by-month/", var, "_", sep=""))
        
        for (yr in 2020:2049){
          system(paste("cdo -s splitmon ", diroutgcmfut, "/by-month/", var, "_", yr, ".nc ", diroutgcmfut, "/by-month/", var, "_", yr, "_", sep=""))
          file.remove(paste(diroutgcmfut, "/by-month/", var, "_", yr, ".nc", sep=""))
        }
      }
      
#       if (!file.exists(paste(diroutgcmfut, "/", var, "_2020_2049_std_day.nc", sep=""))) {
#         
#         system(paste("cdo ymonavg ", diroutgcmfut, "/", var, "_2020_2049_day.nc", " ",  diroutgcmfut, "/", var, "_2020_2049_avg_day.nc", sep=""))
#         system(paste("cdo ymonstd ", diroutgcmfut, "/", var, "_2020_2049_day.nc", " ",  diroutgcmfut, "/", var, "_2020_2049_std_day.nc", sep=""))
#         
#       }
      
    }
  }
 
  ## Reggrid GCM Future
  
  for (gcm in gcmList[startModel:endModel]){
    
    diroutgcmfut <- paste(dirout, "/", basename(gcm), "/2020_2049", sep="")
    diroutgcmfutcut <- paste(diroutcut, "/", basename(gcm), "/2020_2049", sep="")
    if (!file.exists(diroutgcmfut)) {dir.create(diroutgcmfut, recursive=T)}
    if (!file.exists(paste(diroutgcmfut, "/by-month", sep=""))) {dir.create(paste(diroutgcmfut, "/by-month", sep=""), recursive=T)}
    
#     for (var in varlist){
#       
#       if (var == "tasmax"){varmod <- "tmax"}
#       if (var == "tasmin"){varmod <- "tmin"}
#       if (var == "pr"){varmod <- "prec"}
#       
#       for (met in metList){
#         
#         if (!file.exists(paste(diroutgcmfut, "/", varmod, "_2020_2049_", met, "_day.nc", sep=""))) {
#           
#           for (mth in mthList) {
#             
#             mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
#             
#             cat(" Resample: ", var, "_", mth, " ", met, " \n")
#             
#             monNc <- raster(paste(diroutgcmfut, "/", var, "_2020_2049_", met, "_day.nc", sep=""), band=mthMod)
#             monNc <- resample(monNc, raster(nrows=360, ncols=720, xmn=0, xmx=360, ymn=-90, ymx=90), method='bilinear')
#             monNc <- crop(rotate(monNc), bbox)
#             
#             if (varmod == "prec"){
#               monNc <- monNc * 86400
#             } else {
#               monNc <- monNc - 273.15
#             }
#             
#             monNc <- writeRaster(monNc, paste(diroutgcmfutcut, "/", varmod, "_2020_2049_", met, "_day_", mth, "_temp.nc", sep=""), format="CDF", overwrite=T)
#             system(paste("cdo setdate,2049-", mth, "-31,00:00 ", diroutgcmfutcut, "/", varmod, "_2020_2049_", met, "_day_", mth, "_temp.nc ", diroutgcmfutcut, "/", varmod, "_2020_2049_", met, "_day_", mth, ".nc", sep=""))
#             file.remove(paste(diroutgcmfutcut, "/", varmod, "_2020_2049_", met, "_day_", mth, "_temp.nc", sep=""))
#             
#           }
#           
#           ncMthList <- list.files(path=diroutgcmfutcut, pattern=paste(varmod, "_2020_2049_", met, "_day_", "*", sep=""), full.names=TRUE)
#           cat("...merging=",ncMthList,"\n")
#           system(paste("cdo mergetime ", paste(ncMthList, collapse=" "), " ", diroutgcmfutcut, "/", varmod, "_2020_2049_", met, "_day.nc",sep=""))
#           
#           for (nc in ncMthList){
#             
#             file.remove(nc)
#             
#           }
#         }    
#       }
#     }
    
    
    
    for (yr in 2020:2049){
      
      for (mth in mthList) {
        
        mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
        ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
        
        for (var in varlist){
          
          if  (var == "tasmax"){varmod <- "tmax"}
          if (var == "tasmin"){varmod <- "tmin"}
          if (var == "pr"){varmod <- "prec"}
          
          cat(" Resample: ", var, "_", yr, " ", mth, " ", " \n")
          
          if (!file.exists(paste(diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))) {
            
            datedays <- c(paste("0", 1:9, sep=""), paste(10:ndayMth, sep=""))
            
            for (day in datedays){
              
              dayNc <- raster(paste(diroutgcmfut, "/by-month/", var, "_", yr, "_", mth, ".nc", sep=""), band=as.numeric(day))
              dayNcRes <- resample(dayNc, raster(nrows=360, ncols=720, xmn=0, xmx=360, ymn=-90, ymx=90), method='bilinear')
              dayNcRes <- crop(rotate(dayNcRes), bbox)
              
              if (varmod == "prec"){
                dayNcRes <- dayNcRes * 86400
              } else {
                dayNcRes <- dayNcRes - 273.15
              }
              
              
              dayNcRes <- writeRaster(dayNcRes, paste(diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, "_", day, "_temp.nc", sep=""), format="CDF", overwrite=T)
              system(paste("cdo setdate,", yr, "-", mth, "-", day, ",00:00 ", diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, "_", day, "_temp.nc ", diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, "_", day, ".nc", sep=""))
              file.remove(paste(diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, "_", day, "_temp.nc", sep=""))
              
            }
            
            ncDayList <- list.files(path=paste(diroutgcmfutcut, "/by-month", sep=""), pattern=paste(varmod, "_", yr, "_", mth, "*", sep=""), full.names=TRUE)
            cat("...merging=",ncDayList,"\n")
            system(paste("cdo mergetime ", paste(ncDayList, collapse=" "), " ", diroutgcmfutcut, "/by-month/", varmod, "_", yr, "_", mth, ".nc",sep=""))
            
            for (nc in ncDayList){
              
              file.remove(nc)
              
            }
            
          }
        }
      }  
    }    
  }
  
  
  
  
  }

## 6-  CF Calculation
CFCalculation <- function(dirwfd="G:/cenavarro/bid/wfd_0_5_deg_lat", dirgcm="G:/cenavarro/bid/gcm_0_5deg_lat", dirout="G:/cenavarro/bid/cf_0_5_deg_lat"){
  
  require(raster)
  
  
  bbox <- extent(-120,-30,-56,33)
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  varlist <- c("prec", "tmax", "tmin")
  
  if (!file.exists(dirout)) {dir.create(dirout)}
  
  gcmList <- list.dirs(dirgcm, recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList){
    
    for (var in varlist){
      
      if  (var == "tmax"){varWfd == "Tmax"}
      if (var == "tmin"){colNum <- 6}
      if (var == "prec"){colNum <- 7}
      
      for (mth in monthList){
              
        mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
        
        if (!file.exists(paste(dirout, "/", var, "_", mthMod, ".nc", sep=""))) {
          
          daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
          
          
          gcm.avg.fut <- raster(paste(dirgcm, "/", gcm, "/2020_2049/", var, "_avg_", mth, ".nc", sep=""))
          gcm.avg.his <- raster(paste(dirgcm, "/", gcm, "/1960_1990/", var, "_avg_", mth, ".nc", sep=""))
          gcm.std.fut <- raster(paste(dirgcm, "/", gcm, "/2020_2049/", var, "_std_", mth, ".nc", sep=""))
          gcm.std.his <- raster(paste(dirgcm, "/", gcm, "/1960_1990/", var, "_std_", mth, ".nc", sep=""))
          wfd.day <- raster(paste(dirwfd, "/1960_1990/", var, "_std_", mth, ".nc", sep=""))
          
          for (day in 3:dim(wfd.array)[2]){
            
            cat(" CF Calc: ", var, "_", mth, " day", paste(day-2), " \n")
            cfCalc <- gcm.avg.fut + (gcm.std.fut / gcm.std.his * (wfd.array[day] - gcm.std.his))
            
            
            if (var == "prec"){ cfCalc[cfCalc<0] <- 0}
            
            matrixCf <- cbind.data.frame(matrixCf, cfCalc)
            
          }
          
          
          colnames(matrixCf) <- c("id", "lat", "lon", "alt", 1:(dim(wfd.array)[2]-2))
          
          cat(" Writting output tab delimited file\n\n")
          write.table(matrixCf, paste(dirout, "/", var, "_", mthMod, ".txt", sep=""), row.names=F, sep="\t")
          
          matrixCflat <- matrixCf[which(matrixCf$lon>=extlat@xmin & matrixCf$lon<=extlat@xmax & matrixCf$lat>=extlat@ymin & matrixCf$lat<=extlat@ymax),]
          write.table(matrixCflat, paste(diroutlat, "/", var, "_", mthMod, ".txt", sep=""), row.names=F, sep="\t")
          
          colNum <- colNum + 3
          
        }
      }
    }
  }
}

## 7-  BC Calculation
BCCalculation <- function(dirwfdlat="G:/cenavarro/bid/wfd_0_5_deg_lat", dirgcm="G:/cenavarro/bid/gcm_0_5deg", dirout="G:/cenavarro/bid/cf_0_5_deg", diroutlat="G:/cenavarro/bid/cf_0_5_deg_lat"){
    
  require(raster)
  
  extlat <- extent(-120,-30,-56,33)
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  varlist <- c("prec", "tmax", "tmin")
  
  if (!file.exists(dirout)) {dir.create(dirout)}
  if (!file.exists(diroutlat)) {dir.create(diroutlat)}
  
  gcmList <- list.dirs(dirgcm, recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList){
    
     gcmhis.avg.array <- read.table(paste(dirgcm, "/", basename(gcm), "/1950_2000/_avg_1950_2000.txt", sep=""), header=T)
     gcmhis.avg.array <- gcmhis.avg.array[which(gcmhis.avg.array$lon>=extlat@xmin & gcmhis.avg.array$lon<=extlat@xmax & gcmhis.avg.array$lat>=extlat@ymin & gcmhis.avg.array$lat<=extlat@ymax),]
     
     gcmhis.std.array <- read.table(paste(dirgcm, "/", basename(gcm), "/1950_2000/_std_1950_2000.txt", sep=""), header=T)
     gcmhis.std.array <- gcmhis.std.array[which(gcmhis.std.array$lon>=extlat@xmin & gcmhis.std.array$lon<=extlat@xmax & gcmhis.std.array$lat>=extlat@ymin & gcmhis.std.array$lat<=extlat@ymax),]
     
     #  gcmfut.avg.array <- read.table(paste(dirgcm, "/", basename(gcm), "/2020_2049/_avg_2020_2049.txt", sep=""), header=T)
     #  gcmfut.std.array <- read.table(paste(dirgcm, "/", basename(gcm), "/2020_2049/_std_2020_2049.txt", sep=""), header=T)
    
    for (var in varlist){
      
      if  (var == "tmax"){colNum <- 5}
      if (var == "tmin"){colNum <- 6}
      if (var == "prec"){colNum <- 7}
      
      for (mth in monthList){
        
        wfd.array <- read.table("D:/CIAT/Workspace/bid/wfd_0_5_deg_lat/prec_01.txt", header=T)
        wfd.array.avg <- rowMeans(a[4:dim(a)[2]])
        
        if (var == "prec"){
          gcmhis.std <- gcmhis.std.array[colNum]
        } else {
          gcmhis.std <- gcmhis.std.array[colNum] + 273.15
        }
        
        matrixBc <- gcmhis.avg.array[1:4]
        
        mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
        daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
        
        if (!file.exists(paste(diroutlat, "/", var, "_", mthMod, ".txt", sep=""))) {
          
          wfd.array <- read.csv(paste(dirwfd, "/", var, "_", mth, ".csv", sep=""), header=T)      
          
          for (day in 3:dim(wfd.array)[2]){
            
            cat(" CF Calc: ", var, "_", mth, " day", paste(day-2), " \n")
            
            cfCalc <- gcmfut.avg.array[colNum] + (gcmfut.std.array[colNum] / gcmhis.std.array[colNum] * (wfd.array[day] - gcmhis.avg.array[colNum]))
            
            if (var == "prec"){ cfCalc[cfCalc<0] <- 0}
            
            matrixBc <- cbind.data.frame(matrixBc, cfCalc)
            
          }
          
          
          colnames(matrixBc) <- c("id", "lat", "lon", "alt", 1:(dim(wfd.array)[2]-2))
          
          cat(" Writting output tab delimited file\n\n")
          write.table(matrixBc, paste(dirout, "/", var, "_", mthMod, ".txt", sep=""), row.names=F, sep="\t")
          
          matrixBclat <- matrixBc[which(matrixBc$lon>=extlat@xmin & matrixBc$lon<=extlat@xmax & matrixBc$lat>=extlat@ymin & matrixBc$lat<=extlat@ymax),]
          write.table(matrixBclat, paste(diroutlat, "/", var, "_", mthMod, ".txt", sep=""), row.names=F, sep="\t")
          
          colNum <- colNum + 3
          
        }
      }
    }
  }
}

