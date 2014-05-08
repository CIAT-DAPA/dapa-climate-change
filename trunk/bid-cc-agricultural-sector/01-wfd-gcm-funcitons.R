### 0 - Obtain lat, lon, alt & alt strm from wfd coordinates
require(maptools)
require(raster)

dirbase <- "S:/observed/gridded_products/wfd"

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




## 1- Prepare WFD Region and WCL climatology 0.5 deg

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




## 2- Extract values from WFD nc values
require(maptools)
require(raster)
require(ncdf)
require(rgdal)
require(sp)
require(chron)

dirbase <- "S:/observed/gridded_products/wfd"
dirout <- paste(dirbase, "/csv-files", sep="")

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
      
## Wrtie csv output file
      cat(paste("\n ->.  Writing out csv file ", tolower(var), " ", year, mth))
      write.csv(val.array, paste(diroutvar, "/", tolower(var), "-", year, mth, ".csv", sep=""), row.names=F)
    }
    
}








## 3- Average values from WFD to create the 1950-2000 climatology
require(raster)

dirbase <- "S:/observed/gridded_products/wfd/csv-files"
dirout <- "G:/cenavarro/bid/wfd_0_5_deg"
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


## 4- GCM Daily process

require(maptools)
require(raster)
require(ncdf)
require(rgdal)
require(sp)
require(chron)

dirbase <- "S:/observed/gridded_products/wfd"
gcmHisDir <- "T:/gcm/cmip5/raw/daily/historical"
gcmFutDir <- "T:/gcm/cmip5/raw/daily/rcp_45"
dirout <- "G:/cenavarro/bid/gcm_raw_res"
diroutcut <- "G:/cenavarro/bid/gcm_0_5deg_lat"

latlon <- open.ncdf(paste(dirbase, "/raw/WFD-land-lat-long-z.nc", sep=""), write=FALSE)
lon <- get.var.ncdf(latlon, "Longitude")
lat <- get.var.ncdf(latlon, "Latitude")
coords <- data.frame(lon, lat)
close.ncdf(latlon)

gcmList <- list.dirs(dirout, recursive = FALSE, full.names = FALSE)
varlist <- c("tasmax", "tasmin", "pr")
metlist <- c("avg", "std")
mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))

# Get a list of month with and withour 0 in one digit numbers
mthListMod <- c(1:12)
mthMat <- as.data.frame(cbind(mthList, mthListMod))
names(mthMat) <- c("Mth", "MthMod")

bbox <- extent(-120,-30,-56,33)
  
for (gcm in gcmList){
  
  cat(" Obtain values for WFD coordinates : ", "historical ", basename(gcm), " \n")
  
  ##Historical
  for (var in varlist){
    
    diroutgcmhis <- paste(dirout, "/", basename(gcm), "/1960_1990", sep="")
    if (!file.exists(diroutgcmhis)) {dir.create(diroutgcmhis, recursive=T)}
    
    ncList <- list.files(path=paste(gcmHisDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
    
    if (!file.exists(paste(diroutgcmhis, "/", var, "_1960_1990_std_day.nc", sep=""))) {
      
      system(paste("cdo seldate,1960-01-01,1990-12-31 ", ncList[1], " ", diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))
      
      system(paste("cdo ymonavg ", diroutgcmhis, "/", var, "_1960_1990_day.nc", " ",  diroutgcmhis, "/", var, "_1960_1990_avg_day.nc", sep=""))
      system(paste("cdo ymonstd ", diroutgcmhis, "/", var, "_1960_1990_day.nc", " ",  diroutgcmhis, "/", var, "_1960_1990_std_day.nc", sep=""))
    
      }
    
#       system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",diroutgcmhis, "/", var, "_1960_1990_avg_day.nc ",diroutgcmhis, "/lat_", var, "_1960_1990_avg_day.nc",sep=""))
#       system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",diroutgcmhis, "/", var, "_1960_1990_std_day.nc ",diroutgcmhis, "/lat_", var, "_1960_1990_std_day.nc",sep=""))    
    
      system(paste("cdo splitmon ", diroutgcmhis, "/", var, "_1960_1990_avg_day.nc ", diroutgcmhis, "/", var, "_avg_", sep=""))
      system(paste("cdo splitmon ", diroutgcmhis, "/", var, "_1960_1990_std_day.nc ", diroutgcmhis, "/", var, "_std_", sep=""))
      
    
    
    if (file.exists(paste(diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))) {file.remove(paste(diroutgcmhis, "/", var, "_1960_1990_day.nc", sep=""))}
    
  }
  
#   
#   ## Extract values in WFD coordinates
#   
#   diroutgcmhiscut <- paste(diroutcut, "/", basename(gcm), "/1960_1990", sep="")
#   if (!file.exists(diroutgcmhiscut)) {dir.create(diroutgcmhiscut, recursive=T)}
#     
#   for (met in metlist){
#             
#     for (mth in mthList) {
#       
#       mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
#       
#       for (var in varlist){
#         
#         if  (var == "tasmax"){varmod <- "tmax"}
#         if (var == "tasmin"){varmod <- "tmin"}
#         if (var == "pr"){varmod <- "prec"}
#         
#         cat(" Resample: ", var, "_", mth, " ", met, " \n")
#         
#         if (!file.exists(paste(diroutgcmhiscut, "/", varmod, "_", met, "_", mth, ".nc", sep=""))) {
#           
#           gcmAvgMon <- rotate(raster(paste(diroutgcmhis, "/", var, "_", met, "_", mth, ".nc", sep="")))
#           resGcmAvgMon <- resample(gcmAvgMon, raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90), method='bilinear')
#           cutresGcmAvgMon <- crop(resGcmAvgMon, bbox)
#           
#           if (varmod == "prec"){
#             cutresGcmAvgMon <- cutresGcmAvgMon * 86400
#           } else {
#             cutresGcmAvgMon <- cutresGcmAvgMon - 273.15
#           }
#           
#           cutresGcmAvgMon <- writeRaster(cutresGcmAvgMon, paste(diroutgcmhiscut, "/", varmod, "_", met, "_", mth, ".nc", sep=""), overwrite=T)
#           
#           if (file.exists(paste(diroutgcmhis, "/", varmod, "_", met, "_", mth, ".nc", sep=""))) {file.remove(paste(diroutgcmhis, "/", varmod, "_", met, "_", mth, ".nc", sep=""))}
#           
#         }
#       }      
#     } 
#   }    
}

for (gcm in gcmList){
  
  cat(" Obtain values for WFD coordinates : ", "future ", basename(gcm), " \n")
  
  ##Future
  
  for (var in varlist){
    
    diroutgcmfut <- paste(dirout, "/", basename(gcm), "/2020_2049", sep="")
    if (!file.exists(diroutgcmfut)) {dir.create(diroutgcmfut, recursive=T)}
    
    ncList <- list.files(path=paste(gcmFutDir, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
    
    if (!file.exists(paste(diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))) {
      
      system(paste("cdo seldate,2020-01-01,2049-12-31 ", ncList[1], " ", diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))
      
      system(paste("cdo ymonavg ", diroutgcmfut, "/", var, "_2020_2049_day.nc", " ",  diroutgcmfut, "/", var, "_2020_2049_avg_day.nc", sep=""))
      system(paste("cdo ymonstd ", diroutgcmfut, "/", var, "_2020_2049_day.nc", " ",  diroutgcmfut, "/", var, "_2020_2049_std_day.nc", sep=""))
      
    }
    
#     system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",diroutgcmfut, "/", var, "_2020_2049_avg_day.nc ",diroutgcmfut, "/lat_", var, "_2020_2049_avg_day.nc",sep=""))
#     system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",diroutgcmfut, "/", var, "_2020_2049_std_day.nc ",diroutgcmfut, "/lat_", var, "_2020_2049_std_day.nc",sep=""))    
    
    system(paste("cdo splitmon ", diroutgcmfut, "/", var, "_2020_2049_avg_day.nc ", diroutgcmfut, "/", var, "_avg_", sep=""))
    system(paste("cdo splitmon ", diroutgcmfut, "/", var, "_2020_2049_std_day.nc ", diroutgcmfut, "/", var, "_std_", sep=""))
      
    
    
    if (file.exists(paste(diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))) {file.remove(paste(diroutgcmfut, "/", var, "_2020_2049_day.nc", sep=""))}
    
  }
#   
#   
#   ## Extract values in WFD coordinates
#   
#   diroutgcmfutcut <- paste(diroutcut, "/", basename(gcm), "/2020_2049", sep="")
#   if (!file.exists(diroutgcmfutcut)) {dir.create(diroutgcmfutcut, recursive=T)}
#   
#   for (met in metlist){
#     
#     for (mth in mthList) {
#       
#       mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
#       
#       for (var in varlist){
#         
#         if  (var == "tasmax"){varmod <- "tmax"}
#         if (var == "tasmin"){varmod <- "tmin"}
#         if (var == "pr"){varmod <- "prec"}
#         
#         cat(" Resample: ", var, "_", mth, " ", met, " \n")
#         
#         if (!file.exists(paste(diroutgcmfutcut, "/", varmod, "_", met, "_", mth, ".nc", sep=""))) {
#           
#           gcmAvgMon <- rotate(raster(paste(diroutgcmfut, "/", var, "_", met, "_", mth, ".nc", sep="")))
#           resGcmAvgMon <- resample(gcmAvgMon, raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90), method='bilinear') 
#           cutresGcmAvgMon <- crop(resGcmAvgMon, bbox)
#           
#           if (varmod == "prec"){
#             cutresGcmAvgMon <- cutresGcmAvgMon * 86400
#           } else {
#             cutresGcmAvgMon <- cutresGcmAvgMon - 273.15
#           }
#           
#           cutresGcmAvgMon <- writeRaster(cutresGcmAvgMon, paste(diroutgcmfutcut, "/", varmod, "_", met, "_", mth, ".nc", sep=""), overwrite=T)
#           
#           if (file.exists(paste(diroutgcmfut, "/", var, "_", met, "_", mth, ".nc", sep=""))) {file.remove(paste(diroutgcmfut, "/", var, "_", met, "_", mth, ".nc", sep=""))}
#           
#         }
#         
#       }
#     }
#   }
}



## 5-  CF Calculation
require(raster)
dirwfd <- "G:/cenavarro/bid/wfd_0_5_deg_lat"
dirgcm <- "G:/cenavarro/bid/gcm_0_5deg_lat"
dirout <- "G:/cenavarro/bid/cf_0_5_deg_lat"

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





## 6-  BC Calculation
require(raster)
dirwfdlat <- "G:/cenavarro/bid/wfd_0_5_deg_lat"
dirgcm <- "G:/cenavarro/bid/gcm_0_5deg"
dirout <- "G:/cenavarro/bid/cf_0_5_deg"
diroutlat <- "G:/cenavarro/bid/cf_0_5_deg_lat"

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


