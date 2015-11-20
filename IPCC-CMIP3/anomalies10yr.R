require(maptools)
require(raster)
require(ncdf)
require(rgdal)
require(sp)

sres <- "sres_b1"
workDir <- "D:/cenavarro/Request/jvalencia"
#################################################################################################################
# Description: This function is to calculate the anomalies of averaged surfaces of the CMIP5 monhtly climate data
#################################################################################################################

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXX GCM ANOMALIES 10yr CALCULATION XXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat(" \n")


baseDir <- paste("S:/gcm/cmip3/raw_data/", sres, "/original-data", sep="")
clmDir <- "S:/gcm/cmip3/raw_data/historical"
fillDir <-  paste("S:/gcm/cmip3/raw_data/", sres, "/filled", sep="")

# List of variables and months
varList <- c("prec", "tmax", "tmin")
monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
monthListMod <- c(1:12)

# Set number of days by month
ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
names(ndaymtx) <- c("Month", "Ndays", "MonthMod")


gcmList <- list.dirs(baseDir, recursive = FALSE, full.names = FALSE)

for (gcm in gcmList) {
  
  # Get gcm names    
  gcm <- basename(gcm)
  
  yrGcmDir <- paste(baseDir, "/", gcm, "/yearly_files", sep="")
        
  # Average directory
  yrFutGcmDir <- paste(workDir, "/anomalies", sep="")
  if (!file.exists(yrFutGcmDir)) {dir.create(yrFutGcmDir)}
  
  periodList <- c("2021", "2031", "2041", "2061", "2091")
  
  for (period in periodList) {
    
    # Define start and end year
    staYear <- as.integer(period)
    if (period == "2091"){endYear <- as.integer(period) + 8} else {endYear <- as.integer(period) + 9}

    futGcmAvgDir <- paste(workDir, "/anomalies/", sres, "/", gcm, "/", staYear, "_", endYear, sep="")
    if (!file.exists(paste(workDir, "/anomalies/", sres, sep=""))) {dir.create(paste(workDir, "/anomalies/", sres, sep=""))}
    if (!file.exists(paste(workDir, "/anomalies/", sres, "/", gcm, sep=""))) {dir.create(paste(workDir, "/anomalies/", sres, "/", gcm, sep=""))}
    if (!file.exists(futGcmAvgDir)) {dir.create(futGcmAvgDir)}
      
    # Loop around variables
    for (var in varList) {
      
      # Loop around months
      for (mth in monthList) {
        
        # Define month without 0 in one digit number
        mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
        outNcAvg <- paste(futGcmAvgDir, "/", var, "_", mthMod, ".nc", sep="")
        
        outShp <- paste(futGcmAvgDir, "/", var, "_", mthMod, ".shp", sep="")
        
        if (!file.exists(outNcAvg)){
          
          # List of NetCDF files by month for all 30yr period      
          if ((!file.exists(paste(yrGcmDir, "/", staYear, "/", var, "_", mth, ".nc", sep=""))) || (!file.exists(paste(yrGcmDir, "/", endYear, "/", var, "_", mth, ".nc", sep="")))){
            
            clmNcAvg <- raster(paste(clmDir, "/filled/", gcm, "/1961_1990", "/", var, "_", mth, ".asc", sep=""))
            if (period == "2091"){
              staYearMod <- 2070
              endYearMod <- 2099
            } else {
              staYearMod <- as.integer(period) - 11
              endYearMod <- as.integer(period) + 18
            }
              
            fillGcmDir <- paste(fillDir, "/", gcm, "/", staYearMod, "_", endYearMod, sep="")
            mthNcAvg <- raster(paste(fillGcmDir, "/", var, "_", mth, ".asc", sep=""))
              
          } else {
                        
            mthNc <- lapply(paste(yrGcmDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
          
            # Create a stack of list of NC, rotate and convert units in mm/monnth and deg celsious
            if (var == "prec"){
                            
              daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
              mthNcAvg <- rotate(mean(stack(mthNc))) * 86400 * (daysmth)
              clmNcAvg <- rotate(raster(paste(clmDir, "/original-data/", gcm, "/multiyr_avgs/1961_1990", "/", var, "_", mth, ".nc", sep=""))) * 86400 * (daysmth)
              
            } else {
              
              mthNcAvg <- rotate(mean(stack(mthNc))) - 272.15    ## mirar de los dos tipos cual resto y cual no
              clmNcAvg <- rotate(raster(paste(clmDir, "/original-data/", gcm, "/multiyr_avgs/1961_1990", "/", var, "_", mth, ".nc", sep=""))) - 272.15
            }
          
          }
          
          # Write output average NetCDF file
          anomNcAvg <- mthNcAvg - clmNcAvg
          mthNcAvg <- writeRaster(anomNcAvg, outNcAvg, format='CDF', overwrite=T)
          } else {cat(" .> Avg10yr ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")}
        
        if (!file.exists(outShp)){
          
          anomPts <- rasterToPoints(raster(outNcAvg)) 
          
          coords <- data.frame(anomPts[,1:2])
          colnames(coords) <- c("LON", "LAT")
          
          values <- data.frame(anomPts[,3])
          colnames(values) <- c("VALUE")
          
          anomPts <- SpatialPointsDataFrame(coords,values)
          anomShp <- writePointsShape(anomPts, paste(futGcmAvgDir, "/", var, "_", mthMod, sep=""))
          
          cat(" .> Anomalies ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")
          
          } else {cat(" .> Anomalies ", paste("\t ", var, "_", mthMod, sep=""), "\tdone!\n")}
              
      }
    }
  }
}

cat("GCM Anomalies Process Done!")
