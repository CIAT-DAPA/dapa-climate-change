require(maptools)
require(raster)
require(ncdf)
require(rgdal)
require(sp)

#####################################################################################################
# Description: This function is to calculate the averaging surfaces of the CMIP5 monhtly climate data 
#####################################################################################################
GCMAveragePreInd <- function(rcp='historical', baseDir="T:/gcm/cmip5/raw/monthly", trunkDir='D:/CIAT/_tools/dapa-climate-change/IPCC-CMIP5/data') {
  
  
  rcp <- "rcp85"
  baseline <- "historical"
  baseDir <- "T:/gcm/cmip5/raw/monthly"
  trunkDir <- "D:/CIAT/_tools/dapa-climate-change/IPCC-CMIP5/data"
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # Read gcm characteristics table
  gcmStats <- read.table(paste(trunkDir, "/cmip5-", baseline, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  # Get a list of month with and withour 0 in one digit numbers
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)
  
  # Set number of days by month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  # List of variables to average
  # varList <- c("prec", "tmax", "tmin")
  
  # Loop around gcms and ensembles
  for (i in 1:nrow(gcmStats)){
    
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStats)[i,10]) == "ins-yr"){
        
        # Get gcm and ensemble names
        gcm <- paste(as.matrix(gcmStats)[i,2])
        ens <- paste(as.matrix(gcmStats)[i,3])
        
        # Path of each ensemble
        ensDir <- paste(baseDir, "/", baseline, "/", gcm, "/", ens, sep="")
        
        # Directory with monthly splitted files
        mthDir <- paste(ensDir, "/monthly-files", sep="")
        
        # Create output average directory
        avgDir <- paste(ensDir, "/average", sep="")
        if (!file.exists(avgDir)) {dir.create(avgDir)}
        
        # Period list for historical and future pathways
        period <- "1870"
        
        # Define start and end year
        staYear <- as.integer(period)
        endYear <- as.integer(period) + 20
        
        cat("\nAverage over: ", baseline, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
        
        # Loop around variables
        #for (var in varList) {
        var <- "tmean"
        
        # Loop around months
        for (mth in monthList) {
          
          if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, sep=""))) 
          {dir.create(paste(avgDir, "/", staYear, "_", endYear, sep=""))}
          
          # Define month without 0 in one digit number
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          outNcAvg <- paste(avgDir, "/", staYear, "_", endYear, "/", var, "_", mthMod, ".nc", sep="")
          
          if (!file.exists(outNcAvg)){
            
            # List of NetCDF files by month for all 20yr period
            mthNc <- lapply(paste(mthDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
            
            # Create a stack of list of NC, rotate and convert units in mm/monnth and deg celsious
            if (var == "prec"){
              
              daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
              mthNcAvg <- rotate(mean(stack(mthNc))) * 86400 * (daysmth)
              
            } else {
              
              mthNcAvg <- rotate(mean(stack(mthNc))) #- 272.15
            }
            
            # Write output average NetCDF file
            mthNcAvg <- writeRaster(mthNcAvg, outNcAvg, format='CDF', overwrite=T)
            
            cat(" .> ", paste(var, "_", mthMod, sep=""), "\tdone!\n")
            
          } else {cat(" .>", paste(var, "_", mthMod, sep=""), "\tdone!\n")}
          
        }

      # }
    #}
    
      }
    }
    
    
  }
  
  cat("GCM Average Process Done!")
  
  
  
  gcmStatsRcp <- read.table(paste(trunkDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  # Loop around gcms and ensembles
  for (i in 1:nrow(gcmStatsRcp)){
    
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-yr"){
        
        if(paste(as.matrix(gcmStatsRcp)[i,3]) == "r1i1p1"){
        
          # Get gcm and ensemble names
          gcm <- paste(as.matrix(gcmStatsRcp)[i,2])
          ens <- paste(as.matrix(gcmStatsRcp)[i,3])
          
          # Path of each ensemble
          ensDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, sep="")
  
  
  
  
  
}