# rcp <- "rcp85"
# baseline <- "historical"
# baseDir <- "L:/gcm/cmip5/raw/monthly"
# trunkDir <- "G:/_scripts/dapa-climate-change/IPCC-CMIP5/data"
# outDir <- "X:/2C_Target"
# otp <- "00-monthly-data-functions-preindustrial.R"

#####################################################################################################
# Description: This function is to calculate the averaging surfaces of the CMIP5 monhtly climate data 
#####################################################################################################


### Calculate running mean 
rcp='rcp85'
baseline='historical'
baseDir="T:/gcm/cmip5/raw/monthly"
trunkDir='D:/_scripts/dapa-climate-change/IPCC-CMIP5/data'
outDir <- "X:/2C_Target"

gcmStatsRcp <- read.table(paste(trunkDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)

# Get a list of month with and withour 0 in one digit numbers
monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
monthListMod <- c(1:12)

# Set number of days by month
ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Combirn number of month and days in one single data frame
ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
names(ndaymtx) <- c("Month", "Ndays", "MonthMod")

var="tmean"
for (n in 1:50){
  # controlIntpol <- function(i) {
  # Don't include variables without all three variables
  if(!paste(as.matrix(gcmStatsRcp)[n,10]) == "ins-var") {
    
    if(!paste(as.matrix(gcmStatsRcp)[n,10]) == "ins-yr"){
      
      if(paste(as.matrix(gcmStatsRcp)[n,3]) == "r1i1p1"){
        
        # Get gcm and ensemble names
        gcm <- paste(as.matrix(gcmStatsRcp)[n,2])
        ens <- paste(as.matrix(gcmStatsRcp)[n,3])
        
        # Path of each ensemble
        ensDir <- paste(baseDir, "/", baseline, "/", gcm, "/", ens, sep="")
        
        mthDir <- paste(ensDir, "/monthly-files", sep="")
        
        nyear=c(1850:2099)
        ncount=0
        for (i in 1:length(nyear)){ 
          yrNcAnomDirOut <- paste(outDir, "/running_mean/", rcp, "/", gcm, "/", ens, "/", nyear[i], sep="")
          for (mth in monthList) { 
            mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
            if (!file.exists(yrNcAnomDirOut)) {dir.create(yrNcAnomDirOut, recursive = TRUE)}
            
            outNcAvg <- paste(yrNcAnomDirOut, "/", var, "_", mthMod, ".nc", sep="")
            if (!file.exists(outNcAvg)){
              if(i<11){
                if(i==1){
                  selyear <-nyear[1:11][-1]
                }else{
                  j=1
                  i=2
                  k=i-j
                  l=10+i
                  selyear <-nyear[c(j:k,i:l)][-i]
                }
                
              }else{
                j=1
                i=250
                k=i-j
                l=10+i
                m=j+ncount
                selyear < nyear[c(m:k,i:l)][-i]
                ncount=ncount+1
              }          
              
              
              mthNc <- lapply(paste(mthDir, "/", selyear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
              mthNcAvg <- rotate(mean(stack(mthNc)))
              mthNcAvg <- writeRaster(mthNcAvg, outNcAvg, format='CDF', overwrite=T)
            } 
          }
          
          annualrun <- lapply(paste(yrNcAnomDirOut, "/", var, "_", 1:12, ".nc", sep=""), FUN=raster)
          annualrun <- mean(stack(annualrun))    
          annualrun <- writeRaster(annualrun, paste(yrNcAnomDirOut, "/", var, "_ann.nc", sep=""), format='CDF', overwrite=T)
        }
      }}}}

GCMAveragePreInd <- function(rcp='rcp85', baseline='historical', baseDir="T:/gcm/cmip5/raw/monthly", trunkDir='D:/_scripts/dapa-climate-change/IPCC-CMIP5/data',outDir='G:/cenavarro/Request/lparker') {
  
  require(maptools)
  require(raster)
  require(ncdf4)
  require(rgdal)
  require(sp)
  library(parallel)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  rasterTmpFile(paste0("D:/TEMP/")) 
  
  # Read gcm characteristics table
  gcmStats <- read.table(paste(trunkDir, "/cmip5-", baseline, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  gcmStatsRcp <- read.table(paste(trunkDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  
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
  
  dataMatrix_3 <- c("model", "ens", "rcp", 1850:2099)
  
  
  
  # sfExport("rcp")
  # sfExport("gcm")
  # sfExport("ens")
  # sfExport("futDir")
  # sfExport("curAvgDir")
  # sfExport("basePer")
  # sfExport("outDir")
  # 
  # Loop around gcms and ensembles
  #for (i in 60:nrow(gcmStatsRcp)){
  for (i in 1:1){
    # controlIntpol <- function(i) {
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-yr"){
        
        if(paste(as.matrix(gcmStatsRcp)[i,3]) == "r1i1p1"){
          
          # Get gcm and ensemble names
          gcm <- paste(as.matrix(gcmStatsRcp)[i,2])
          ens <- paste(as.matrix(gcmStatsRcp)[i,3])
          
          # Path of each ensemble
          ensDir <- paste(baseDir, "/", baseline, "/", gcm, "/", ens, sep="")
          
          # Directory with monthly splitted files
          mthDir <- paste(ensDir, "/monthly-files", sep="")
          
          # Create output average directory
          avgDir <- paste(ensDir, "/average", sep="")
          if (!file.exists(avgDir)) {dir.create(avgDir)}
          
          # Period list for historical and future pathways
          period <- "1850"
          
          # Define start and end year
          staYear <- as.integer(period)
          endYear <- as.integer(period) + 50
          
          cat("\nAverage over: ", baseline, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
          
          # Loop around variables
          #for (var in varList) {
          var <- "tmean"
          if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, sep=""))) 
          {dir.create(paste(avgDir, "/", staYear, "_", endYear, sep=""))}
          
          # Loop around months
          for (mth in monthList) {
            
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
                
                mthNcAvg <- rotate(mean(stack(mthNc)))# - 272.15
              }
              
              # Write output average NetCDF file
              mthNcAvg <- writeRaster(mthNcAvg, outNcAvg, format='CDF', overwrite=T)
              
              cat(" .> ", paste(var, "_", mthMod, sep=""), "\tdone!\n")
              
            } else {
              
              cat(" .>", paste(var, "_", mthMod, sep=""), "\tdone!\n")
              
              
            }
            
          }  
          
          cat("\nAverage over ", baseline, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," done! \n\n")
          
          cat("\nAnomalies over ", baseline, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
          
          
          
          ### Calculate anomalies
          
          
          for (yr in 1850:2099){
            
            mthFutDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files", sep="")
            
            if ((file.exists(paste(mthDir, "/", yr, "/", var, "_01.nc", sep=""))) || (file.exists(paste(mthFutDir, "/", yr, "/", var, "_01.nc", sep="")))) {
              
              yrNcAnomDirOut <- paste(outDir, "/anomalies/", rcp, "/", gcm, "/", ens, "/", yr, sep="")
              if (!file.exists(yrNcAnomDirOut)) {dir.create(yrNcAnomDirOut, recursive = TRUE)}
              
              if (!file.exists(paste(yrNcAnomDirOut, "/", var, "_ann.nc", sep=""))){
                for (mth in monthList) {
                  
                  mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
                  if (!file.exists(paste(yrNcAnomDirOut, "/", var, "_", mthMod, ".nc", sep=""))){
                    
                    mthNcAvg <- raster(paste(avgDir, "/", staYear, "_", endYear, "/", var, "_", mthMod, ".nc", sep=""))
                    
                    if (yr <= 2005){
                      
                      yrNc <- rotate(raster(paste(mthDir, "/", yr, "/", var, "_", mth, ".nc", sep="")))
                      
                    } else {
                      
                      
                      yrNc <- rotate(raster(paste(mthFutDir, "/", yr, "/", var, "_", mth, ".nc", sep="")))
                      
                    }
                    
                    
                    yrNcAnom <- yrNc - mthNcAvg
                    yrNcAnom <- writeRaster(yrNcAnom, paste(yrNcAnomDirOut, "/", var, "_", mthMod, ".nc", sep=""), format='CDF', overwrite=T)
                    
                  }
                }
                
                annualAnom <- lapply(paste(yrNcAnomDirOut, "/", var, "_", 1:12, ".nc", sep=""), FUN=raster)
                annualAnom <- mean(stack(annualAnom))
                annualAnom <- writeRaster(annualAnom, paste(yrNcAnomDirOut, "/", var, "_ann.nc", sep=""), format='CDF', overwrite=T)
                
              } else {
                
                annualAnom <- raster(paste(yrNcAnomDirOut, "/", var, "_ann.nc", sep=""))
                
              }
              
              
              yrNcAnomMean <- cellStats(annualAnom, stat='mean', na.rm=TRUE)
              
            } else{
              
              yrNcAnomMean <- "NA"
              
            }
            
            
            if (yr == "1850"){
              dataMatrix_2 <- c(gcm, ens, rcp, yrNcAnomMean)
              
            } else {
              dataMatrix_2 <- c(dataMatrix_2,yrNcAnomMean)
            }
            
            cat(" .>", yr, "\tdone!\n")
            
            
          }
          
          
          dataMatrix_3 <- rbind(dataMatrix_3,dataMatrix_2)
          
          
        }
      }
    }
    #}
    #system.time(sfSapply(as.vector(dates), controlIntpol))
  }
  
  
  write.csv(dataMatrix_3, paste(outDir, "/anomalies_1880s_part_3.csv", sep=""), row.names=F)
  cat("GCM Summary Done!")
}

x="X:/2C_Target/running_mean/rcp85/csiro_access1_0/r1i1p1/1851/tmean_1.nc"
plot(raster(x))


rcp <- "rcp85"
baseline <- "historical"
baseDir <- "L:/gcm/cmip5/raw/monthly"
trunkDir <- "G:/_scripts/dapa-climate-change/IPCC-CMIP5/data"
outDir <- "G:/cenavarro/Request/lparker"
cruDir <- "S:/observed/gridded_products/cru-ts-v3-21/20yr_averages/1980_2000"

GCMDisaggregate <- function(rcp='rcp85', baseDir="L:/gcm/cmip5/raw/monthly", trunkDir='G:/_scripts/dapa-climate-change/IPCC-CMIP5/data', outDir='G:/cenavarro/Request/lparker', cruDir="S:/data/observed/gridded_products/cru-ts-v3-21/20yr_averages/1980_2000") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXX GCM AVERAGE CALCULATION XXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  # Read gcm characteristics table
  gcmStatsRcp <- read.table(paste(trunkDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
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
  
  # Loop around gcms and ensembles
  for (i in 1:nrow(gcmStatsRcp)){
    
    # Don't include variables without all three variables
    if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStatsRcp)[i,10]) == "ins-yr"){
        
        if(paste(as.matrix(gcmStatsRcp)[i,3]) == "r1i1p1"){
          
          # Get gcm and ensemble names
          ens <- "r1i1p1"
          gcm <- paste(as.matrix(gcmStatsRcp)[i,2])
          
          hisDir <- paste(baseDir, "/historical/", gcm, "/", ens, "/monthly-files", sep="")
          futDir <- paste(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files", sep="")
          
          
          # Create output average directory
          avgDir <- paste(outDir, "/20yr_averages", sep="")
          if (!file.exists(avgDir)) {dir.create(avgDir)}
          
          # Define start and end year
          staYear <- "2056"
          endYear <- "2076"
          
          if (!file.exists(paste(avgDir, "/", staYear, "_", endYear, "/", gcm, sep=""))) 
          {dir.create(paste(avgDir, "/", staYear, "_", endYear, "/", gcm, sep=""), recursive=TRUE)}
          
          cat("\nAverage over: ", rcp, " ", gcm, " ", ens, " ", paste(staYear, "_", endYear, sep="")," \n\n")
          
          # Loop around variables
          for (var in varList) {
            
            # Loop around months
            for (mth in monthList) {
              
              # Define month without 0 in one digit number
              mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
              outAscAvg <- paste(avgDir, "/", staYear, "_", endYear, "/", gcm, "/", var, "_", mthMod, ".asc", sep="")
              
              if (!file.exists(outAscAvg)){
                
                # List of NetCDF files by month for all 30yr period
                futNc <- lapply(paste(futDir, "/", staYear:endYear, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
                hisNc <- lapply(paste(hisDir, "/", 1980:2000, "/", var, "_", mth, ".nc", sep=""), FUN=raster)
                
                futNc <- mean(stack(futNc))
                hisNc <- mean(stack(hisNc))
                
                
                # Create a stack of list of NC, rotate and convert units in mm/monnth and deg celsious
                if (var == "prec"){
                  
                  daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
                  mthNc <- futNc - hisNc
                  mthNcAvg <- rotate(mean(stack(mthNc))) * 86400 * (daysmth)
                  
                } else {
                  
                  mthNc <- futNc - hisNc
                  mthNcAvg <- rotate(mean(stack(mthNc)))
                }
                
                
                # rs <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, ncols=720, nrows=360)
                rs <- raster(xmn=-26, xmx=64, ymn=-47, ymx=38, ncols=180, nrows=170)
                mthNcAvg <- setExtent(mthNcAvg, extent(rs), keepres=FALSE, snap=TRUE)
                mthNcAvgRes  <- resample(mthNcAvg, rs, method='ngb')
                
                cruAsc <- raster(paste(cruDir, "/", var, "_", mthMod, ".asc", sep=""))
                cruAsc <- setExtent(cruAsc, extent(rs), keepres=FALSE, snap=TRUE)
                cruAscRes  <- resample(cruAsc, rs, method='ngb')
                
                dissAvg <- cruAscRes + mthNcAvgRes
                
                #                 dissAvg[][dissAvg[]<0]=0
                
                # Write output average NetCDF file
                dissAvgOut <- writeRaster(dissAvg, outAscAvg, format='ascii', overwrite=FALSE)
                
                cat(" .> ", paste(var, "_", mthMod, sep=""), "\tdone!\n")
                
              } else {cat(" .>", paste(var, "_", mthMod, sep=""), "\tdone!\n")}
              
              #               if (var == "tmin"){
              #                 
              #                 tmeanAsc <- raster(paste(avgDir, "/", staYear, "_", endYear, "/", gcm, "/tmax_", mthMod, ".asc", sep="")) - raster(paste(avgDir, "/", staYear, "_", endYear, "/", gcm, "/tmin_", mthMod, ".asc", sep=""))
              #                 tmeanAsc <- writeRaster(tmeanAsc, paste(avgDir, "/", staYear, "_", endYear, "/", gcm, "/tmean_", mthMod, ".asc", sep=""), format='ascii', overwrite=FALSE)
              #                 cat(" .>", paste("tmean_", mthMod, sep=""), "\tdone!\n")
              #               
              #               }
              
              
            }
          }
          
        }
      }
    }
    
  }
  
  
}



rcp <- "rcp85"
ens <- "r1i1p1"
baseline <- "historical"
baseDir <- "X:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/anomalies"
outDir <- "X:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/anomalies_africa"
otp <- "00-monthly-data-functions-preindustrial.R"
var <- "tmean"
region <- "X:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/anomalies_africa/_region/af0.shp"

GCM_Cut_Daily_Preindustrial <- function(baseDir="Y:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/anomalies", ens="r1i1p1", rcp="historical", xmin=-18, xmax=-11, ymin=12, ymax=17, staYr=1850, endYr=2100, outDir="D:/CIAT/Workspace/cmip5_daily_test", shift="yes") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX GCM CUT DAILY DATA CMIP5XXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  rcpDir <- paste(baseDir, "/", rcp, sep="")
  region <- shapefile(region)
  
  gcmList <- list.dirs(paste(rcpDir, sep=""), recursive = FALSE, full.names = FALSE)
  
  dataMatrix_3 <- c("model", "ens", "rcp", 1850:2099)
  
  for (gcm in gcmList) {
    
    model <- basename(gcm)
    
    #     yrList <- list.dirs(paste(gcm, "/", ens, sep=""), recursive = FALSE, full.names = FALSE)
    
    for (yrDir in 1850:2099) {
      
      nc <- paste(gcm, "/", ens, "/", yrDir, "/", var, "_ann.nc", sep="")
      outNcDir <- paste(outDir, "/", rcp, "/", model, "/", ens, "/", yrDir, sep="")
      ncName <- basename(nc)
      
      ## Cut
      
      if (!file.exists(paste(outNcDir))) {dir.create(outNcDir, recursive=TRUE)}
      
      if (file.exists(nc)) {
        outNc <- paste(outNcDir, "/", ncName, sep="")
        
        if (!file.exists(paste(outNc))) {
          
          ncCrop <- mask(raster(nc), region)
          ncDay <- writeRaster(ncCrop, outNc, format='CDF', overwrite=TRUE)
          cat("\t\t ", yrDir, " ", model, " \n")
          
        }
      }
    }
    
    cat("\n Cut nc files for: ", rcp, " ", model, " ", ens, " done!\n\n")
    
  }
  
  for (gcm in gcmList) {
    
    model <- basename(gcm)
    for (yrDir in 1850:2099){
      
      outNc <- paste(outDir, "/", rcp, "/", model, "/", ens, "/", yrDir, "/", var, "_ann.nc", sep="")
      
      if (file.exists(paste(outNc))) {
        
        yrNcAnomMean <- cellStats(raster(outNc), stat='mean', na.rm=TRUE)
        
      } else{
        
        yrNcAnomMean <- "NA"
        
      }
      
      
      if (yrDir == "1850"){
        
        dataMatrix_2 <- c(model, ens, rcp, yrNcAnomMean)
        
      } else {
        
        dataMatrix_2 <- c(dataMatrix_2,yrNcAnomMean)
      }
      
      cat(" .>", yrDir, "\tdone!\n")
      
      
    }
    
    
    dataMatrix_3 <- rbind(dataMatrix_3,dataMatrix_2)
    
    
  }
  
  write.csv(dataMatrix_3, paste(outDir, "/anomalies_1880s.csv", sep=""), row.names=F)
  cat("GCM Summary Done!")  
  
}

cat("GCM Cut Daily Process Done!")

