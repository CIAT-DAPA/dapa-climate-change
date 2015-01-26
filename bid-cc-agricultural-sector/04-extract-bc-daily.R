require(raster)
require(chron)

extractDailyValues <- function(var="rain"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_0_5deg_lat"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  gcmList <- list.dirs(iDir, recursive = FALSE, full.names = FALSE)
  
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  stFile <- read.csv(stFile)
  coordinates <- stFile[8:9]
  
  years.gcm.hist = 1971:2000
  years.gcm.fut = 2020:2049
  
  months = c(paste(0,1:9,sep=''),10:12)
  
  dates.gcm.hist = seq(as.Date('1971-01-01'),as.Date('2000-12-31'),by=1)
  dates.gcm.fut = seq(as.Date('2020-01-01'),as.Date('2049-12-31'),by=1)

  
  if (var == "tmax" || var == "tmin"){
    
    dataMatrix <- c()
    
    for (i in 1:length(gcmList)){
      
      for (j in 1:length(years.gcm.hist)){
        
        for(k in 1:12){
          
          cat(paste(gcmList[i], years.gcm.hist[j], months[k], "\n"))
          
          gcmHist <- stack(paste(iDir, "/", gcmList[i], "/1971_2000/by_month/", var, "_", years.gcm.hist[j], "_", months[k], ".nc",sep=""))
          value <- extract(gcmHist, coordinates)
          
          dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], years.gcm.hist[j], months[k], t(value)))
        }
        
      }
      
      for (j in 1:length(years.gcm.fut)){
        
        for(k in 1:12){
          
          cat(paste(gcmList[i], years.gcm.fut[j], months[k], "\n"))
          
          gcmFut <- stack(paste(iDir, "/", gcmList[i], "/2020_2049/by_month/", var, "_", years.gcm.fut[j], "_", months[k], ".nc",sep=""))
          value <- extract(gcmHist, coordinates)
          
          dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], dates.gcm.fut[j], months[k], t(value)))
        }
        
      }
      
    }
    
    colnames(databygcm) <- c("GCM", "Year", "Month", "Day",stFile$Cell_ID_prec_rsds)
    
    #   Writting csv file 
    write.csv(databygcm, paste(oDir, "/bc_daily_", var, ".csv", sep=""), row.names=F)
    
  } else {
    
    databygcmhist <- c()   
    cellsID <- stFile$Cell_ID_prec_rsds
    
    for (i in 1:length(gcmList)){
      
      gcmHist <- load(paste(iDir, "/", gcmList[i], "/1971_2000/bc_", var, "_1950_2000.Rdat",sep=""))
      
      #       dates <- names(dataMatrix[,4:length])
      
      Years <- format(as.Date(dates.gcm.hist), "%Y")
      Months <- format(as.Date(dates.gcm.hist), "%m")
      Days <- format(as.Date(dates.gcm.hist), "%d")
      
      databyid <- c()
      
      for(j in 1:length(cellsID)){
        
        selID <- dataMatrix[which(dataMatrix$CellID == cellsID[j]),]
        
        databyid <- cbind(databyid, t(selID[,7674:ncol(selID)]))
        
        cat(gcmList[i], cellsID[j], "\n")
      }
      
      databygcmhist <- rbind(databygcmhist, cbind(gcmList[i], Years, Months, Days, databyid))
      
    }
    
    colnames(databygcmhist) <- c("GCM", "Year", "Month", "Day", stFile$Cell_ID_prec_rsds)
    
    #   Writting csv file 
    write.csv(databygcmhist, paste(oDir, "/bc_daily_", var, "_hist.csv", sep=""), row.names=F)
    
#     
#     databygcmfut <- c()   
#     cellsID <- stFile$Cell_ID_prec_rsds
#     
#     for (i in 1:length(gcmList)){
#       
#       gcmHist <- load(paste(iDir, "/", gcmList[i], "/2020_2049/bc_", var, "_2020_2049.Rdat",sep=""))
#       
#       #       dates <- names(dataMatrix[,4:length])
#       
#       Years <- format(as.Date(dates.gcm.fut), "%Y")
#       Months <- format(as.Date(dates.gcm.fut), "%m")
#       Days <- format(as.Date(dates.gcm.fut), "%d")
#       
#       databyid <- c()
#       
#       for(j in 1:length(cellsID)){
#         
#         selID <- dataMatrix[which(dataMatrix$CellID == cellsID[j]),]
#         
#         databyid <- cbind(databyid, t(selID))
#         
#         cat(gcmList[i], cellsID[j], "\n")
#       }
#       
#       databygcmfut <- rbind(databygcmfut, cbind(gcmList[i], Years, Months, Days, databyid))
#       
#     }
#     
#     colnames(databygcmfut) <- c("GCM", "Year", "Month", "Day", stFile$Cell_ID_prec_rsds)
#     
#     #   Writting csv file 
#     write.csv(databygcmfut, paste(oDir, "/bc_daily_", var, "_hist.csv", sep=""), row.names=F)
#     
#     
    
  }
  
}


extractDailyValuesWFD <- function(var="prec"){
  
  #Arguments
  iDir <- "D:/CIAT/climate_change/wfd"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  stFile <- read.csv(stFile)
  
  if(!file.exists(paste(oDir, "/wfd_daily_", var, ".csv", sep=""))){
    
    if(var == "prec"){varmod <- "Rainf"}
    if(var == "rsds"){varmod <- "SWdown"}
    if(var == "tmax"){varmod <- "Tmax"}
    if(var == "tmin"){varmod <- "Tmin"}
    
    ncList <- list.files(iDir, recursive = FALSE, pattern=paste("lat_", varmod, "*", sep=""), full.names = FALSE)
    
    if (!file.exists(oDir)) {dir.create(oDir)}
    
    coordinates <- stFile[8:9]
    
    dataMatrix <- c()
    
    for (i in 1:length(ncList)){
      
      cat(ncList[i], "\n")
      
      dates <- read.csv(paste(iDir, "/dates/mth_", i, ".csv",sep=""))
      mthStk <- stack(paste(iDir, "/", ncList[i], sep=""))
      
      xmin(mthStk) = xmin(mthStk) - 360  #shift to proper longitude
      xmax(mthStk) = xmax(mthStk) - 360
      
      value <- extract(mthStk, coordinates)
      
      if (var == "prec"){value <- value * 86400} 
      if (var == "tmin"){value <- value - 273.15}
      if (var == "tmax"){value <- value - 273.15}
      
      dataMatrix <- rbind(dataMatrix, cbind(dates, t(value)))
      
    }
    
    colnames(dataMatrix) <- c("Year", "Month", "Day", stFile$Cell_ID_prec_rsds)
    
    #   Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/wfd_daily_", var, ".csv", sep=""), row.names=F)
    
  } else {
    
    outExt <- read.csv(paste(oDir, "/wfd_daily_", var, ".csv", sep=""))
    
    years.wfd.hist = 1971:2000
    
    dataMatrix <- c()
    
    for (j in 1:length(years.wfd.hist)){
      
      for(k in 1:12){
        
        selYr <- outExt[which(outExt$Year == years.wfd.hist[j]),]
        selMth <- selYr[which(selYr$Month == k),]
        
        if (var == "prec"){
          mthMean <- colSums(selMth[,4:length(selMth)], na.rm = FALSE, dims = 1)
        } else {
          mthMean <- colMeans(selMth[,4:length(selMth)], na.rm = FALSE, dims = 1)
        }
        
        dataMatrix <- rbind(dataMatrix, cbind(years.wfd.hist[j], k, t(mthMean)))
        
        cat(years.wfd.hist[j], k)
        
      }
    }
    
    colnames(dataMatrix) <- c("Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/wfd_monthly_", var, ".csv", sep=""), row.names=F)
    
  }

}


aggregateDailyValuesGCM <- function(var="prec"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_0_5deg_lat"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  stFile <- read.csv(stFile)

  years.wfd.hist = c(1971:2000, 2020:2049)
  
  months = c(paste(0,1:9,sep=''),10:12)
  
  gcmList <- list.dirs(iDir, recursive = FALSE, full.names = FALSE)
  
  if(!file.exists(paste(oDir, "/bc_monthly_", var, ".csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/bc_daily_", var, ".csv", sep=""))
    
    dataMatrix <- c()
    
    for (i in 1:length(gcmList)){
      
      selGCM <- outExt[which(outExt$GCM == gcmList[i]),]
      
      for (j in 1:length(years.wfd.hist)){
        
        selYr <- selGCM[which(selGCM$Year == years.wfd.hist[j]),]
        
        for(k in 1:12){
          
          selMth <- selYr[which(selYr$Month == k),]
          
          if (var == "prec"){
            mthMean <- colSums(selMth[,4:length(selMth)], na.rm = FALSE, dims = 1)
          } else {
            mthMean <- colMeans(selMth[,4:length(selMth)], na.rm = FALSE, dims = 1)
          }
          
          dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], years.wfd.hist[j], k, t(mthMean)))
          
          cat(gcmList[i], years.wfd.hist[j], k, "\n")
          
        }
      }
    }
    
    colnames(dataMatrix) <- c("GCM","Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/bc_monthly_", var, ".csv", sep=""), row.names=F)
  }
  
  
  
}

