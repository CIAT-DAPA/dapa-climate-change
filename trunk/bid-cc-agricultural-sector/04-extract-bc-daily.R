require(raster)

extractDailyValues <- function(var="prec"){
  
  #Arguments
  iDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  gcmList <- list.dirs(iDir, recursive = FALSE, full.names = FALSE)
  gcmList <- c(gcmList[1:14], gcmList[16:20])
  
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
    
    
    if (!file.exists(paste(oDir, "/bc_daily_", var, "_hist.csv", sep=""))){
      
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
    }
    
    
    
    if (!file.exists(paste(oDir, "/bc_daily_", var, "_fut.csv", sep=""))){
      
      databygcmfut <- c()   
      cellsID <- stFile$Cell_ID_prec_rsds
      
      Years <- format(as.Date(dates.gcm.fut), "%Y")
      Months <- format(as.Date(dates.gcm.fut), "%m")
      Days <- format(as.Date(dates.gcm.fut), "%d")
      
      for (i in 1:length(gcmList)){
        
        gcmFut <- load(paste(iDir, "/", gcmList[i], "/2020_2049/bc_", var, "_2020_2049.Rdat",sep=""))
        
        #       dates <- names(dataMatrix[,4:length])
        
        databyid <- c()
        
        for(j in 1:length(cellsID)){
          
          selID <- dataMatrixFut[which(dataMatrixFut$CellID == cellsID[j]),]
          
          databyid <- cbind(databyid, t(selID[,4:ncol(selID)]))
          
          cat(gcmList[i], cellsID[j], "\n")
        }
        
        databygcmfut <- rbind(databygcmfut, cbind(gcmList[i], Years, Months, Days, databyid))
        
      }
      
      colnames(databygcmfut) <- c("GCM", "Year", "Month", "Day", stFile$Cell_ID_prec_rsds)
      
      #   Writting csv file 
      write.csv(databygcmfut, paste(oDir, "/bc_daily_", var, "_fut.csv", sep=""), row.names=F)
    }
    
    
  }
  
}


extractDailyValuesGCMRaw <- function(var="tasmin"){
  #Arguments
  require(raster)
  require(ncdf)
  require(rgdal)
  
  var <- "tasmin"
  iDir <- "D:/cenavarro/bid/gcm_raw_res"
  stFile <- "W:/bid-cc-agricultural-sector/01-climate-data/sample_type_climates.csv"
  oDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_extracts"
  
  gcmList <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "cnrm_cm5", "csiro_mk3_6_0", "gfld_esm2g", "gfld_esm2m", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "ipsl_cm5b_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_cc", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncc_noresm1_m")
  
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  stFile <- read.csv(stFile)
  coordinates <- stFile[8:9]
  
  years.gcm.hist = 1971:2000
  years.gcm.fut = 2020:2049
  
  months = c(paste(0,1:9,sep=''),10:12)
  
  dates.gcm.hist = seq(as.Date('1971-01-01'),as.Date('2000-12-31'),by=1)
  dates.gcm.fut = seq(as.Date('2020-01-01'),as.Date('2049-12-31'),by=1)
  
  if (var == "tasmax"){varmod <- "tmax"}
  if (var == "tasmin"){varmod <- "tmin"}
  if (var == "pr"){varmod <- "prec"}
  if (var == "rsds"){varmod <- "rsds"}
  
  dataMatrix <- c()
  
  for (i in 8:20){
    
    for (j in 1:length(years.gcm.hist)){
      
      for(k in 1:12){
        
        cat(paste(gcmList[i], years.gcm.hist[j], months[k], "\n"))
        
        gcmHist <- stack(paste(iDir, "/", gcmList[i], "/1950_2000/by-month/", var, "_", years.gcm.hist[j], "_", months[k], ".nc",sep=""))
        
        xmin(gcmHist) <- xmin(gcmHist)-360
        xmax(gcmHist) <- xmax(gcmHist)-360
        
        value <- extract(gcmHist, coordinates)
        value <- value - 273.15
        
        dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], years.gcm.hist[j], months[k], t(value)))
      }
      
    }
    
    for (j in 1:length(years.gcm.fut)){
      
      for(k in 1:12){
        
        cat(paste(gcmList[i], years.gcm.fut[j], months[k], "\n"))
        
        gcmFut <- stack(paste("W:/bid-cc-agricultural-sector/01-climate-data/gcm_raw_res", "/", gcmList[i], "/2020_2049/by-month/", var, "_", years.gcm.fut[j], "_", months[k], ".nc",sep=""))
        
        xmin(gcmFut) <- xmin(gcmFut)-360
        xmax(gcmFut) <- xmax(gcmFut)-360
        
        value <- extract(gcmFut, coordinates)
        value <- value - 273.15
        
        dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], years.gcm.fut[j], months[k], t(value)))
      }
      
    }
    
  }
  
  colnames(dataMatrix) <- c("GCM", "Year", "Month", stFile$Cell_ID_prec_rsds)
  
  #   Writting csv file 
  write.csv(dataMatrix, paste(oDir, "/gcm_raw_daily_", varmod, ".csv", sep=""), row.names=F)
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


aggregateDailyValuesGCM <- function(var="tmax"){
  
  #Arguments
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  stFile <- read.csv(stFile)
  
  if(!file.exists(paste(iDir, "/bc_monthly_", var, ".csv", sep=""))){
    
    outExt <- read.csv(paste(iDir, "/bc_daily_", var, ".csv", sep=""))   
    
    if (var == "prec"){
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN="sum")
      
      outExt[outExt == 0] <- NA
      fun <- function(x) { sd(x, na.rm=T) }
      mthStd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN=fun)
      
    } else {
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN="mean")
      
      fun <- function(x) { sd(x) }
      mthStd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year, outExt$Month), FUN=fun)
      
    }

    colnames(mthMean) <- c("GCM","Year", "Month", stFile$Cell_ID_prec_rsds)
    colnames(mthStd) <- c("GCM","Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(mthMean, paste(iDir, "/bc_monthly_", var, ".csv", sep=""), row.names=F)
    write.csv(mthStd, paste(iDir, "/bc_monthly_", var, "_std.csv", sep=""), row.names=F)
    
  }
    
}


aggregateDailyValuesGCMRaw <- function(var="tmin"){
  
  #Arguments
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  stFile <- read.csv(stFile)
  
  if(!file.exists(paste(iDir, "/gcm_raw_monthly_", var, ".csv", sep=""))){
    
    outExt <- read.csv(paste(iDir, "/gcm_raw_daily_", var, ".csv", sep=""))   
    
    if (var == "prec"){
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN="sum")
      
      outExt[outExt == 0] <- NA
      fun <- function(x) { sd(x, na.rm=T) }
      mthStd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN=fun)
      
    } else {
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN="mean")
      
      fun <- function(x) { sd(x) }
      mthStd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$GCM, outExt$Year,outExt$Month), FUN=fun)
      
    }
    
    colnames(mthMean) <- c("GCM","Year", "Month", stFile$Cell_ID_prec_rsds)
    colnames(mthStd) <- c("GCM","Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(mthMean, paste(iDir, "/gcm_raw_monthly_", var, ".csv", sep=""), row.names=F)
    write.csv(mthStd, paste(iDir, "/gcm_raw_monthly_", var, "_std.csv", sep=""), row.names=F)
    
  }
  
}


aggregateDailyValuesWFD <- function(var="rsds"){
  
  #Arguments
  stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  stFile <- read.csv(stFile)
  
  if(!file.exists(paste(iDir, "/wfd_monthly_", var, ".csv", sep=""))){
    
    outExt <- read.csv(paste(iDir, "/wfd_daily_", var, ".csv", sep=""))
    
    if (var == "prec"){
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$Year,outExt$Month), FUN="sum")
    } else {
      mthMean <- aggregate(outExt[,4:length(outExt)], by=list(outExt$Year,outExt$Month), FUN="mean")
    }
    
    fun <- function(x) { sd(x) }
    mthStd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$Year,outExt$Month), FUN=fun)
    
    
    colnames(mthMean) <- c("Year", "Month", stFile$Cell_ID_prec_rsds)
    colnames(mthStd) <- c("Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(mthMean, paste(iDir, "/wfd_monthly_", var, ".csv", sep=""), row.names=F)
    write.csv(mthStd, paste(iDir, "/wfd_monthly_", var, "_std.csv", sep=""), row.names=F)
    
  }
  
}


countZeros <- function(var="tmax"){
  
  #Arguments
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  years = c(1971:2000, 2020:2049)
  
  months = c(paste(0,1:9,sep=''),10:12)
  
  gcmList <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "cnrm_cm5", "csiro_mk3_6_0", "gfld_esm2g", "gfld_esm2m", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "ipsl_cm5b_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncc_noresm1_m")
    
  if(!file.exists(paste(oDir, "/bc_monthly_", var, "_freq.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/bc_daily_", var, ".csv", sep=""))
    
    dataMatrix <- c()
    
    for (i in 1:length(gcmList)){
      
      selGCM <- outExt[which(outExt$GCM == gcmList[i]),]
      
      for (j in 1:length(years)){
        
        selYr <- selGCM[which(selGCM$Year == years[j]),]
        
        for(k in 1:12){
          
          selMth <- selYr[which(selYr$Month == k),]
          
          if (var == "prec"){
            mthZeros <- colSums(selMth[,5:length(selMth)] >= 1, na.rm=T)
            stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
            
          } else {
            mthZeros <- colSums(selMth[,4:length(selMth)] >= 30, na.rm=T)
            stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
          }
          
          dataMatrix <- rbind(dataMatrix, cbind(gcmList[i], years[j], k, t(mthZeros)))
          
          cat(gcmList[i], years[j], k, "\n")
          
        }
      }
    }
    
    stFile <- read.csv(stFile)
    
    colnames(dataMatrix) <- c("GCM", "Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/bc_monthly_", var, "_freq.csv", sep=""), row.names=F)
    
  }
  
}


countZerosWfd <- function(var="tmax"){
  
  #Arguments
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  years = c(1971:2000)
  
  months = c(paste(0,1:9,sep=''),10:12)
  
  if(!file.exists(paste(oDir, "/wfd_monthly_", var, "_freq.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/wfd_daily_", var, ".csv", sep=""))
    
    dataMatrix <- c()
    
    for (j in 1:length(years)){
      
      selYr <- outExt[which(outExt$Year == years[j]),]
      
      for(k in 1:12){
        
        selMth <- selYr[which(selYr$Month == k),]
        
        if (var == "prec"){
          mthZeros <- colSums(selMth[,4:length(selMth)] != 0, na.rm=T)
          stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
          
        } else {
          mthZeros <- colSums(selMth[,4:length(selMth)] > 30, na.rm=T)
          stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
        }
        
        dataMatrix <- rbind(dataMatrix, c(years[j], k, as.vector(mthZeros)))
        
        cat(years[j], k, "\n")
        
      }
    }
    
    
    stFile <- read.csv(stFile)
    
    colnames(dataMatrix) <- c("Year", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/wfd_monthly_", var, "_freq.csv", sep=""), row.names=F)
    
  }
  
}


30yrStd <- function(var="tmin"){
  
  #Arguments
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  if(!file.exists(paste(oDir, "/bc_monthly_", var, "_std_30yrfut.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/bc_monthly_", var, ".csv", sep=""))
    outExt[outExt == 0] <- NA
    
    selYrHist <- outExt[which(1971 <= outExt$Year & outExt$Year <= 2000),]
    selYrFut <- outExt[which(2020 <= outExt$Year & outExt$Year <= 2049),]
    
    fun <- function(x) { sd(x) }
    
    stdHist <- aggregate(selYrHist[,4:length(selYrHist)], by=list(selYrHist$GCM, selYrHist$Month), FUN=fun)
    stdFut <- aggregate(selYrFut[,4:length(selYrFut)], by=list(selYrFut$GCM, selYrFut$Month), FUN=fun)
    
    if (var == "prec"){
      stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
    } else {
      stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates.csv"
    }
    
    stFile <- read.csv(stFile)
    
    colnames(stdHist) <- c("GCM", "Month", stFile$Cell_ID_prec_rsds)
    colnames(stdFut) <- c("GCM", "Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(stdHist, paste(oDir, "/bc_monthly_", var, "_std_30yrhis.csv", sep=""), row.names=F)
    write.csv(stdFut, paste(oDir, "/bc_monthly_", var, "_std_30yrfut.csv", sep=""), row.names=F)
    
  }
  
}


30yrStdWfd <- function(var="rsds"){
  
  #Arguments
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  if(!file.exists(paste(oDir, "/wfd_monthly_", var, "_std_30yr.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/wfd_monthly_", var, ".csv", sep=""))
    
    fun <- function(x) { sd(x) }
    std30yrWfd <- aggregate(outExt[,4:length(outExt)], by=list(outExt$Month), FUN=fun)
    
    stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
    stFile <- read.csv(stFile)
    
    colnames(std30yrWfd) <- c("Month", stFile$Cell_ID_prec_rsds)
    
    # Writting csv file 
    write.csv(std30yrWfd, paste(oDir, "/wfd_monthly_", var, "_std_30yr.csv", sep=""), row.names=F)
    
  }
  
}


changesGCMTemp <- function(var="tmin"){
  
  #Arguments
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  years = c(2020:2049)
  months = c(paste(0,1:9,sep=''),10:12)


  if(!file.exists(paste(oDir, "/bc_monthly_", var, "_chg.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/bc_monthly_", var, ".csv", sep=""))
    outExt <- outExt[which(2020 <= outExt$Year & outExt$Year <= 2049),]

    bsl <- read.csv(paste(oDir, "/bc_monthly_", var, "_bsl.csv", sep=""))
    
    dataMatrix <- c()
    
    for (i in 1:nrow(outExt)){
     
      gcm_yr <- outExt[i,1:3]
      mth <- as.numeric(outExt[i,3])
      
      values_bc <- as.numeric(outExt[i,4:length(outExt)])
      values_bsl <- as.numeric(bsl[,mth+3])
      values_chg <- values_bc - values_bsl
      
      dataMatrix <- rbind(dataMatrix, cbind(gcm_yr, t(values_chg)))
    }
    
    colnames(dataMatrix) <- c("GCM", "Year","Month", bsl$CellID)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/bc_monthly_", var, "_chg.csv", sep=""), row.names=F)
    
  }
  
  
}


changesGCM <- function(var="prec"){
  
  #Arguments
  iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_0_5deg_lat"
  oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_extracts"
  
  years = c(2020:2049)
  months = c(paste(0,1:9,sep=''),10:12)
 
  if(!file.exists(paste(oDir, "/bc_monthly_", var, "_chg_mod.csv", sep=""))){
    
    outExt <- read.csv(paste(oDir, "/bc_monthly_", var, ".csv", sep=""))
    outExt <- outExt[which(2020 <= outExt$Year & outExt$Year <= 2049),]
    
    dataMatrix <- c()
    
    for (i in 1:nrow(outExt)){
      
      gcm <- as.character(outExt[i,1])
      gcm_yr <- outExt[i,1:3]
      mth <- as.numeric(outExt[i,3])
      
      values_bc <- as.numeric(outExt[i,4:length(outExt)])
      
      bsl <- read.csv(paste(iDir,"/", gcm,  "/1971_2000/bsl_", var, ".csv", sep=""))
      values_bsl <- as.numeric(bsl[,mth+3])
      bslmod <- bsl[bsl < 1] == 1
      
      if (var == "prec"){
        values_chg <- values_bc - values_bsl
      } else{
        values_chg <- values_bc - values_bsl
      }
      
      
      dataMatrix <- rbind(dataMatrix, cbind(gcm_yr, t(values_chg)))
    }
    
    colnames(dataMatrix) <- c("GCM", "Year","Month", bsl$CellID)
    
    # Writting csv file 
    write.csv(dataMatrix, paste(oDir, "/bc_monthly_", var, "_chg_mod.csv", sep=""), row.names=F)
    
  }
  
  
}
