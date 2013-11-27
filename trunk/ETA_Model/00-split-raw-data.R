#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
# Date: November 2013
#-----------------------------------------------------------------------

# baseDir="U:/data/rcm/eta/raw/hadcm_40km"
# xmin=-81.999995
# xmax=-24.400283
# ymin=-49.200006
# ymax=11.199692
# outDir="U:/data/rcm/eta/processed/sa_20min"
# shift="yes"
# source("00-split-daily.R")
# otp <- ETA_Cut_Hourly(baseDir, xmin, xmax, ymin, ymax, outDir, shift)

# baseDir="U:/data/rcm/eta/processed/sa_20min/hourly"
# scen="sres_a1b"
# outDir="U:/data/rcm/eta/processed/sa_20min/monthly"
# source("00-split-daily.R")
# otp <- ETA_Calcs_Monthly(baseDir, scen, outDir)

# baseDir="U:/data/rcm/eta/processed/sa_20min/hourly"
# scen="sres_a1b"
# outDir="U:/data/rcm/eta/processed/sa_20min/monthly"
# source("00-split-daily.R")
# otp <- ETA_Calcs_Daily(baseDir, scen, outDir)

# baseDir="U:/data/rcm/eta/processed/sa_20min/monthly"
# scen="historical"
# outDir="U:/data/rcm/eta/processed/sa_20min/30yrAverage"
# source("00-split-daily.R")
# otp <- ETA_Calcs_30yrAvg(baseDir, scen, outDir)

# baseDir="U:/data/rcm/eta/processed/sa_20min/30yrAverage"
# scen="historical "
# outDir="S:/data/portals/ccafs_climate/download_data/files/data/eta/eta_south_america"
# source("00-split-daily.R")
# otp <- ETA_Zip_Asciis(baseDir, scen, outDir)



ETA_Cut_Hourly <- function(baseDir="U:/rcm/eta/raw/hadcm_40km", xmin=-81.999995, xmax=-24.400283, ymin=-49.200006, ymax=11.199692, outDir="U:/rcm/eta/processed/sa_20min", shift="yes") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX ETA CUT HOURLY XXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  ## Convert to NetCDF (multiband) file format and cut lateral boundaries
  
  gcmList <- list.dirs(paste(baseDir, sep=""), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {

    modName <- tolower((basename(gcm)))
    
    scenList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)
  
    for (scen in scenList) {
      
      scenName <- sapply(strsplit(basename(scen), '[_]'), "[[", 5)
      if (scenName == "futuro"){scenName <- "sres_a1b"} else {scenName <- "historical"}
      
      cat("\n Cut ETA Daily files for: ", scenName, " ", modName, "\n\n")
      
      ctlList <- list.files(paste(scen, "/", sep=""), full.names = TRUE, pattern=".ctl")
      
      procDir <- paste(scen, "/temp", sep="")
      if (!file.exists(paste(procDir))) {dir.create(procDir, recursive=TRUE)}
      
      cat("\n .> Corverting grads files to NetCDF format \n\n")
      
      for (ctl in ctlList) {
        
        ctlName <- sapply(strsplit(basename(ctl), '[.]'), "[[", 1)
        
        outNc <- paste(scen, "/", ctlName, ".nc", sep="")
        if (!file.exists(outNc)){
          
          ## Corvert grads files to NetCDF format
          system(paste("cdo -f nc import_binary ", ctl, " ", procDir, "/", ctlName, ".nc", sep=""))
          
          ## Cut borders 
          system(paste("cdo sellonlatbox,", xmin, ",", xmax, ",", ymin, ",", ymax, " ", procDir, "/", ctlName, ".nc", " ", outNc , sep=""))
          
          cat("\t .> ", ctlName, " converted\n")
          
        }
      }
      
      # Remove temporal folder
      unlink(procDir, recursive=TRUE)
      
    
      if (shift == "yes"){
        
        cat("\n .> Split NetCDF files in daily data \n\n")
        
        outNcDir <- paste(outDir, "/", scenName, "/", modName, sep="")
        if (!file.exists(paste(outNcDir))) {dir.create(outNcDir, recursive=TRUE)}
        
        ncList <- list.files(paste(scen, "/", sep=""), full.names = TRUE, pattern=".nc")
        for (nc in ncList) {
            
          ncName <- sapply(strsplit(basename(nc), '[.]'), "[[", 1)          
          var <- sapply(strsplit(ncName, '[_]'), "[[", 3)
          if (var == "tp2m"){var <- "tmean"} else if (var == "mntp"){var <- "tmin"} else if (var == "mxtp"){var <- "tmax"}
          
          staYear <- substr(sapply(strsplit(ncName, '[_]'), "[[", 4), 10, 13)
          endYear <- as.character(as.numeric(staYear) + 30)
          
          verFile <- paste(outDir, "/", scenName, "/", modName, "_", var, "_", staYear, "_", endYear, "_split_daily_done.txt", sep="")      
          if (!file.exists(verFile)){

            
            ## Split in yearly files
            
            system(paste("cdo splityear ", nc, " ", outNcDir, "/",  var, "_", sep=""))          
            ncYrList <- list.files(outNcDir, full.names = TRUE, pattern=".nc")

            for (ncYr in ncYrList) {
              
              year <- substr(sapply(strsplit(basename(ncYr), '[_]'), "[[", 2), 1, 4)
              
              if (!file.exists(paste(outNcDir, "/", year, "/", var, "_123118.nc", sep=""))) {
                
                cat("\t .> Split ", var, " ", year, " \n")
                
                if (!file.exists(paste(outNcDir, "/", year, sep=""))) {dir.create(paste(outNcDir, "/", year, sep=""))}
                
                
                system(paste("cdo splitmon ", ncYr, " ", outNcDir, "/",  year, "/", var, "_", sep=""))
                
                ncMthList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", sep=""))
                for (ncMth in ncMthList) {
                  
                  month <- substr(sapply(strsplit(basename(ncMth), '[_]'), "[[", 2), 1, 2)
                  
                  system(paste("cdo splitday ", ncMth, " ", outNcDir, "/",  year, "/", var, "_", month, sep=""))
                  file.remove(ncMth)
                
                }
                
                ncDayList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", sep=""))
                
                for (ncDay in ncDayList) {
                  
                  month <- substr(sapply(strsplit(basename(ncDay), '[_]'), "[[", 2), 1, 2)
                  day <- substr(sapply(strsplit(basename(ncDay), '[_]'), "[[", 2), 3, 4)
                  
                  system(paste("cdo splithour ", ncDay, " ", outNcDir, "/",  year, "/", var, "_", month, day, sep=""))
                  file.remove(ncDay)
    

                  }
              
                  
                if (year == "2040" && endYear == "2040"){
                  
                  ncHourList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", sep=""))
                  for (ncHour in ncHourList) {file.remove(ncHour)}
                }
                      
                if (year == "2070" && endYear == "2070"){
                  
                  ncHourList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", sep=""))
                  for (ncHour in ncHourList) {file.remove(ncHour)}
                  
                }
                
                file.remove(ncYr)
                
              } 
              
            }
            
            cat("\n .> Cut ETA Daily files for: ", scenName, " ", modName, " ", var, " ", staYear, "_", endYear," done!\n\n")
            
            opnFile <- file(verFile, open="w")
            cat(paste("ETA Cut daily ", scenName, " ", modName, " for extent ", xmin, ",", xmax, ",", ymin, ",", ymax, " was processed on ", date(), sep=""), file=opnFile)
            close.connection(opnFile)
            
          } else {cat("\n .> Cut ETA Daily files for: ", scenName, " ", modName, " ", var, " ", staYear, "_", endYear, " done!\n\n")}
          
        } 
        
      }

    }
  }
  
  cat("ETA Cut Daily Process Done!")
  
  
}

ETA_Calcs_Monthly <- function(baseDir="U:/data/rcm/eta/processed/sa_20min", scen="historical", outDir="U:/data/rcm/eta/processed/sa_20min/monthly") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX ETA CALCS MONTHLY XXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  varList <- c("prec", "tmax", "tmin", "tmean")
  monthListMod <- c(1:12)
  
  # Set number of days by month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  gcmList <- list.dirs(paste(baseDir, "/", scen,  sep=""), recursive = FALSE, full.names = FALSE)
  for (gcm in gcmList) {
    
    modName <- tolower(basename(gcm))
    
    yrList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)
    
    
    for (yr in yrList) {
      
      year <- basename(yr)
      
      cat("\n Avg ETA Monthly files for: ", scen, " ", modName, " ", year, "\n\n")
      
      outNcDir <- paste(outDir, "/", scen, "/", modName, "/", year, sep="")
      if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
      
      for (var in varList){
        
        for (mth in monthList){
          
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          outNc <- paste(outNcDir, "/",  var, "_", mthMod, ".nc", sep="")
          
          if (!file.exists(paste(outNc, sep=""))) {
            
            ncMthVarList <- list.files(paste(yr, "/", sep=""), full.names = TRUE, pattern=paste(var, "_", mth, sep=""))  
            
            if (!length(ncMthVarList) == 0){
              
              if (var == "prec"){
                ncMthStack <- sum(stack(lapply(ncMthVarList, FUN=raster)), na.rm = TRUE) * 1000
              } else {
                ncMthStack <- mean(stack(lapply(ncMthVarList, FUN=raster)), na.rm = TRUE) - 272.15
              }
              
              ncMth <- writeRaster(ncMthStack, outNc, format='CDF', overwrite=TRUE)
            
              cat("\n .> Average : ", var, " ",  mth," done! \n")
              
            }
            
          } 
        }
      }
      
    }
  }
  
  cat("ETA Calcs Monthly Process Done!")
  
  
}

ETA_Calcs_Daily <- function(baseDir="U:/data/rcm/eta/processed/sa_20min/hourly", scen="sres_a1b", outDir="U:/data/rcm/eta/processed/sa_20min/daily") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX ETA CALCS DAILY XXxxXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  varList <- c("prec", "tmax", "tmin", "tmean")
  monthListMod <- c(1:12)
  
  gcmList <- list.dirs(paste(baseDir, "/", scen,  sep=""), recursive = FALSE, full.names = FALSE)
  for (gcm in gcmList[3:4]) {
    
    modName <- tolower(basename(gcm))
    
    yrList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)
    
    
    for (yr in yrList) {
      
      year <- basename(yr)
      
      # Set number of days by month
      if (leap.year(as.numeric(year)) == FALSE){
        ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)  
      } else{
        ndays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
      }
      
      # Combirn number of month and days in one single data frame
      ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
      names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
      
      
      cat("\n Avg ETA Daily files for: ", scen, " ", modName, " ", year, "\n\n")
      
      outNcDir <- paste(outDir, "/", scen, "/", modName, "/", year, sep="")
      if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
      
      for (var in varList){
        
        for (mth in monthList){
          
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          days <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
          
          for (day in 1:days){
            
            if (day < 10) {day <- paste(0, day, sep="")}
            
            outNc <- paste(outNcDir, "/",  var, "_", mth, day, ".nc", sep="")
            
            if (!file.exists(paste(outNc, sep=""))) {
              
              ncDayVarList <- list.files(paste(yr, "/", sep=""), full.names = TRUE, pattern=paste(var, "_", mth, day, sep=""))  
              
              if (!length(ncDayVarList) == 0){
                
                if (var == "prec"){
                  ncDayStack <- sum(stack(lapply(ncDayVarList, FUN=raster)), na.rm = TRUE) * 1000
                } else {
                  ncDayStack <- mean(stack(lapply(ncDayVarList, FUN=raster)), na.rm = TRUE) - 272.15
                }
                
                ncDay <- writeRaster(ncDayStack, outNc, format='CDF', overwrite=TRUE)
                
                cat("\n .> Average : ", var, " ",  mth, day, " done!")
                
              }
              
            } 
          }
        }
      }
      
    }
  }
  
  cat("ETA Calcs Monthly Process Done!")
  
  
}

ETA_Calcs_30yrAvg <- function(baseDir="U:/data/rcm/eta/processed/sa_20min/monthly", scen="sres_a1b", outDir="U:/data/rcm/eta/processed/sa_20min/30yrAverage") {
  
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX ETA CALCS 30YR AVG XXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  varList <- c("prec", "tmax", "tmin", "tmean")
  monthListMod <- c(1:12)
  
  # Set number of days by month
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  gcmList <- list.dirs(paste(baseDir, "/", scen,  sep=""), recursive = FALSE, full.names = FALSE)
  for (gcm in gcmList) {
    
    modName <- tolower(basename(gcm))
    
    
    if (scen == "historical"){
      staYear <- "1960"
      endYear <- "1989"
            
      outNcDir <- paste(outDir, "/", scen, "/", modName, "/", staYear, "_1990", sep="")
      if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
      
      cat("\n Avg ETA 30yrAvg files for: ", scen, " ", modName, " ", staYear, "-", endYear, "\n\n")
      varList <- c("tmax", "tmin")
      for (var in varList){
        
        for (mth in monthList){
          
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          outAsc <- paste(outNcDir, "/",  var, "_", mthMod, ".asc", sep="")
          
          if (!file.exists(paste(outAsc, sep=""))) {
            
            ncYrList <- paste(gcm, "/", staYear:endYear, "/", var, "_", mthMod, ".nc", sep="")
            ncMthStack <- mean(stack(lapply(ncYrList, FUN=raster)), na.rm = TRUE)
            
            ncMth <- writeRaster(ncMthStack, outAsc, format='ascii', overwrite=TRUE)
            
            cat("\n .> Average : ", var, " ",  mth," done! \n")
            
          } else{cat("\n .> Average : ", var, " ",  mth," done! \n")}
          
        }
      }
      
    } else {
      
      periodList  <- c("2010", "2040", "2070")
    
      for (period in periodList) {
        
        # Define start and end year
        staYear <- as.integer(period)
        endYear <- as.integer(period) + 29
        
        outNcDir <- paste(outDir, "/", scen, "/", modName, "/", staYear, "_", endYear, sep="")
        
        if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
        
        if (period == "2070") {endYear <- as.integer(period) + 28}
        
        cat("\n Avg ETA 30yrAvg files for: ", scen, " ", modName, " ", staYear, "-", endYear, "\n\n")
        
        for (var in varList){
          
          if (file.exists(paste(gcm, "/", staYear, "/", var, "_1.nc", sep=""))) {
              
            for (mth in monthList){
              
              mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
              outAsc <- paste(outNcDir, "/",  var, "_", mthMod, ".asc", sep="")
              
              if (!file.exists(paste(outAsc, sep=""))) {
                
                ncYrList <- paste(gcm, "/", staYear:endYear, "/", var, "_", mthMod, ".nc", sep="")
                ncMthStack <- mean(stack(lapply(ncYrList, FUN=raster)))
              
                ncMth <- writeRaster(ncMthStack, outAsc, format='ascii', overwrite=TRUE)
                
                cat(" .> Average : ", var, " ",  mth," done! \n")
                
              } else {cat(" .> Average : ", var, " ",  mth," done! \n")}
            }
          }
        }
      }
    }
  }
  
  cat("ETA Calcs 30yrAveragees Process Done!")
  
}

ETA_Zip_Asciis <- function(baseDir="U:/data/rcm/eta/processed/sa_20min/30yrAverage", scen="sres_a1b", outDir="S:/data/portals/ccafs_climate/download_data/files/data/eta/eta_south_america") {

  gcmList <- list.dirs(paste(baseDir, "/", scen,  sep=""), recursive = FALSE, full.names = FALSE)
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXX ETA PUBLISH DATA XXXXXXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")

  for (gcm in gcmList) {
    
    modName <- tolower(basename(gcm))
    res <- "20min"
    region <- "sa"
    periodList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)
    
    for (period in periodList) {
      
      if (scen == "historical"){scenName <- "baseline"} else {scenName <- scen}
      
      if (basename(period) == "1960_1990") {periodName <- "1970s"
      } else if (basename(period) == "2010_2039") {periodName <- "2020s"
      } else if (basename(period) == "2040_2069") {periodName <- "2050s"
      } else if (basename(period) == "2070_2099") {periodName <- "2080s"}

      cat("\n Publish files from: ", scenName, " ", modName, " ", periodName, "\n\n")
      
      ascList <- list.files(paste(period, sep=""), full.names = TRUE, pattern=".asc")
  
      for (asc in ascList) {
        
        var <- sapply(strsplit(basename(asc), '[_]'), "[[", 1)
        
        outZipDir <- paste(outDir, "/", scenName, "/", periodName, "/", modName, "/", res, sep="")
        outZipName <- paste(modName, "_", scenName, "_", periodName, "_", var, "_", res, "_", region, "_asc.zip", sep="")
        
        if (!file.exists(paste(outZipDir, "/", outZipName, sep=""))) {
          
        system(paste("7za a ", outZipDir, "/", outZipName, " ", asc, sep=""))
        
        cat(" .> Compress: ", basename(asc), " done! \n")
        } else {cat(" .> Compress: ", basename(asc), " done! \n")}
      }
    }
  }
}


