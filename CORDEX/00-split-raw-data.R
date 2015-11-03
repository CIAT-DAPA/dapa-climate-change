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
  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA CUT HOURLY XXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")
  
  ## Convert to NetCDF (multiband) file format and cut lateral boundaries
  
  gcmList <- list.dirs(paste(baseDir, sep=""), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {

    modName <- tolower((basename(gcm)))
    
    scenList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)
  
    for (scen in scenList) {
      
      scenName <- sapply(strsplit(basename(scen), '[_]'), "[[", 5)
      if (scenName == "futuro"){scenName <- "sres_a1b"} else {scenName <- "historical"}
      
      cat("/n Cut ETA Daily files for: ", scenName, " ", modName, "/n/n")
      
      ctlList <- list.files(paste(scen, "/", sep=""), full.names = TRUE, pattern=".ctl")
      
      procDir <- paste(scen, "/temp", sep="")
      if (!file.exists(paste(procDir))) {dir.create(procDir, recursive=TRUE)}
      
      cat("/n .> Corverting grads files to NetCDF format /n/n")
      
      for (ctl in ctlList) {
        
        ctlName <- sapply(strsplit(basename(ctl), '[.]'), "[[", 1)
        
        outNc <- paste(scen, "/", ctlName, ".nc", sep="")
        if (!file.exists(outNc)){
          
          ## Corvert grads files to NetCDF format
          system(paste("cdo -f nc import_binary ", ctl, " ", procDir, "/", ctlName, ".nc", sep=""))
          
          ## Cut borders 
          system(paste("cdo sellonlatbox,", xmin, ",", xmax, ",", ymin, ",", ymax, " ", procDir, "/", ctlName, ".nc", " ", outNc , sep=""))
          
          cat("/t .> ", ctlName, " converted/n")
          
        }
      }
      
      # Remove temporal folder
      unlink(procDir, recursive=TRUE)
      
    
      if (shift == "yes"){
        
        cat("/n .> Split NetCDF files in daily data /n/n")
        
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
                
                cat("/t .> Split ", var, " ", year, " /n")
                
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
            
            cat("/n .> Cut ETA Daily files for: ", scenName, " ", modName, " ", var, " ", staYear, "_", endYear," done!/n/n")
            
            opnFile <- file(verFile, open="w")
            cat(paste("ETA Cut daily ", scenName, " ", modName, " for extent ", xmin, ",", xmax, ",", ymin, ",", ymax, " was processed on ", date(), sep=""), file=opnFile)
            close.connection(opnFile)
            
          } else {cat("/n .> Cut ETA Daily files for: ", scenName, " ", modName, " ", var, " ", staYear, "_", endYear, " done!/n/n")}
          
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
  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA CALCS MONTHLY XXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")
  
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
      
      cat("/n Avg ETA Monthly files for: ", scen, " ", modName, " ", year, "/n/n")
      
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
            
              cat("/n .> Average : ", var, " ",  mth," done! /n")
              
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
  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA CALCS DAILY XXxxXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")
  
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
      
      
      cat("/n Avg ETA Daily files for: ", scen, " ", modName, " ", year, "/n/n")
      
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
                
                cat("/n .> Average : ", var, " ",  mth, day, " done!")
                
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
  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA CALCS 30YR AVG XXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")
  
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
      
      cat("/n Avg ETA 30yrAvg files for: ", scen, " ", modName, " ", staYear, "-", endYear, "/n/n")
      varList <- c("tmax", "tmin")
      for (var in varList){
        
        for (mth in monthList){
          
          mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
          outAsc <- paste(outNcDir, "/",  var, "_", mthMod, ".asc", sep="")
          
          if (!file.exists(paste(outAsc, sep=""))) {
            
            ncYrList <- paste(gcm, "/", staYear:endYear, "/", var, "_", mthMod, ".nc", sep="")
            ncMthStack <- mean(stack(lapply(ncYrList, FUN=raster)), na.rm = TRUE)
            
            ncMth <- writeRaster(ncMthStack, outAsc, format='ascii', overwrite=TRUE)
            
            cat("/n .> Average : ", var, " ",  mth," done! /n")
            
          } else{cat("/n .> Average : ", var, " ",  mth," done! /n")}
          
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
        
        cat("/n Avg ETA 30yrAvg files for: ", scen, " ", modName, " ", staYear, "-", endYear, "/n/n")
        
        for (var in varList){
          
          if (file.exists(paste(gcm, "/", staYear, "/", var, "_1.nc", sep=""))) {
              
            for (mth in monthList){
              
              mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
              outAsc <- paste(outNcDir, "/",  var, "_", mthMod, ".asc", sep="")
              
              if (!file.exists(paste(outAsc, sep=""))) {
                
                ncYrList <- paste(gcm, "/", staYear:endYear, "/", var, "_", mthMod, ".nc", sep="")
                ncMthStack <- mean(stack(lapply(ncYrList, FUN=raster)))
              
                ncMth <- writeRaster(ncMthStack, outAsc, format='ascii', overwrite=TRUE)
                
                cat(" .> Average : ", var, " ",  mth," done! /n")
                
              } else {cat(" .> Average : ", var, " ",  mth," done! /n")}
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
  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA PUBLISH DATA XXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")

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

      cat("/n Publish files from: ", scenName, " ", modName, " ", periodName, "/n/n")
      
      ascList <- list.files(paste(period, sep=""), full.names = TRUE, pattern=".asc")
  
      for (asc in ascList) {
        
        var <- sapply(strsplit(basename(asc), '[_]'), "[[", 1)
        
        outZipDir <- paste(outDir, "/", scenName, "/", periodName, "/", modName, "/", res, sep="")
        outZipName <- paste(modName, "_", scenName, "_", periodName, "_", var, "_", res, "_", region, "_asc.zip", sep="")
        
        if (!file.exists(paste(outZipDir, "/", outZipName, sep=""))) {
          
        system(paste("7za a ", outZipDir, "/", outZipName, " ", asc, sep=""))
        
        cat(" .> Compress: ", basename(asc), " done! /n")
        } else {cat(" .> Compress: ", basename(asc), " done! /n")}
      }
    }
  }
}

########################## CORDEX ######################


CORDEX_Calcs_Monthly <- function(baseDir="U:/rcm/cordex/AFR-44", scen="historical", region="AFR-44",time="mon",ens="r1i1p1",ver="v1") {
  
  require(maptools)
  require(raster)
  require(ncdf)
  require(rgdal)
  require(sp)
  require(chron)
  
  scen="historical"
  region="SAM-44"
  time="mon"
  #ens="r1i1p1"
  vers="r2"
  #basegrid="U:/rcm/cordex/organized/base_grid" # "/mnt/data_cluster_5/rcm/cordex/organized/base_grid"# para convertir las coordenadas tipo curvilinear a regular grid esto es only for files con r2
  baseDir=paste0("U:/rcm/cordex/",region) #  paste0("/mnt/data_cluster_5/rcm/cordex/",region)#
  outDir="U:/rcm/cordex/organized" # "/mnt/data_cluster_5/rcm/cordex/organized"# 
  boxExtent=extent(-87,-32,-56.5,18.5)
  #maskWorld=raster("S:/admin_boundaries/masks_world/mask15m")
  #maskbase=crop(maskWorld,boxExtent)

  
  cat(" /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXX ETA CALCS MONTHLY XXXXXXXXXXXXX /n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /n")
  cat(" /n")
  
  
  #monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  #varList <- c("prec", "tmax", "tmin", "tmean")
  #monthListMod <- c(1:12)
  
  # Set number of days by month
  #ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  # Combirn number of month and days in one single data frame
  #ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  #names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  
  
  gcmList <- list.dirs(paste0(baseDir,"/",time,"/", scen), recursive = FALSE, full.names = FALSE)
  varList<- c("pr","tas","tasmax","tasmin","rsds","sfcwind","hur")
  
  for (gcm in gcmList) {
    

    for(var in varList){
      
      if (var == "pr"){
        varmod="prec"
      } else if (var == "tas") {
        varmod="tmean"
      } else if (var == "tasmax") {
        varmod="tmax"
      }else if (var == "tasmin") {
        varmod="tmin"
      }else{
        varmod=var
      }      
      ensList <- list.dirs(paste0(baseDir,"/",time,"/", scen,"/",gcm), recursive = FALSE, full.names = FALSE)
      
      for(ens in ensList){
        yrList <- list.files(paste0(baseDir,"/",time,"/", scen,"/", gcm,"/",ens), recursive = FALSE, full.names = FALSE,pattern = paste0(var,"_"))
        if(length(yrList)>0){
          nyear=c()
          for( yr in yrList){
            ver=sapply(strsplit(yr, '[_]'), "[[", 7)
            #if(ver=="r2"){
              years <- gsub(".nc","",sapply(strsplit(yr, '[_]'), "[[", 9))  
              staYear <- as.numeric(substr(sapply(strsplit(years, '[-]'), "[[", 1),1,4))
              endYear <- as.numeric(gsub(".nc","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 4)))
              nyear=c(nyear,staYear,endYear)
            #} # if ver
          }          
          if(!is.null(nyear)){
            cat(region,scen,gcm,var)
            cat("\n")
            if(length(yrList)==length(seq(min(nyear),max(nyear),by = 9))-1){
              setwd(paste0(baseDir,"/",time,"/", scen,"/", gcm,"/",ens))
              
              outNcDir <- paste0(outDir,"/",region, "/",scen,"/",gcm,"/",ens,"/",ver)
              if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
              monNC=paste0(outNcDir,"/monthly-files")  
              if (!file.exists(paste(monNC, sep=""))) {dir.create(paste(monNC, sep=""), recursive=TRUE)}
              
              outfile=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'_tmp.nc')
              outfilePrj=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'_prj.nc')
              
              outfileTemp=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'.nc')
              
              if (!file.exists(outfile) && !file.exists(outfileTemp)){
                cat("-> processing mergetime")
                system(paste0('cdo -f nc  mergetime ',paste(yrList, collapse = ' '),' ',outfile),intern=TRUE)
                cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
                
                #               if (var == "rsds" || var == "sfcwind" ){
                #                 system(paste0('cdo -f nc mergetime ',paste(yrList, collapse = ' '),' ',outfileTemp),intern=TRUE)
                #                 cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
                #               }else{
                #                 system(paste0('cdo -f nc  mergetime ',paste(yrList, collapse = ' '),' ',outfile),intern=TRUE)
                #                 cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
                #               }
                cat("\n")
              }else{
                cat("..exist mergetime!")
                cat("\n")
              }
              
              if (file.exists(outfile) && !file.exists(outfileTemp)){
                cat("-> processing converting units")
                
                
                system(paste0('cdo remapbil,U:/rcm/cordex/organized/targetgrid -setgrid,U:/rcm/cordex/organized/sam44_grid,',' ',outfile,' ',outfilePrj),intern=TRUE)
                

                cat("\n")
                if (var == "pr"){
                  varmod="prec"
                  system(paste0("cdo -mulc,86400 -sellonlatbox,",boxExtent@xmin,',',boxExtent@xmax,',',boxExtent@ymin,',',boxExtent@ymax,' ', outfilePrj,' ',outfileTemp))
                } else if (var == "tas") {
                  varmod="tmean"
                  system(paste0("cdo -subc,273.15 -sellonlatbox,",boxExtent@xmin,',',boxExtent@xmax,',',boxExtent@ymin,',',boxExtent@ymax,' ', outfilePrj,' ',outfileTemp))
                } else if (var == "tasmax") {
                  varmod="tmax"
                  system(paste0("cdo -subc,273.15 -sellonlatbox,",boxExtent@xmin,',',boxExtent@xmax,',',boxExtent@ymin,',',boxExtent@ymax,' ', outfilePrj,' ',outfileTemp))
                }else if (var == "tasmin") {
                  varmod="tmin"
                  system(paste0("cdo -subc,273.15 -sellonlatbox,",boxExtent@xmin,',',boxExtent@xmax,',',boxExtent@ymin,',',boxExtent@ymax,' ', outfilePrj,' ',outfileTemp))
                }else{
                  system(paste0("cdo sellonlatbox,",boxExtent@xmin,',',boxExtent@xmax,',',boxExtent@ymin,',',boxExtent@ymax,' ', outfilePrj,' ',outfileTemp))
                  varmod=var
                }
                
              }
              if (file.exists(outfileTemp) && !file.exists(paste0(monNC, "/", max(nyear),"/", varmod, "_12.nc"))){
                if (file.exists(outfileTemp) && !file.exists(paste0(monNC, "/",  var, "_",max(nyear),'.nc'))){
                  cat("-> processing splityear")
                  cat("\n")
                  system(paste("cdo splityear ", outfileTemp, " ", monNC, "/",  var, "_", sep=""))              
                  
                }
              }
              for(nmon in seq(min(nyear),max(nyear),by = 1)){
                if(!file.exists(paste0(monNC, "/",nmon, "/",varmod, "_12.nc", sep=""))){
                  if (!file.exists(paste0(monNC,"/",nmon))) {dir.create(paste0(monNC,"/",nmon), recursive=TRUE)}
                  ncyear=paste0(monNC,"/",var,"_",nmon,".nc")
                  system(paste("cdo splitmon ", ncyear, " ", monNC, "/",nmon, "/",varmod, "_", sep=""))
                  unlink(ncyear)
                }
              }             
              
              if (file.exists(outfileTemp)){
                unlink(outfile)
                unlink(outfilePrj)
              }
              cat("done!")
            }            
          }          
        }
        
      }
      
    }
    
  }
  
  
}

  
  ############### Para AFR-44 ##################
  yrList <- list.files(paste0(baseDir,"/",time,"/", scen,"/", gcm,"/",ens), recursive = FALSE, full.names = FALSE,pattern = paste0(var,"_"))
  if(length(yrList)>0){
    nyear=c()
    for( yr in yrList){
      ver=sapply(strsplit(yr, '[_]'), "[[", 7)
      if(ver=="r2"){
        years <- gsub(".nc","",sapply(strsplit(yr, '[_]'), "[[", 9))  
        staYear <- as.numeric(substr(sapply(strsplit(years, '[-]'), "[[", 1),1,4))
        endYear <- as.numeric(gsub(".nc","",substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 4)))
        nyear=c(nyear,staYear,endYear)
      }
    }
    if(!is.null(nyear)){
      cat(region,scen,gcm,var)
      cat("\n")
      if(length(yrList)==length(seq(min(nyear),max(nyear),by = 10))+1){
        setwd(paste0(baseDir,"/",time,"/", scen,"/", gcm,"/",ens))
        
        outNcDir <- paste0(outDir,"/",region, "/",scen,"/",gcm,"/",ens,"/",ver)
        if (!file.exists(paste(outNcDir, sep=""))) {dir.create(paste(outNcDir, sep=""), recursive=TRUE)}
        monNC=paste0(outNcDir,"/monthly-files")  
        if (!file.exists(paste(monNC, sep=""))) {dir.create(paste(monNC, sep=""), recursive=TRUE)}
        
        outfile=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'_tmp.nc')
        outfilePrj=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'_prj.nc')
        
        outfileTemp=paste0(outNcDir,'/',substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear),'.nc')
        
        if (!file.exists(outfile) && !file.exists(outfileTemp)){
          cat("-> processing mergetime")
          system(paste0('cdo -f nc  mergetime ',paste(yrList, collapse = ' '),' ',outfile),intern=TRUE)
          cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
          
          #               if (var == "rsds" || var == "sfcwind" ){
          #                 system(paste0('cdo -f nc mergetime ',paste(yrList, collapse = ' '),' ',outfileTemp),intern=TRUE)
          #                 cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
          #               }else{
          #                 system(paste0('cdo -f nc  mergetime ',paste(yrList, collapse = ' '),' ',outfile),intern=TRUE)
          #                 cat(" ..Done mergetime",paste0(substr(yrList[1], 1, nchar(yrList[1])-16),min(nyear),'_',max(nyear)))
          #               }
          cat("\n")
        }else{
          cat("..exist mergetime!")
          cat("\n")
        }
        
        if (file.exists(outfile) && !file.exists(outfileTemp)){
          cat("-> processing converting units")
          
          
          system(paste0('cdo setgrid,',basegrid,' ',outfile,' ',outfilePrj),intern=TRUE)
          
          cat("\n")
          if (var == "pr"){
            varmod="prec"
            system(paste0("cdo -mulc,86400 -setgridtype,lonlat ", outfilePrj,' ',outfileTemp))
          } else if (var == "tas") {
            varmod="tmean"
            system(paste0("cdo -subc,273.15 -setgridtype,lonlat ", outfilePrj,' ',outfileTemp))
          } else if (var == "tasmax") {
            varmod="tmax"
            system(paste0("cdo -subc,273.15 -setgridtype,lonlat ", outfilePrj,' ',outfileTemp))
          }else if (var == "tasmin") {
            varmod="tmin"
            system(paste0("cdo -subc,273.15 -setgridtype,lonlat ", outfilePrj,' ',outfileTemp))
          }else{
            system(paste0("cdo -setgridtype,lonlat ", outfilePrj,' ',outfileTemp))
            varmod=var
          }
          
        }
        if (file.exists(outfileTemp) && !file.exists(paste0(monNC, "/",  var, "_",max(nyear),'.nc'))){
          cat("-> processing splityear")
          cat("\n")
          system(paste("cdo splityear ", outfileTemp, " ", monNC, "/",  var, "_", sep=""))              
          
        }
        for(nmon in seq(min(nyear),max(nyear),by = 1)){
          if(!file.exists(paste0(monNC, "/",nmon, "/",varmod, "_12.nc", sep=""))){
            if (!file.exists(paste0(monNC,"/",nmon))) {dir.create(paste0(monNC,"/",nmon), recursive=TRUE)}
            ncyear=paste0(monNC,"/",var,"_",nmon,".nc")
            system(paste("cdo splitmon ", ncyear, " ", monNC, "/",nmon, "/",varmod, "_", sep=""))
            unlink(ncyear)
          }
        }             
        
        if (file.exists(outfileTemp)){
          unlink(outfile)
          unlink(outfilePrj)
        }
        cat("done!")
      }
    }
  }
  
  ##########################################


af_44i=raster("C:/Users/jetarapues/Desktop/TEMP_ESC/rotated_pole_test/afr-44i.nc",band=1)
af_44=raster("C:/Users/jetarapues/Desktop/TEMP_ESC/rotated_pole_test/afr-44.nc",band=1)
af_44mod=raster("C:/Users/jetarapues/Desktop/TEMP_ESC/rotated_pole_test/output_file.nc",band=1)

sam_44=raster("D:/TEMP/test/cordex/tas_sam44.nc",band=1)
sam_44_con=raster("D:/TEMP/test/cordex/prj_tas_sam44.nc",band=1)
sam_44_t=raster("D:/TEMP/test/cordex/tra_tas_sam44.nc",band=1)


cordi2=raster("U:/rcm/cordex/organized/SAM-44/historical/SMHI-RCA4/r12i1p1/v3/monthly-files/1951/tmean_01.nc",band=1)
cordi3=raster("D:/Documentos/CIAT/PhD-master/Future/Results/outfile2.nc",band=1)
shp=shapefile("S:/admin_boundaries/adminFiles/world_no_antarctica.shp")

plot(raster("U:/rcm/cordex/organized/AFR-44/historical/KNMI-RACMO22T/r1i1p1/v1/tas_AFR-44_MetEir-ECEARTH_historical_r1i1p1_KNMI-RACMO22T_v1_mon_1950_2005.nc",band=1),add=T)

plot(cordi2)
plot(shp)
extlat <- extent(-80,-40,-25,10)

plot(shp)
plot(shp,add=T)



writeRaster(sam_44, "D:/TEMP/test/sam_44.tiff", format="GTiff", overwrite=T)

crds <- matrix(data=c(9.05, 48.52), ncol=2) 
library(rgdal)
project(crds*(pi/180), paste("+proj=ob_tran +o_proj=longlat","+o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"), TRUE) 

spPoint <- SpatialPoints(coords=crds*(pi/180), 
                         proj4string=CRS(paste("+proj=ob_tran +o_proj=longlat +o_lon_p=-162", 
                                               "+o_lat_p=39.25 +lon_0=180 +ellps=sphere +no_defs"))) 
spTransform(spPoint, CRS("+proj=longlat +ellps=sphere +no_defs")) 


########################################
#cdo griddes rotated.nc > mygrid
#cdo remapbil,rs -setgrid,sourcegrid tas_sam44.nc prj_tas_sam44.nc
# cdo -setgridtype,lonlat prj_tas_sam44.nc tra_tas_sam44.nc

# res=0.44
# loni=254.28
# lonf=343.02
# lati=-52.66
# latf=18.50
# chec=c(254.28)
# gridLat=c(-52.66)
# Nx=146
# Ny=167
# 
# while(loni <= lonf) {
#   loni <- loni+res; 
#   print(loni);
#   chec=c(chec,loni)
# }
# while(lati <= latf) {
#   lati <- lati+res; 
#   print(lati);
#   gridLat=c(gridLat,lati)
# }
# 
# te=split(chec,as.numeric(gl(length(chec),7,length(chec)))) 
# telat=split(gridLat,as.numeric(gl(length(gridLat),7,length(gridLat)))) 
# 
# 
# verFile="D:/TEMP/test/griddone.txt"
# opnFile <- file(verFile, open="w")
# for(i in 1:length(te)){
#     cat("xvals     =",do.call(rbind, te[i]),"\n", file=opnFile)
# 
# }
# for(i in 1:length(te)){
#   cat("xvals     =",do.call(rbind, te[i]),"\n", file=opnFile)
#   
# }
# close.connection(opnFile)


lonI=265.88 -360
latMax=18.50
rs <- raster(xmn=lonI, xmx=146*0.44+lonI, ymn=latMax-167*0.44, ymx=latMax, ncols=146, nrows=167)
rs[] <- 1
writeRaster(rs, "D:/TEMP/test/rs.ncf", format="CDF", overwrite=T)

admin=shapefile("S:/admin_boundaries/adminFiles/world_adm0_no_antarctica.shp")
plot(raster(outfileTemp))
plot(admin,add=T)
plot(extent(rs),add=T)
plot(extent(-87,-32,-56.5,18.5),add=T)

