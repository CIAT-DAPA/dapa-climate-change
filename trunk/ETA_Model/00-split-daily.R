#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
# Date: November 2013
#-----------------------------------------------------------------------

baseDir="U:/rcm/eta/raw/hadcm_40km"
xmin=-81.999995
xmax=-24.400283
ymin=-49.200006
ymax=11.199692
outDir="U:/rcm/eta/processed/sa_20min"
shift="yes"

ETA_Cut_Daily <- function(baseDir="U:/rcm/eta/raw/hadcm_40km", xmin=-81.999995, xmax=-24.400283, ymin=-49.200006, ymax=11.199692, outDir="U:/rcm/eta/processed/sa_20min", shift="yes") {
  
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
  
  gcmList <- list.dirs(paste(baseDir, sep=""), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {
    
    modName <- tolower((basename(gcm)))
    
    scenList <- list.dirs(paste(gcm, sep=""), recursive = FALSE, full.names = FALSE)

    for (scen in scenList) {
      
      scenName <- sapply(strsplit(basename(scen), '[_]'), "[[", 5)
      if (scenName == "futuro"){scenName <- "sres_a1b"} else {scenName <- "historical"}
      
      cat("\n Cut ETA Daily files for: ", scenName, " ", modName, "\n\n")
      
      verFile <- paste(outDir, "/", scenName, "/", modName, "_split_daily_done.txt", sep="")      
      if (!file.exists(verFile)){
  
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
        
        unlink(procDir, recursive=TRUE)
        
        if (shift == "yes"){
          
          cat("\n .> Split NetCDF files in daily data \n\n")
          
          outNcDir <- paste(outDir, "/", scenName, "/", modName, sep="")
          if (!file.exists(paste(outNcDir))) {dir.create(outNcDir, recursive=TRUE)}
          
          ncList <- list.files(paste(scen, "/", sep=""), full.names = TRUE, pattern=".nc")
          
          for (nc in ncList) {
            
            ncName <- sapply(strsplit(basename(nc), '[.]'), "[[", 1)
            
            ## Split in yearly files
            var <- sapply(strsplit(ncName, '[_]'), "[[", 3)
            if (var == "tp2m"){var <- "tmean"} else if (var == "mntp"){var <- "tmin"} else if (var == "mxtp"){var <- "tmax"}
            
            staYear <- substr(sapply(strsplit(ncName, '[_]'), "[[", 4), 10, 13)
            endYear <- as.character(as.numeric(staYear) + 30)

            ## Split  
            
            system(paste("cdo splityear ", nc, " ", outNcDir, "/",  var, "_", sep=""))          
            ncYrList <- list.files(outNcDir, full.names = TRUE)

            for (ncYr in ncYrList) {
              
              year <- substr(sapply(strsplit(basename(ncYr), '[_]'), "[[", 2), 1, 4)
              
              cat("\t .> Split ", var, " ", year, " \n")
              
              if (!file.exists(paste(outNcDir, "/", year, sep=""))) {dir.create(paste(outNcDir, "/", year, sep=""))}
              
              system(paste("cdo splitmon ", ncYr, " ", outNcDir, "/",  year, "/", var, "_", sep=""))
              file.remove(ncYr)
              
              ncMthList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE)
              
              for (ncMth in ncMthList) {
                
                month <- substr(sapply(strsplit(basename(ncMth), '[_]'), "[[", 2), 1, 2)
                
                system(paste("cdo splitday ", ncMth, " ", outNcDir, "/",  year, "/", var, "_", month, sep=""))
                file.remove(ncMth)
                            
                ncDayList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", sep=""))
                
                for (ncDay in ncDayList) {
                  
                  day <- substr(sapply(strsplit(basename(ncDay), '[_]'), "[[", 2), 3, 4)
                  system(paste("cdo splithour ", ncDay, " ", outNcDir, "/",  year, "/", var, "_", month, day, sep=""))
                  file.remove(ncDay)
                  
                  ncHourList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE, pattern=paste(var, "_", month, day, sep=""))
                  if (var == "prec"){
                    ncHourStack <- sum(stack(lapply(ncHourList, FUN=raster))) * 1000
                  } else {
                    ncHourStack <- mean(stack(lapply(ncHourList, FUN=raster))) - 272.15
                  }
                  
                  doy <- julian(as.integer(month),as.integer(day),as.integer(year),c(1,0,as.integer(year)))
                  ncDay <- writeRaster(ncHourStack, paste(outNcDir, "/",  year, "/", var, "_", doy, ".nc", sep=""), format='CDF', overwrite=TRUE)
                  
                }
              }
            }              
          }    
        }
        
        
        opnFile <- file(verFile, open="w")
        cat(paste("ETA Cut daily ", scenName, " ", modName, " for extent ", xmin, ",", xmax, ",", ymin, ",", ymax, " was processed on ", date(), sep=""), file=opnFile)
        close.connection(opnFile)
        
        cat("\n Cut ETA Daily files for: ", scenName, " ", modName, " done!\n\n")
        
      } else {cat("\n Cut ETA Daily files for: ", scenName, " ", modName, " done!\n\n")}
    }
  }
  
  cat("ETA Cut Daily Process Done!")
  
  
}
