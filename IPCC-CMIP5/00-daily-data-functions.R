#-----------------------------------------------------------------------
# Author: Carlos Navarro
# CIAT-CCAFS
# c.e.navarro@cgiar.org
# Date: October 2013
#-----------------------------------------------------------------------


# baseDir="T:/gcm/cmip5/raw/daily"
# ens="r1i1p1"
# rcp="historical"
# xmin=33
# xmax=44
# ymin=30
# ymax=39
# outDir <- "D:/CIAT/Workspace/cmip5_daily_test"
# shift <- "yes"

GCM_Cut_Daily <- function(baseDir="T:/gcm/cmip5/raw/daily", ens="r1i1p1", rcp="historical", xmin=-18, xmax=-11, ymin=12, ymax=17, outDir="D:/CIAT/Workspace/cmip5_daily_test", shift="yes") {
  
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
  
  gcmList <- list.dirs(paste(rcpDir, sep=""), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {
    
    model <- basename(gcm)
    
    ncList <- list.files(paste(gcm, "/", ens, sep=""), full.names = TRUE)
    outNcDir <- paste(outDir, "/", rcp, "/", ens, sep="")
    
    for (nc in ncList) {
      
      var <- sapply(strsplit(basename(nc), '[_]'), "[[", 1)
      
      if (var == "tas" || var == "tasmax" || var == "tasmin" || var == "pr" || var == "rsds"){
          
        years <- sapply(strsplit(basename(nc), '[_]'), "[[", 6)
        staYear <- sapply(strsplit(years, '[-]'), "[[", 1)
        endYear <- substr(sapply(strsplit(years, '[-]'), "[[", 2), 1, 8)
              
        # descNc <- system(paste("cdo sinfov ", nc, sep=""), intern=TRUE)
        # system(paste("cdo showname ", nc, sep=""))
        
        cat(" Cut nc files for: ", rcp, " ", model, " ", ens, " ",  var, " \n\n")
        
        if (xmin < 0) {xmin <- xmin + 360}
        if (xmax < 0) {xmax <- xmax + 360}
        
        
        ## Cut
        
        if (!file.exists(paste(outNcDir))) {dir.create(outNcDir, recursive=TRUE)}
        
        outNc <- paste(outNcDir, "/", var, "_", staYear, "_", endYear, ".nc", sep="")
        
        if (!file.exists(outNc)){
          
          system(paste("cdo sellonlatbox,", xmin, ",", xmax, ",", ymin, ",", ymax, " ", nc , " ", outNc , sep=""))
          
        }
      
  
        if (shift == "yes"){
            
          ## Split  
          
          system(paste("cdo splityear ", outNc, " ", outNcDir, "/",  var, "_", sep=""))
          file.remove(outNc)
          
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
              
            }
            
            # Rotate
            
            ncDayList <- list.files(paste(outNcDir, "/",  year, sep=""), full.names = TRUE)
            
            xminNc <- xmin(raster(ncDayList[1]))
            xmaxNc <- xmax(raster(ncDayList[1]))
            yminNc <- ymin(raster(ncDayList[1]))
            ymaxNc <- ymax(raster(ncDayList[1]))
    
            if (xminNc > 180) {xminNc <- xminNc - 360}
            if (xmaxNc > 180) {xmaxNc <- xmaxNc - 360}
    
            ncDayStack <- stack(lapply(ncDayList, FUN=raster))
            
            rs <- raster(xmn=xminNc, xmx=xmaxNc, ymn=yminNc, ymx=ymaxNc)
            ncDayStack <- setExtent(ncDayStack, extent(rs), keepres=FALSE, snap=TRUE)
            
            
            # Writting Outputs
            
            for (i in 1:dim(ncDayStack)[[3]]){
    
              month <- substr(sapply(strsplit(basename(ncDayList[i]), '[_]'), "[[", 2), 1, 2)
              day <- substr(sapply(strsplit(basename(ncDayList[i]), '[_]'), "[[", 2), 3, 4)          
              
              cat("\t\t ", var, " ", month, " ", day, " \n")
              
              doy <- julian(as.integer(month),as.integer(day),as.integer(year),c(1,0,as.integer(year)))
              
              ncDay <- paste(outNcDir, "/",  year, "/", var, "_", doy, ".nc", sep="")  
              ncDay <- writeRaster(ncDayStack[[i]], ncDay, format='CDF', overwrite=TRUE)
              
              file.remove(ncDayList[i])
              
              cat("\t\t ", var, " ", year, " ", doy, " \n")
              
            } 
            
            cat("\n\t .> Split ", var, " ", year, " done! \n")
            
          } 
          
        }
      }
    }
    verFile <- paste(rcpDir, "/", model, "_", ens, "_cut_daily_done.txt", sep="")
    opnFile <- file(verFile, open="w")
    cat(paste("Cut CMIP5 daily ", ens, " ", rcp, " for extent ", xmin, ",", xmax, ",", ymin, ",", ymax, " on ", date(), sep=""), file=opnFile)
    close.connection(opnFile)
        
    cat("\n Cut nc files for: ", rcp, " ", model, " ", ens, " done!\n\n")
    
  }
  
  cat("GCM Cut Daily Process Done!")
  

}
  