# Julian Ramirez, dawnpatrolmustaine@gmail.com
# gcmdata <- NcToAscii(scenario="20C3M")

GCMAverage <- function(scenario='20C3M') {
  
  if (!toupper(scenario) %in% c("20C3M","SRES_A1B","SRES_A2","SRES_B1")) {
    stop('Scenario', scenario, ' is not supported')
  }
  
  basedir <- "F://climate_change//IPCC_CMIP3"
  scendir <- paste(basedir, "//", scenario, sep="")
  
  baseOut <- paste(scendir, "//average", sep="")
  if (!file.exists(baseOut)) {
    dir.create(baseOut)
  }
  
  # Listing and looping the periods
  
  perlist <- list.files(paste(scendir, "//", "bccr_bcm2_0//multiyr_avgs", sep=""), pattern="*_*")
  
  for (period in perlist) {
    
    cat(paste("Processing period", period),"\n")
    
    outPeriod <- paste(baseOut, "//", period, sep="")
    if (!file.exists(outPeriod)) {
      dir.create(outPeriod)
    }
    
    # Listing and looping the variables
    
    varlist <- list.files(paste(scendir, "//", "bccr_bcm2_0//multiyr_avgs//", period, sep=""), pattern="prec_01.nc")
    
    for (variable in varlist) {
      
      varname <- unlist(strsplit(unlist(strsplit(variable, ".", fixed=T))[1], "_", fixed=T))[1]
      
      cat(paste("Processing variable ", varname, "\n"))
      
      # Looping the months
      
      monthList <- c("01","02") #,"03","04","05","06","07","08","09","10","11","12")
      
      for (month in monthList) {
        
        cat(paste("Processing month ", month, "\n"))
        
        outFile <- paste(outPeriod, "//", varname, "_", month, ".asc", sep="")
        
        # Listing and looping the models
        
        modlist <- list.files(scendir, pattern="*_*")
        modlist <- modlist[which(modlist[] != "average")]
        m <- 0
        
        for (modname in modlist) {
          
          cat(paste("\n", "Processing model ", modname, "\n"))
          
          inFile <- paste(scendir, "//", modname, "//multiyr_avgs//", period, "//", varname, "_", month, ".asc", sep="")
          
          if (m == 0) {
            #cat(outFile, "\n")
            #cat(inFile, "\n")
            
            if (file.exists(inFile)) {
              rs <- raster(inFile)
              rs <- readAll(rs)
              
              rsum <- rs
              m <- m+1
            }
          } else {
            if (file.exists(inFile)) {
              
              #cat(inFile, "\n")
              
              rs <- raster(inFile)
              rs <- readAll(rs)
              
              if (xres(rs) == xres(rsum)) {
                cat("\n", "Not resampling ", "\n")
                
                rsum <- rsum + rs
                m <- m+1
              } else {
                
                dfr <- data.frame(raster=c("rs", "rsum"), resol=c(xres(rs), xres(rsum)))
                mnResRaster <- trim(dfr[which(dfr[,2] == min(dfr[,2])),1])
                mxResRaster <- trim(dfr[which(dfr[,2] == max(dfr[,2])),1])
                
                nCols <- ncol(get(paste(mnResRaster)))
                nRows <- nrow(get(paste(mnResRaster)))
                
                xMin <- xmin(get(paste(mnResRaster)))
                xMax <- xmax(get(paste(mnResRaster)))
                yMin <- ymin(get(paste(mnResRaster)))
                yMax <- ymax(get(paste(mnResRaster)))
                
                nwrs <- raster(get(paste(mnResRaster)))
                
                cat(ncol(get(paste(mnResRaster))), nrow(get(paste(mnResRaster))),"\n")
                cat(ncol(get(paste(mxResRaster))), nrow(get(paste(mxResRaster))),"\n")
                cat(ncol(nwrs), nrow(nwrs), "\n")
                
                rm(dfr)
                
                cat("\n", "Resampling ", mxResRaster, " with ", mnResRaster, "\n")
                
                nwrs <- resample(get(paste(mxResRaster)), nwrs, method='ngb')
                rsum <- rsum + nwrs
                rm(nwrs)
                
                m <- m+1
              }
            }
          }
        }
        
        cat("Average over ", m, " models", "\n")
        
        rsum <- rsum / m
        rsum <- writeRaster(rsum, outFile, format='ascii', overwrite=TRUE)
        
      }
      
    } 
    
  }
  setwd("F://climate_change//_scripts")
  return("Done!")
}
 
#for (i in 1:12) {
#  if (i < 10) { mth <- paste("0", i, sep="") } else {
#    mth <- paste(i)
#  }
#  jpeg(paste("mth_", mth, ".jpg", sep=""))
#  rs <- raster(paste("prec_", mth, ".asc", sep=""))
#  plot(rs)
#  dev.off()
#	 cat(i, "\n")
#}