# Julian Ramirez, dawnpatrolmustaine@gmail.com
# gcmdata <- NcToAscii(scenario="20C3M")

#Code to transform NetCDF monthly datasets to ESRI Ascii raster files. This is the first script to run. This should be followed
#by GCMVerification.R

require(rgdal)
require(raster)

NcToAscii <- function(scenario='20C3M', drive="T") {
  
  mths <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymtx <- as.data.frame(cbind(mths, ndays))
  names(ndaymtx) <- c("Month", "Ndays")
  
  if (!toupper(scenario) %in% c("20C3M","SRES_A1B","SRES_A2","SRES_B1")) {
    stop('Scenario', scenario, ' is not supported')
  }
  
  gcm.char <- read.csv("gcm_chars.csv")
  
  basedir <- paste(drive, ":/climate_change/IPCC_CMIP3/", sep="")
  scendir <- paste(basedir, "/", scenario, "/original", sep="")
  
  # Listing and looping the models
  
  modlist <- list.files(scendir, pattern="*_*")
  m <- 1
  
  for (modname in modlist) {
    
    cat(paste("Processing model ", modname, "\n"))
    
    folder <- "multiyr_avgs"
    
    workdir <- paste(scendir, "/", modname, "/", folder, sep="")
    
    # Listing the periods to process
    
    perlist <- list.files(workdir, pattern="*_*")
    i <- 1
  
    for (period in perlist) {
      datadir <- paste(workdir, "/", period, sep="")
      #filelist <- intersect(list.files(datadir, pattern=".nc"), list.files(datadir, pattern="prec_")) #only for prec
	  filelist <- list.files(datadir, pattern=".nc")
	  
      cat(paste("Processing period", period),"\n")
      	
      j <- 1
      	
      setwd(datadir)
  	
      # Listing the monthly files to process
      
      for (filename in filelist) {
        
        cat(paste("Processing file", filename),"\n")
        
        outasciiname <- paste(unlist(strsplit(filename, ".", fixed=T))[1], ".asc", sep="")
        varname <- unlist(strsplit(outasciiname, "_", fixed=T))[1]
        mth <- substring(unlist(strsplit(outasciiname, "_", fixed=T))[2], 1, 2)
		
		mlt <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
		
        system(paste("gdal_translate -of AAIGrid -sds", filename, outasciiname, sep=" "))
        
        file.remove(paste(outasciiname, "1", sep=""))
        file.remove(paste(outasciiname, "1", ".aux.xml", sep=""))
        
        file.remove(paste(outasciiname, "2", sep=""))
        file.remove(paste(outasciiname, "2", ".aux.xml", sep=""))
        
        file.rename(paste(outasciiname, "3", sep=""), outasciiname)
        file.rename(paste(outasciiname, "3", ".aux.xml", sep=""), paste(outasciiname, ".aux.xml", sep=""))
        
        # Scanning the file to get the data within a vector
        
        nToSkip <- gcm.char[which(gcm.char$model == modname),12]
        
        ValsVector <- scan(outasciiname, skip=nToSkip, na.string="9.969209968386869e+036")
        
        nCols <- gcm.char[which(gcm.char$model == modname),9]
        nRows <- gcm.char[which(gcm.char$model == modname),10]
        
        OnCols <- gcm.char[which(gcm.char$model == modname),4]
        OnRows <- gcm.char[which(gcm.char$model == modname),5]
        
        # Creating the output and temporary rasters
        
        rs <- raster(nrow=OnRows, ncol=OnCols, xmn=0, xmx=360, ymn=-90, ymx=90)
        rs[] <- ValsVector
        
        rs2 <- raster(nrow=nRows, ncol=nCols, xmn=0, xmx=360, ymn=-90, ymx=90)
        rs2 <- resample(rs, rs2, method="ngb")
        
        rs2 <- flip(rs2, direction="y")
        rs2 <- rotate(rs2)
        
        if (varname == "prec") {
          rs2 <- rs2*86400*(mlt)
        } else {
          rs2 <- rs2 - 272.15
        }
	 	
        output <- writeRaster(rs2, paste(outasciiname, sep=""), format="ascii", overwrite=TRUE)
        remove(rs)
        remove(rs2)
		  
        j <- j + 1
      }
      
      i <- i + 1
    }
    m <- m + 1
  }
  setwd("C:/CIAT_work/_tools/dapa-climate-change/trunk/IPCC-CMIP3")
  return(gcm.char)
}