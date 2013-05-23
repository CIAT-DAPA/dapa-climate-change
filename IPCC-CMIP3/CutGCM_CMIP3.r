require(raster)
require(ncdf)
require(rgdal)

# source("CutGCM_CMIP3.R")
# baseDir <- "S:/data/gcm/cmip3/raw_data"
# region <- extent(-180, 0, -90, 90)
# outDir <- "G:/jtarapues/Request/request_adieye"
# startYr <- 1950
# endYr <- 2000
# otp <- GCMCut_Raw(baseDir, region, startYr, endYr, outDir)


GCMCut_Raw <- function(baseDir="S:/data/gcm/cmip3/raw_data", region=extent(-180, 0, -90, 90), startYr=1950, endYr=2000, sres="historical", outDir="G:/jtarapues/Request/request_adieye") {
  
  # sresList <- c("historical", "sres_a1b", "sres_a2", "sres_b1")
  
  #for (sres in sresList) {
    
  sresDir <- paste(baseDir, "/", sres, "/", "original-data", sep="")
  gcmList <- list.dirs(sresDir, recursive = FALSE, full.names = FALSE) 
    
  for (gcmDir in gcmList) {
   
    gcm <- basename(gcmDir)
    yrList <- list.dirs(paste(gcmDir, "/yearly_files", sep=""), recursive = FALSE, full.names = FALSE)

    for (yrDir in yrList) {

      yr <- basename(yrDir)
      
      if (yr > startYr) {
        
        if (yr < endYr) {
        
          cat(paste("\n\nProcessing ", sres, " ", gcm, " ", yr, "\n\n", sep=""))
          
          ascList <- list.files(yrDir, pattern=".asc", full.names = TRUE)
          
          
          outAscDir <- paste(outDir, "/", sres, "/", gcm, "/", yr, sep="")
          if (!file.exists(outAscDir)) {
            dir.create(outDir)
            dir.create(paste(outDir, "/", sres, sep=""))
            dir.create(paste(outDir, "/", sres, "/", gcm, sep=""))
            dir.create(outAscDir)
          }
          
          for(asc in ascList){
            
            ascCrop <- crop(raster(asc), region)
            ascName <- basename(asc)
            
            cat(paste("\t ", ascName, sep=""))

            # ncsplit <- strsplit(nc, '[.]')
            # ncRaster <- rotate(raster(paste(regionDir, "/baseline/", nc, sep=""))) * 0 + 1
            
            if (!file.exists(paste(outAscDir, "/", ascName, sep=""))) {
              
              ascWrite <- writeRaster(ascCrop, paste(outAscDir, "/", ascName, sep=""), format="ascii", overwrite=F)
              cat(" ..done\n")
              
            } else {cat(" ..done\n")}
          
          }
        
        }
      }
       
    }

  }

}