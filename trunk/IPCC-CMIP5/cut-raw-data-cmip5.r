require(raster)
require(ncdf)
require(rgdal)

# source("cut-raw-data-cmip5.r")
# baseDir <- "T:/data/gcm/cmip5/raw/monthly"
# region <- extent(-180, 0, -90, 90)
# outDir <- "G:/jtarapues/Request/request_adieye/cmip5"
# startYr <- 1950
# endYr <- 2000
# ens <- "r1i1p1"
# rcp <- "historical"
# otp <- GCMCut_Raw(baseDir, region, startYr, endYr, ens, rcp, outDir)


GCMCut_Raw <- function(baseDir="S:/data/gcm/cmip3/raw_data", region=extent(-180, 0, -90, 90), startYr=1950, endYr=2000, ens="r1i1p1", rcp="historical", outDir="G:/jtarapues/Request/request_adieye") {
  
  # rcpList <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
  
  # for (rcp in rcpList) {
    
  rcpDir <- paste(baseDir, "/", rcp, sep="")
  
  gcmStats <- read.table(paste(baseDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  
  for (i in 1:nrow(gcmStats)){
   
    gcm <- paste(as.matrix(gcmStats)[i,2])
    ensAll <- paste(as.matrix(gcmStats)[i,3])
    
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var") {
      
      if(!paste(as.matrix(gcmStats)[i,10]) == "ins-yr"){
        
        
        if (ensAll == ens){
            
          yrList <- list.dirs(paste(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files", sep=""), recursive = FALSE, full.names = FALSE)
      
          for (yrDir in yrList) {
      
            yr <- basename(yrDir)
            
            if (yr > startYr) {
              
              if (yr < endYr) {
              
                cat(paste("\n\nProcessing ", rcp, " ", gcm, " ", yr, "\n\n", sep=""))
                
                ncList <- list.files(yrDir, pattern=".nc", full.names = TRUE)
                
                outAscDir <- paste(outDir, "/", rcp, "/", gcm, "/", yr, sep="")
                if (!file.exists(outAscDir)) {
                  dir.create(outDir)
                  dir.create(paste(outDir, "/", rcp, sep=""))
                  dir.create(paste(outDir, "/", rcp, "/", gcm, sep=""))
                  dir.create(outAscDir)
                }
                
                for(nc in ncList){
                  
                  ncName <- basename(nc)
                  var <- sapply(strsplit(ncName, '[_]'), "[[", 1)
                  ncNoExt <- sapply(strsplit(ncName, '[.]'), "[[", 1)
                  
                  if (!var == "tmean") {
                    if (!var == "hur") {
                      if (!var == "rsds") {
                    
                        ncCrop <- crop(rotate(raster(nc)), region)
                        
                        
                        cat(paste("\t ", ncName, sep=""))
                        # ncsplit <- strsplit(nc, '[.]')
                        # ncRaster <- rotate(raster(paste(regionDir, "/baseline/", nc, sep=""))) * 0 + 1
                        
                        if (!file.exists(paste(outAscDir, "/", ncName, sep=""))) {
                          
                          ascWrite <- writeRaster(ncCrop, paste(outAscDir, "/", ncNoExt, ".nc", sep=""), format="CDF", overwrite=F)
                          cat(" ..done\n")
                          
                        } else {cat(" ..done\n")}
                      
                      }
                    }
                  }
                  
                }
              
              }
            }
             
          }
    
        }
    
      } 
    }
  }
  

}