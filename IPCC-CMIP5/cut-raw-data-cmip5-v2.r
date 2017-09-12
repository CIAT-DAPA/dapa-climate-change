require(raster)
require(ncdf)
require(rgdal)

source("cut-raw-data-cmip5.r")
baseDir <- "T:/gcm/cmip5/raw/monthly"
region <- extent(-82, -63, -7, 16)
outDir <- "D:/jetarapues/Request/Request_oguerra"
startYr <- 2015
endYr <- 2030
ens <- "r1i1p1"
rcp <- "rcp45"
gcmlist=c("bcc_csm1_1", "bcc_csm1_1_m", "ncar_ccsm4", "cesm1_cam5", "csiro_mk3_6_0", "fio_esm", "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_r", "nimr_hadgem2_ao", "mohc_hadgem2_es", "ipsl_cm5a_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mri_cgcm3", "ncc_noresm1_m")
otp <- GCMCut_Raw(baseDir, region, startYr, endYr, ens, rcp, outDir,gcmlist)


GCMCut_Raw <- function(baseDir="S:/data/gcm/cmip3/raw_data", region=extent(-180, 0, -90, 90), startYr=2015, endYr=2030, ens="r1i1p1", rcp="rcp45", outDir="D:/Request/request_oguerra") {
  
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  
  for (rcp in rcpList) {
    
    rcpDir <- paste(baseDir, "/", rcp, sep="")
    
    checkALL=gcmlist[which(gcmlist=="ALL")]
    if(length(checkALL)==1){
      gcmlist <-  list.dirs(rcpDir, recursive = FALSE, full.names = FALSE)
    }
    # 
    # gcmStats <- read.table(paste(baseDir, "/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
    # monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
    
    for (i in 1:length(gcmlist)){
     
      gcm <- gcmlist[i]
  
      yrList <- list.dirs(paste(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files", sep=""), recursive = FALSE, full.names = F)
      
      yrList <-yrList[which(yrList>=startYr & yrList<=endYr)]
      
      for (yrDir in yrList) {
  
        yr <- basename(yrDir)
  
            cat(paste("\n\nProcessing ", rcp, " ", gcm, " ", yr, "\n\n", sep=""))
            
            ncList <- list.files(paste0(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files/",yrDir) , pattern=".nc", full.names = TRUE)
            
            outAscDir <- paste(outDir, "/", rcp, "/", gcm, "/", yr, sep="")
            if (!file.exists(outAscDir)) {
              #dir.create(outDir)
              dir.create(paste(outDir, "/", rcp, sep=""))
              dir.create(paste(outDir, "/", rcp, "/", gcm, sep=""))
              dir.create(outAscDir)
            }
            
            for(nc in ncList){
              
              ncName <- basename(nc)
              var <- sapply(strsplit(ncName, '[_]'), "[[", 1)
              ncNoExt <- sapply(strsplit(ncName, '[.]'), "[[", 1)
              
              if (!var == "tme") {
                if (!var == "hur") {
                  if (!var == "rsds") {
                
                    ncCrop <- crop(rotate(raster(nc)), region)
                    if (var == "prec"){
                      ncCrop <- ncCrop * 86400
                    }else if (var == "srad") {
                      ncCrop <- ncCrop * 0.0864  # W m-2 to MJ m-2 day-1
                    }else if(var =="tmax" || var =="tmin" || var =="tmean") {
                      ncCrop <- ncCrop - 273.15
                    }
                    
                    cat(paste("\t ", ncName, sep=""))
                    # ncsplit <- strsplit(nc, '[.]')
                    # ncRaster <- rotate(raster(paste(regionDir, "/baseline/", nc, sep=""))) * 0 + 1
                    
                    if (!file.exists(paste(outAscDir, "/", ncName, sep=""))) {
                      
                      ascWrite <- writeRaster(ncCrop, paste(outAscDir, "/", ncNoExt, ".tif", sep=""), format="GTiff", overwrite=T)
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