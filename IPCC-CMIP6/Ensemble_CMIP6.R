# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Ensemble of CMIP6 GCM data

# Load libraries
require(raster)
require(ncdf)
require(maptools)
require(rgdal)
require(stringr)

## Parameters
dDir <- "E:/ipcc_6ar_wcl_downscaled"
oDir <- "D:/cenavarro/request/ajines"
# scn_list <- c("ssp_126", "ssp_245", "ssp_370", "ssp_585")
scn_list <- c("ssp_245", "ssp_585")
perList <- c("2050s", "2090s")
# perList <- c("2030s", "2050s", "2070s", "2090s")
varList <- c("prec", "tmax", "tmin")
res <- "2_5min"
resmod <- "2_5m"
mask <- readOGR("S:/admin_boundaries/regional-files/Africa/admin-0-countries.shp")

# seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

for (scn in scn_list){
  
  for (period in perList) {
    
    oDirEns <- paste0(oDir, "/", scn, "/", period)
    if (!file.exists(oDirEns)) {dir.create(oDirEns, recursive=T)}
    
    cat("Ensemble over: ", scn, period, "\n")
    
    gcmList <- list.dirs(paste0(dDir, "/", scn, "/", period), recursive = FALSE, full.names = FALSE)
    gcmList <- gcmList[!grepl("bcc_csm2_mr", gcmList)]
    
    for (var in varList){
      
      setwd(paste(dDir, "/", scn, "/", period, sep=""))
      
      gcmStack <- stack(paste0(gcmList, "/", res, "/", gcmList, "_", str_replace(scn, "_", ""), "_", period, "_", var, "_", resmod, "_no_tile_tif", ".tif"))
      gcmStack_crop <- crop(gcmStack, mask)
      
      
      for (mth in 1:12){
        
        if (!file.exists(paste(oDirEns, "/", var, "_", mth, ".tif", sep=""))) {
          
          gcmStack_crop_mth <- gcmStack_crop[[seq(0, 22)*12+mth]]
          gcmMean <- mean(gcmStack_crop_mth, na.rm=TRUE)
          writeRaster(gcmMean, paste(oDirEns, "/", var, "_", mth, '.tif',sep=''), format="GTiff", overwrite=T)
          
        }
        
      }
      
    }
    
  }
  
}


