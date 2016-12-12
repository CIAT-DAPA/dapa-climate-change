### Author: Julián Ardila & Carlos Navarro c.e.navarro@cgiar.org
### Date: February 2016

require(raster)
require(ncdf)
require(maptools)
require(rgdal)

bDir  <- "Z:/DATA/WP2"
mask <- raster(paste0(bDir, "/00_zones/region/5km_k/mask"))
varList <- c("prec", "tmin", "tmax")
rcpList <- c("rcp26", "rcp45", "rcp85")
perList <- c("2020_2049", "2040_2069", "2070_2099")
# rcp <- "rcp26"
rcp <- "rcp45"
# rcp <- "rcp85"

# for (rcp in rcpList){
  
  gcmList <- list.dirs(paste0(bDir, "/03_Future_data/anomalies_2_5min/", rcp), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList) {
    
    for (period in perList) {
      
      ####ruta de promedios de estaciones
      bslDir <- paste0(bDir, "/02_Gridded_data/baseline_2_5min/average")
      anomDir <- paste0(bDir, "/03_Future_data/anomalies_2_5min/", rcp, "/", gcm, "/", period)
      oDir <- paste0(bDir, "/03_Future_data/downscaling_bsl_2_5min/", rcp, "/", gcm, "/", period)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      
      for (var in varList){
        
        cat("Donwscaling process over ", rcp, var, "\n")
        
        for(j in 1:12){
          
          oTif <- paste0(oDir, "/", var, "_",j, ".tif")
          
          if (!file.exists(oTif)){
            
            if (!file.exists(paste0(oDir, "/", var, "_", j, ".asc"))){
              
              ##carga las anomalias de cada gcm segun rcp especificado
              anom <- raster(paste0(anomDir, "/", var, "_", j, ".nc"))
              
              ###carga datos de promedios generados de estaciones
              bsl <- raster(paste0(bslDir, "/", var, "_1981_2010_", j, ".asc"))
              
              crs(anom) <- crs(bsl)
              #               anom <- resample(anom, bsl)
              
              if (var == "prec"){
                del <- bsl * abs( 1 + anom)
              } else {
                del <- bsl + anom
              }
              
              writeRaster(del, paste0(oDir, "/", var, "_", j, ".asc"))
              
            } 
            
            
            if (var == "prec"){
              del_msk <- raster(paste0(oDir, "/", var, "_", j, ".asc"))
            } else {
              del_msk <- raster(paste0(oDir, "/", var, "_", j, ".asc"))* 10
            }
            
            del_msk <- crop(del_msk, extent(mask))
            del_msk <- mask(del_msk, mask)
            writeRaster(del_msk, oTif, format="GTiff", overwrite=F, datatype='INT2S')
            
          }
          
        }
        
      }
      
      cat("Donwscaling process over ", rcp, "tmean", "\n")
      
      ## Calcular temperatura media
      for (j in 1:12) {
        oTif <- paste0(oDir, "/tmean_",j, ".tif")
        
        if (!file.exists(oTif)){
          del_tx <- stack(paste0(oDir, "/tmax_", j, ".tif"))
          del_tn <- stack(paste0(oDir, "/tmin_", j, ".tif"))
          del <- (del_tx + del_tn) / 2
          dtr <- (del_tx - del_tn)
          
          #     del <- crop(del, extent(mask))
          #     del <- mask(del, mask)
          writeRaster(del, oTif, format="GTiff", datatype='INT2S')
          writeRaster(dtr, paste0(oDir, "/dtr_",j, ".tif"), format="GTiff", datatype='INT2S')
          
        }
      }
      
    }  
  }
  
  cat("Ensemble over: ", rcp)
  varList <- c("prec", "tmin", "tmax", "tmean", "dtr")
    
  for (period in perList) {
    
    oDirEns <- paste0(bDir, "/03_Future_data/downscaling_bsl_2_5min_ens/", rcp, "/", period)
    if (!file.exists(oDirEns)) {dir.create(oDirEns, recursive=T)}
    
    setwd(paste(bDir, "/03_Future_data/downscaling_bsl_2_5min/", rcp, sep=""))
    
    for (var in varList){
      
      for (mth in 1:12){

        gcmStack <- stack(lapply(paste0(gcmList, "/", period, "/", var, "_", mth, ".tif"),FUN=raster))
        
        gcmMean <- mean(gcmStack)
        fun <- function(x) { sd(x) }
        gcmStd <- calc(gcmStack, fun)
        
        gcmMean <- trunc(gcmMean)
        gcmStd <- trunc(gcmStd)
        
        gcmMean <- writeRaster(gcmMean, paste(oDirEns, "/", var, "_", mth, ".tif", sep=""), overwrite=FALSE, format="GTiff", datatype='INT2S')
        gcmStd <- writeRaster(gcmStd, paste(oDirEns, "/", var, "_", mth, "_sd.asc", sep=""), overwrite=FALSE)
        
      }
    }
    
  }
# }

