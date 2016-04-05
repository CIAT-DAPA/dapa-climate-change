### Author: Julián Ardila & Carlos Navarro c.e.navarro@cgiar.org
### Date: February 2016
### Purpouse: Get the downscaling outputs from interpolated surfaces and IDEAM's anomalies interpolated 

require(raster)
require(ncdf)
require(maptools)
require(rgdal)

bDir  <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima"
mask <- raster("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask_v1.tif")
varList <- c("prec", "tmin", "tmax")
rcpList <- c("rcp26", "rcp45", "rcp85")

for (rcp in rcpList){
  
  ####ruta de promedios de estaciones
  bslDir <- paste0(bDir, "/baseline/llanos/average")
  anomDir <- paste0(bDir, "/anomalias/ideam/", rcp)
  oDir <- paste0(bDir, "/downscaling/llanos/", rcp)
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (var in varList){
    
    cat("Donwscaling process over ", rcp, var, "\n")
    
    for(j in 1:12){
      
      oTif <- paste0(oDir, "/", var, "_",j, ".tif")
      
      if (!file.exists(oTif)){
      
        if (!file.exists(paste0(oDir, "/", var, "_", j, ".asc"))){
          
        ##carga las anomalias de cada gcm segun rcp especificado
        anom <- raster(paste0(anomDir, "/", var, "_", j, ".asc"))
        
        ###carga datos de promedios generados de estaciones
        bsl <- raster(paste0(bslDir, "/", var, "_", j, ".asc"))
        
        crs(anom) <- crs(bsl)
        anom <- resample(anom, bsl)
        
        if (var == "prec"){
          del <- bsl * abs( 1 + (anom / 100))
        } else {
          del <- bsl + anom
        }
        
        writeRaster(del, paste0(oDir, "/", var, "_", j, ".asc"))
        
        } else {
          
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
  
  }
  
  cat("Donwscaling process over ", rcp, "tmean", "\n")
  
  ## Calcular temperatura media
  for (j in 1:12) {
    oTif <- paste0(oDir, "/tmean_",j, ".tif")
    
    del_tx <- stack(paste0(oDir, "/tmax_", j, ".tif"))
    del_tn <- stack(paste0(oDir, "/tmin_", j, ".tif"))
    del <- (del_tx + del_tn) / 2
    
#     del <- crop(del, extent(mask))
#     del <- mask(del, mask)
    writeRaster(del, oTif, format="GTiff", datatype='INT2S')
    
  }
  
}

