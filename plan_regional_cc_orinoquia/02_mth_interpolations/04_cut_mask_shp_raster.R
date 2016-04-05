require(raster)
require(maptools)
require(rgdal)

dirbase <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
mask <- raster("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask_v1.tif")

varList <- c("prec", "tmax", "tmin")
outDir <- paste(dirbase, sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

setwd(dirbase)

# for(rs in rsList){
for (var in varList){
  
  rsStk <- stack(paste0(dirbase, "/", var, "_", 1:12, ".asc"))
  
  if (!file.exists(paste0(outDir, "/", var, "_", 12, ".tif"))) {
    
    rsCrop <- crop(rsStk, extent(mask))
    rsMask <- mask(rsCrop, mask)
    
    if (var == "prec"){
      rsMask <- round(rsMask, digits = 0)
    } else {
      rsMask <- round(rsMask * 10, digits = 0)
    }
    
    for (i in 1:12){
      
      oTif <- paste0(outDir, "/", var, "_",i, ".tif")
      tifWrite <- writeRaster(rsMask[[i]], oTif, format="GTiff", overwrite=T, datatype='INT2S')
      cat(paste0(" ", var, "_",i, " cut done\n"))
      
    }
  }
}



