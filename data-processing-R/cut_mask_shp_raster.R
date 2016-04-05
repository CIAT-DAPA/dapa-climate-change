require(raster)
require(maptools)
require(rgdal)

dirbase <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

# setwd(dirbase)

rsList <- list.files(dirbase, pattern=".asc", full.names = TRUE)

outDir <- paste(dir, "/_cropped", sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

for(rs in rsList){
  
  rsName <- basename(rs)
  
  if (!file.exists(paste0(outDir, "/", rsName, sep=""))) {
    rsCrop <- crop(raster(rs), extent(mask))
    rsMask <- mask(rsCrop, mask)
    
    ascWrite <- writeRaster(rsMask, paste0(outDir, "/", rsName, sep=""), overwrite=F)
    cat(paste0(" ", rsName, " cut done\n"))
  }
  
}
