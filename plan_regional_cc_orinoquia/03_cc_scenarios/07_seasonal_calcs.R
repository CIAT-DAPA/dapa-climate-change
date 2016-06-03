##################
## Seasonal
##################
require(raster)

iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/llanos/rcp85"
varList <- c("prec", "tmax", "tmin")
seasons <- list("ann"=1:12, "def"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11)

for (var in varList){
  
  for(i in 1:length(seasons)){
    
    if (!file.exists(paste(iDir, "/", var, "_", names(seasons)[i], ".asc", sep=""))) {
      
      stk <- stack(paste0(iDir, "/", var, "_", seasons[[i]], ".asc"))
      
      if (var == "prec"){
        mean <- sum(stk)
      } else {
        mean <- mean(stk)
      }
      
      writeRaster(mean, paste(iDir, "/", var, "_", names(seasons)[i], ".asc", sep=""), format="ascii", overwrite=F)
      
    }
  }
  
}



iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
varList <- c("rhum")
# varList <- c("prec", "tmax", "tmin")
seasons <- list("ann"=1:12, "def"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11)
ext <- ".asc"

for (var in varList){
  
  for(i in 1:length(seasons)){
    
    if (!file.exists(paste(iDir, "/", var, "_", names(seasons)[i], ext, sep=""))) {
      
      stk <- stack(paste0(iDir, "/", var, "_", seasons[[i]], ext))
      
      if (var == "prec"){
        mean <- sum(stk)
      } else {
        mean <- mean(stk)
      }
      
      writeRaster(mean, paste(iDir, "/", var, "_", names(seasons)[i], ext, sep=""), overwrite=F)
      
    }
  }
  
}


### CHanges by seasons

require(raster)

bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
fDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/llanos/rcp85"
aDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/ideam/rcp85"
mask <- raster("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask_v1.tif")

varList <- c("prec") #, "tmax", "tmin")
seasons <- list("ann"=1:12, "def"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11)

for (var in varList){
  
  for(i in 1:length(seasons)){
    
    # if (!file.exists(paste(aDir, "/", var, "_", names(seasons)[i], ".asc", sep=""))) {
      
      bRs <- raster(paste0(bDir, "/", var, "_", names(seasons)[i], ".asc"))
      fRs <- raster(paste0(fDir, "/", var, "_", names(seasons)[i], ".asc"))
      
      if (var == "prec"){
        mean <- (fRs - bRs) / bRs
      } else {
        mean <- (fRs - bRs)
      }
      
      # writeRaster(mean, paste(aDir, "/", var, "_", names(seasons)[i], ".asc", sep=""), format="ascii", overwrite=F)
      
      mean_msk <- crop(mean, extent(mask))
      mean_msk <- mask(mean_msk, mask) * 100
      writeRaster(mean_msk, paste(aDir, "/", var, "_", names(seasons)[i], "_per.tif", sep=""), format="GTiff", overwrite=F, datatype='FLT4S')
      
      
    }
  # }
  
}






