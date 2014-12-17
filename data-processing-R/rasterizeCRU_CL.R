##############################################################################
# Name: Rasterize CRU Climatology (v.2 1961-1990)
# Purpose: Script to generate rasterized surfaces from CRU_CL_v2.0
# Author: Carlos Navarro
##############################################################################

library(raster)
library(rgdal)

iDir <- "S:/observed/gridded_products/cru-cl-v2-0"
oDir <- paste(iDir, "/global_10min", sep="")

if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}

varList <- c("pre", "tmp", "dtr")

for (var in varList){
  
  if (var == "pre"){
    varmod <- "prec"
  } else if (var == "tmp"){
    varmod <- "tmean"
  } else{
    varmod <- var
  }
  
  datFile <- read.table(paste(iDir, "/grid_10min_", var, ".dat", sep=""))
  names(datFile) <- c("lat", "lon", 1:12)
  
  basRs <- raster(nrows=1080, ncols=2160, xmn=-180, xmx=180, ymn=-90, ymx=90)
  
  coords <- cbind(datFile[2],datFile[1])

  
  for (mth in 1:12){

    outTif <- paste(oDir, "/", varmod, "_", mth, ".tif", sep="")
    
    if (!file.exists(outTif)) {
      
      outRs <- rasterize(coords, basRs, datFile[mth+2], fun='last')  
      outRs <- writeRaster(outRs, outTif, format='GTiff', overwrite=FALSE)
      
      cat(" .> ", paste("\t ", varmod, "_", mth, sep=""), "\tdone!\n")
      
    }
  }
  
}


for (mth in 1:12){
  
  outTmin <- paste(oDir, "/tmin_", mth, ".tif", sep="")  
  outTmax <- paste(oDir, "/tmax_", mth, ".tif", sep="")
  
  if (!file.exists(outTmin)) {

    tmean <- raster(paste(oDir, "/tmean_", mth, ".tif", sep=""))
    dtr <- raster(paste(oDir, "/dtr_", mth, ".tif", sep=""))
    
    tmax <- tmean + 0.5 * dtr
    tmin <- tmean - 0.5 * dtr
    
    outRs <- writeRaster(tmax, outTmax, format='GTiff', overwrite=FALSE)
    cat(" .> ", paste("\t tmax_", mth, sep=""), "\tdone!\n")
    
    outRs <- writeRaster(tmin, outTmin, format='GTiff', overwrite=FALSE)
    cat(" .> ", paste("\t tmin_", mth, sep=""), "\tdone!\n")
 
  }
}

