library(raster)

obsDir <- "S:/observed/gridded_products/chirps/daily"
iDir <- "D:/cenavarro/request/request_ee/00_admin_data"
oDir <- "D:/cenavarro/request/request_ee/01_baseline/col_salvajina"
obsPer <- "1981_2025"
region <- "col_salvajina"
maskCnt <- raster(paste(iDir,"/", region, ".tif", sep=''))
# maskCnt <- extent(maskCnt)
# maskCnt <- extent(raster(xmn=xmin(maskCnt)-1, xmx=xmax(maskCnt)+1, ymn=ymin(maskCnt)-1, ymx=ymax(maskCnt)+1))
years.hist = strsplit(obsPer, "_")[[1]][1]:strsplit(obsPer, "_")[[1]][2]  
chirps_data <- list.files(path=obsDir, pattern='*.tif$', full.names=FALSE)

croprs <- function(obsDir="", maskCnt="", oDir="", chirps_data="", i){
  library(raster)  
  require(rgdal)
  cat(i, "\n")
  tif <- chirps_data[i]
  if(!file.exists(tif)){
    rs_crop <- mask(crop(raster(paste0(obsDir, "/", tif)), maskCnt), maskCnt)
    writeRaster(rs_crop, paste0(oDir, "/", tif), format="GTiff", overwrite=T, datatype='INT2S')
  }
}
# 
# resample <- function(obsDir="", maskCnt="", oDir="", chirps_data="", i){
#   library(raster)  
#   cat(i, "\n")
#   tif <- chirps_data[i]
#   rs_resample <- resample(raster(paste0(obsDir, "/", tif)), raster(maskCnt, res=0.25))
#   writeRaster(rs_resample, paste0(oDir, "/", tif), format="GTiff", overwrite=T, datatype='INT2S')
# }

## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

# Export functions and variables
sfExport("croprs")
#sfExport("resample")
sfExport("obsDir")
sfExport("maskCnt")
sfExport("oDir")
sfExport("chirps_data")

control <- function(i) { #define a new function
  
  library(raster)    
  cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
  croprs(obsDir, maskCnt, oDir, chirps_data, i)
  #resample(obsDir, maskCnt, oDir, chirps_data, i)
  
}

system.time(sfSapply(as.vector(1:length(chirps_data)), control))


#stop the cluster calculation
sfStop()

