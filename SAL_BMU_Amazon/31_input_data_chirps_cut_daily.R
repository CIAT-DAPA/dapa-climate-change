# sfStop()

library(raster)

# obsDir <- "S:/observed/gridded_products/chirps/daily/32bits"
obsDir <- "/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/02_Gridded_data/chirps_0_05deg_amz"
iDir <- "/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"
# oDir <- "Z:/DATA/WP2/02_Gridded_data/chirps_0_05deg_amz"
oDir <- "/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/02_Gridded_data/chirps_0_25deg_amz"
obsPer <- "1981_2010"
region <- "amz"
maskCnt <- raster(paste(iDir,"/data/", region, "_regions_rst/", region, "_base_lr.tif", sep=''))
maskCnt <- extent(maskCnt)
# maskCnt <- extent(raster(xmn=xmin(maskCnt)-1, xmx=xmax(maskCnt)+1, ymn=ymin(maskCnt)-1, ymx=ymax(maskCnt)+1))
years.hist = strsplit(obsPer, "_")[[1]][1]:strsplit(obsPer, "_")[[1]][2]  
chirps_data <- list.files(path=obsDir, pattern='*.tif$', full.names=FALSE)


croprs <- function(obsDir="", maskCnt="", oDir="", chirps_data="", i){
  cat(i, "\n")
  tif <- chirps_data[i]
  rs_crop <- crop(raster(paste0(obsDir, "/", tif)), maskCnt)
  writeRaster(rs_crop, paste0(oDir, "/", tif), format="GTiff", overwrite=T, datatype='INT2S')
}

res_chirps <- function(obsDir="", maskCnt="", oDir="", chirps_data="", i){
  cat(i, "\n")
  tif <- chirps_data[i]
  if (!file.exists(paste0(oDir, "/", tif))){
    rs_resample <- resample(raster(paste0(obsDir, "/", tif)), raster(maskCnt, res=0.25))
    writeRaster(rs_resample, paste0(oDir, "/", tif), format="GTiff", overwrite=F, datatype='INT2S')    
  }
}

## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=24) #initiate cluster

# Export functions and variables
# sfExport("croprs")
sfExport("res_chirps")
sfExport("obsDir")
sfExport("maskCnt")
sfExport("oDir")
sfExport("chirps_data")

control <- function(i) { #define a new function
  library(raster)
  cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
  # croprs(obsDir, maskCnt, oDir, chirps_data, i)
  res_chirps(obsDir, maskCnt, oDir, chirps_data, i)
  
}

system.time(sfSapply(as.vector(1:length(chirps_data)), control))


#stop the cluster calculation
sfStop()

