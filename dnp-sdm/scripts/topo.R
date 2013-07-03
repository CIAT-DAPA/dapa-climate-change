#JRV 2013
#CIAT / CCAFS
stop("!")

#load libraries
library(raster)

#working directory
#wd <- "/media/DATA/CIAT_work/DNP-biodiversity"
wd <- "/nfs/a102/eejarv/DNP-biodiversity"
setwd(wd)

#1. load mask (from bioclimatic)
bioDir <- "./env-data/bioclim_gtiff"
msk <- raster(paste(bioDir,"/bio_1.tif",sep=""))

#2. load elevation dataset
topDir <- "./env-data/topography"
alt <- raster(paste(topDir,"/SRTM_1km.tif",sep=""))

#3. crop the elevation dataset to the desired mask
if (!file.exists(paste(topDir,"/alt.tif",sep=""))) {
  alt_c <- crop(alt,msk,filename=paste(topDir,"/alt.tif",sep=""),format="GTiff")
} else {
  alt_c <- raster(paste(topDir,"/alt.tif",sep=""))
}

#4. use Horn (1981) (neighbors = 8) algorithm (mor suitable for smoother surfaces)
if (!file.exists(paste(topDir,"/slope.tif",sep=""))) {
  slp <- terrain(alt_c,opt='slope',unit='degrees',neighbors=8,
                 filename=paste(topDir,"/slope.tif",sep=""),format="GTiff")
}

if (!file.exists(paste(topDir,"/aspect.tif",sep=""))) {
  asp <- terrain(alt_c,opt='aspect',unit='degrees',neighbors=8,
                 filename=paste(topDir,"/aspect.tif",sep=""),format="GTiff")
}


