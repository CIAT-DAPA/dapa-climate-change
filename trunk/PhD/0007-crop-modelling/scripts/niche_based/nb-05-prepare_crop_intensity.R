#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

### prepare the Ramankutty cropping intensity layer

#load libraries
library(raster); library(rgdal)

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
cinDir <- paste(envDir,"/crop_intensity",sep="")

#load indian extent
msk <- raster(paste(clmDir,"/wcl_ind_2_5min/prec_1.tif",sep=""))
msk <- extent(msk)

#load soil dul and dll
cintens <- raster(paste(cinDir,"/Cropland2000_5min.tif",sep=""))

#crop to Indian extent
cintens <- crop(cintens,msk)
cintens <- cintens*100

#write rasters
writeRaster(cintens,paste(cinDir,"/ci_ind.tif",sep=""),format="GTiff")



