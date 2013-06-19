#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

### prepare the Sacks calendar for necessary analyses.

#load libraries
library(raster); library(rgdal)

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
solDir <- paste(envDir,"/soil",sep="")

#load indian extent
msk <- raster(paste(clmDir,"/wcl_ind_2_5min/prec_1.tif",sep=""))
msk <- extent(msk)

#load soil dul and dll
dul <- raster(paste(solDir,"/Glam_FAO_SOIL.nc",sep=""),varname="dul")
dll <- raster(paste(solDir,"/Glam_FAO_SOIL.nc",sep=""),varname="rll")

#crop to Indian extent
dul <- crop(dul,msk)
dll <- crop(dll,msk)

#calculate asw
asw <- dul-dll #there is not much spatial variability in this one: perhaps just use dul

#write rasters
writeRaster(dul,paste(solDir,"/dul_ind.tif",sep=""),format="GTiff")
writeRaster(dll,paste(solDir,"/dll_ind.tif",sep=""),format="GTiff")
writeRaster(asw,paste(solDir,"/asw_ind.tif",sep=""),format="GTiff")




