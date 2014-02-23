#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the CIMMYT ME data and rasterise it. Convert to Iizumi's grid of 1.125 x 1.125

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
soDir <- paste(wd,"/data/soils",sep="")
sdataDir <- "~/Leeds-work/datasets/soils"
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

#read in soil data
rll_rs <- raster(paste(sdataDir,"/Glam_FAO_SOIL.nc",sep=""),varname="rll")
rll_rs <- crop(rll_rs, yrs)
rll_rs <- resample(rll_rs, yrs, method="bilinear")
rll_rs <- writeRaster(rll_rs, paste(soDir, "/rll_lr.tif",sep=""), format="GTiff")

dul_rs <- raster(paste(sdataDir,"/Glam_FAO_SOIL.nc",sep=""),varname="dul")
dul_rs <- crop(dul_rs, yrs)
dul_rs <- resample(dul_rs, yrs, method="bilinear")
dul_rs <- writeRaster(dul_rs, paste(soDir, "/dul_lr.tif",sep=""), format="GTiff")

sat_rs <- raster(paste(sdataDir,"/Glam_FAO_SOIL.nc",sep=""),varname="sat")
sat_rs <- crop(sat_rs, yrs)
sat_rs <- resample(sat_rs, yrs, method="bilinear")
sat_rs <- writeRaster(sat_rs, paste(soDir, "/sat_lr.tif",sep=""), format="GTiff")

#calculate asw from dul-rll
asw_rs <- dul_rs - rll_rs
asw_rs <- writeRaster(asw_rs, paste(soDir, "/asw_lr.tif",sep=""), format="GTiff")


