#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the CIMMYT ME data and rasterise it. Convert to Iizumi's grid of 1.125 x 1.125

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
sdDir <- paste(wd,"/data/crop_calendar_sacks",sep="")
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

## main season
#read in main season variables: harvest.start, harvest.end, plant.start, plant.end
har_i <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="harvest.start")
har_i <- crop(har_i, yrs)
har_i <- resample(har_i, yrs, method="bilinear")
har_i <- writeRaster(har_i, paste(sdDir, "/major_maize_harvest.start.tif",sep=""), format="GTiff")

har_f <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="harvest.end")
har_f <- crop(har_f, yrs)
har_f <- resample(har_f, yrs, method="bilinear")
har_f <- writeRaster(har_f, paste(sdDir, "/major_maize_harvest.end.tif",sep=""), format="GTiff")

sow_i <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="plant.start")
sow_i <- crop(sow_i, yrs)
sow_i <- resample(sow_i, yrs, method="bilinear")
sow_i <- writeRaster(sow_i, paste(sdDir, "/major_maize_plant.start.tif",sep=""), format="GTiff")

sow_f <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="plant.end")
sow_f <- crop(sow_f, yrs)
sow_f <- resample(sow_f, yrs, method="bilinear")
sow_f <- writeRaster(sow_f, paste(sdDir, "/major_maize_plant.end.tif",sep=""), format="GTiff")


### second season
#read in main season variables: harvest.start, harvest.end, plant.start, plant.end
har_i <- raster(paste(sdDir,"/Maize.2.crop.calendar.nc",sep=""), varname="harvest.start")
har_i <- crop(har_i, yrs)
har_i <- resample(har_i, yrs, method="bilinear")
har_i <- writeRaster(har_i, paste(sdDir, "/second_maize_harvest.start.tif",sep=""), format="GTiff")

har_f <- raster(paste(sdDir,"/Maize.2.crop.calendar.nc",sep=""), varname="harvest.end")
har_f <- crop(har_f, yrs)
har_f <- resample(har_f, yrs, method="bilinear")
har_f <- writeRaster(har_f, paste(sdDir, "/second_maize_harvest.end.tif",sep=""), format="GTiff")

sow_i <- raster(paste(sdDir,"/Maize.2.crop.calendar.nc",sep=""), varname="plant.start")
sow_i <- crop(sow_i, yrs)
sow_i <- resample(sow_i, yrs, method="bilinear")
sow_i <- writeRaster(sow_i, paste(sdDir, "/second_maize_plant.start.tif",sep=""), format="GTiff")

sow_f <- raster(paste(sdDir,"/Maize.2.crop.calendar.nc",sep=""), varname="plant.end")
sow_f <- crop(sow_f, yrs)
sow_f <- resample(sow_f, yrs, method="bilinear")
sow_f <- writeRaster(sow_f, paste(sdDir, "/second_maize_plant.end.tif",sep=""), format="GTiff")


