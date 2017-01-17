#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the Sacks planting and harvesting dates and rasterise them. 
#Convert to WFD's grid of 0.5 x 0.5

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
sdDir <- paste(wd,"/data/crop_calendar_sacks",sep="")
metDir <- paste(wd,"/data/meteorology/wfd_data/Rainf_daily_WFD_GPCC",sep="")

outDir <- paste("~/Leeds-work/climate-for-breeding/data/crop_calendar_sacks")

#read in WFD data for getting mask
mrs <- paste(metDir,"/Rainf_daily_WFD_GPCC_195001.nc",sep="")
nc <- open.ncdf(mrs)
ncdata <- get.var.ncdf(nc,nc$var[["Rainf"]]) #get data from nc connection
brs <- raster(nrow=360,ncol=720,xmn=-180,xmx=180,ymn=-90,ymx=90) #base raster
nt <- ncol(ncdata)
mrs <- brs
mrs[as.numeric(nc$var[["Rainf"]]$dim[[1]]$vals)] <- ncdata[,1]
mrs[which(!is.na(mrs[]))] <- 1
nc <- close.ncdf(nc)

## main season
#read in main season variables: harvest.start, harvest.end, plant.start, plant.end
har_i <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="harvest.start")
har_i <- crop(har_i, mrs)
har_i <- resample(har_i, mrs, method="bilinear")
har_i <- writeRaster(har_i, paste(outDir, "/major_maize_harvest.start.tif",sep=""), format="GTiff")

har_f <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="harvest.end")
har_f <- crop(har_f, mrs)
har_f <- resample(har_f, mrs, method="bilinear")
har_f <- writeRaster(har_f, paste(outDir, "/major_maize_harvest.end.tif",sep=""), format="GTiff")

sow_i <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="plant.start")
sow_i <- crop(sow_i, mrs)
sow_i <- resample(sow_i, mrs, method="bilinear")
sow_i <- writeRaster(sow_i, paste(outDir, "/major_maize_plant.start.tif",sep=""), format="GTiff")

sow_f <- raster(paste(sdDir,"/Maize.crop.calendar.nc",sep=""), varname="plant.end")
sow_f <- crop(sow_f, mrs)
sow_f <- resample(sow_f, mrs, method="bilinear")
sow_f <- writeRaster(sow_f, paste(outDir, "/major_maize_plant.end.tif",sep=""), format="GTiff")





