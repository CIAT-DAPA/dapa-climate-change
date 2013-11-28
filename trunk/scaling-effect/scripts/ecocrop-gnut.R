#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#produce EcoCrop runs for maize in the Sahel for the scaling study
###

#load packages
library(rgdal); library(raster); library(maptools); data(wrld_simpl)

#source functions
#src.dir <- "/mnt/a102/eejarv/scaling-effect"
#src.dir <- "/nfs/a102/eejarv/scaling-effect"
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
figDir <- paste(bDir,"/figures_new",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

if (!file.exists(runDir)) {dir.create(runDir)}
if (!file.exists(figDir)) {dir.create(figDir)}
crop_name <- "gnut"

######
###### prepare planting dates first

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))

#load sow and harvest dates
if (!file.exists(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest.tif",sep=""))) {
  pdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/plant.filled.asc",sep=""))
  hdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/harvest.filled.asc",sep=""))
  
  #crop the data
  pdate <- crop(pdate,msk)
  hdate <- crop(hdate,msk)
  
  #write crop calendar
  pdate <- writeRaster(pdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant.tif",sep=""),format="GTiff")
  hdate <- writeRaster(hdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest.tif",sep=""),format="GTiff")
} else {
  pdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant.tif",sep=""))
  hdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest.tif",sep=""))
}

#crop and plot the harvested area data
if (!file.exists(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))) {
  ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/harvested.area.fraction.asc",sep=""))
  ahar <- crop(ahar,msk)
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""),format="GTiff")
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.nc",sep=""),format="CDF")
} else {
  ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
}

#crop and plot the yield data
if (!file.exists(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""))) {
  yield <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/groundnut_yield.asc",sep=""))
  yield <- crop(yield,msk)
  yield <- writeRaster(yield,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""),format="GTiff")
  yield <- writeRaster(yield,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.nc",sep=""),format="CDF")
} else {
  yield <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""))
}


#### spam data processing
if (!file.exists(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv_spam.tif",sep=""))) {
  ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/spam2000v3r6_harvested-area_total_Groundnut.asc",sep=""))
  ahar <- crop(ahar,msk)
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv_spam.tif",sep=""),format="GTiff")
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv_spam.nc",sep=""),format="CDF")
} else {
  ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv_spam.tif",sep=""))
}


#resample crop calendar to all cascade resolutions
resol <- "12km"
if (!file.exists(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest_",resol,".tif",sep=""))) {
  tmsk <- raster(paste(lsmDir,"/Glam_",resol,"_lsm.nc",sep=""))
  tmsk[which(tmsk[] > 0)] <- 1
  tmsk[which(tmsk[] < 0)] <- 0
  
  fac <- round(xres(tmsk)/xres(pdate))
  if (fac == 1) {
    #resample the thing
    tpdate <- resample(pdate,tmsk,method="ngb")
    thdate <- resample(hdate,tmsk,method="ngb")
  } else {
    fac <- xres(tmsk)/xres(pdate)
    #tpdate <- aggregate(pdate,fact=fac,FUN=function(x) {round(min(x,na.rm=T),0)},expand=T)
    tpdate <- pdate
    tpdate <- resample(tpdate,tmsk,method="ngb")
    
    #thdate <- aggregate(hdate,fact=fac,FUN=function(x) {round(mean(x,na.rm=T),0)},expand=T)
    thdate <- hdate
    thdate <- resample(thdate,tmsk,method="ngb")
  }
  
  #write output raster
  tpdate <- writeRaster(tpdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant_",resol,".tif",sep=""),format="GTiff")
  thdate <- writeRaster(thdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest_",resol,".tif",sep=""),format="GTiff")
  rm(list=c("tpdate","thdate","tmsk"))
}


