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
runDir <- paste(bDir,"/model-runs_gnut",sep="")
figDir <- paste(bDir,"/figures_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

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
  
  #fix the harvest date raster
  hdate[which(hdate[] == 364.5)] <- 304.0
  hdate[which(hdate[] == 322.5)] <- 304.0
  hdate[which(hdate[] == 309.0)] <- 304.0
  hdate[which(hdate[] == 228.0)] <- 304.0
  
  #fix the planting date raster
  pdate[which(pdate[] == 182.0)] <- 177.5
  pdate[which(pdate[] == 172.0)] <- 177.5
  pdate[which(pdate[] == 171.5)] <- 177.5
  pdate[which(pdate[] == 92.0)] <- 177.5
  pdate[which(pdate[] == 168.5)] <- 177.5
  
  hdate@crs <- wrld_simpl@proj4string
  pdate@crs <- wrld_simpl@proj4string
  
  #write crop calendar
  pdate <- writeRaster(pdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant.tif",sep=""),format="GTiff",overwrite=T)
  hdate <- writeRaster(hdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest.tif",sep=""),format="GTiff",overwrite=T)
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

#crop the yield data
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
for (resol in c("12km","3deg")) {
  #resol <- "12km"
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
    tpdate <- writeRaster(tpdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant_",resol,".tif",sep=""),format="GTiff",overwrite=T)
    thdate <- writeRaster(thdate,paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest_",resol,".tif",sep=""),format="GTiff",overwrite=T)
    rm(list=c("tpdate","thdate","tmsk"))
  }
}

###
### run model for initial testing
#parameter values

###from PhD thesis
params <- read.csv(paste(runDir,"/parameter_sets.csv",sep=""))
selpar <- read.csv(paste(runDir,"/runs_discard.csv",sep=""))#[,c("RUN","SEL")]

#select highest auc one
maxauc <- selpar$RUN[which(selpar$HIGH.AUC == max(selpar$HIGH.AUC))]
params <- params[which(params$RUN == 7),]

#trial 1
rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
tkill <- params$KILL[2]; tmin <- params$MIN[2]; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- params$MAX[2] #trial 1

#trial 2
rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
tkill <- params$KILL[2]; tmin <- params$MIN[2]; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- 400 #trial 1

#trial 3
rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
tkill <- params$KILL[2]; tmin <- 100; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- 400 #trial 1

trial <- 3
outf <- paste(runDir,"/calib/run_",trial,sep="")

#run the model
eco <- suitCalc(climPath=paste(clmDir,"/sahel_5min",sep=""), 
                sowDat=pdate@file@name,
                harDat=hdate@file@name,
                Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                Ropmax=ropmax,Rmax=rmax, 
                outfolder=outf,
                cropname=crop_name,ext=".tif",cropClimate=F)

#plot the results
png(paste(outf,"/out_psuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[1]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_tsuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[2]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()



####
#### goes with 3 as the final parameterisation. Although 5 and 4 are not too different
####

#2. run model with each of the cascade runs
for (resol in resList) {
  #resol <- resList[1]
  odataDir <- paste(clmDir,"/cascade_",resol,sep="")
  
  trunDir <- paste(runDir,"/",resol,sep="")
  if (!file.exists(trunDir)) {dir.create(trunDir)}
  
  #option 2. fix the sowing and harvesting dates to 153 (if earlier) and 296 (if later)
  tpdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant_",gsub("_exp","",resol),".tif",sep=""))
  thdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest_",gsub("_exp","",resol),".tif",sep=""))
  
  #correct sowing date
  tpdate2 <- tpdate
  tpdate2[which(tpdate[] < 153)] <- 153
  tpdate2[which(tpdate[] > 270)] <- NA
  
  #correct harvest date
  thdate2 <- thdate
  thdate2[which(thdate[] > 296)] <- 296
  thdate2[which(thdate[] < 150)] <- NA
  thdate2[which(thdate[] < 215)] <- 243
  
  #calculate crop duration
  calcdur <- function(x) {
    a <- x[1] #a is sowing date
    b <- x[2] #b is harvest value
    if (is.na(a) | is.na(b)) {
      z <- NA
    } else {
      if (a > b) {z <- NA} else {z <- b - a}
    }
    return(z)
  }
  
  tdur <- calc(stack(tpdate2,thdate2),fun=calcdur)
  
  #write the three datasets
  tcalDir <- paste(trunDir,"/calendar",sep="")
  if (!file.exists(tcalDir)) {dir.create(tcalDir)}
  
  tpdate2 <- writeRaster(tpdate2,paste(tcalDir,"/plant_",resol,".tif",sep=""),format="GTiff")
  thdate2 <- writeRaster(thdate2,paste(tcalDir,"/harvest_",resol,".tif",sep=""),format="GTiff")
  tdur <- writeRaster(tdur,paste(tcalDir,"/duration_",resol,".tif",sep=""),format="GTiff")
  
  #b. run model with specified parameters
  rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
  tkill <- params$KILL[2]; tmin <- 100; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- 400 #trial 1
  
  trial <- 3
  outf <- paste(trunDir,"/run_",trial,sep="")
  
  #run the model
  eco <- suitCalc(climPath=odataDir, 
                  sowDat=tpdate2@file@name,
                  harDat=thdate2@file@name,
                  Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                  Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                  Ropmax=ropmax,Rmax=rmax, 
                  outfolder=outf,
                  cropname=crop_name,ext=".tif",cropClimate=F)
  
  png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
  par(mar=c(3,3,1,2))
  rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
  plot(wrld_simpl,add=T)
  grid(lwd=1.5)
  dev.off()
}


###
#perform the 3deg runs
trunDir <- paste(runDir,"/3deg",sep="")
if (!file.exists(trunDir)) {dir.create(trunDir)}

tcalDir <- paste(trunDir,"/calendar",sep="")
if (!file.exists(tcalDir)) {dir.create(tcalDir)}

tpdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant_3deg.tif",sep=""))
thdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest_3deg.tif",sep=""))

#correct sowing date
tpdate2 <- tpdate
tpdate2[which(tpdate[] < 153)] <- 153
tpdate2[which(tpdate[] > 270)] <- NA

#correct harvest date
thdate2 <- thdate
thdate2[which(thdate[] > 296)] <- 296
thdate2[which(thdate[] < 150)] <- NA
thdate2[which(thdate[] < 215)] <- 243

#calculate duration
tdur <- calc(stack(tpdate2,thdate2),fun=calcdur)

#write three rasters
tpdate2 <- writeRaster(tpdate2,paste(tcalDir,"/plant_3deg.tif",sep=""),format="GTiff")
thdate2 <- writeRaster(thdate2,paste(tcalDir,"/harvest_3deg.tif",sep=""),format="GTiff")
tdur <- writeRaster(tdur,paste(tcalDir,"/duration_3deg.tif",sep=""),format="GTiff")

for (resol in resList) {
  #resol <- resList[1]
  odataDir <- paste(clmDir,"/cascade_3deg-",resol,sep="")
  
  trial <- 3
  outf <- paste(trunDir,"/",resol,"-run_",trial,sep="")
  
  #run the model
  eco <- suitCalc(climPath=odataDir, 
                  sowDat=tpdate2@file@name,
                  harDat=thdate2@file@name,
                  Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                  Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                  Ropmax=ropmax,Rmax=rmax, 
                  outfolder=outf,
                  cropname=crop_name,ext=".tif",cropClimate=F)
  
  png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
  par(mar=c(3,3,1,2))
  rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
  plot(wrld_simpl,add=T)
  grid(lwd=1.5)
  dev.off()
}





