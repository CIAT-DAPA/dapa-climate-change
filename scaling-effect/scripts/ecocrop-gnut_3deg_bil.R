#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#produce EcoCrop runs for groundnut in the Sahel for the scaling study
###

#load packages
library(rgdal); library(raster); library(maptools); data(wrld_simpl)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
figDir <- paste(bDir,"/figures_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

crop_name <- "gnut"

######
###### prepare planting dates first

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))

#get planting and harvest dates
pdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_plant.tif",sep=""))
hdate <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_harvest.tif",sep=""))

#harvested area data
ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))

#yield data
yield <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""))

#### spam data
ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv_spam.tif",sep=""))

#2. run model with 3deg bilinear data
resol <- "3deg-12km_exp_bil"
dataDir <- paste(clmDir,"/cascade_",resol,sep="")
tres <- "3deg"

dataDir <- paste(clmDir,"/cascade_",resol,sep="")

trunDir <- paste(runDir,"/3deg",sep="")
if (!file.exists(trunDir)) {dir.create(trunDir)}

#a. start and end of season
#sowing and harvest date
tcalDir <- paste(trunDir,"/calendar",sep="")
tpdate2 <- paste(tcalDir,"/plant_3deg.tif",sep="")
thdate2 <- paste(tcalDir,"/harvest_3deg.tif",sep="")

#parameters
params <- read.csv(paste(runDir,"/parameter_sets.csv",sep=""))
selpar <- read.csv(paste(runDir,"/runs_discard.csv",sep=""))#[,c("RUN","SEL")]

rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
tkill <- params$KILL[2]; tmin <- 100; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- 400 #trial 1

trial <- 3

#b. run model with specified parameters
outf <- paste(trunDir,"/12km_exp_bil-run_",trial,sep="")

#run the model
eco <- suitCalc(climPath=dataDir, 
                sowDat=tpdate2,
                harDat=thdate2,
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






