#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_new",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#model run details
trial <- 6
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#find other interesting points
msk2 <- raster(paste(lsmDir,"/3deg_mask.tif",sep=""))

#new points
s1 <- extent(-1.5,1.5,6,9)
s2 <- extent(1.5,4.5,6,9)
s3 <- extent(4.5,7.5,6,9)
s4 <- extent(4.5,7.5,3,6)
s5 <- extent(7.5,10.5,3,6)

p00 <- extent(msk)
p00@ymax <- 15

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string

#extract data
extn <- extent(msk)
extn@ymax <- 15
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

#read 12km suitability raster
resol <- "12km_exp"
suit <- raster(paste(runDir,"/",resol,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit[which(suit[] == 0)] <- NA

#make data.frame for constructing histogram
his_df <- xy
his_df$suit <- extract(suit,xy[,c("x","y")])
his_df <- his_df[which(!is.na(his_df$suit)),]
#his_df <- his_df[which(his_df$suit < 100 & his_df$suit > 0),]

#construct histogram
this <- hist(his_df$suit,breaks=seq(0,100,by=10),plot=F)
this_01 <- hist(his_df$suit[which(his_df$aharv >= 0.1)],breaks=seq(0,100,by=10),plot=F)

#read obs suitability raster
suit_obs <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_obs <- crop(suit_obs, msk2)
suit_obs[which(suit_obs[] == 0)] <- NA

aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))

xy <- as.data.frame(xyFromCell(suit_obs,which(!is.na(suit_obs[]))))
xy <- cbind(cell=cellFromXY(suit_obs,xy[,c("x","y")]),xy)
xy$aharv <- extract(aharv, xy[,c("x","y")])

his_dfo <- xy
his_dfo$suit <- extract(suit_obs,xy[,c("x","y")])
his_dfo <- his_dfo[which(!is.na(his_dfo$suit)),]
#his_dfo <- his_dfo[which(his_dfo$suit < 100 & his_dfo$suit > 0),]

#construct histogram
this_o <- hist(his_dfo$suit,breaks=seq(0,100,by=10),plot=F)
this_01o <- hist(his_dfo$suit[which(his_dfo$aharv >= 0.1)],breaks=seq(0,100,by=10),plot=F)

#plot histograms
pdf(paste(figDir,"/histograms.pdf",sep=""),width=10,height=7,pointsize=15)
par(mar=c(4.5,4.5,1,1))
plot(this$mids,this$counts/(sum(this$counts)),ty="l",col="red",xlim=c(0,100),ylim=c(0,1),
     xlab="Suitability (%)",ylab="Relative frequency",lwd=2)
lines(this_01$mids,this_01$counts/(sum(this_01$counts)),col="blue",lwd=2)
lines(this_o$mids,this_o$counts/(sum(this_o$counts)),col="red",lty=2,lwd=2)
lines(this_01o$mids,this_01o$counts/(sum(this_01o$counts)),col="blue",lty=2,lwd=2)
grid(lwd=1.25)
legend(x=0,y=1,cex=1.0,lty=c(1,1,2,2),col=c("red","blue","red","blue"),lwd=rep(1.5,4),
       legend=c("All areas (CASCADE)","Harvested (CASCADE)", "All areas (Obs)", "Harvested (Obs)"))
dev.off()



