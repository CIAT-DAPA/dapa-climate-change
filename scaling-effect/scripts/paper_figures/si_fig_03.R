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
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures_v2",sep="")

################################################################################################
########## for maize
#model run details
runDir <- paste(bDir,"/model-runs",sep="")
trial <- 6
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

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

#construct histogram
this_all <- hist(his_df$suit,breaks=seq(0,100,by=10),plot=F)
this_niche <- hist(his_df$suit[which(his_df$aharv >= 0.1)],breaks=seq(0,100,by=10),plot=F)

#plot histograms
pdf(paste(figDir,"/SI-Fig03a_maize.pdf",sep=""),width=8,height=5,pointsize=12)
par(mar=c(4.5,4.5,1,1))
plot(this_all$mids,this_all$counts/(sum(this_all$counts)),ty="l",col="black",xlim=c(0,100),ylim=c(0,1),
     xlab="Suitability (%)",ylab="Relative frequency",lwd=2)
lines(this_niche$mids,this_niche$counts/(sum(this_niche$counts)),col="blue",lwd=2)
grid(lwd=1.25)
abline(v=mean(his_df$suit),col="black",lty=2)
abline(v=mean(his_df$suit[which(his_df$aharv >= 0.05)]),col="blue",lty=2)
legend(x=0,y=1,cex=1.0,lty=c(1,1),col=c("black","blue"),lwd=rep(1.5,2),
       legend=c("All areas","Niche"))
dev.off()


##############################################################################
### for groundnut
runDir <- paste(bDir,"/model-runs_gnut",sep="")
trial <- 3
crop_name <- "gnut"

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string

#extract data
extn <- extent(msk)
extn@ymax <- 15
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
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

#construct histogram
this_all <- hist(his_df$suit,breaks=seq(0,100,by=10),plot=F)
this_niche <- hist(his_df$suit[which(his_df$aharv >= 0.05)],breaks=seq(0,100,by=10),plot=F)

#plot histograms
pdf(paste(figDir,"/SI-Fig03b_groundnut.pdf",sep=""),width=8,height=5,pointsize=12)
par(mar=c(4.5,4.5,1,1))
plot(this_all$mids,this_all$counts/(sum(this_all$counts)),ty="l",col="black",xlim=c(0,100),ylim=c(0,1),
     xlab="Suitability (%)",ylab="Relative frequency",lwd=2)
lines(this_niche$mids,this_niche$counts/(sum(this_niche$counts)),col="blue",lwd=2)
grid(lwd=1.25)
abline(v=mean(his_df$suit),col="black",lty=2)
abline(v=mean(his_df$suit[which(his_df$aharv >= 0.05)]),col="blue",lty=2)
legend(x=0,y=1,cex=1.0,lty=c(1,1),col=c("black","blue"),lwd=rep(1.5,2),
       legend=c("All areas","Niche"))
dev.off()



