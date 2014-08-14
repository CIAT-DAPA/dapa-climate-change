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
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
figDir <- paste(bDir,"/figures_new",sep="")

#get mask from CASCADE output
msk <- raster(paste(bDir,"/lsm/Glam_12km_lsm.nc",sep=""))
extn <- extent(msk)
extn@ymax <- 15
msk <- crop(msk, extn)


#### for maize
####################################################################################
####################################################################################
#load harvested area data
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar <- crop(ahar, msk)

#load monfreda yield data
yield_mon <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_yield.tif",sep=""))
yield_mon <- crop(yield_mon, msk)

#load spam yield
yield_spm <- raster(paste(bDir,"/calendar/Maize.crop.calendar/spam2000v3r6_yield_total_maize.asc",sep=""))
yield_spm <- crop(yield_spm, msk) / 1000
yield_spm[which(yield_spm[] == 0)] <- NA

#extract coordinates from area harvested raster
xy <- as.data.frame(xyFromCell(ahar, which(!is.na(ahar[]))))
xy$ahar <- extract(ahar, xy[,c("x","y")])
xy$yield_mon <- extract(yield_mon, xy[,c("x","y")])
xy$yield_spm <- extract(yield_spm, xy[,c("x","y")])

brks_mon <- seq(0,max(xy$yield_mon,na.rm=T)+.5,by=0.2)
his_mon_all <- hist(xy$yield_mon, breaks=brks_mon,plot=F)
his_mon_har <- hist(xy$yield_mon[which(xy$ahar >= 0.1)], breaks=brks_mon,plot=F)

brks_spm <- seq(0,max(xy$yield_spm,na.rm=T)+.5,by=0.2)
his_spm_all <- hist(xy$yield_spm, breaks=brks_spm, plot=F)
his_spm_har <- hist(xy$yield_spm[which(xy$ahar >= 0.1)], breaks=brks_spm, plot=F)

#produce the plot
pdf(paste(figDir,"/obs_yield_comparison_maize.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.4), xlim=c(0,3.5), xlab="Yield (tonnes / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1)
#grid()
#dev.off()

#pdf(paste(figDir,"/obs_yield_comparison_MapSPAM.pdf",sep=""), height=8,width=10,pointsize=15)
#par(mar=c(5,5,1,1),las=1,lwd=1.75)
#plot(his_spm_all$mids, his_spm_all$counts/sum(his_spm_all$counts),ty="l", col="blue", 
#     ylim=c(0,.4),xlim=c(0,3.5),xlab="Yield (Ton / ha)", ylab="Fractional count")
lines(his_spm_all$mids, his_spm_all$counts/sum(his_spm_all$counts), col="blue",lty=2)
lines(his_spm_har$mids, his_spm_har$counts/sum(his_spm_har$counts), col="red", lty=2)
abline(v=mean(xy$yield_spm,na.rm=T),lty=2,col="blue",lwd=1)
abline(v=mean(xy$yield_spm[which(xy$ahar >= 0.1)],na.rm=T),lty=2,col="red",lwd=1)
grid()
legend(1.95,.4,legend=c("Monfreda","MapSPAM"),col=c("black","black"),lty=c(1,2),bg="white")
legend(2.8,.4,legend=c("All areas","Niche"),col=c("blue","red"),lty=c(1,1),bg="white")
dev.off()




#### for gnut
####################################################################################
####################################################################################
#load harvested area data
ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
ahar <- crop(ahar, msk)

#load monfreda yield data
yield_mon <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""))
yield_mon <- crop(yield_mon, msk)

#load spam yield
yield_spm <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/spam2000v3r6_yield_total_Groundnut.asc",sep=""))
yield_spm <- crop(yield_spm, msk) / 1000
yield_spm[which(yield_spm[] == 0)] <- NA

#extract coordinates from area harvested raster
xy <- as.data.frame(xyFromCell(ahar, which(!is.na(ahar[]))))
xy$ahar <- extract(ahar, xy[,c("x","y")])
xy$yield_mon <- extract(yield_mon, xy[,c("x","y")])
xy$yield_spm <- extract(yield_spm, xy[,c("x","y")])

brks_mon <- seq(0,max(xy$yield_mon,na.rm=T)+.5,by=0.25)
his_mon_all <- hist(xy$yield_mon, breaks=brks_mon,plot=F)
his_mon_har <- hist(xy$yield_mon[which(xy$ahar >= 0.1)], breaks=brks_mon,plot=F)

brks_spm <- seq(0,max(xy$yield_spm,na.rm=T)+.5,by=0.25)
his_spm_all <- hist(xy$yield_spm, breaks=brks_spm, plot=F)
his_spm_har <- hist(xy$yield_spm[which(xy$ahar >= 0.1)], breaks=brks_spm, plot=F)

#produce the plot
pdf(paste(bDir,"/figures_gnut/obs_yield_comparison_gnut.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.6), xlim=c(0,3.5), xlab="Yield (tonnes / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1)
#grid()
#dev.off()

#pdf(paste(figDir,"/obs_yield_comparison_MapSPAM.pdf",sep=""), height=8,width=10,pointsize=15)
#par(mar=c(5,5,1,1),las=1,lwd=1.75)
#plot(his_spm_all$mids, his_spm_all$counts/sum(his_spm_all$counts),ty="l", col="blue", 
#     ylim=c(0,.4),xlim=c(0,3.5),xlab="Yield (Ton / ha)", ylab="Fractional count")
lines(his_spm_all$mids, his_spm_all$counts/sum(his_spm_all$counts), col="blue",lty=2)
lines(his_spm_har$mids, his_spm_har$counts/sum(his_spm_har$counts), col="red", lty=2)
abline(v=mean(xy$yield_spm,na.rm=T),lty=2,col="blue",lwd=1)
abline(v=mean(xy$yield_spm[which(xy$ahar >= 0.1)],na.rm=T),lty=2,col="red",lwd=1)
grid()
legend(1.95,.6,legend=c("Monfreda","MapSPAM"),col=c("black","black"),lty=c(1,2),bg="white")
legend(2.8,.6,legend=c("All areas","Niche"),col=c("blue","red"),lty=c(1,1),bg="white")
dev.off()


