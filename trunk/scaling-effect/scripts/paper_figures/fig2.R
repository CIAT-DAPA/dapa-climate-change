#Julian Ramirez-Villegas
#Feb 2012
#UoL / CCAFS / CIAT

###
#plot AR5 style for maize (EcoCrop)
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
library(ggplot2); library(reshape2)

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

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#read sensitivity results
outsens_12km <- read.csv(paste(sensDir_12km,"/sensitivity_result.csv",sep=""))
outsens_3d12 <- read.csv(paste(sensDir_3d12,"/sensitivity_result.csv",sep=""))

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/Fig2c_ar5_ecocrop.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_12km$temp, tchg_12km$reldiff_har,pch=22,col=tcol,cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km","12 km niche"),col=c("blue","blue","blue"),pch=c(1,4,22),bg="white")
dev.off()



