#Julian Ramirez-Villegas
#Feb 2014
#UoL / CCAFS / CIAT

###
#plot EcoCrop boxplots for lines in Fig. 2: 12km, 12km-niche, 3-deg
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
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp_bil",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures_v2",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#read sensitivity results
outsens_12km <- read.csv(paste(sensDir_12km,"/sensitivity_result_raw.csv",sep=""))
outsens_3d12 <- read.csv(paste(sensDir_3d12,"/sensitivity_result_raw.csv",sep=""))

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_12km_all <- outsens_12km[which(outsens_12km$prec == 0 & outsens_12km$type == "all"),]
outsens_12km_har <- outsens_12km[which(outsens_12km$prec == 0 & outsens_12km$type == "har"),]

outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]
outsens_3d12_all <- outsens_3d12[which(outsens_3d12$prec == 0 & outsens_3d12$type == "all"),]
outsens_3d12_har <- outsens_3d12[which(outsens_3d12$prec == 0 & outsens_3d12$type == "har"),]

pdf(paste(figDir,"/SI-Fig05d_ecocrop_12km.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.25)
boxplot(outsens_12km_all$diff~outsens_12km_all$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue")
grid()
boxplot(outsens_12km_all$diff~outsens_12km_all$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue",add=T)
dev.off()

pdf(paste(figDir,"/SI-Fig05e_ecocrop_12km_niche.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.25)
boxplot(outsens_12km_har$diff~outsens_12km_har$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue")
grid()
boxplot(outsens_12km_har$diff~outsens_12km_har$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue",add=T)
dev.off()


pdf(paste(figDir,"/SI-Fig05f_ecocrop_3deg.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.25)
boxplot(outsens_3d12_all$diff~outsens_3d12_all$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue")
grid()
boxplot(outsens_3d12_all$diff~outsens_3d12_all$temp,pch=3,cex=0.5,col="white",border="black",
        ylim=c(-100,20),xlab="Temperature change (K)", ylab="Suitability change (%)",
        outcol="red",medcol="red",boxcol="blue",add=T)
dev.off()


