###
#do 96 sensitivity runs over the whole of West Africa
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
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_obs <- paste(runDir,"/sens_obs",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")
sensDir_3dobs <- paste(runDir,"/sens_3deg-obs",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_gnut",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#read sensitivity results
outsens_12km <- read.csv(paste(sensDir_12km,"/sensitivity_result.csv",sep=""))
outsens_obs <- read.csv(paste(sensDir_obs,"/sensitivity_result.csv",sep=""))
outsens_3d12 <- read.csv(paste(sensDir_3d12,"/sensitivity_result.csv",sep=""))
outsens_3dobs <- read.csv(paste(sensDir_3dobs,"/sensitivity_result.csv",sep=""))


#AR5 style for 12km explicit runs
pdf(paste(figDir,"/AR5_style_scatter/scatterplot_AR5_style_gnut_12km_exp.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
for (pchg in c(0,-.3,-.6)) {
  #pchg <- 0
  tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
  tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]
  
  if (pchg == 0) {tcol <- "blue"}
  if (pchg == -.3) {tcol <- "black"}
  if (pchg == -.6) {tcol <- "red"}
  
  if (pchg == 0) {
    plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,6),ylim=c(-80,40),col=tcol,
         xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
    points(tchg_12km$temp, tchg_12km$reldiff_har,pch=22,col=tcol,cex=1.5)
    points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)
  } else {
    points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col=tcol,cex=1.5)
    points(tchg_12km$temp, tchg_12km$reldiff_har,pch=22,col=tcol,cex=1.5)
    points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)
  }
}
grid()
legend(3.5,40,legend=c("3 degree","12 km","12 km niche"),col=c("black","black","black"),pch=c(1,4,22),bg="white")
legend(4.75,40,legend=c("0% ppt","-30% ppt","-60% ppt"),col=c("blue","black","red"),lty=c(1,1,1),bg="white")
dev.off()


###
#AR5 style with spread for 12km explicit runs
pdf(paste(figDir,"/AR5_style_scatter/scatterplot_AR5_style_gnut_12km_exp_spread.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
for (pchg in c(0,-.3,-.6)) {
  #pchg <- 0
  tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
  tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]
  
  if (pchg == 0) {tcol <- "blue"; polcol <- rgb(red=0,green=0,blue=255,50,maxColorValue=255)}
  if (pchg == -.3) {tcol <- "black"; polcol <- rgb(red=0,green=0,blue=0,50,maxColorValue=255)}
  if (pchg == -.6) {tcol <- "red"; polcol <- rgb(red=255,green=0,blue=0,50,maxColorValue=255)}
  
  if (pchg == 0) {
    plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,6),ylim=c(-80,50),col=tcol,
         xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
    pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
    pol_y <- c(tchg_12km$reldiff_all-tchg_12km$suit_all_sd,rev(tchg_12km$reldiff_all+tchg_12km$suit_all_sd))
    polygon(pol_x,pol_y,col=polcol,border=NA)
    points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col=tcol,cex=1.5)
  } else {
    points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col=tcol,cex=1.5)
    pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
    pol_y <- c(tchg_12km$reldiff_all-tchg_12km$suit_all_sd,rev(tchg_12km$reldiff_all+tchg_12km$suit_all_sd))
    polygon(pol_x,pol_y,col=polcol,border=NA)
    points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col=tcol,cex=1.5)
  }
}
grid()
legend(4.75,50,legend=c("0% ppt","-30% ppt","-60% ppt"),col=c("blue","black","red"),border=NA,
       pch=c(4,4,4),bg="white",fill=c(rgb(red=0,green=0,blue=255,50,maxColorValue=255),
                                      rgb(red=0,green=0,blue=0,50,maxColorValue=255),
                                      rgb(red=255,green=0,blue=0,50,maxColorValue=255)))
dev.off()


### another version of the AR5-stlye spread one
###
#AR5 style with spread for 12km explicit runs
pdf(paste(figDir,"/AR5_style_scatter/scatterplot_AR5_style_gnut_12km_exp_spread_2.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)

pchg <- -.30
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,6),ylim=c(-80,45),col="blue",
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
pol_y <- c(tchg_12km$reldiff_all-tchg_12km$suit_all_sd,rev(tchg_12km$reldiff_all+tchg_12km$suit_all_sd))
polygon(pol_x,pol_y,col=rgb(red=0,green=0,blue=255,50,maxColorValue=255),border=NA)
points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col="blue",cex=1.5)

points(tchg_12km$temp, tchg_12km$reldiff_har,pch=22,col="black",cex=1.5)
pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
pol_y <- c(tchg_12km$reldiff_har-tchg_12km$suit_har_sd,rev(tchg_12km$reldiff_har+tchg_12km$suit_har_sd))
polygon(pol_x,pol_y,col=rgb(red=0,green=0,blue=0,50,maxColorValue=255),border=NA)

points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col="red",cex=1.5)
pol_x <- c(tchg_3d12$temp,rev(tchg_3d12$temp))
pol_y <- c(tchg_3d12$reldiff_all-tchg_3d12$suit_all_sd,rev(tchg_3d12$reldiff_all+tchg_3d12$suit_all_sd))
polygon(pol_x,pol_y,col=rgb(red=255,green=0,blue=0,50,maxColorValue=255),border=NA)

grid()
legend(4.5,45,legend=c("12 km niche","3 degree","12 km"),col=c("black","red","blue"),
       pch=c(22,1,4),bg="white",fill=c(rgb(red=0,green=0,blue=0,50,maxColorValue=255),
                                       rgb(red=255,green=0,blue=0,50,maxColorValue=255),
                                       rgb(red=0,green=0,blue=255,50,maxColorValue=255)),border=NA)
dev.off()


#with dP=0
pdf(paste(figDir,"/AR5_style_scatter/scatterplot_AR5_style_gnut_12km_exp_spread_3.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)

pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,6),ylim=c(-80,45),col="blue",
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
pol_y <- c(tchg_12km$reldiff_all-tchg_12km$suit_all_sd,rev(tchg_12km$reldiff_all+tchg_12km$suit_all_sd))
polygon(pol_x,pol_y,col=rgb(red=0,green=0,blue=255,50,maxColorValue=255),border=NA)
points(tchg_12km$temp, tchg_12km$reldiff_all,pch=4,col="blue",cex=1.5)

points(tchg_12km$temp, tchg_12km$reldiff_har,pch=22,col="black",cex=1.5)
pol_x <- c(tchg_12km$temp,rev(tchg_12km$temp))
pol_y <- c(tchg_12km$reldiff_har-tchg_12km$suit_har_sd,rev(tchg_12km$reldiff_har+tchg_12km$suit_har_sd))
polygon(pol_x,pol_y,col=rgb(red=0,green=0,blue=0,50,maxColorValue=255),border=NA)

points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col="red",cex=1.5)
pol_x <- c(tchg_3d12$temp,rev(tchg_3d12$temp))
pol_y <- c(tchg_3d12$reldiff_all-tchg_3d12$suit_all_sd,rev(tchg_3d12$reldiff_all+tchg_3d12$suit_all_sd))
polygon(pol_x,pol_y,col=rgb(red=255,green=0,blue=0,50,maxColorValue=255),border=NA)

grid()
legend(4.5,45,legend=c("12 km niche","3 degree","12 km"),col=c("black","red","blue"),
       pch=c(22,1,4),bg="white",fill=c(rgb(red=0,green=0,blue=0,50,maxColorValue=255),
                                       rgb(red=255,green=0,blue=0,50,maxColorValue=255),
                                       rgb(red=0,green=0,blue=255,50,maxColorValue=255)),border=NA)
dev.off()



######################################################################################
#AR5 style for observed
pdf(paste(figDir,"/AR5_style_scatter/scatterplot_AR5_style_gnut_obs.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
for (pchg in c(0,-.3,-.6)) {
  #pchg <- 0
  tchg_obs <- outsens_obs[which(outsens_obs$prec == pchg & outsens_obs$temp != -1),]
  tchg_3dobs <- outsens_3dobs[which(outsens_3dobs$prec == pchg & outsens_3dobs$temp != -1),]
  
  if (pchg == 0) {tcol <- "blue"}
  if (pchg == -.3) {tcol <- "black"}
  if (pchg == -.6) {tcol <- "red"}
  
  if (pchg == 0) {
    plot(tchg_obs$temp, tchg_obs$reldiff_all,ty="p",pch=4,xlim=c(0,6),ylim=c(-50,20),col=tcol,
         xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
    points(tchg_obs$temp, tchg_obs$reldiff_har,pch=22,col=tcol,cex=1.5)
    points(tchg_3dobs$temp, tchg_3dobs$reldiff_all,pch=1,col=tcol,cex=1.5)
  } else {
    points(tchg_obs$temp, tchg_obs$reldiff_all,pch=4,col=tcol,cex=1.5)
    points(tchg_obs$temp, tchg_obs$reldiff_har,pch=22,col=tcol,cex=1.5)
    points(tchg_3dobs$temp, tchg_3dobs$reldiff_all,pch=1,col=tcol,cex=1.5)
  }
}
grid()
legend(3.5,20,legend=c("3 degree","12 km","12 km niche"),col=c("black","black","black"),pch=c(1,4,22),bg="white")
legend(4.75,20,legend=c("0% ppt","-30% ppt","-60% ppt"),col=c("blue","black","red"),lty=c(1,1,1),bg="white")
dev.off()


