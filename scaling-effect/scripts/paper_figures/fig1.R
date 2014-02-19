#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#produce EcoCrop runs for maize in the Sahel for the scaling study
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
#library(Cairo); library(cairoDevice)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
figDir <- paste(bDir,"/paper_figures",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

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

pdf(file=paste(figDir,"/Fig1a_obs_hist_maize.pdf",sep=""),height=6,width=8,pointsize=12,family="Helvetica")
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.6), xlim=c(0,3.5), xlab="Yield (ton / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1.5)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1.5)
grid()
legend(2.8,.6,legend=c("All areas","Niche"),col=c("blue","red"),lty=c(1,1),bg="white")
dev.off()


#potential sites for maize for selection
s1 <- extent(-1.5,1.5,6,9)
s2 <- extent(1.5,4.5,6,9)
s3 <- extent(4.5,7.5,6,9)
s4 <- extent(4.5,7.5,3,6)
s5 <- extent(7.5,10.5,3,6)

ahar_s1 <- crop(ahar, s1); ahar_s1 <- ahar_s1 * area(ahar_s1); ahar_s1 <- sum(ahar_s1[],na.rm=T)
ahar_s2 <- crop(ahar, s2); ahar_s2 <- ahar_s2 * area(ahar_s2); ahar_s2 <- sum(ahar_s2[],na.rm=T)
ahar_s3 <- crop(ahar, s3); ahar_s3 <- ahar_s3 * area(ahar_s3); ahar_s3 <- sum(ahar_s3[],na.rm=T)
ahar_s4 <- crop(ahar, s4); ahar_s4 <- ahar_s4 * area(ahar_s4); ahar_s4 <- sum(ahar_s4[],na.rm=T)
ahar_s5 <- crop(ahar, s5); ahar_s5 <- ahar_s5 * area(ahar_s5); ahar_s5 <- sum(ahar_s5[],na.rm=T)

ahar_sdf <- data.frame(SITE=1:5,AHAR=c(ahar_s1,ahar_s2,ahar_s3,ahar_s4,ahar_s5))

#so we're looking into R2 and R3
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
  }
  if (rev) {pal <- rev(pal)}
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal,region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}


rs_print <- function(p,pdfName) {
  pdf(pdfName,height=ht,width=wt,pointsize=14)
  print(p)
  dev.off()
}

#figure details
ht <- 6
rs <- msk
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))

ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string
ahar <- resample(ahar,msk)

p00 <- extent(msk)
p00@ymax <- 15

#figure with locations
pdf(paste(figDir,"/Fig1c_area_harvested_maize.pdf",sep=""), height=6,width=8,pointsize=12,family="Helvetica")
tplot <- rs_levplot2(ahar,zn=0,zx=1,nb=10,brks=c(0,0.02,0.04,0.06,.08,.1,.2,.6,.8,1),scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "M1",cex=1.5))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "M2",cex=1.5))
print(tplot)
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

brks_mon <- seq(0,max(xy$yield_mon,na.rm=T)+.5,by=0.2)
his_mon_all <- hist(xy$yield_mon, breaks=brks_mon,plot=F)
his_mon_har <- hist(xy$yield_mon[which(xy$ahar >= 0.1)], breaks=brks_mon,plot=F)

brks_spm <- seq(0,max(xy$yield_spm,na.rm=T)+.5,by=0.2)
his_spm_all <- hist(xy$yield_spm, breaks=brks_spm, plot=F)
his_spm_har <- hist(xy$yield_spm[which(xy$ahar >= 0.1)], breaks=brks_spm, plot=F)

#produce the plot
pdf(paste(figDir,"/Fig1b_obs_hist_gnut.pdf",sep=""), height=6,width=8,pointsize=12,family="Helvetica")
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.6), xlim=c(0,3.5), xlab="Yield (ton / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1)
grid()
legend(2.8,.6,legend=c("All areas","Niche"),col=c("blue","red"),lty=c(1,1),bg="white")
dev.off()

#research sites
s1 <- extent(-16.5,-13.5,12,15)
s2 <- extent(7.5,10.5,12,15)

#load harvested area and locations on top
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string
ahar <- resample(ahar,msk)

#produce plot
pdf(paste(figDir,"/Fig1d_area_harvested_gnut.pdf",sep=""), height=6,width=8,pointsize=12)
tplot <- rs_levplot2(ahar,zn=0,zx=1,nb=10,brks=c(0,0.02,0.04,0.06,.08,.1,.2,.6,.8,1),scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "G1",cex=1.5))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "G2",cex=1.5))
print(tplot)
dev.off()

