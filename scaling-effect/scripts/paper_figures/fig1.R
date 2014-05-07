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
figDir <- paste(bDir,"/paper_figures_v2",sep="")
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

brks_mon <- seq(0,max(xy$yield_mon,na.rm=T)+.5,by=0.2)
his_mon_all <- hist(xy$yield_mon, breaks=brks_mon,plot=F)
his_mon_har <- hist(xy$yield_mon[which(xy$ahar >= 0.1)], breaks=brks_mon,plot=F)

pdf(file=paste(figDir,"/Fig1a_obs_hist_maize.pdf",sep=""),height=6,width=8,pointsize=16,family="Helvetica")
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.6), xlim=c(0,3.5), xlab="Yield (ton / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1.5)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1.5)
grid()
legend(1.9,.6,legend=c("All areas","Growing areas > 10 %"),col=c("blue","red"),lty=c(1,1),bg="white",cex=0.9)
dev.off()


#site for maize
m1 <- extent(1.5,4.5,6,9)

#plotting function
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
pdf(paste(figDir,"/Fig1c_area_harvested_maize.pdf",sep=""), height=4,width=6,pointsize=16,family="Helvetica")
tplot <- rs_levplot2(ahar,zn=0,zx=1,nb=10,brks=c(0,0.02,0.04,0.06,.08,.1,.2,.6,.8,1),scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(m1,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((m1@xmin+m1@xmax)*.5, (m1@ymin+m1@ymax)*.5, "M",cex=1.5))
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

#extract coordinates from area harvested raster
xy <- as.data.frame(xyFromCell(ahar, which(!is.na(ahar[]))))
xy$ahar <- extract(ahar, xy[,c("x","y")])
xy$yield_mon <- extract(yield_mon, xy[,c("x","y")])

brks_mon <- seq(0,max(xy$yield_mon,na.rm=T)+.5,by=0.2)
his_mon_all <- hist(xy$yield_mon, breaks=brks_mon,plot=F)
his_mon_har <- hist(xy$yield_mon[which(xy$ahar >= 0.1)], breaks=brks_mon,plot=F)

#produce the plot
pdf(paste(figDir,"/Fig1b_obs_hist_gnut.pdf",sep=""), height=6,width=8,pointsize=16,family="Helvetica")
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(his_mon_all$mids, his_mon_all$counts/sum(his_mon_all$counts),ty="l", col="blue", 
     ylim=c(0,.6), xlim=c(0,3.5), xlab="Yield (ton / ha)", ylab="Fractional count")
lines(his_mon_har$mids, his_mon_har$counts/sum(his_mon_har$counts), col="red")
abline(v=mean(xy$yield_mon,na.rm=T),lty=1,col="blue",lwd=1)
abline(v=mean(xy$yield_mon[which(xy$ahar >= 0.1)],na.rm=T),lty=1,col="red",lwd=1)
grid()
legend(1.9,.6,legend=c("All areas","Growing areas > 5 %"),col=c("blue","red"),lty=c(1,1),bg="white",cex=0.9)
dev.off()

#research site
g2 <- extent(7.5,10.5,12,15)

#load harvested area and locations on top
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string
ahar <- resample(ahar,msk)

#produce plot
pdf(paste(figDir,"/Fig1d_area_harvested_gnut.pdf",sep=""), height=4,width=6,pointsize=16)
tplot <- rs_levplot2(ahar,zn=0,zx=1,nb=10,brks=c(0,0.02,0.04,0.06,.08,.1,.2,.6,.8,1),scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(g2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((g2@xmin+g2@xmax)*.5, (g2@ymin+g2@ymax)*.5, "G",cex=1.5))
print(tplot)
dev.off()

