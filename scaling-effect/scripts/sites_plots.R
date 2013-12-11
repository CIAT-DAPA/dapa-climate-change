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
if (!file.exists(paste(lsmDir,"/3deg_mask.tif",sep=""))) {
  msk2 <- msk
  msk2[which(!is.na(msk2[]))] <- rnorm(length(which(!is.na(msk2[]))),10,2)
  writeRaster(msk2,paste(lsmDir),format="GTiff")
} else {
  msk2 <- raster(paste(lsmDir,"/3deg_mask.tif",sep=""))
}
#then check in arcgis

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


####
#### plotting functions
#functions to make plot
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


#figure with locations
pdf(paste(figDir,"/sites.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(msk,0,1,nb=1,scale=NA,col_i="grey 80",col_f="grey 80",ncol=3,rev=F,leg=F)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s4@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


#figure with harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string
ahar <- resample(ahar,msk)

pdf(paste(figDir,"/sites_ahar.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(ahar,0,1,nb=10,scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s4@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


###
resol <- "12km_exp"
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
gsrain <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
rsx <- gsrain

rsx[which(rsx[] > 5000)] <- 5100
rbrks <- c(seq(0,5000,by=250),5100)

pdf(paste(figDir,"/gsrain_",resol,".pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(rbrks),brks=rbrks,scale=NA,
                     col_i="#DEEBF7",col_f="#08306B",ncol=9,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


#plot temperature data
gstemp <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))
rsx <- gstemp*0.1
minval <- min(gstemp[],na.rm=T)
maxval <- max(gstemp[],na.rm=T)

tbrks <- seq(10,45,by=2.5)

pdf(paste(figDir,"/gstemp_",resol,".pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(tbrks),brks=tbrks,scale="YlOrRd",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()



#plot the worldclim ones
gsrain <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
gsrain <- crop(gsrain,msk)
gsrain <- resample(gsrain,rsx)
rsx <- gsrain

pdf(paste(figDir,"/gsrain_wcl.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(rbrks),brks=rbrks,scale=NA,
                     col_i="#DEEBF7",col_f="#08306B",ncol=9,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


gstemp <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))
gstemp <- crop(gstemp,msk)
gstemp <- resample(gstemp,rsx)
rsx <- gstemp*0.1

pdf(paste(figDir,"/gstemp_wcl.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(tbrks),brks=tbrks,scale="YlOrRd",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s1,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s1@xmin+s1@xmax)*.5, (s1@ymin+s1@ymax)*.5, "R1",cex=2))
tplot <- tplot + layer(sp.polygons(as(s2,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s2@xmin+s2@xmax)*.5, (s2@ymin+s2@ymax)*.5, "R2",cex=2))
tplot <- tplot + layer(sp.polygons(as(s3,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s3@xmin+s3@xmax)*.5, (s3@ymin+s3@ymax)*.5, "R3",cex=2))
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "R4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.8,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "R5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()
