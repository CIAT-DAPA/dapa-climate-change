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

#extent
p00 <- extent(msk)
p00@ymax <- 20


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

#figure with harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA
ahar@crs <- wrld_simpl@proj4string
ahar <- resample(ahar,msk)
ahar <- crop(ahar, p00)

pdf(paste(figDir,"/eval_aharv.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(ahar,zn=0,zx=1.4,nb=10,brks=c(0,0.02,0.04,0.06,.08,.1,.2,.6,.8,1,1.5),
                     scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
print(tplot)
dev.off()

#suitability figure (obs)
resol <- "calib"
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
gsuit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
#gsuit[which(gsuit[] == 0)] <- NA
gsuit <- resample(gsuit,msk,method="ngb")
gsuit <- crop(gsuit, p00)

pdf(paste(figDir,"/eval_suit_obs.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(gsuit,zn=0,zx=100,nb=20,scale="RdYlGn",ncol=11,rev=T,leg=T)
print(tplot)
dev.off()


#suitability figure (12km explicit)
resol <- "12km_exp"
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
gsuit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
#gsuit[which(gsuit[] == 0)] <- NA
gsuit <- resample(gsuit,msk,method="ngb")
gsuit <- crop(gsuit, p00)

pdf(paste(figDir,"/eval_suit_12km_exp.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(gsuit,zn=0,zx=100,nb=20,scale="RdYlGn",ncol=11,rev=T,leg=T)
print(tplot)
dev.off()


