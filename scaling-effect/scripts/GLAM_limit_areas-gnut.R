#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#extra_plots
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

figDir <- paste(bDir,"/paper_figures_v2/extra_plots",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#model run details
trial <- 3
crop_name <- "gnut"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#load suitability and precip raster from EcoCrop run
suit <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
gsrain <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
gstemp <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))

#ecocrop parameters
params <- read.csv(paste(runDir,"/parameter_sets.csv",sep=""))
selpar <- read.csv(paste(runDir,"/runs_discard.csv",sep=""))#[,c("RUN","SEL")]
maxauc <- selpar$RUN[which(selpar$HIGH.AUC == max(selpar$HIGH.AUC))]
params <- params[which(params$RUN == 7),]
rmin <- params$MIN[1]; ropmin <- params$OPMIN[1]; ropmax <- params$OPMAX[1]; rmax <- params$MAX[1] #trial 1
tkill <- params$KILL[2]; tmin <- 100; topmin <- params$OPMIN[2]; topmax <- params$OPMAX[2]; tmax <- 400 #trial 1

#load GLAM output file (YGP=1)
glam_yield <- raster(paste(bDir,"/GLAM_runs_v2/groundnut_12km.nc",sep=""),varname="Yield")
glam_yield[which(glam_yield[] >= 2e20)] <- NA

#load GLAM output file (YGP=1)
glam_yield050 <- raster(paste(bDir,"/GLAM_runs_v2/groundnut_12km_YGP050.nc",sep=""),varname="Yield")
glam_yield050[which(glam_yield050[] >= 2e20)] <- NA

#set NA anything below rmin and tkill
eco_msk <- raster(paste(bDir,"/model-runs/12km_exp/run_6/maiz_gsrain.tif",sep=""))
eco_msk[which(!is.na(eco_msk[]))] <- 1
eco_msk[which(gsrain[] <= rmin)] <- NA
eco_msk[which(gstemp[] <= tkill)] <- NA

xy <- as.data.frame(xyFromCell(eco_msk, which(is.na(eco_msk[]))))
glam_yield[cellFromXY(glam_yield,xy)] <- NA
glam_yield050[cellFromXY(glam_yield050,xy)] <- NA

#map of observed yield data
oyield <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_yield.tif",sep=""))
oyield[cellFromXY(oyield,xy)] <- NA

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


#figure details
ht <- 6
rs <- msk
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))

oyield@crs <- wrld_simpl@proj4string
oyield <- resample(oyield,msk)

p00 <- extent(msk)
p00@ymax <- 15

g2 <- extent(7.5,10.5,12,15)

#figure with locations
pdf(paste(figDir,"/gnut_yield_monfreda.pdf",sep=""), height=5,width=7,pointsize=12,family="Helvetica")
tplot <- rs_levplot2(oyield,zn=NA,zx=NA,nb=NA,brks=seq(0,4.5,by=0.25),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(g2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((g2@xmin+g2@xmax)*.5, (g2@ymin+g2@ymax)*.5, "G",cex=1.5))
print(tplot)
dev.off()

#glam yield YGP=1
gyield <- glam_yield * 0.001

pdf(paste(figDir,"/gnut_yield_glam.pdf",sep=""), height=5,width=7,pointsize=12,family="Helvetica")
tplot <- rs_levplot2(gyield,zn=NA,zx=NA,nb=NA,brks=seq(0,4.5,by=0.25),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(g2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((g2@xmin+g2@xmax)*.5, (g2@ymin+g2@ymax)*.5, "G",cex=1.5))
print(tplot)
dev.off()


#glam yield YGP=0.5
gyield <- glam_yield050 * 0.001

pdf(paste(figDir,"/gnut_yield_glam_ygp050.pdf",sep=""), height=5,width=7,pointsize=12,family="Helvetica")
tplot <- rs_levplot2(gyield,zn=NA,zx=NA,nb=NA,brks=seq(0,4.5,by=0.25),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(g2,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((g2@xmin+g2@xmax)*.5, (g2@ymin+g2@ymax)*.5, "G",cex=1.5))
print(tplot)
dev.off()




