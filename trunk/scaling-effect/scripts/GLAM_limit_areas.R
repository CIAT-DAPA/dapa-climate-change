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
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

figDir <- paste(bDir,"/paper_figures_v2/extra_plots",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#model run details
trial <- 6
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#load suitability and precip raster from EcoCrop run
suit <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
gsrain <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
gstemp <- raster(paste(runDir,"/12km_exp/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))

#ecocrop parameters
rmin <- 200; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6
tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6

#load GLAM output file (YGP=1)
glam_yield <- raster(paste(bDir,"/GLAM_runs_v2/maize_12km.nc",sep=""),varname="Yield")
glam_yield[which(glam_yield[] >= 2e20)] <- NA

#load GLAM output file (YGP=1)
glam_yield050 <- raster(paste(bDir,"/GLAM_runs_v2/maize_12km_YGP050.nc",sep=""),varname="Yield")
glam_yield050[which(glam_yield050[] >= 2e20)] <- NA

#set NA anything below rmin and tkill
eco_msk <- gsrain
eco_msk[which(!is.na(eco_msk[]))] <- 1
eco_msk[which(gsrain[] <= rmin)] <- NA
eco_msk[which(gstemp[] <= tkill)] <- NA

xy <- as.data.frame(xyFromCell(eco_msk, which(is.na(eco_msk[]))))
glam_yield[cellFromXY(glam_yield,xy)] <- NA
glam_yield050[cellFromXY(glam_yield050,xy)] <- NA

#map of observed yield data
oyield <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_yield.tif",sep=""))

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
                             bg = "white", fg = "grey20", pch = 20)
  
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

m1 <- extent(1.5,4.5,6,9)

#figure with locations
pdf(paste(figDir,"/maize_yield_monfreda.pdf",sep=""), height=5,width=7,pointsize=15,family="Helvetica")
tplot <- rs_levplot2(oyield,zn=NA,zx=NA,nb=NA,brks=seq(0,10,by=0.5),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(m1,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((m1@xmin+m1@xmax)*.5, (m1@ymin+m1@ymax)*.5, "M",cex=1.5))
print(tplot)
dev.off()

#glam yield YGP=1
gyield <- glam_yield * 0.001

pdf(paste(figDir,"/maize_yield_glam.pdf",sep=""), height=5,width=7,pointsize=15,family="Helvetica")
tplot <- rs_levplot2(gyield,zn=NA,zx=NA,nb=NA,brks=seq(0,10,by=0.5),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(m1,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((m1@xmin+m1@xmax)*.5, (m1@ymin+m1@ymax)*.5, "M",cex=1.5))
print(tplot)
dev.off()


#glam yield YGP=0.5
gyield <- glam_yield050 * 0.001

pdf(paste(figDir,"/maize_yield_glam_ygp050.pdf",sep=""), height=5,width=7,pointsize=15,family="Helvetica")
tplot <- rs_levplot2(gyield,zn=NA,zx=NA,nb=NA,brks=seq(0,10,by=0.5),scale="Spectral",col_i="red",col_f="#FEE0D2",ncol=11,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(m1,'SpatialPolygons'),lwd=1.25,col="blue"))
tplot <- tplot + layer(panel.text((m1@xmin+m1@xmax)*.5, (m1@ymin+m1@ymax)*.5, "M",cex=1.5))
print(tplot)
dev.off()




