#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2015
stop("!")

#plot growing zones

#input directories
#wd <- "~/Leeds-work/param-unc-gnut"
#wd <- "/nfs/workspace_cluster_8/CCAFS_Leeds/param-unc-gnut"
wd <- "/nfs/a101/earjr/param-unc-gnut"
psrc.dir <- paste(wd,"/scripts/parunc_rfd",sep="")
runs_dir <- paste(wd,"/crop_model_runs_rfd",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
res_dir <- paste(wd,"/optimisation_output_rfd",sep="")
skill_dir <- paste(wd,"/model_skill_rfd",sep="")
if (!file.exists(skill_dir)) {dir.create(skill_dir,recursive=T)}
taylor_dir <- paste(skill_dir,"/taylor_diagrams",sep="")
if (!file.exists(taylor_dir)) {dir.create(taylor_dir,recursive=T)}
gainplots_dir <- paste(skill_dir,"/skillgains_maps",sep="")
if (!file.exists(gainplots_dir)) {dir.create(gainplots_dir,recursive=T)}

#source some more functions
source(paste(psrc.dir,"/taylor_diagram_function.R",sep=""))

#load objects (initial conditions and yields)
load(paste(mdata_dir,"/inputs_all.RData",sep=""))
iratio_all[,3:ncol(iratio_all)] <- 0 #for _rfd optimisation

#the following is based on results of script check_mean_yields_loess.R
xy_all$ISSEL_F <- 0
xy_all$ISSEL_F[which(xy_all$CELL == 326)] <- 1 #zone 1
xy_all$ISSEL_F[which(xy_all$CELL == 672)] <- 1 #zone 2
xy_all$ISSEL_F[which(xy_all$CELL == 745)] <- 1 #zone 3
xy_all$ISSEL_F[which(xy_all$CELL == 687)] <- 1 #zone 4
xy_all$ISSEL_F[which(xy_all$CELL == 891)] <- 1 #zone 5

#combination of model, input weather and zone
all_runs <- expand.grid(gmodel=c("std","rue"), wth_input=c("wfd","classic"))



######### source plotting stuff
#load needed libraries
library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

###
#function to plot maps
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,colours=NA,now_inc=T,panel_names=NA) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
    if (!is.na(colours[1])) {pal <- colours}
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
    if (!is.na(colours[1])) {pal <- colours}
  }
  if (rev) {pal <- rev(pal)}
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  if (is.na(panel_names[1])) {panel_names <- names(rsin)}
  
  #set theme
  this_theme <- custom.theme(fill = pal, region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin), names.attr=panel_names,
                             xlab='', ylab='') + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))

