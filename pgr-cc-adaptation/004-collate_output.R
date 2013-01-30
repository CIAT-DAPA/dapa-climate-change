#Julian Ramirez-Villegas
#Jan 2012

library(raster); library(sfsmisc); library(maptools)

#src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/pgr-cc-adaptation"
source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))

# #base directories
# gcmDir <- "/mnt/a102/eejarv/CMIP5/Amon"
# bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
# scratch <- "~/Workspace/pgr_analogues"

gcmDir <- "/nfs/a102/eejarv/CMIP5/Amon"
bDir <- "/nfs/a17/eejarv/pgr-cc-adaptation"
scratch <- "/scratch/eejarv/pgr_analogues"

#input directories
hisDir <- paste(gcmDir,"/historical_amon",sep="")
rcpDir <- paste(gcmDir,"/rcp45_amon",sep="")
cfgDir <- paste(bDir,"/config",sep="")
cruDir <- paste(bDir,"/cru-data",sep="")
if (!file.exists(scratch)) {dir.create(scratch)}

#output directories
out_bDir <- paste(bDir,"/outputs",sep="")
if (!file.exists(out_bDir)) {dir.create(out_bDir)}

#configuration details and initial data
yi_h <- 1966; yf_h <- 2005
yi_f1 <- 2016; yf_f1 <- 2055
yi_f2 <- 2056; yf_f2 <- 2095
gcmList <- read.csv(paste(cfgDir,"/gcm_list.csv",sep=""))

#area harvested rasters
ha_rs <- list()
ha_rs$RICE <- raster(paste(bDir,"/crop-data/Rice.harea/rice_harea_glo.tif",sep=""))
ha_rs$WSPR <- raster(paste(bDir,"/crop-data/Wheat.harea/wheat_harea_glo.tif",sep=""))
ha_rs$WWIN <- raster(paste(bDir,"/crop-data/Wheat.harea/wheat_harea_glo.tif",sep=""))
ha_rs$MILL <- raster(paste(bDir,"/crop-data/Millet.harea/millet_harea_glo.tif",sep=""))
ha_rs$SORG <- raster(paste(bDir,"/crop-data/Sorghum.harea/sorghum_harea_glo.tif",sep=""))

#cru raster to filter out NAs
dum_rs <- raster(paste(cruDir,"/cru_ts_3_10.1966.2005.tmx.dat.nc",sep=""),band=0)

#data.frame of grid cells to extract
gCells <- data.frame(LOC=which(!is.na(dum_rs[])))
gCells$LON <- xFromCell(dum_rs,gCells$LOC)
gCells$LAT <- yFromCell(dum_rs,gCells$LOC)
gCells$RICE <- extract(ha_rs$RICE,cbind(x=gCells$LON,y=gCells$LAT))
gCells$RICE[which(!is.na(gCells$RICE))] <- 1
gCells$RICE[which(is.na(gCells$RICE))] <- 0
gCells$WSPR <- extract(ha_rs$WSPR,cbind(x=gCells$LON,y=gCells$LAT))
gCells$WSPR[which(!is.na(gCells$WSPR))] <- 1
gCells$WSPR[which(is.na(gCells$WSPR))] <- 0
gCells$WWIN <- extract(ha_rs$WWIN,cbind(x=gCells$LON,y=gCells$LAT))
gCells$WWIN[which(!is.na(gCells$WWIN))] <- 1
gCells$WWIN[which(is.na(gCells$WWIN))] <- 0
gCells$MILL <- extract(ha_rs$MILL,cbind(x=gCells$LON,y=gCells$LAT))
gCells$MILL[which(!is.na(gCells$MILL))] <- 1
gCells$MILL[which(is.na(gCells$MILL))] <- 0
gCells$SORG <- extract(ha_rs$SORG,cbind(x=gCells$LON,y=gCells$LAT))
gCells$SORG[which(!is.na(gCells$SORG))] <- 1
gCells$SORG[which(is.na(gCells$SORG))] <- 0
gCells$TOTL <- gCells$RICE+gCells$WSPR+gCells$WWIN+gCells$MILL+gCells$SORG
gCells <- gCells[which(gCells$TOTL > 0),]
row.names(gCells) <- 1:nrow(gCells)

#gcm
#batch 1 --> c(1,16,22,17)
gcm_i <- 16
gcm <- paste(gcmList$GCM[gcm_i])
ens <- paste(gcmList$ENS[gcm_i])

#output GCM-specific directory
gcm_outDir <- paste(out_bDir,"/",gcmList$GCM_ENS[gcm_i],sep="")


###################################################################################
###################################################################################
###################################################################################
#grab data from individual calculations
if (!file.exists(paste(gcm_outDir,"/all_outputs.RData",sep=""))) {
  all_out <- as.data.frame(t(sapply(1:nrow(gCells),get_loc_fraction)))
  all_out$LOC <- unlist(all_out$LOC)
  all_out$RICE1 <- unlist(all_out$RICE1); all_out$RICE2 <- unlist(all_out$RICE2)
  all_out$WSPR1 <- unlist(all_out$WSPR1); all_out$WSPR2 <- unlist(all_out$WSPR2)
  all_out$WWIN1 <- unlist(all_out$WWIN1); all_out$WWIN2 <- unlist(all_out$WWIN2)
  all_out$MILL1 <- unlist(all_out$MILL1); all_out$MILL2 <- unlist(all_out$MILL2)
  all_out$SORG1 <- unlist(all_out$SORG1); all_out$SORG2 <- unlist(all_out$SORG2)
  save(list=c("all_out","dum_rs"),file=paste(gcm_outDir,"/all_outputs.RData",sep=""))
} else {
  load(paste(gcm_outDir,"/all_outputs.RData",sep=""))
}

######################################
#load data into a raster
if (!file.exists(paste(gcm_outDir,"/raster_outputs.RData",sep=""))) {
  #output list
  out_rs <- list()
  
  #rice
  out_rs$RICE <- list()
  out_rs$RICE$P_2035 <- dum_rs
  out_rs$RICE$P_2035[] <- NA
  out_rs$RICE$P_2035[all_out$LOC] <- all_out$RICE1
  out_rs$RICE$P_2075 <- dum_rs
  out_rs$RICE$P_2075[] <- NA
  out_rs$RICE$P_2075[all_out$LOC] <- all_out$RICE2
  
  #wspr
  out_rs$WSPR <- list()
  out_rs$WSPR$P_2035 <- dum_rs
  out_rs$WSPR$P_2035[] <- NA
  out_rs$WSPR$P_2035[all_out$LOC] <- all_out$WSPR1
  out_rs$WSPR$P_2075 <- dum_rs
  out_rs$WSPR$P_2075[] <- NA
  out_rs$WSPR$P_2075[all_out$LOC] <- all_out$WSPR2
  
  #wwin
  out_rs$WWIN <- list()
  out_rs$WWIN$P_2035 <- dum_rs
  out_rs$WWIN$P_2035[] <- NA
  out_rs$WWIN$P_2035[all_out$LOC] <- all_out$WWIN1
  out_rs$WWIN$P_2075 <- dum_rs
  out_rs$WWIN$P_2075[] <- NA
  out_rs$WWIN$P_2075[all_out$LOC] <- all_out$WWIN2
  
  #whea (min of wspr and wwin)
  out_rs$WHEA <- list()
  out_rs$WHEA$P_2035 <- calc(stack(out_rs$WWIN$P_2035,out_rs$WSPR$P_2035),fun=function(x) {min(x,na.rm=T)})
  out_rs$WHEA$P_2075 <- calc(stack(out_rs$WWIN$P_2075,out_rs$WSPR$P_2075),fun=function(x) {min(x,na.rm=T)})
  #out_rs$WHEA$P_2035 <- (out_rs$WWIN$P_2035 + out_rs$WSPR$P_2035) / 2
  #out_rs$WHEA$P_2075 <- (out_rs$WWIN$P_2075 + out_rs$WSPR$P_2075) / 2
  
  #mill
  out_rs$MILL <- list()
  out_rs$MILL$P_2035 <- dum_rs
  out_rs$MILL$P_2035[] <- NA
  out_rs$MILL$P_2035[all_out$LOC] <- all_out$MILL1
  out_rs$MILL$P_2075 <- dum_rs
  out_rs$MILL$P_2075[] <- NA
  out_rs$MILL$P_2075[all_out$LOC] <- all_out$MILL2
  
  #sorg
  out_rs$SORG <- list()
  out_rs$SORG$P_2035 <- dum_rs
  out_rs$SORG$P_2035[] <- NA
  out_rs$SORG$P_2035[all_out$LOC] <- all_out$SORG1
  out_rs$SORG$P_2075 <- dum_rs
  out_rs$SORG$P_2075[] <- NA
  out_rs$SORG$P_2075[all_out$LOC] <- all_out$SORG2
  
  save(out_rs,file=paste(gcm_outDir,"/raster_outputs.RData",sep=""))
} else {
  load(paste(gcm_outDir,"/raster_outputs.RData",sep=""))
}


#####################################
#GCM-specific plots
#figDir
fig_dir <- paste(gcm_outDir,"/figures",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir)}

#making plots
plot_overlap("RICE", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("RICE", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WSPR", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WSPR", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WWIN", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WWIN", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WHEA", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WHEA", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("MILL", "P_2035", xt=extent(-30,160,-45,75), fig_dir)
plot_overlap("MILL", "P_2075", xt=extent(-30,160,-45,75), fig_dir)
plot_overlap("SORG", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("SORG", "P_2075", xt=extent(-130,160,-45,75), fig_dir)







