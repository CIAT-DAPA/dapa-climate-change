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

#output directories
out_bDir <- paste(bDir,"/outputs",sep="")

#configuration details and initial data
gcmList <- read.csv(paste(cfgDir,"/gcm_list.csv",sep=""))

#cru raster to filter out NAs
if (!file.exists(paste(cfgDir,"/dum_rs.RData",sep=""))) {
  dum_rs <- raster(paste(cruDir,"/cru_ts_3_10.1966.2005.tmx.dat.nc",sep=""),band=0)
  save(dum_rs,file=paste(cfgDir,"/dum_rs.RData",sep=""))
} else {
  load(paste(cfgDir,"/dum_rs.RData",sep=""))
}

#construct or load grid cells data.frame
if (!file.exists(paste(cfgDir,"/gCells.RData",sep=""))) {
  #area harvested rasters
  ha_rs <- list()
  ha_rs$RICE <- raster(paste(bDir,"/crop-data/Rice.harea/rice_harea_glo.tif",sep=""))
  ha_rs$WSPR <- raster(paste(bDir,"/crop-data/Wheat.harea/wheat_harea_glo.tif",sep=""))
  ha_rs$WWIN <- raster(paste(bDir,"/crop-data/Wheat.harea/wheat_harea_glo.tif",sep=""))
  ha_rs$MILL <- raster(paste(bDir,"/crop-data/Millet.harea/millet_harea_glo.tif",sep=""))
  ha_rs$SORG <- raster(paste(bDir,"/crop-data/Sorghum.harea/sorghum_harea_glo.tif",sep=""))
  
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
  
  save(gCells,file=paste(cfgDir,"/gCells.RData",sep=""))
} else {
  load(paste(cfgDir,"/gCells.RData",sep=""))
}

#gcm
#gcms must be c(1:6,9,16,19,22)
gcm_i <- 1
gcm <- paste(gcmList$GCM[gcm_i])
ens <- paste(gcmList$ENS[gcm_i])

#output GCM-specific directory
gcm_outDir <- paste(out_bDir,"/",gcmList$GCM_ENS[gcm_i],sep="")


###################################################################################
###################################################################################
###################################################################################
#grab data from individual calculations
if (!file.exists(paste(gcm_outDir,"/adap_outputs.RData",sep=""))) {
  adap_out <- as.data.frame(t(sapply(1:nrow(gCells),get_loc_adapt)))
  for (i in 1:ncol(adap_out)) {adap_out[,i] <- unlist(adap_out[,i])}
  save(list=c("adap_out","dum_rs"),file=paste(gcm_outDir,"/adap_outputs.RData",sep=""))
} else {
  load(paste(gcm_outDir,"/adap_outputs.RData",sep=""))
}

######################################
#load data into a raster
if (!file.exists(paste(gcm_outDir,"/adap_raster_outputs.RData",sep=""))) {
  #output list
  out_rs <- list()
  
  #rice 2035
  out_rs$RICE <- list()
  out_rs$RICE$BUF_2035 <- raster(dum_rs)
  out_rs$RICE$BUF_2035[adap_out$LOC] <- adap_out$RICE1_BUF
  out_rs$RICE$CTR_2035 <- raster(dum_rs)
  out_rs$RICE$CTR_2035[adap_out$LOC] <- adap_out$RICE1_CTR
  out_rs$RICE$GLO_2035 <- raster(dum_rs)
  out_rs$RICE$GLO_2035[adap_out$LOC] <- adap_out$RICE1_GLO
  
  #rice 2075
  out_rs$RICE$BUF_2075 <- raster(dum_rs)
  out_rs$RICE$BUF_2075[adap_out$LOC] <- adap_out$RICE2_BUF
  out_rs$RICE$CTR_2075 <- raster(dum_rs)
  out_rs$RICE$CTR_2075[adap_out$LOC] <- adap_out$RICE2_CTR
  out_rs$RICE$GLO_2075 <- raster(dum_rs)
  out_rs$RICE$GLO_2075[adap_out$LOC] <- adap_out$RICE2_GLO
  
  #wspr 2035
  out_rs$WSPR <- list()
  out_rs$WSPR$BUF_2035 <- raster(dum_rs)
  out_rs$WSPR$BUF_2035[adap_out$LOC] <- adap_out$WSPR1_BUF
  out_rs$WSPR$CTR_2035 <- raster(dum_rs)
  out_rs$WSPR$CTR_2035[adap_out$LOC] <- adap_out$WSPR1_CTR
  out_rs$WSPR$GLO_2035 <- raster(dum_rs)
  out_rs$WSPR$GLO_2035[adap_out$LOC] <- adap_out$WSPR1_GLO
  
  #wspr 2075
  out_rs$WSPR$BUF_2075 <- raster(dum_rs)
  out_rs$WSPR$BUF_2075[adap_out$LOC] <- adap_out$WSPR2_BUF
  out_rs$WSPR$CTR_2075 <- raster(dum_rs)
  out_rs$WSPR$CTR_2075[adap_out$LOC] <- adap_out$WSPR2_CTR
  out_rs$WSPR$GLO_2075 <- raster(dum_rs)
  out_rs$WSPR$GLO_2075[adap_out$LOC] <- adap_out$WSPR2_GLO
  
  #wwin 2035
  out_rs$WWIN <- list()
  out_rs$WWIN$BUF_2035 <- raster(dum_rs)
  out_rs$WWIN$BUF_2035[adap_out$LOC] <- adap_out$WWIN1_BUF
  out_rs$WWIN$CTR_2035 <- raster(dum_rs)
  out_rs$WWIN$CTR_2035[adap_out$LOC] <- adap_out$WWIN1_CTR
  out_rs$WWIN$GLO_2035 <- raster(dum_rs)
  out_rs$WWIN$GLO_2035[adap_out$LOC] <- adap_out$WWIN1_GLO
  
  #wwin 2075
  out_rs$WWIN$BUF_2075 <- raster(dum_rs)
  out_rs$WWIN$BUF_2075[adap_out$LOC] <- adap_out$WWIN2_BUF
  out_rs$WWIN$CTR_2075 <- raster(dum_rs)
  out_rs$WWIN$CTR_2075[adap_out$LOC] <- adap_out$WWIN2_CTR
  out_rs$WWIN$GLO_2075 <- raster(dum_rs)
  out_rs$WWIN$GLO_2075[adap_out$LOC] <- adap_out$WWIN2_GLO
  
  #whea 2035 (min of wspr and wwin)
  out_rs$WHEA <- list()
  out_rs$WHEA$BUF_2035 <- calc(stack(out_rs$WWIN$BUF_2035,out_rs$WSPR$BUF_2035),fun=function(x) {min(x,na.rm=T)})
  out_rs$WHEA$CTR_2035 <- calc(stack(out_rs$WWIN$CTR_2035,out_rs$WSPR$CTR_2035),fun=function(x) {min(x,na.rm=T)})
  out_rs$WHEA$GLO_2035 <- calc(stack(out_rs$WWIN$GLO_2035,out_rs$WSPR$GLO_2035),fun=function(x) {min(x,na.rm=T)})
  
  #whea 2075
  out_rs$WHEA$BUF_2075 <- calc(stack(out_rs$WWIN$BUF_2075,out_rs$WSPR$BUF_2075),fun=function(x) {min(x,na.rm=T)})
  out_rs$WHEA$CTR_2075 <- calc(stack(out_rs$WWIN$CTR_2075,out_rs$WSPR$CTR_2075),fun=function(x) {min(x,na.rm=T)})
  out_rs$WHEA$GLO_2075 <- calc(stack(out_rs$WWIN$GLO_2075,out_rs$WSPR$GLO_2075),fun=function(x) {min(x,na.rm=T)})
  
  #mill 2035
  out_rs$MILL <- list()
  out_rs$MILL$BUF_2035 <- raster(dum_rs)
  out_rs$MILL$BUF_2035[adap_out$LOC] <- adap_out$MILL1_BUF
  out_rs$MILL$CTR_2035 <- raster(dum_rs)
  out_rs$MILL$CTR_2035[adap_out$LOC] <- adap_out$MILL1_CTR
  out_rs$MILL$GLO_2035 <- raster(dum_rs)
  out_rs$MILL$GLO_2035[adap_out$LOC] <- adap_out$MILL1_GLO
  
  #mill 2075
  out_rs$MILL$BUF_2075 <- raster(dum_rs)
  out_rs$MILL$BUF_2075[adap_out$LOC] <- adap_out$MILL2_BUF
  out_rs$MILL$CTR_2075 <- raster(dum_rs)
  out_rs$MILL$CTR_2075[adap_out$LOC] <- adap_out$MILL2_CTR
  out_rs$MILL$GLO_2075 <- raster(dum_rs)
  out_rs$MILL$GLO_2075[adap_out$LOC] <- adap_out$MILL2_GLO
  
  #sorg 2035
  out_rs$SORG <- list()
  out_rs$SORG$BUF_2035 <- raster(dum_rs)
  out_rs$SORG$BUF_2035[adap_out$LOC] <- adap_out$SORG1_BUF
  out_rs$SORG$CTR_2035 <- raster(dum_rs)
  out_rs$SORG$CTR_2035[adap_out$LOC] <- adap_out$SORG1_CTR
  out_rs$SORG$GLO_2035 <- raster(dum_rs)
  out_rs$SORG$GLO_2035[adap_out$LOC] <- adap_out$SORG1_GLO
  
  #sorg 2075
  out_rs$SORG$BUF_2075 <- raster(dum_rs)
  out_rs$SORG$BUF_2075[adap_out$LOC] <- adap_out$SORG2_BUF
  out_rs$SORG$CTR_2075 <- raster(dum_rs)
  out_rs$SORG$CTR_2075[adap_out$LOC] <- adap_out$SORG2_CTR
  out_rs$SORG$GLO_2075 <- raster(dum_rs)
  out_rs$SORG$GLO_2075[adap_out$LOC] <- adap_out$SORG2_GLO
  
  save(out_rs,file=paste(gcm_outDir,"/adap_raster_outputs.RData",sep=""))
} else {
  load(paste(gcm_outDir,"/adap_raster_outputs.RData",sep=""))
}


#####################################
#GCM-specific plots
#figDir
fig_dir <- paste(gcm_outDir,"/figures",sep="")

#making plots
plot_overlap("RICE", "BUF_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("RICE", "BUF_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WSPR", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WSPR", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WWIN", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WWIN", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WHEA", "P_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("WHEA", "P_2075", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("MILL", "P_2035", xt=extent(-30,160,-45,75), fig_dir)
plot_overlap("MILL", "P_2075", xt=extent(-30,160,-45,75), fig_dir)
plot_overlap("SORG", "BUF_2035", xt=extent(-130,160,-45,75), fig_dir)
plot_overlap("SORG", "BUF_2075", xt=extent(-130,160,-45,75), fig_dir)






