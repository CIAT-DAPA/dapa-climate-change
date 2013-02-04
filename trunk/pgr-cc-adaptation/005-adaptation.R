#Julian Ramirez-Villegas
#Jan 2012

library(raster); library(sfsmisc); library(maptools)

# src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
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

###################################################################
#the buffer, country, and globe searcher
#1. if in a 250 km buffer there is a location whose current climate
#   overlaps 95% with the novel climate
#2. if within a country there is a location whose current climate overlaps 95% with the
#   novel climate
#3. if within the globe there is a location whose current climate overlaps 95% with the
#   novel climate

#gcms must be c(1:6,9,16,19,22)

#gcm
gcm_i <- 3
gcm <- paste(gcmList$GCM[gcm_i])
ens <- paste(gcmList$ENS[gcm_i])

#output GCM-specific directory
gcm_outDir <- paste(out_bDir,"/",gcmList$GCM_ENS[gcm_i],sep="")

#produce or load raster of countries
if (!file.exists(paste(cfgDir,"/wld_rs.RData",sep=""))) {
  data(wrld_simpl)
  wld_rs <- rasterize(wrld_simpl,dum_rs)
  save(wld_rs,file=paste(cfgDir,"/wld_rs.RData",sep=""))
} else {
  load(file=paste(cfgDir,"/wld_rs.RData",sep=""))
}

#extract country IDs
gCells$CID <- extract(wld_rs,cbind(x=gCells$LON,y=gCells$LAT))

#check existence
gCells$STATUS <- sapply(as.numeric(row.names(gCells)),check_done_adapt)
#gCells$STATUS <- F
gCells_u <- gCells[which(!gCells$STATUS),]
calc_adapt_gridcell(as.numeric(row.names(gCells_u))[1])


#run in serial, due to i/o problems
#for (k in as.numeric(row.names(gCells_u))) {calc_adapt_gridcell(k)}


#location
#calc_adapt_gridcell(1)

####################################################################
####################################################################
#number of cpus to use
if (nrow(gCells_u) > 14) {ncpus <- 14} else {ncpus <- nrow(gCells)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("gcmDir")
sfExport("bDir")
sfExport("scratch")
sfExport("hisDir")
sfExport("rcpDir")
sfExport("cfgDir")
sfExport("cruDir")
sfExport("out_bDir")
sfExport("gcmList")
sfExport("wld_rs")
sfExport("gCells")
sfExport("gCells_u")
sfExport("gcm_i")
sfExport("gcm")
sfExport("ens")
sfExport("gcm_outDir")

#sets of runs
#run the function in parallel
system.time(sfSapply(as.vector(as.numeric(row.names(gCells_u))),calc_adapt_gridcell))

#stop the cluster
sfStop()




