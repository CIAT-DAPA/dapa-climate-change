#Julian Ramirez-Villegas
#Jan 2012

library(raster); library(sfsmisc); library(maptools)

src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/pgr-cc-adaptation"
source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))

#base directories
gcmDir <- "/mnt/a102/eejarv/CMIP5/Amon"
bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
scratch <- "~/Workspace/pgr_analogues"

# gcmDir <- "/nfs/a102/eejarv/CMIP5/Amon"
# bDir <- "/nfs/a17/eejarv/pgr-cc-adaptation"
# scratch <- "/scratch/eejarv/pgr_analogues"

#input directories
hisDir <- paste(gcmDir,"/historical_amon",sep="")
rcpDir <- paste(gcmDir,"/rcp45_amon",sep="")
cfgDir <- paste(bDir,"/config",sep="")
cruDir <- paste(bDir,"/cru-data",sep="")
if (!file.exists(scratch)) {dir.create(scratch)}

#output directories
out_bDir <- paste(bDir,"/outputs",sep="")

#configuration details and initial data
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



###################################################################
#the buffer, country, and globe searcher
#1. if in a 250 km buffer there is a location whose current climate
#   overlaps 95% with the novel climate
#2. if within a country there is a location whose current climate overlaps 95% with the
#   novel climate
#3. if within the globe there is a location whose current climate overlaps 95% with the
#   novel climate

#gcm
gcm_i <- 1
gcm <- paste(gcmList$GCM[gcm_i])
ens <- paste(gcmList$ENS[gcm_i])

#output GCM-specific directory
gcm_outDir <- paste(out_bDir,"/",gcmList$GCM_ENS[gcm_i],sep="")

#produce raster of countries
data(wrld_simpl)
wld_rs <- rasterize(wrld_simpl,dum_rs)
gCells$CID <- extract(wld_rs,cbind(x=gCells$LON,y=gCells$LAT))

#location
loc <- 1
crop_name <- "WSPR"
period <- 2035

#get details
allCells <- gCells
cell <- gCells$LOC[loc]; cid <- gCells$CID[loc]
lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
sorg <- gCells$SORG[loc]

#location of file in chunks of 10k files. File number limitation
if (loc <= 10000) {
  datFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
} else if (loc > 10000 & loc <= 20000) {
  datFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
} else if (loc > 20000) {
  datFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
}

#load the future non-overlapped distribution
load(datFile)
gsData <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$GS_DATA
qlims <- as.numeric(quantile(gsData$CRU,probs=c(0.01,0.99)))
cru_in <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_CRU
cru_in <- cru_in[which(cru_in$x >= qlims[1] & cru_in$x <= qlims[2]),]

del_ot <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_DEL
del_ot <- del_ot[which(del_ot$x > max(cru_in$x)),]
rm(output)
#plot(cru_in$x,cru_in$y,ty="l",xlim=c(0,35))
#lines(del_ot$x,del_ot$y,ty="l",col="blue")
#abline(v=min(del_ot$x))
#lines(t_cru_in$x,t_cru_in$y,col="red")

if (nrow(del_ot) > 0) {
  #choose locations within 250 km
  allCells$DIST <- pointDistance(c(lon,lat),cbind(x=gCells$LON,y=gCells$LAT),longlat=T)/1000
  allCells <- allCells[-loc,]
  this_cells <- allCells[which(allCells$DIST <= 250),]
  
  if (nrow(this_cells) == 0) {
    xval_buf <- 0
  } else {
    #here test for location how much overlap there is
    a_loc <- as.numeric(row.names(this_cells))
    xval_buf <- sapply(a_loc,calc_novel_overlap,del_ot,gcm_outDir,crop_name,period)
    xval_buf <- max(xval_buf,na.rm=T)
  }
  
  #choose locations within country
  this_cells <- allCells[which(allCells$CID == cid),]
  if (nrow(this_cells) == 0) {
    xval_ctr <- 0
  } else {
    #here test for location how much overlap there is
    a_loc <- as.numeric(row.names(this_cells))
    xval_ctr <- sapply(a_loc,calc_novel_overlap,del_ot,gcm_outDir,crop_name,period)
    xval_ctr <- max(xval_ctr,na.rm=T)
  }
  
} else {
  xval_buf <- 1
  xval_ctr <- 1
  xval_glo <- 1
}


