#Julian Ramirez-Villegas
#Jan 2012

library(raster); library(sfsmisc)

src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))

#base directories
gcmDir <- "/mnt/a102/eejarv/CMIP5/Amon"
bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
scratch <- "~/Workspace/pgr_analogues"

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
crop_name <- "Rice"
yi_h <- 1966; yf_h <- 2005
yi_f1 <- 2016; yf_f1 <- 2055
yi_f2 <- 2056; yf_f2 <- 2095
gcmList <- read.csv(paste(cfgDir,"/gcm_list.csv",sep=""))
ha_rs <- raster(paste(bDir,"/crop-data/",crop_name,".harea/",tolower(crop_name),"_harea_glo.tif",sep=""))
pl_rs <- raster(paste(bDir,"/crop-data/",crop_name,".crop.calendar.fill/plant_month.tif",sep=""))
hr_rs <- raster(paste(bDir,"/crop-data/",crop_name,".crop.calendar.fill/harvest_month.tif",sep=""))

#output crop directory
coutDir <- paste(out_bDir,"/",crop_name,sep="")
if (!file.exists(coutDir)) {dir.create(coutDir)}

#data.frame of grid cells to extract
gCells <- data.frame(LOC=which(!is.na(ha_rs[])))
gCells$LON <- xFromCell(ha_rs,gCells$LOC)
gCells$LAT <- yFromCell(ha_rs,gCells$LOC)
gCells$PL <- extract(pl_rs,cbind(x=gCells$LON,y=gCells$LAT))
gCells$HR <- extract(hr_rs,cbind(x=gCells$LON,y=gCells$LAT))

#gcm
gcm_i <- 1
gcm <- paste(gcmList$GCM[gcm_i])
ens <- paste(gcmList$ENS[gcm_i])

#output GCM-specific directory
gcm_outDir <- paste(coutDir,"/",gcmList$GCM_ENS[gcm_i],sep="")
if (!file.exists(gcm_outDir)) {dir.create(gcm_outDir)}

#example location (later into sapply)
for (i in 1:7300) {
  system.time(analyse_gridcell(1))
}


