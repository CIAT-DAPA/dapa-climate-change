#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

library(raster)

#source directories
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD"

#source functions of interest
source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_rain-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/01.make_wth-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/signals/climateSignals-functions.R",sep=""))

#base and data directories
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
climDir <- paste(bDir,"/climate-data",sep="")
glamDir <- paste(bDir,"/GLAM",sep="")
obsDir <- paste(climDir,"/gridcell-data/IND",sep="")
hisDir <- paste(climDir,"/gridcell-data/IND_CMIP5",sep="")
rcpDir <- paste(climDir,"/gridcell-data/IND_RCP45",sep="")

#additional details and directories
cropName <- "gnut"
ver <- "v6"
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
ascDir <- paste(glamInDir,"/ascii",sep="")
sowDir <- paste(ascDir,"/sow",sep="")
wthDir_his <- paste(glamInDir,"/ascii/wth-cmip5_hist",sep="")
wthDir_rcp <- paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep="")

#base output directories
bcDir_his <- paste(climDir,"/gridcell-data/IND_CMIP5_BC",sep="")
bcDir_rcp <- paste(climDir,"/gridcell-data/IND_RCP45_BC",sep="")
wthDirBc_his <- paste(ascDir,"/wth-cmip5_hist_bc",sep="")
wthDirBc_rcp <- paste(ascDir,"/wth-cmip5_rcp45_bc",sep="")
if (!file.exists(wthDirBc_his)) {dir.create(wthDirBc_his)}
if (!file.exists(wthDirBc_rcp)) {dir.create(wthDirBc_rcp)}

###variable names
vn <- "rain"
vn_gcm <- "pr"

#years initial and final
yi_h <- 1961
yf_h <- 2000
yi_f <- 2020
yf_f <- 2049

#load gridcell data.frame
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#data that is needed by the wth generation process
rabi_sow <- raster(paste(cropDir,"/",tolower(cropName),"-zones/plant_rabi.asc",sep=""))

#load GCM list data.frame
gcmChars <- read.table(paste(src.dir,"/0008-CMIP5/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")

#listing GCMs
gcmList_his <- list.files(wthDir_his,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp,pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#permutation of gridcells and GCMs
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList)

#number of cpus to use
if (nrow(all_proc) > 3) {ncpus <- 3} else {ncpus <- nrow(all_proc)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("bDir")
sfExport("climDir")
sfExport("glamDir")
sfExport("obsDir")
sfExport("hisDir")
sfExport("rcpDir")
sfExport("cropName")
sfExport("ver")
sfExport("cropDir")
sfExport("glamInDir")
sfExport("ascDir")
sfExport("sowDir")
sfExport("wthDir_his")
sfExport("wthDir_rcp")
sfExport("bcDir_his")
sfExport("bcDir_rcp")
sfExport("wthDirBc_his")
sfExport("wthDirBc_rcp")
sfExport("vn")
sfExport("vn_gcm")
sfExport("yi_h")
sfExport("yf_h")
sfExport("yi_f")
sfExport("yf_f")
sfExport("cells")
sfExport("rabi_sow")
sfExport("gcmChars")
sfExport("all_proc")

#run the bias correction function in parallel
system.time(sfSapply(as.vector(1:nrow(all_proc)),wrap_bc_wthmkr))

#run the wth file generation function in parallel
#system.time(sfSapply(as.vector(1:nrow(all_proc)),make_bc_wth_wrapper))

#stop the cluster
sfStop()


#temporary processing in eljefe
for (j in 5656:5850) {
  wrap_bc_wthmkr(j)
}






