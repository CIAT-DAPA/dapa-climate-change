#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

library(raster)
library(maptools); data(wrld_simpl)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD"

source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_cf_temp-functions.R",sep=""))

#config details
cropName <- "gnut"
ver <- "v6"
gcmChars <- read.table(paste(src.dir,"/0008-CMIP5/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")

#directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
clmDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")

#data directories
obsDir <- paste(clmDir,"/IND",sep="")
hisDir <- paste(clmDir,"/IND_CMIP5",sep="")
rcpDir <- paste(clmDir,"/IND_RCP45",sep="")

#output directories
outDir_cf <- paste(clmDir,"/IND_RCP45_DEL",sep="")
outDir_bc <- paste(clmDir,"/IND_RCP45_SH",sep="")
outDir_hbc <- paste(clmDir,"/IND_CMIP5_SH",sep="")

if (!file.exists(outDir_cf)) {dir.create(outDir_cf)}
if (!file.exists(outDir_bc)) {dir.create(outDir_bc)}
if (!file.exists(outDir_hbc)) {dir.create(outDir_hbc)}

#locations
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#list of GCMs
gcmList_his <- list.files(paste(glamInDir,"/ascii/wth-cmip5_hist",sep=""),pattern="_ENS_")
gcmList_rcp <- list.files(paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep=""),pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#per grid cell apply these methods to maximum and minimum temperature and srad
#use period 1966-1993 as baseline
#use period 2021-2049 as future
yi_h <- 1965; yf_h <- 1993
yi_f <- 2021; yf_f <- 2049

#########
#variable matrix
varmx <- data.frame(OBS=c("cru_tmn","cru_tmx","rain","srad_e40"),
                    MOD=c("tasmin","tasmax","pr","rsds"),
                    BC=c(T,T,T,T),CF=c(T,T,T,T),FUN=c("mean","mean","sum","mean"),
                    CHG=c("abs","abs","rel","rel"))

#length=34

#produce the grid cell csv files
#for (k in 1:17) {st <- wrapper_gcm_bc_cf(k)} #part 1
#for (k in 18:34) {st <- wrapper_gcm_bc_cf(k)} #part 2

#from these, generate the wth files
#permutation of gridcells and GCMs
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList)

#data that is needed by the wth generation process
rabi_sow <- raster(paste(cropDir,"/",tolower(cropName),"-zones/plant_rabi.asc",sep=""))

#base output directories
ascDir <- paste(glamInDir,"/ascii",sep="")
sowDir <- paste(ascDir,"/sow",sep="")
wthDir_del <- paste(ascDir,"/wth-cmip5_rcp45_del",sep="") #pr is loci
wthDir_sh <- paste(ascDir,"/wth-cmip5_rcp45_sh",sep="") #pr is loci
wthDir_hsh <- paste(ascDir,"/wth-cmip5_hist_sh",sep="") #pr is loci

if (!file.exists(wthDir_del)) {dir.create(wthDir_del)}
if (!file.exists(wthDir_sh)) {dir.create(wthDir_sh)}
if (!file.exists(wthDir_hsh)) {dir.create(wthDir_hsh)}

#first test
for (j in 4421:6630) {
  oaa <- make_cf_bc_wth_wrapper(j)
}

#1 2210
#2211 4420
#4421 6630



