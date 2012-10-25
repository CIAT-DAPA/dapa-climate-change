#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#source functions of interest
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/cmip5/07.glam-cmip5_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
maxiter <- 15 #to grab last optim values
plot_all <- F #do i want to plot (not for eljefe)
use_scratch <- T

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
ascDir <- paste(glamInDir,"/ascii",sep="")
sowDir <- paste(ascDir,"/sow",sep="")

#weather data directories
wthDir_his_rw <- paste(glamInDir,"/ascii/wth-cmip5_hist",sep="")
wthDir_rcp_rw <- paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep="")
wthDir_his_bc <- paste(ascDir,"/wth-cmip5_hist_bc",sep="")
wthDir_rcp_bc <- paste(ascDir,"/wth-cmip5_rcp45_bc",sep="")

############################################
#here you need to put the permutation factors
#Factor 1: Gridcells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#Factor 2: GCMs
gcmList_his <- list.files(wthDir_his_rw,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp_rw,pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#Factor 3: parameter sets
parsetList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
parsetList <- parsetList[which(parsetList$ISSEL==1),]
parsetList <- parsetList$EXPID

#Factor 4: experiments
#conditionals may be needed inside the final function
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain")
expList_rcp <- c("rcp_allin","rcp_bcrain")

#Factor 5: planting date
#only relevant for rcp experiments
sdList <- c(-15:15)

#permutation of gridcells and GCMs
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList)








