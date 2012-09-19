#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

##### recalibrate glam for gridcells of interest, all plausible parameter sets
##### and for each GCM


#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#maxiter <- 10
#base_exp <- 11


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
scratch <- "/scratch/eejarv/cmip5_hist"
selection <- "v6"
maxiter <- 15 #to grab last optim values

#name of crop and other details
cropName <- "gnut"
plot_all <- F #do i want to plot (not for eljefe)
use_scratch <- T

source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/cmip5/03.glam-cmip5_hist-functions.R",sep=""))

#other directories
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
gcmDir <- paste(cropDir,"/inputs/ascii/wth-cmip5_hist",sep="")

####load list of experiments to perform
parset_list <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
runs_ref <- parset_list[which(parset_list$ISSEL==1),]
row.names(runs_ref) <- 1:nrow(runs_ref)

#list of gcms and update runs_ref
gcmList <- list.files(gcmDir,pattern="_ENS")
runs_ref <- expand.grid(EXPID=runs_ref$EXPID,GCM=gcmList)

#number of cpus to use
if (nrow(runs_ref) > 14) {ncpus <- 14} else {ncpus <- nrow(runs_ref)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("bDir")
sfExport("maxiter")
sfExport("seeds")
sfExport("cropName")
sfExport("runs_ref")
sfExport("plot_all")
sfExport("selection")
sfExport("use_scratch")
sfExport("scratch")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(runs_ref)),glam_cmip5_hist_wrapper))

#stop the cluster
sfStop()

