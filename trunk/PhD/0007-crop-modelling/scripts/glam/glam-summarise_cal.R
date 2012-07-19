#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
base_exp <- 32
selection <- "v5"


#sourcing functions
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-summarise_cal_wrapper.R",sep=""))

#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(sample(1:9999,20))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)

#do i want to plot (not for eljefe)
plot_all <- F

#this_run <- 1
if (nrow(runs_ref) > 10) {ncpus <- 10} else {ncpus <- nrow(runs_ref)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("bDir")
sfExport("cDir")
sfExport("seeds")
sfExport("cropName")
sfExport("runs_ref")
sfExport("plot_all")
sfExport("selection")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(runs_ref)),glam_summarise_cal_wrapper))

#stop the cluster
sfStop()


