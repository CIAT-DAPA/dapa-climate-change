#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#maxiter <- 10
#base_exp <- 11


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
scratch <- "/scratch/eejarv/calib"
maxiter <- 15
selection <- "v6"
base_exp <- 33


source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))


####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(NA,sample(1:9999,49))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)
runs_ref2 <- expand.grid(SID=runs_ref$SID,RUN=1:5)
runs_ref <- merge(runs_ref2,runs_ref,by="SID",all=T,sort=F)

cropName <- "gnut"

#do i want to plot (not for eljefe)
plot_all <- F
use_scratch <- T

#number of cpus to use
if (nrow(runs_ref) > 8) {ncpus <- 8} else {ncpus <- nrow(runs_ref)}

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

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(runs_ref)),glam_optimise_ygp_ipdate_wrapper))

#stop the cluster
sfStop()






