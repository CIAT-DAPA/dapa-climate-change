#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#maxiter <- 10
#version <- "c"

#run <- 1
#expID <- "10"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 10
version <- "c"

#run <- 1 2 3 4 5
#expID <- "10"

####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(sample(1:9999,20),NA)
#seeds <- c(NA)

expIDs <- c(10:(9+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)
runs_ref2 <- expand.grid(SID=runs_ref$SID,RUN=1:5)
runs_ref <- merge(runs_ref2,runs_ref,by="SID",all=T,sort=F)

#source all needed functions
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/glam-optimise-glo_wrapper.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

cropName <- "gnut"

#source(paste(src.dir,"/glam-optimise-glo.R",sep=""))

if (nrow(runs_ref) > 8) {ncpus <- 8} else {ncpus <- nrow(runs_ref)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("bDir")
sfExport("maxiter")
sfExport("version")
sfExport("seeds")
sfExport("cropName")
sfExport("runs_ref")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(runs_ref)),glam_optimise_glo_wrapper))

#stop the cluster
sfStop()






