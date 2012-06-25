#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#maxiter <- 10
#run <- 1
#version <- "c"
#expID <- "10"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 10
#run <- 1 2 3 4 5
version <- "c"
expID <- "10"

####list of seeds to randomise parameter list
set.seed(512)
seeds <- sample(1:9999,20)
sid <- 1

#source all needed functions
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/glam-optimise-glo_wrapper.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))


#source(paste(src.dir,"/glam-optimise-glo.R",sep=""))

ncpus <- nrow(procList)
if (ncpus>7) {ncpus <- 7}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("bDir")
sfExport("maxiter")
sfExport("version")
sfExport("expID")
sfExport("seeds")
sfExport("sid")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_GCM_glam_optimise_ygp))

#stop the cluster
sfStop()






