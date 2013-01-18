#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#4. calculate monthly means (tas) and totals (prec) for ERA-40 data

library(raster)

#variables to be set

### eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#see4-75 Centos6
#src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
#e40Dir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#see4-75 windows7
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#dawnpatrol Centos6
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
e40Dir <- "/media/PhD_data_01/PhD-work/crop-modelling/climate-data/ERA-40"

#sourcing functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

yi <- 1961
yf <- 2000

ncpus <- length(yi:yf)
if (ncpus>4) {ncpus <- 4}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export functions
sfExport("src.dir")
sfExport("src.dir2")
sfExport("e40Dir")
sfExport("yi")
sfExport("yf")

#run the function in parallel
system.time(sfSapply(as.vector(yi:yf),wrapper_monthly_E40))

#stop the cluster
sfStop()












