###
#do 96 sensitivity runs over the whole of West Africa
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
library(ggplot2); library(reshape2)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_obs <- paste(runDir,"/sens_obs",sep="")
sensDir_3deg12km <- paste(runDir,"/sens_3deg-12km_exp",sep="")
sensDir_3degobs <- paste(runDir,"/sens_3deg-obs",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_new",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))
if (!file.exists(paste(sensDir,"/sensitivity_runs.csv",sep=""))) {
  write.csv(sensruns,paste(sensDir,"/sensitivity_runs.csv",sep=""),quote=T,row.names=F)
}

#read sensitivity results
outsens <- read.csv(paste(sensDir,"/sensitivity_result.csv",sep=""))



