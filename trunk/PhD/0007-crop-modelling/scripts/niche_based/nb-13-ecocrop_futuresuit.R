#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT
stop("!")

#load packages
library(rgdal); library(raster)

#i/o directories
cropName <- "gnut"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
modDir <- paste(nbDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")
prjDir <- paste(ecoDir,"/proj",sep="")

#selected model runs
skill <- read.csv(paste(ecoDir,"/data/runs_discard.csv",sep=""))
ecoRuns <- skill$RUN[which(skill$SEL)]

#details for calculations
rcpList <- c("del","loci","raw")
gcmList <- list.files(paste(prjDir,"/rcp_del",sep=""))

#calculate future change in suitability for each future-baseline combination


#calculate means and quantiles


#calculate probability of suit change above and below certain thresholds





