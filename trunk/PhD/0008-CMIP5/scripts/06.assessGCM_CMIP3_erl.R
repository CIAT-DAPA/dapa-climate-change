#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

library(raster); library(rgdal); library(maptools)

#6. Calculation of metrics as stated in methods

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
e40Dir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"


#source functions
source(paste(src.dir2,"/scripts/CMIP3-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#input directories and configurations
inputDD <- paste(mdDir,"/assessment/input-data",sep="")
outputDD <- paste(mdDir,"/assessment/output-data",sep="")

#administrative areas data
admDir <- paste(inputDD,"/adm-data",sep="")

#list of gcms and countries/regions
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:4,GCM=c("pr","tas","dtr","rd"),WCL=c("prec","tmean","dtr",NA),
                     CL_CRU=c("prec","tmean","dtr","wet"),TS_CRU=c("pre","tmp","dtr","wet"),
                     E40=c("prec","tasm","dtr","wet"),CL_WST=c("rain","tean","dtr",NA),
                     TS_WST=c("pr","tas","dtr",NA))

#scaling factors to datasets per variable
scList <- data.frame(VID=1:4,GCM=c(1,1,1,1),WCL=c(1,1,1,NA),
                     CL_CRU=c(1,1,1,1),TS_CRU=c(0.1,0.1,0.1,0.01),
                     E40=c(1,1,1,1),CL_WST=c(1,1,1,NA),
                     TS_WST=c(1,0.1,0.1,NA))


#processes to complete
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
isoList <- regions$ISO
procList <- expand.grid(GCM=gcmList,ISO=isoList)


#################################################################################
#################################################################################
######################### REVISION FOR ERL PAPER ################################
#################################################################################
#################################################################################
#a. mean climates: for each area using the values of GCM gridcells and the mean
#                  values of the datasets calculate the following
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse

#climatology data
clWCL <- paste(inputDD,"/wcl-data",sep="")
clCRU <- paste(inputDD,"/cru-data",sep="")
clE40 <- e40Dir
clWST <- paste(inputDD,"/wcl-weather-stations",sep="")

#check those that are done already
procList <- check_done(procList,"x_rev2.proc")

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("e40Dir")
sfExport("inputDD")
sfExport("outputDD")
sfExport("clWCL")
sfExport("clCRU")
sfExport("clE40")
sfExport("clWST")
sfExport("admDir")
sfExport("vnList")
sfExport("scList")
sfExport("procList")
sfExport("regions")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),mean_climate_skill_revised2))

#stop the cluster
sfStop()


#######################################################################
#b. interannual variability: calculate the VI as in Scherrer 2011
#                            so that the issue of non-matching series is overcome

#specify initial and final years
yi <- 1961
yf <- 2000

#time series data
tsWST <- paste(inputDD,"/all-weather-stations",sep="")
tsE40 <- e40Dir
tsCRU <- paste(inputDD,"/cru-ts-data",sep="")

#check those that are done already
procList <- check_done(procList,"z_rev.proc")

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("e40Dir")
sfExport("inputDD")
sfExport("outputDD")
sfExport("tsCRU")
sfExport("tsE40")
sfExport("tsWST")
sfExport("admDir")
sfExport("vnList")
sfExport("scList")
sfExport("procList")
sfExport("regions")
sfExport("yi")
sfExport("yf")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),interannual_vi_revised))

#stop the cluster
sfStop()

