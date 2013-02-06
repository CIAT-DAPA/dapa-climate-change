#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

library(raster)

# #variables to be set
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
# src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# mdDir <- "/nfs/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
# cmip5Dir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "W:/eejarv/IPCC_CMIP3/20C3M/original-data"
#cmip5Dir <- "V:/eejarv/CMIP5"


#variables to be set
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
cmip5Dir <- "/mnt/a102/eejarv/CMIP5"


#source functions
source(paste(src.dir2,"/scripts/CMIP3-functions.R",sep=""))


#list of gcms and countries/regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:3,GCM=c("prec","tmean","dtr"),WCL=c("prec","tmean","dtr"),
                     CL_CRU=c("prec","tmean","dtr"),TS_CRU=c("pre","tmp","dtr"),
                     E40=c("prec","tasm","dtr"),CL_WST=c("rain","tean","dtr"),
                     TS_WST=c("pr","tas","dtr"))

##################################################################################
#processes to complete
gcmList <- list.files(mdDir)
isoList <- regions$ISO
dsetList <- c("vi-CRU","vi-E40","vi-WST")
vnList <- c("prec","tmean","dtr")
procList <- expand.grid(GCM=gcmList,OBS=dsetList,VAR=vnList)
procList$GCM <- paste(procList$GCM)
procList$OBS <- paste(procList$OBS); procList$VAR <- paste(procList$VAR)
#procList <- procList[-which(procList$OBS == "vi-E40" & procList$VAR == "dtr"),]

#create output folder
oDir <- paste(cmip5Dir,"/assessment/output-data-cmip3/_summary",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

odir_rs <- paste(oDir,"/interannual-skill-vi",sep="")
if (!file.exists(odir_rs)) {dir.create(odir_rs)}

#this_proc <- 1
#for (i in 1:50) {summarise_interannual_vi(i)}

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir2")
sfExport("mdDir")
sfExport("regions")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("isoList")
sfExport("procList")
sfExport("dsetList")
sfExport("vnList")
sfExport("oDir")
sfExport("odir_rs")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),summarise_interannual_vi))

#stop the cluster
sfStop()





#######################################
#calculate average of all climate models for each metric
#interannual variability index (vi)

vnList <- c("prec","tmean","dtr")
metList <- c("vi")
sList <- c("DJF","MAM","JJA","SON","ANN")
gcmList <- list.files(mdDir)
gcmList <- gcmList[which(gcmList != "multi_model_mean")]

procList <- expand.grid(VAR=vnList,MET=metList,SEAS=sList)
#this_proc <- 1

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir2")
sfExport("mdDir")
sfExport("regions")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("vnList")
sfExport("procList")
sfExport("metList")
sfExport("oDir")
sfExport("odir_rs")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),mean_summary_interannual_vi))

#stop the cluster
sfStop()



#######################################
### ERA-40
#calculate average of all climate models for each metric
#interannual variability index (vi)

vnList <- c("prec","tmean","dtr")
metList <- c("vi")
sList <- c("DJF","MAM","JJA","SON","ANN")
gcmList <- list.files(mdDir)

procList <- expand.grid(VAR=vnList,MET=metList,SEAS=sList)
#this_proc <- 1

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir2")
sfExport("mdDir")
sfExport("regions")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("vnList")
sfExport("procList")
sfExport("metList")
sfExport("oDir")
sfExport("odir_rs")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),mean_summary_interannual_vi_e40))

#stop the cluster
sfStop()

