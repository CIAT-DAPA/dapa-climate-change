#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

library(raster)

#variables to be set
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))


#list of gcms and countries/regions
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:3,GCM=c("pr","tas","dtr"),WCL=c("prec","tmean","dtr"),
                     CL_CRU=c("prec","tmean","dtr"),TS_CRU=c("pre","tmp","dtr"),
                     E40=c("prec","tasm",NA),CL_WST=c("rain","tean","dtr"),
                     TS_WST=c("pr","tas","dtr"))

#processes to complete
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
isoList <- regions$ISO
dsetList <- c("ts-CRU","ts-E40","ts-WST")
vnList <- c("pr","tas","dtr")
procList <- expand.grid(GCM=gcmList,OBS=dsetList,VAR=vnList)
procList$GCM <- paste(procList$GCM)
procList$OBS <- paste(procList$OBS); procList$VAR <- paste(procList$VAR)
procList <- procList[-which(procList$OBS == "ts-E40" & procList$VAR == "dtr"),]

#create output folder
oDir <- paste(mdDir,"/assessment/output-data/_summary",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

odir_rs <- paste(oDir,"/interannual-skill",sep="")
if (!file.exists(odir_rs)) {dir.create(odir_rs)}

#this_proc <- 1
#summarise_interannual(144)

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>10) {ncpus <- 10}

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
system.time(sfSapply(as.vector(1:nrow(procList)),summarise_interannual))

#stop the cluster
sfStop()



#######################################
#calculate average of all climate models for each metric

metList <- c("rmse","mbr","ccoef")
sList <- c("DJF","MAM","JJA","SON","ANN")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))

for (met in metList) {
  #met <- metList[1]
  for (seas in sList) {
    #seas <- sList[1]
    
    #load all models and all datasets (ts-CRU and ts-WST)
    
    
  }
}


