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


#variables to be set
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a102/eejarv/CMIP5"


#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))


#list of gcms and countries/regions
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:4,GCM=c("pr","tas","dtr","rd"),WCL=c("prec","tmean","dtr",NA),
                     CL_CRU=c("prec","tmean","dtr","wet"),TS_CRU=c("pre","tmp","dtr","wet"),
                     E40=c("prec","tasm","dtr","wet"),CL_WST=c("rain","tean","dtr",NA),
                     TS_WST=c("pr","tas","dtr",NA))



##################################################################################
#processes to complete
#interannual variability index
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
isoList <- regions$ISO
dsetList <- c("vi_rev-CRU","vi_rev-E40","vi_rev-WST")
vnList <- c("pr","tas","dtr","rd")
procList <- expand.grid(GCM=gcmList,OBS=dsetList,VAR=vnList)
procList$GCM <- paste(procList$GCM)
procList$OBS <- paste(procList$OBS); procList$VAR <- paste(procList$VAR)
procList <- procList[-which(procList$OBS == "vi_rev-WST" & procList$VAR == "rd"),]
#procList <- procList[-which(procList$OBS == "vi_rev-E40" & procList$VAR == "dtr"),]

#create output folder
oDir <- paste(mdDir,"/assessment/output-data/_summary_revised2",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

odir_rs <- paste(oDir,"/interannual-skill-vi",sep="")
if (!file.exists(odir_rs)) {dir.create(odir_rs)}

#this_proc <- 1
#summarise_interannual_vi_revised(1)
#for (k in 1:nrow(procList)) {summarise_interannual_vi_revised(k)}

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>20) {ncpus <- 20}

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
system.time(sfSapply(as.vector(1:nrow(procList)),summarise_interannual_vi_revised))

#stop the cluster
sfStop()



#######################################
#calculate average of all climate models for each metric
#interannual variability index (vi)

vnList <- c("pr","tas","dtr","rd")
metList <- c("vi")
sList <- c("DJF","MAM","JJA","SON","ANN")
gcmList <- unique(paste(gcmChars$GCM,"_",gcmChars$Ensemble,sep=""))
gcmList <- gcmList[which(gcmList != "multi_model_mean_ENS_r1i1p1")]

procList <- expand.grid(VAR=vnList,MET=metList,SEAS=sList)
#this_proc <- 1

for (k in 1:nrow(procList)) {mean_summary_interannual_vi_revised(k)}
for (k in 1:nrow(procList) {mean_summary_interannual_vi_e40(k)}


     
     
