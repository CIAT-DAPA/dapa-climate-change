#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"


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

#scaling factors to datasets per variable
scList <- data.frame(VID=1:3,GCM=c(1,1,1),WCL=c(1,1,1),
                     CL_CRU=c(1,1,1),TS_CRU=c(0.1,0.1,0.1),
                     E40=c(1,1,NA),CL_WST=c(1,1,1),
                     TS_WST=c(1,0.1,0.1))


#processes to complete
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
isoList <- regions$ISO
dsetList <- c("ts-CRU","cl-E40","ts-WST")
vnList <- c("pr","tas","dtr")
procList <- expand.grid(GCM=gcmList,OBS=dsetList,VAR=vnList)
procList$GCM <- paste(procList$GCM)
procList$OBS <- paste(procList$OBS); procList$VAR <- paste(procList$VAR)

#create output folder
oDir <- paste(mdDir,"/assessment/output-data/_summary",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

this_proc <- 1

#get gcm 
gcm_ens <- paste(procList$GCM[this_proc])
gcm <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[1]
ens <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[2]
dset <- paste(procList$OBS[this_proc])
vn <- paste(procList$VAR[this_proc])

#read base raster to get characteristics
rs <- raster(paste(mdDir,"/baseline/",gcm,"/",ens,"_monthly/1985/",vn,"_01.tif",sep=""))

#load the data for all countries
iso <- isoList[1]














