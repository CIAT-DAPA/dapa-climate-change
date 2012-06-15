#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT


#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
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
procList <- expand.grid(GCM=gcmList,ISO=isoList)


#time series data
tsWST <- paste(inputDD,"/all-weather-stations",sep="")
tsE40 <- e40Dir
tsCRU <- paste(inputDD,"/cru-ts-data",sep="")


x <- lapply(1:nrow(procList),check_proc)
y <- do.call("rbind", x)
y <- y[which(!y$ISOK),]
y$GCM_ENS <- paste(y$GCM,"_ENS_",y$ENS,sep="")

procRepeat <- unique(y$GCM_ENS)
procRepeat <- expand.grid(GCM=procRepeat,ISO=isoList)

#### remove the files which are done
for (i in 1:nrow(procRepeat)) {
  iso <- paste(procRepeat$ISO[i])
  reg <- paste(regions$REGION[which(regions$ISO == iso)])
  gcm <- unlist(strsplit(paste(procRepeat$GCM[i]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procRepeat$GCM[i]),"_ENS_",fixed=T))[2]
  
  oDir <- paste(outputDD,"/",reg,"/",iso,sep="")
  procDir <- paste(oDir,"/y.proc",sep="")
  for (vid in 1:3) {
    vn_gcm <- paste(vnList$GCM[vid]) #variable name
    procFil <- paste(procDir,"/",vn_gcm,"_",gcm,"_",ens,".proc",sep="") #check file
    if (file.exists(procFil)) {
      rmf <- file.remove(procFil)
    }
    
    cru <- read.csv(paste(outputDD,"/",reg,"/",iso,"/ts-CRU/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))
    wst <- read.csv(paste(outputDD,"/",reg,"/",iso,"/ts-WST/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))
    e40 <- read.csv(paste(outputDD,"/",reg,"/",iso,"/ts-E40/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))
    
  }
}







