#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#1. Calculate monthly totals for pr, tas, and dtr

#Get CMIP5 weather data
library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

yi <- 1961
yf <- 2005

#get the list of unprocessed GCMs
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
gcmList <- unique(gcmChars$GCM)

mList <- 1:length(gcmList)

ncpus <- length(mList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export functions
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("yi")
sfExport("yf")

#run the function in parallel
system.time(sfSapply(as.vector(mList),wrapper_monthly_TS))

#stop the cluster
sfStop()


########here calculate MMM (multi-model means)
#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
vnList <- c("pr","tas","dtr")
gcmList <- unique(data.frame(GCM=gcmChars$GCM,ENS=gcmChars$Ensemble)) #model runs to average
gcmList$DIR <- paste(mdDir,"/",gcmList$GCM,"/",gcmList$ENS,"_monthly",sep="")

for (vn in vnList) {
  #vn <- vnList[1]
  cat("processing variable",vn,"\n")
  for (year in yi:yf) {
    #year <- yi
    for (m in 1:12) {
      if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
      fList <- paste(gcmList$DIR,"/",year,"/",vn,"_",mth,".tif",sep="")
      fPres <- as.character(sapply(fList,checkExists))
      fPres <- fPres[which(!is.na(fPres))]
      
      #loop through files, resampling them accordingly and adding them to a list, from which
      #a rasterstack is created and then use calc() to get the mean, and just write it
      
    }
  }
}









