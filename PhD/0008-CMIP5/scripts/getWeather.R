#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
#mdDir <- "V:/eejarv/CMIP5/baseline"
#i <- 1 #gcm to process

source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

ys <- 1961
ye <- 2002

#get the list of unprocessed GCMs
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
cDataDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
outDir <- paste(cDataDir,"/IND_CMIP5",sep="")
cropName <- "gnut"
cells <- read.csv(paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
gcmList <- unique(gcmChars$GCM)
mList <- c()
for (i in 1:length(gcmList)) {
  gcm <- gcmList[i]
  outGCMDir <- paste(outDir,"/",gcm,sep="")
  thisGCM <- gcmChars[which(gcmChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  for (ens in ensList) {
    outEnsDir <- paste(outGCMDir,"/",ens,sep="")
    thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
    vnList <- c("pr","tasmin","tasmax","rsds")
    for (vn in vnList) {
      outVarDir <- paste(outEnsDir,"/",vn,sep="")
      flist <- list.files(outVarDir,pattern="\\.csv")
      if (length(flist) != nrow(cells)) {
        mList <- c(mList,i)
      }
    }
    
  }
}
mList <- unique(mList)

ncpus <- length(mList)
if (ncpus>12) {ncpus <- 12}

#od <- CMIP5_extract(cells=all_cells,cChars=gcmChars,dum_rs=drs,i=1,yi=1961,yf=2002,oDir=outDir)

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export functions
sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("mdDir")
sfExport("ys")
sfExport("ye")

#run the function in parallel
system.time(sfSapply(as.vector(mList),wrapper_CMIP_extract))

#stop the cluster
sfStop()




