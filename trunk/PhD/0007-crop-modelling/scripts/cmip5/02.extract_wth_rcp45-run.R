#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#sourcing needed functions
source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir3,"/scripts/cmip5/02.extract_wth_rcp45-functions.R",sep=""))

#other details
ys <- 2020
ye <- 2049

#input directories
#model_dir <- "V:/eejarv"
#base_dir <- "W:/eejarv/PhD-work/crop-modelling"
model_dir <- "/nfs/a102/eejarv"
base_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
mdDir <- paste(model_dir,"/CMIP5/rcp45",sep="")


#get the list of unprocessed GCMs
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
cDataDir <- paste(base_dir,"/climate-data/gridcell-data",sep="")
outDir <- paste(cDataDir,"/IND_RCP45",sep="")
cropName <- "gnut"
cells <- read.csv(paste(base_dir,"/GLAM/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
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

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export functions
sfExport("src.dir")
sfExport("src.dir2")
sfExport("src.dir3")
sfExport("base_dir")
sfExport("model_dir")
sfExport("mdDir")
sfExport("ys")
sfExport("ye")

#run the function in parallel
system.time(sfSapply(as.vector(mList),wrapper_RCP45_extract))

#stop the cluster
sfStop()







