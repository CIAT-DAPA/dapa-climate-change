#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

#load libraries
library(raster)

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/interpolate-functions.R",sep=""))

#defining basic stuff
anuDir <- "C:/anu/Anuspl43/bin"
inDir <- "F:/PhD-work/crop-modelling/climate-data"
gdir <- paste(inDir,"/gsod-daily",sep="")
hdir <- paste(inDir,"/ghcn-daily",sep="")
cdir <- paste(inDir,"/CIAT-daily/selection_afr",sep="")

#######
##Define the region i want to work with
re <- "afr"

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=3) #initiate cluster

#export functions
sfExport("interpolateDay")
sfExport("createRunFile")
sfExport("createPrjFile")
sfExport("writeDat")
sfExport("leap")

#export variables
sfExport("anuDir")
sfExport("inDir")
sfExport("gdir")
sfExport("hdir")
sfExport("cdir")
sfExport("re")

count <- 1
#for (ye in 1960:2009) {
for (ye in 1971:2009) {
  cat("Processing",ye,"\n")
  
  #loading the input data
  goData <- read.csv(paste(gdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
  goData$USAF <- NULL; goData$WBAN <- NULL #remove extra fields
  if (nrow(goData)>0) {
    goData$SOURCE <- "GSOD"
  } else {
    goData <- cbind(goData,SOURCE=logical(0))
  }
  
  ghData <- read.csv(paste(hdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
  if (nrow(ghData)>0) {
    ghData$SOURCE <- "GHCN"
  } else {
    ghData <- cbind(ghData,SOURCE=logical(0))
  }
  
  if (file.exists(paste(cdir,"/grouped_output-",re,"/",ye,".csv",sep=""))) {
    ciData <- read.csv(paste(cdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
    ciData$SOURCE <- "CIAT"
    #merge the three datasets
    gaData <- rbind(goData,ghData,ciData)
  } else {
    #merge only gsod and ghcn
    gaData <- rbind(goData,ghData)
  }
  
  #check whether leap year so to remove day 366 if needed
  nd <- leap(ye)
  if (nd==365) {gaData$X366 <- NULL}
  
  #interpolations directory
  iDir <- paste(inDir,"/daily-interpolations-v3",sep=""); if (!file.exists(iDir)) {dir.create(iDir)}
  if (file.exists(paste(iDir,"/",ye,"-",re,sep=""))) { #check if files exist in folder
    fl <- list.files(paste(iDir,"/",ye,"-",re,sep=""))
  } else {
    fl <- 0
  }
  
  if (length(fl) != nd) {
    controlIntpol <- function(i) { #define a new function
      library(raster)
      interpolateDay(i,gaData,iDir,ye,re)
    }
    sfExport("ye"); sfExport("iDir"); sfExport("gaData")
    system.time(sfSapply(as.vector(1:nd), controlIntpol))
  }
  count <- count+1
}

#stop the cluster calculation
sfStop()


## testing
# for (i in 1:nd) {
#   cat(i," ")
#   interpolateDay(i,gaData,iDir,ye,re)
# }
