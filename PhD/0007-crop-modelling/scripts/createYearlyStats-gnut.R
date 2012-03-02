#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#sourcing important functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
#src.dir <- "/home/jramirez/dapa-climate-change/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep=""))

src.dir2<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "/home/jramirez/dapa-climate-change/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/detrender-functions.R",sep=""))


#Function to put the district data of a year into a raster and return it back
#preliminary step before the upscaling to gricells
outYearDir <- paste(cd,"/raster/yearly",sep="")
if (!file.exists(outYearDir)) {dir.create(outYearDir)}

dataType <- "fourier"
inyData <- fouData
outDataDir <- paste(outYearDir,"/",dataType,sep="")
if (!file.exists(outDataDir)) {dir.create(outDataDir)}

####!!!!
#or parallelise years
library(snowfall)
sfInit(parallel=T,cpus=4) #initiate cluster

#export functions
sfExport("createYearRaster")

#export variables
sfExport("inyData")
sfExport("outYearDir")
sfExport("outDataDir")
sfExport("rk")
sfExport("dataType")

#run the control function
system.time(sfSapply(as.vector(66:95), controlYear))

#stop the cluster
sfStop()

