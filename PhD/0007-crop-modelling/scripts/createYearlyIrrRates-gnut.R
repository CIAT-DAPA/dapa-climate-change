#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#sourcing important functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep=""))

src.dir2<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/detrender-functions.R",sep=""))

#Function to put the district data of a year into a raster and return it back
#preliminary step before the upscaling to gricells
bDir <- "F:/PhD-work/crop-modelling/GLAM"
#bDir <- "~/PhD-work/crop-modelling/GLAM"
irrDir <- paste(bDir,"/model-runs/GNUT/irrigated_area",sep="")
outYearDir <- paste(irrDir,"/raster/yearly",sep="")
if (!file.exists(outYearDir)) {dir.create(outYearDir)}

##############################################################################
######## HARVESTED AREAS
##############################################################################

pfx <- "IA"
dataType <- "raw"
inyData <- read.table(paste(irrDir,"/data/IND2-gnut-irrigated_areas.tab",sep=""),sep="\t",header=T)
outDataDir <- outYearDir

#raster with district IDs
rk <- raster(paste(bDir,"/climate-signals-yield/GNUT/0_base_grids/india-1min-disid.tif",sep=""))
rk <- readAll(rk)

####!!!!
#or parallelise years
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions
sfExport("createYearRaster")

#export variables
sfExport("inyData")
sfExport("outYearDir")
sfExport("outDataDir")
sfExport("rk")
sfExport("pfx")
sfExport("dataType")

#important fields
iyr <- 66; fyr <- 94
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}

#run the control function
system.time(sfSapply(as.vector(tser), controlYear))

#stop the cluster
sfStop()



##############################################################################
######## HARVESTED AREAS
##############################################################################

pfx <- "IA"
dataType <- "raw"
inyData <- read.table(paste(irrDir,"/data/IND2-gnut-irrigated_areas.tab",sep=""),sep="\t",header=T)
outDataDir <- outYearDir

#raster with district IDs
rk <- raster(paste(bDir,"/climate-signals-yield/GNUT/0_base_grids/india-1min-disid.tif",sep=""))
rk <- readAll(rk)

