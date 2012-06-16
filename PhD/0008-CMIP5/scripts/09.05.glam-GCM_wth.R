#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#get a given domain (from the cells-process.csv), and make a *grid.txt file
#for reference. This file will not be used by GLAM, but it will be needed if you want
#to map the results

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"

source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#cmDir <- "W:/eejarv/PhD-work/crop-modelling"
#cmipDir <- "V:/eejarv/CMIP5"
cmDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
cmipDir <- "/nfs/a102/eejarv/CMIP5"

bDir <- paste(cmDir,"/GLAM",sep="")
cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")

#these are the cells that have both yield and rainfall data
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#get longitude and latitude (row and column)
rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/0_base_grids/igp_dummy.tif",sep=""))
cells$COL <- colFromX(rs,cells$X)
cells$ROW <- rowFromY(rs,cells$Y)

#output folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth_fut",sep="")

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
procList <- data.frame(GCM=gcmList)

#wrapper_create_wth_cmip5(1)

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("cmDir")
sfExport("bDir")
sfExport("cropName")
sfExport("cropDir")
sfExport("cmipDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("cells")
sfExport("asc_dir")
sfExport("sow_dir")
sfExport("wth_dir")
sfExport("gcmChars")
sfExport("procList")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_create_wth_cmip5))

#stop the cluster
sfStop()

