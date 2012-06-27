#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"

#sourcing functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir2,"/scripts/09.00.glam-wrappers.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

#input directories and model
cropName <- "gnut"
runs_set <- "bio_runs"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
runs_odir <- paste(glam_dir,"/model-runs/",runs_set,sep="")


#list of perturbations
procList <- list.files(runs_odir,pattern="p-")


#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#get the mask needed (to which data will be appended)
ncFile <- paste(bDir,"/../climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
ydDir <- paste(bDir,"/climate-signals-yield/GNUT/raster/gridded",sep="")

metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)
msk[] <- NA

#method of yield detrending
method <- "lin"

#load irrigation rates
irDir <- paste(cDir,"/irrigated_ratio",sep="")
ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))


###select process
#this_proc <- 1

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>10) {ncpus <- 10}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("cmipDir")
sfExport("cropName")
sfExport("runs_set")
sfExport("cDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("runs_odir")
sfExport("procList")
sfExport("cells")
sfExport("msk")
sfExport("method")
sfExport("irDir")
sfExport("ir_stk")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_summarise_biol))

#stop the cluster
sfStop()


