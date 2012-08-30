#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012

#Create daily weather data in the format of WTH files for CMIP5 baseline runs
#this script will first check if the GCM data are valid for the years that will be
#simulated. Similarly, this

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"

#source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/cmip5/01.make_wth-functions.R",sep=""))

#base directory
#base_dir <- "W:/eejarv/PhD-work/crop-modelling"
base_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"

#other details
crop_name <- "groundnut"
crop_short <- "gnut"
ver <- "v6"
max_cpus <- 10

#input directories
glam_dir <- paste(base_dir,"/GLAM",sep="")
clim_dir <- paste(base_dir,"/climate-data",sep="")
crop_dir <- paste(glam_dir,"/model-runs/",toupper(crop_short),sep="")
input_dir <- paste(crop_dir,"/inputs",sep="")
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")

#output directories
wth_dir <- paste(asc_dir,"/wth-cmip5",sep="")
if (!file.exists(wth_dir)) {dir.create(wth_dir)}

#data that is needed by the process
cells <- read.csv(paste(input_dir,"/calib-cells-selection-",ver,".csv",sep=""))
rabi_sow <- raster(paste(crop_dir,"/",tolower(crop_short),"-zones/plant_rabi.asc",sep=""))

#list of gcms
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcm_list <- unique(paste(gcm_chars$GCM,"_ENS_",gcm_chars$Ensemble,sep=""))
proc_list <- data.frame(PID=1:length(gcm_list),GCM=gcm_list)

#list processes
done_list <- unlist(lapply(1:nrow(proc_list),check_progress,wth_dir))
done_list <- done_list[which(!is.na(done_list))]
if (length(done_list)==0) {
  proc_list <- data.frame(GCM=NA)
} else {
  proc_list <- proc_list[done_list,]
}

#determine number of cpus
if (nrow(proc_list) > max_cpus) {ncpus <- max_cpus} else {ncpus <- nrow(proc_list)}

#testing runs!
#process a given GCM #23 for monthly #test 43 for missing data
#wth_cmip5_wrapper(1)

#then run in parallel
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("base_dir")
sfExport("crop_name")
sfExport("crop_short")
sfExport("ver")
sfExport("max_cpus")
sfExport("glam_dir")
sfExport("clim_dir")
sfExport("crop_dir")
sfExport("input_dir")
sfExport("asc_dir")
sfExport("sow_dir")
sfExport("wth_dir")
sfExport("cells")
sfExport("rabi_sow")
sfExport("gcm_chars")
sfExport("gcm_list")
sfExport("proc_list")
sfExport("gcm_chars")

#run the function in parallel
system.time(sfSapply(as.vector(proc_list$PID),wth_cmip5_wrapper))

#stop the cluster
sfStop()







