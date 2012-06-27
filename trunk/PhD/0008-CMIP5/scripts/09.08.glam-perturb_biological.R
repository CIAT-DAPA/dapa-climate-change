#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#run GLAM parameters using perturbed crop parameters

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


#check the existence of three parameters needed for sourcing this script
if (class(try(get("src.dir"),silent=T)) == "try-error") {
  stop("src.dir needs to be set")
}

if (class(try(get("bDir"),silent=T)) == "try-error") {
  stop("bDir needs to be set")
}

if (class(try(get("src.dir2"),silent=T)) == "try-error") {
  stop("src.dir2 needs to be set")
}

if (class(try(get("cmipDir"),silent=T)) == "try-error") {
  stop("cmip5 dir needs to be set")
}

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM

#source all needed functions
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))
source(paste(src.dir2,"/scripts/09.00.glam-wrappers.R",sep=""))


#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
pDir <- paste(input_dir,"/params",sep="") #parameter files
unp_rundir <- paste(glam_dir,"/model-runs/unperturbed_calib_ygp_ipdate",sep="")

#data folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth",sep="")

#output directory
runs_odir <- paste(glam_dir,"/model-runs/bio_runs",sep="")
if (!file.exists(runs_odir)) {dir.create(runs_odir)}

#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#load irrigation data
irDir <- paste(cDir,"/irrigated_ratio",sep="")
ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))

####perturbations and hts types
pert_runs <- read.table(paste(src.dir2,"/data/GLAMperturb.tab",sep=""),header=T,sep="\t")
hts_types <- read.table(paste(src.dir2,"/data/HTSTypes.tab",sep=""),header=T,sep="\t")
tds_types <- data.frame(TYPE=c("TDS1","TDS2"),HIMIN=c(0.1,0.25),FSW=c(0.1,0.01))

#this_pt <- 7
#run_perturbed_biol(1)

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=7)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("cmipDir")
sfExport("cropName")
sfExport("cDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("pDir")
sfExport("unp_rundir")
sfExport("runs_odir")
sfExport("asc_dir")
sfExport("sow_dir")
sfExport("wth_dir")
sfExport("cells")
sfExport("irDir")
sfExport("ir_stk")
sfExport("pert_runs")
sfExport("hts_types")
sfExport("tds_types")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(pert_runs)),wrapper_perturbed_biol))

#stop the cluster
sfStop()

