#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "F:/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"

#source(paste(src.dir2,"/scripts/09.06.glam-GCM_optimise-ygp.R",sep=""))

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
source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir2,"/scripts/09.00.glam-wrappers.R",sep=""))

#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
pDir <- paste(input_dir,"/params",sep="") #parameter files

#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#output folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth_fut",sep="")

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
procList <- data.frame(GCM=gcmList)

#this_proc <- 1

runs_odir <- paste(glam_dir,"/model-runs/gcm_runs",sep="")
if (!file.exists(runs_odir)) {dir.create(runs_odir)}


############## check which runs are done already
ndList <- c()
for (i in 1:nrow(procList)) {
  gcm <- unlist(strsplit(paste(procList$GCM[i]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[i]),"_ENS_",fixed=T))[2]
  
  procDir <- paste(runs_odir,"/x.proc",sep="")
  procFil <- paste(procDir,"/",gcm,"_",ens,".proc",sep="") #check file
  if (!file.exists(procFil)) {
    ndList <- c(ndList,i)
  }
}
ndList <- unique(ndList)
procList <- procList[ndList,]
procList <- data.frame(GCM=procList)
if (nrow(procList) != 0) {row.names(procList) <- 1:nrow(procList)}



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
sfExport("cDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("pDir")
sfExport("cells")
sfExport("asc_dir")
sfExport("sow_dir")
sfExport("wth_dir")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("procList")
sfExport("runs_odir")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_GCM_glam_optimise_ygp))

#stop the cluster
sfStop()



########################################################################################
########################################################################################
###################### zip all GCM wth data
wth_dir <- zip_unzip_wth_fut(procList,wth_dir,unzip=F)




