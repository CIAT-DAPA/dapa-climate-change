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

base_dir <- "W:/eejarv/PhD-work/crop-modelling"
base_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"

ver <- "v6"

glam_dir <- paste(base_dir,"/GLAM",sep="")
clim_dir <- paste(base_dir,"/climate-data",sep="")
crop_name <- "groundnut"
crop_short <- "gnut"
crop_dir <- paste(glam_dir,"/model-runs/",toupper(crop_short),sep="")
input_dir <- paste(crop_dir,"/inputs",sep="")

#these are the cells that have both yield and rainfall data
cells <- read.csv(paste(input_dir,"/calib-cells-selection-",ver,".csv",sep=""))

#get longitude and latitude (row and column)
rs <- raster(paste(glam_dir,"/climate-signals-yield/",toupper(crop_short),"/0_base_grids/igp_dummy.tif",sep=""))
cells$COL <- colFromX(rs,cells$X)
cells$ROW <- rowFromY(rs,cells$Y)

#output folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth-cmip5",sep="")
rabi_sow <- raster(paste(crop_dir,"/",tolower(crop_short),"-zones/plant_rabi.asc",sep=""))

if (!file.exists(wth_dir)) {dir.create(wth_dir)}

#list of gcms
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcm_list <- unique(paste(gcm_chars$GCM,"_ENS_",gcm_chars$Ensemble,sep=""))
proc_list <- data.frame(GCM=gcm_list)

#process a given GCM
this_proc <- 1 #23 for monthly #test 43 for missing data

#then run!






