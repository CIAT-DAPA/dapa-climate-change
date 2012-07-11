#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#functions to summarise a given GLAM run based on a given inner folder
#and a given variable to be summarised

#irrigation fraction needs to be input

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source functions
source(paste(src.dir,"/scripts/ecoglam/eg-get_run_data-functions.R",sep=""))
source(paste(src.dir,"/scripts/signals/climateSignals-functions.R",sep=""))

#libraries
library(raster)


#details of crop and base folder of runs and data
setup <- list()
setup$B_DIR <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#setup$BDIR <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
setup$CROP_NAME <- "gnut"
setup$CROP_LONG <- "groundnut"
setup$POT <- F #switch to get either maximum yield (YGP=1), or actual yield (YGP-limited)
setup$CROP_DIR <- paste(setup$B_DIR,"/model-runs/",toupper(setup$CROP_NAME),sep="")
setup$EXPID <- 10
if (setup$EXPID < 10) {setup$EXPID <- paste("0",setup$EXPID,sep="")} else {setup$EXPID <- paste(setup$EXPID)}
setup$SELECTION <- "v4"
setup$CAL_DIR <- paste(setup$CROP_DIR,"/calib",sep="")
setup$EXP_DIR <- paste(setup$CAL_DIR,"/exp-",setup$EXPID,"_outputs",sep="")
setup$OUT_DIR <- paste(setup$CROP_DIR,"/ecg_analyses",sep="")
if (!file.exists(setup$OUT_DIR)) {dir.create(setup$OUT_DIR)}
setup$OUT_GDIR <- paste(setup$OUT_DIR,"/glam_output",sep="")
if (!file.exists(setup$OUT_GDIR)) {dir.create(setup$OUT_GDIR)}

#additional data
vnames <- read.table(paste(src.dir,"/data/GLAM-varnames.tab",sep=""),header=T,sep="\t") #variable names
ref_grid <- read.csv(paste(setup$CROP_DIR,"/inputs/calib-cells-selection-",setup$SELECTION,".csv",sep="")) #gridcells

#spatial grid
ncFile <- paste(setup$B_DIR,"/../climate-data/IND-TropMet_day/0_input_data/india_data.nc",sep="")
ydDir <- paste(setup$B_DIR,"/climate-signals-yield/GNUT/raster/gridded",sep="")
metFile <- raster(ncFile,band=0); yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
mask <- maskCreate(metFile,yldFile); mask[] <- NA

#irrigation rates
ifrc_dir <- paste(setup$CROP_DIR,"/irrigated_ratio",sep="")

###
#non-potential yields
gy_data <- get_grid_data(run_setup=setup,cells=ref_grid,irr_dir=ifrc_dir,varnames=vnames,vid=8) #get yield data
odir <- write_all_data(gdata=gy_data,cells=ref_grid,run_setup=setup,varnames=vnames,msk=mask,vid=8) #write yield data

#potential (ygp=1) yields
setup$POT <- T
gy_data <- get_grid_data(run_setup=setup,cells=ref_grid,irr_dir=ifrc_dir,varnames=vnames,vid=8) #get yield data
odir <- write_all_data(gdata=gy_data,cells=ref_grid,run_setup=setup,varnames=vnames,msk=mask,vid=8) #write yield data








