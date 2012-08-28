#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools); library(MASS)
data(wrld_simpl)
stop("not to run yet")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop" #local
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop" #eljefe
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

source(paste(src.dir3,"/scripts/ecoglam/eg-ecocrop_gnut-functions.R",sep=""))
source(paste(src.dir,"/src/getUniqueCoord.R",sep=""))
source(paste(src.dir,"/src/randomSplit.R",sep=""))
source(paste(src.dir,"/src/extractClimateData.R",sep=""))
source(paste(src.dir,"/src/calibrationParameters.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/watbal.R",sep=""))
source(paste(src.dir,"/src/getParameters.R",sep=""))
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/validation.R",sep=""))
source(paste(src.dir,"/src/createMask.R",sep=""))
source(paste(src.dir,"/src/accuracy.R",sep=""))


#basic information
crop_name <- "gnut"
vnames <- read.table(paste(src.dir3,"/data/GLAM-varnames.tab",sep=""),sep="\t",header=T)
r_dir <- "W:/eejarv/PhD-work/crop-modelling"
#r_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
b_dir <- paste(r_dir,"/GLAM",sep="")
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
ec_dir <- paste(crop_dir,"/ecg_analyses/ecocrop-",tolower(crop_name),sep="")
glam_dir <- paste(crop_dir,"/ecg_analyses/glam_output",sep="")

###read the experiments that will be used
parset_list <- read.csv(paste(crop_dir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expid_list <- parset_list$EXPID[which(parset_list$ISSEL==1)]


#####################################################################
#####################################################################
#####################################################################
#3. coupling of both models
for (j in 1:length(expid_list)) {
  out_dir <- wrapper_eg_combine(j)
}







