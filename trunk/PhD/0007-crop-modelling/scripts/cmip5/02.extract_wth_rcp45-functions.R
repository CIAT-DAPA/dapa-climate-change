#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#sourcing needed functions
source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#input directories
#base_dir <- "V:/eejarv"
base_dir <- "/nfs/a102/eejarv"
gcm_dir <- paste(base_dir,"/CMIP5/rcp45",sep="")

#other details
yi <- 2020
yf <- 2049

#load GCM characteristics
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
gcm_list <- unique(gcm_chars$GCM)
gcm_list <- gcm_list[-7] #temporary because ichec_ec_earth does not have rsds


#load gridcells


#load the mask of india


#load the GCM data


#resample GCM data


#put it into format, one file per gridcell "cell-153.csv"
#YEAR,1,2,3,4,5,6,7


#beware of
#    additional "2" in GFDL files (012 instead of 01 in month, and *.nc2 instead of *.nc)
#    some rsds data are monthly
#    dont modify calendar type, but convert units

#write files into folder






