#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014

#############################################################################################
####### See wfd_wfdei_checks.R 
#############################################################################################

#packages
library(raster); library(rgdal); library(ncdf); library(rasterVis); library(maptools)
data(wrld_simpl)

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
yi_dir <- paste(wd,"/data/yield_data_maize",sep="")

#load objects
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

#define dataset, period, variable
dataset <- "WFDEI" #WFD, WFDEI
years <- 1982:2005 #1982:2001 for WFD, 1982:2005 for WFDEI

#i/o directories for cru
cru_idir <- "~/Leeds-work/datasets/meteorology/cru-ts-v3-21"
cru_odir <- paste(met_dir,"/cru-ts-v3-21",sep="")
if (!file.exists(cru_odir)) {dir.create(cru_odir)}

#i/o directories for worldclim
wcl_idir <- "~/Leeds-work/datasets/meteorology/worldclim_global_5min"
wcl_odir <- paste(met_dir,"/worldclim",sep="")
if (!file.exists(wcl_odir)) {dir.create(wcl_odir)}

#/o directory for figs
fig_dir <- paste(wd,"/text/wfd_wfdei_checks/",tolower(dataset),sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir,recursive=T)}

#determine extent to cut the resampled netcdfs (of CRU and WorldClim)
yrs <- raster(paste(yi_dir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
bbox <- extent(yrs)
