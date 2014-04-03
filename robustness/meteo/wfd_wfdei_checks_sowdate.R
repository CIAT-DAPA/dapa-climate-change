#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014

#############################################################################################
####### See wfd_wfdei_checks.R. I realised the annual precip totals were ok, so calculated
####### totals of precip for the Sacks et al. (2010) defined growing season.
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
dataset <- "WFD" #WFD, WFDEI
years <- 1982:2001 #1982:2001 for WFD, 1982:2005 for WFDEI

#/o directory for figs
fig_dir <- paste(wd,"/text/wfd_wfdei_checks/",tolower(dataset),sep="")




