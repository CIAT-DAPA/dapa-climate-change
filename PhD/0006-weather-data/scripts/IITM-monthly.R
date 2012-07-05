#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

wd <- "W:/eejarv/PhD-work/crop-modelling/climate-data/IND-TropMet"
setwd(wd)

library(raster)

iniyr <- 1960
finyr <- 2008

