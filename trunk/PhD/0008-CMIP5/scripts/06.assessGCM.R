#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

library(raster); library(rgdal); library(maptools)

#6. Calculation of metrics as stated in methods

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#input directories and configurations
inputDD <- paste(mdDir,"/assessment/input-data",sep="")
outputDD <- paste(mdDir,"/assessment/output-data",sep="")

#climatology data
clWCL <- paste(inputDD,"/wcl-data",sep="")
clCRU <- paste(inputDD,"/cru-data",sep="")
clE40 <- e40Dir
clWST <- paste(inputDD,"/wcl-weather-stations",sep="")
clGCM <- paste(mdDir,"/baseline",sep="")

#time series data
tsWST <- paste(inputDD,"/all-weather-stations",sep="")
tsE40 <- e40Dir
tsCRU <- 



#a. mean climates: for each area using the values of GCM gridcells and the mean
#                  values of the datasets calculate the following
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse





#b. interannual variability: for each gridcell using the monthly series of GCMs,
#                            matched with the data (scaled) from each source, calculate
#                            the following:
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse

