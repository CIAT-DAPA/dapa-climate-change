require(rgdal)
require(raster)

source("000.zipWrite.R")

setOptions(overwrite=T)

bdir <- "F:/gap_analysis_publications/gap_phaseolus"
idir <- paste(bdir, "/modeling_data", sep="")
odir <- paste(bdir, "/samples_calculations", sep="")

spID <- "Phaseolus_acutifolius"
spOutFolder <- paste(odir, "/" spID, 

occ <- read.csv(paste(idir, "/occurrence_files/", spID, ".csv", sep=""))
xy <- occ[,2:3]

msk <- paste(idir, "/masks/mask.asc", sep="")

dgrid <- distanceFromPoints(msk, xy)
dumm <- zipWrite(dgrid, 