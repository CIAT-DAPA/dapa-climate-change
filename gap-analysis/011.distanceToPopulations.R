require(rgdal)
require(raster)

bdir <- "F:/gap_analysis_publications/gap_phaseolus"
idir <- paste(bdir, "/modeling_data", sep="")

spID <- "Phaseolus_acutifolius"

occ <- read.csv(paste(idir, "/occurrence_files/", spID, ".csv", sep=""))
xy <- occ[,2:3]

msk <- paste(idir, "/masks/mask.asc", sep="")

dgrid <- distanceFromPoints(msk, xy)
