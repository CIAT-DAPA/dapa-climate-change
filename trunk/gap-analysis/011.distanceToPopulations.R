require(rgdal)
require(raster)

source("000.zipWrite.R")

setOptions(overwrite=T)

bdir <- "F:/gap_analysis_publications/gap_phaseolus"
spID <- "Phaseolus_acutifolius"

populationDistance <- function(bdir, spID) {
	idir <- paste(bdir, "/modeling_data", sep="")
	odir <- paste(bdir, "/samples_calculations", sep="")
	spOutFolder <- paste(odir, "/", spID, sep="")

	occ <- read.csv(paste(idir, "/occurrence_files/", spID, ".csv", sep=""))
	xy <- occ[,2:3]

	msk <- raster(paste(idir, "/masks/mask.asc", sep=""))

	dgrid <- distanceFromPoints(msk, xy)
	dumm <- zipWrite(dgrid, spOutFolder, "pop-dist.asc.gz")
}

summarizeDistances <- function(bdir) {
	spList <- list.files(paste(idir, "/modeling_data/occurrence_files", sep=""))
	sppC <- 1
	
	for (spp in spList) {
		spp <- unlist(strsplit(spp, ".", fixed=T))[1]
		
		cat("Processing taxon", spp, "\n")
		
		
		
	}
}