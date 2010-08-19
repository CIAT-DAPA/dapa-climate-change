require(rgdal)
require(raster)

source("000.zipWrite.R")

setOptions(overwrite=T)

#bdir <- "F:/gap_analysis_publications/gap_phaseolus"
#spID <- "Phaseolus_acutifolius"

populationDistance <- function(bdir, spID) {
	idir <- paste(bdir, "/modeling_data", sep="")
	odir <- paste(bdir, "/samples_calculations", sep="")
	spOutFolder <- paste(odir, "/", spID, sep="")
	
	cat("Loading occurrences \n")
	occ <- read.csv(paste(idir, "/occurrence_files/", spID, ".csv", sep=""))
	xy <- occ[,2:3]
	
	cat("Loading mask \n")
	msk <- raster(paste(idir, "/masks/mask.asc", sep=""))
	
	cat("Distance from points \n")
	dgrid <- distanceFromPoints(msk, xy)
	dgrid[which(is.na(msk[]))] <- NA
	
	cat("Writing output \n")
	dumm <- zipWrite(dgrid, spOutFolder, "pop-dist.asc.gz")
	return(dgrid)
}

summarizeDistances <- function(bdir) {
	spList <- list.files(paste(bdir, "/modeling_data/occurrence_files", sep=""))
	sppC <- 1
	
	for (spp in spList) {
		spp <- unlist(strsplit(spp, ".", fixed=T))[1]
		
		cat("Processing taxon", spp, "\n")
		
		dg <- populationDistance(bdir, spp)
		
	}
	return(spList)
}