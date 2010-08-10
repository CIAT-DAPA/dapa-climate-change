require(raster)
require(maptools)
gpclibPermit()

source("zipWrite.R")

createNARaster <- function(spID, inDir) {
	cat("\n")
	cat("Taxon", spID,"\n")
	
	inNADir <- paste(inDir, "/native-areas/polyshps", sep="")
	outNADir <- paste(inDir, "/native-areas/asciigrids", sep="")
	
	if (!file.exists(outNADir)) {
		dir.create(outNADir)
	}
	
	outFolder <- paste(outNADir, "/", spID, sep="")
	if (!file.exists(outFolder)) {
		dir.create(outFolder)
	}
	
	shpName <- paste(inNADir, "/", spID, "/narea.shp", sep="")
	
	#Reading polygon shapefile and mask
	
	cat("Reading and converting \n")
	pol <- readShapePoly(shpName)
	rs <- raster(paste(inDir, "/masks/mask.asc", sep=""))
	
	pa <- polygonsToRaster(pol, rs)

	pa[which(!is.na(pa[]))] <- 1
	
	cat("Writing output \n")
	pa <- zipWrite(pa, outFolder, "narea.asc.gz")
	
	return(pa)
}

#Loop the species
inDir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
spID <- "Phaseolus_acutifolius"
spList <- list.files(paste(inDir, "/native-areas/polyshps", sep=""))

for (spp in spList) {
	ot <- createNARaster(spp, inDir)
}
