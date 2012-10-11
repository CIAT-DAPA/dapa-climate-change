require(rgdal)
require(raster)
require(maptools)
gpclibPermit()

source("000.zipWrite.R")
source("000.zipRead.R")

createNARaster <- function(spID, inDir) {
	cat("\n")
	cat("Taxon", spID,"\n")
	
	inNADir <- paste(inDir, "/native-areas/polyshps", sep="")
	outNADir <- paste(inDir, "/native-areas/asciigrids", sep="")
	outFolder <- paste(outNADir, "/", spID, sep="")
	
	if (!file.exists(paste(outFolder, "/narea.asc.gz", sep=""))) {
		
		cat("Not processed, thus processing \n")
		
		if (!file.exists(outNADir)) {
			dir.create(outNADir)
		}
		
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
		pa[which(is.na(pa[]) & rs[] == 1)] <- 0
		
		cat("Writing output \n")
		pa <- zipWrite(pa, outFolder, "narea.asc.gz")
		return(pa)
	} else {
		cat("Already processed \n")
		#pa <- zipRead(outFolder, "narea.asc.gz")
	}
}

#Loop the species
inDir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
spID <- "Phaseolus_acutifolius"
spList <- list.files(paste(inDir, "/native-areas/polyshps", sep=""))

for (spp in spList) {
	ot <- createNARaster(spp, inDir)
}
