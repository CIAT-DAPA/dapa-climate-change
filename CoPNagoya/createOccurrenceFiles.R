#Take the list of species to model and create a new file with Andean occurrences of selected species and climate data

#fields $AndesMountain, $AndesADM0
#If Nocc < 40, P1,P4,P12,P15,Io,Iod2
#Else P1,P4,P5,P6,P12,P15,P16,P17,Io,Iod2

require(rgdal)
require(sp)
require(raster)

createOccFiles <- function(occ, spL, clDir, i, j) {
	
	#occ <- "./modeling-data/andean-species-data-sampleArea.csv"
	#spL <- "./modeling-data/speciesListToModel.csv"
	#clDir <-  "C:/CIAT_work/COP_CONDESAN/climateData/andesADM0/baseline/20C3M/WorldClim-30s-bioclim/1950_2000"

	outDir <- "splitted-occurrence-files"
	if (!file.exists(outDir)) {
		dir.create(outDir)
	}

	spL <- read.csv(spL)[i:j,]
	occ <- read.csv(occ)

	for (sp in spL$IDSpecies) {
		spData <- occ[which(occ$IDSpecies == sp),]
		spData <- spData[which(spData$AndesADM0 == 1),]
		coords <- cbind(spData$Lon,spData$Lat)
		
		rsL <- c("bio_1","bio_4","bio_5","bio_6","bio_12","bio_15","bio_16","bio_17","io","iod2")
		
		spDataOut <- cbind(spData$IDSpecies, spData$Lon, spData$Lat)
		
		for (rst in rsL) {
			rs <- paste(clDir, "/", rst, ".asc", sep="")
			rs <- raster(rs)
			
			vls <- xyValues(rs, coords)
			spDataOut <- cbind(spDataOut, vls)
		}
		
		spDataOut <- as.data.frame(spDataOut)
		names(spDataOut) <- c("taxon","lon","lat",rsL)
		csvName <- paste(outDir, "/", sp, ".csv", sep="")
		write.csv(spDataOut, csvName, row.names=F, quote=F)
		rm(spDataOut)
		
	}
	return("Done")
}