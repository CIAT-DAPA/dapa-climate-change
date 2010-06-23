#Take the list of species to model and create a new file with Andean occurrences of selected species and climate data

#fields $AndesMountain, $AndesADM0
#If Nocc < 40, P1,P4,P12,P15,Io,Iod2
#Else P1,P4,P5,P6,P12,P15,P16,P17,Io,Iod2

require(rgdal)
require(sp)
require(raster)

createOccFiles <- function(occ, spL, clDir, outDir, i, j) {
	
	#occ <- "./modeling-data/andean-species-data-sampleArea.csv"
	#spL <- "./modeling-data/speciesListToModel.csv"
	#clDir <-  "C:/CIAT_work/COP_CONDESAN/climateData/andesADM0/baseline/20C3M/WorldClim-30s-bioclim/1950_2000"
	
	cat("\n")
	
	#outDir <- "splitted-occurrence-files"
	if (!file.exists(outDir)) {
		dir.create(outDir)
	}

	spL <- read.csv(spL)
	
	if (j > nrow(spL)) {
		cat("j is greater than number of taxa, using nrow instead \n")
		j <- nrow(spL)
	}
	
	cat("Loading occurrence files \n")
	
	spL <- spL[i:j,]
	occ <- read.csv(occ)
	occ <- occ[which(occ$AndesADM0 == 1),]
	
	spcounter <- 1
	
	occ <- occ[which(occ$IDSpecies %in% spL$IDSpecies),]
	coords <- cbind(occ$Lon,occ$Lat)
	
	spDataOut <- cbind(occ$IDSpecies, occ$Lon, occ$Lat)
	rsL <- c("bio_1","bio_4","bio_5","bio_6","bio_12","bio_15","bio_16","bio_17","io","iod2")
	
	cat("Extracting climate data \n")
	
	for (rst in rsL) {
		cat("      ->Raster", rst, "\n")
		rs <- paste(clDir, "/", rst, ".asc", sep="")
		rs <- raster(rs)
		
		vls <- xyValues(rs, coords)
		spDataOut <- cbind(spDataOut, vls)
	}
	
	spDataOut <- as.data.frame(spDataOut)
	names(spDataOut) <- c("taxon","lon","lat",rsL)
	
	#Cleaning the file
	
	cat("Cleaning the occurrences before printing \n")
	
	bCol <- 4
	for (rst in rsL) {
		cat("      ->Variable", rst, "\n")
		spDataOut <- spDataOut[which(!is.na(spDataOut[,bCol])),]
		bCol <- bCol + 1
	}
	
	cat("Now printing \n")
	
	for (sp in spL$IDSpecies) {
		
		nspp <- j-i+1
		cat("      ...", paste(round(spcounter/nspp*100,2),"% Completed", sep=""), "\n")
		
		spData <- spDataOut[which(spDataOut$taxon == sp),]
		
		csvName <- paste(outDir, "/", sp, ".csv", sep="")
		write.csv(spData, csvName, row.names=F, quote=F)
		rm(spData)
		
		spcounter <- spcounter+1
	}
	return("Done")
}
