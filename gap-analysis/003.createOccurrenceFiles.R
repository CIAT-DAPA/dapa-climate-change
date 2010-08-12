#Take the list of species to model and create a new file with occurrences of selected species and climate data

createOccFiles <- function(occ, spL, outDir, i, j) {
	
	cat("\n")
	
	if (!file.exists(outDir)) {
		dir.create(outDir)
	}
	
	cat("Loading occurrence file \n")
	
	occ <- read.csv(occ)
	
	spL <- unique(occ$taxon)
	
	if (j > length(spL)) {
		cat("j is greater than number of taxa, using nrow instead \n")
		j <- length(spL)
	}
	spL <- spL[i:j]
	
	spcounter <- 1
	
	cat("Now printing \n")
	
	for (sp in spL) {
		
		nspp <- j-i+1
		cat("      ...", paste(round(spcounter/nspp*100,2),"% Completed", sep=""), "\n")
		
		spData <- occ[which(occ$taxon == sp),]
		
		csvName <- paste(outDir, "/", sp, ".csv", sep="")
		write.csv(spData, csvName, row.names=F, quote=F)
		rm(spData)
		
		spcounter <- spcounter+1
	}
	return("Done")
}
