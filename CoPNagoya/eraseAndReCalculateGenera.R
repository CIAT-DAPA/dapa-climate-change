require(rgdal)
require(raster)

source("zipRead.R")
source("speciesRichness.R")

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXX RECALCULATION XXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

#Define variables
#idir <- "L:/COP_CONDESAN"
#type <- "aves"
#calctype <- "richness"
#OSys="nt"

recalculation <- function(idir, type, calctype, OSys) {
	#Define directories where data is stored
	summDir <- paste(idir, "/summaries/", tolower(calctype), "-", tolower(type), sep="")
	genListFile <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToEraseAndReCalculate.csv", sep="")

	#Read the file
	genList <- read.csv(genListFile)

	#Deleting and re-calculating the genera folders
	genC <- 1
	for (gen in genList$IDGenus) {
		
		cat("\n")
		cat("...Processing genus", gen, paste("...",round(genC/length(genList$IDGenus)*100,2),"%",sep=""), "\n")
		
		#Name of the genus folder
		genDir <- paste(summDir, "/gn-", gen, sep="")
		
		#Determining if the genus needs to be re-calculated
		isDisc <- genList$IsDiscarded[which(genList$IDGenus == gen)]
		
		if (file.exists(genDir)) {
			cat("Erasing the folder \n")
			system(paste("rm", "-r", genDir))
			if (isDisc == 0) {
				cat("Re-calculating the genus \n")
				ot <- speciesRichness(idir, summDir, gen, type, OSys=OSys)
			} else {
				cat("The genus was finally discarded \n")
			}
		} else {
			cat("The genus folder didnt exist anyway \n")
		}
		genC <- genC+1
	}
}