require(rgdal)
require(sp)
require(raster)

source("zipRead.R")

#This script verifies the consistence of genus files (2010_2039_TenPercentile_NullAdap & 2040_2069_TenPercentile_FullAdap) 
#to detect the problem with final calculation of species richness

#idir <- "L:/COP_CONDESAN" #"/mnt/GeoData/COP_CONDESAN"
#type <- "aves"
#calctype <- "richness"

richVerification <- function(idir, type, calctype) {
	
	#Stuff where the error was detected
	sres <- "A2"
	thr <- "TenPercentile"
	mig <- "NullAdap"
	periods <- c("2010_2039", "2040_2069")


	verDir <- paste(idir, "/occurrences/modeling-data/", type, "-verification", sep="")
	summDir <- paste(idir, "/summaries/", calctype, "-", type, sep="")
	genList <- list.files(summDir, pattern="gn-")

	otFile <- paste(idir, "/summaries/errors-", calctype, "_", type,".log", sep="")
	zz <- file(otFile, "w")

	#Loop through the genera
	for (gen in genList) {
		
		genID <- substring(gen, 4, 6)
		genDir <- paste(summDir, "/", gen, sep="")
		
		cat("\n")
		cat("Verifying genus", genID, "\n")
		
		#Loop through the periods
		for (per in periods) {
			
			cat("**", per, "...", sep="")
			
			sampleFile <- read.csv(paste(verDir, "/", per, ".csv", sep=""))
			
			#Load the raster
			rsName <- paste("richness_future_SRES_", sres, "_disaggregated_", per, "_", thr, "_", mig, ".asc.gz", sep="")
			rs <- zipRead(genDir, rsName)
			
			vals <- xyValues(rs, sampleFile)
			lth <- length(which(is.na(vals)))
			
			if (lth != 0) {
				cat("**Error found, and being reported! \n")
				cat(gen, per, "\n", file=zz)
			}
			
		}
		cat("\n")
	}

	close(zz)
}
