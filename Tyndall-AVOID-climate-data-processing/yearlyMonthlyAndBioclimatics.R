#Script to calculate average climates over a set of years by loading the data on different years

require(raster)
require(rgdal)
require(BioCalc)

cat("Available avoid scenarios are: \n")
cat("A1B_A16r2h \n")
cat("A1B_A16r4l \n")
cat("A1B_A16r5l \n")
cat("A1B_A30r2h \n")
cat("A1B_A30r5l \n")

cat("\n")
cat("yearlyCalc(inputAVOIDScenDIR, AVOIDScenario) \n")
cat("\n")

yearlyCalc <- function(inputAVOIDScen, scen) {
	
	inFolder <- paste(inputAVOIDScen, "//", scen, "//yearly_asciis", sep="")
	cat("Processing scenario", scen, "\n")
	
	gcmList <- list.files(paste(inFolder, sep=""), pattern="_")
	
	varList <- c("dtr", "pre", "tmp")
	
	for (GCM in gcmList) {
		outAVOID <- paste(inFolder, "//", GCM, sep="")
		
		cat("\n")
		cat("Model", GCM, "\n")
		
		inGCMFolder <- paste(inFolder, "//", GCM, sep="")
		
		periodList <- list.files(inGCMFolder)
		
		for (period in periodList) {
			
			cat("\n")
			cat("Year", period, "\n")
			
			outFolder <- paste(outAVOID, "//", period, sep="")
			
			for (m in 1:12) {
			  
			  cat("\n")
			  cat("Month", m, "\n")
			  
				for (procVar in varList) {
					
					cat("Variable", procVar, "\n")
					
					if (procVar == "pre") {
						oVar <- "prec"
					} else if (procVar == "tmp") {
						oVar <- "tmean"
					} else {
						oVar <- "trnge"
					}
					
					oFile <- paste(inGCMFolder, "//", period, "//", oVar, "_", m, ".asc", sep="")
					
					if (!file.exists(paste(oFile))) {
						
						cat("...loadData \n")
						
						fileName <- paste(inGCMFolder, "//", period, "//", procVar, "_all.asc", sep="")
						inData <- read.csv(fileName)
						names(inData) <- c("IDP", "CRUcell", "x", "y", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
						
						spp <- SpatialPoints(inData[,3:4])
						spx <- SpatialPixels(spp)
						rs <- raster(spx)
						
						rm(spp)
						rm(spx)
						
						cat("...xyFromCell \n")
						
						xyMatrix <- xyFromCell(rs, 1:ncell(rs))
						xyMatrix <- cbind(cell=(1:ncell(rs)), xyMatrix)
						
						cat("...Merge the data \n")
						
						outMerge <- merge(xyMatrix, inData, all=T)
						valOrd <- order(outMerge$cell, decreasing=F)
						outMergeOrd <- outMerge[valOrd,]
						
						rm(outMerge)
						rm(valOrd)
						rm(xyMatrix)
						rm(inData)
						
						rs[] <- outMergeOrd[,(m+6)]
						
						if (procVar == "tmp") {
							rs <- rs*10
						} else if (procVar == "dtr") {
							rs <- rs*10
						}
						
						rs <- writeRaster(rs, oFile, format='ascii', overwrite=T)
						
						rm(rs)
					}
					
				}
				
				trng <- raster(paste(inGCMFolder, "//", period, "//", "trnge_", m, ".asc", sep=""))
				tmen <- raster(paste(inGCMFolder, "//", period, "//", "tmean_", m, ".asc", sep=""))
				
				tmax <- (tmen + (trng / 2))
				tmin <- (tmen - (trng / 2))
				
				rm(trng)
				rm(tmen)
				
				tmax <- writeRaster(tmax, paste(inGCMFolder, "//", period, "//", "tmax_", m, ".asc", sep=""), format='ascii', overwrite=T)
				tmin <- writeRaster(tmin, paste(inGCMFolder, "//", period, "//", "tmin_", m, ".asc", sep=""), format='ascii', overwrite=T)
				
				rm(tmax)
				rm(tmin)
				
			}
			bioclim <- BioCalc(outFolder, ext=".asc", format='ascii')
		}
		
		rm(bioclim)
		
	}
	
	return("Done!")
}