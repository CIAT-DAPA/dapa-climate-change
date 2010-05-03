#Script to calculate average climates over a set of years by loading the data on different years

require(raster)
require(rgdal)

#cat("Available avoid scenarios are: \n")
#cat("A1B_A16r2h \n")
#cat("A1B_A16r4l \n")
##cat("A1B_A30r2h \n")
#cat("A1B_A30r5l \n")

cat("\n")
cat("describeAVOID(inputAVOIDScenDIR) \n")
cat("\n")

describeAVOID <- function(inputAVOIDScen) {
	
	AVOIDList <- list.files(inputAVOIDScen)
	
	saveCounter <- 1
	
	for (scen in AVOIDList) {
		
		outMx <- matrix(ncol=8, nrow=(7*7*67))
		outMx <- as.data.frame(outMx)
		names(outMx) <- c("MODEL", "PERIOD", "RASTER", "NCELL", "MEAN", "STDV", "MAX", "MIN")
		
		inFolder <- paste(inputAVOIDScen, "//", scen, sep="")
		cat("Processing scenario", scen, "\n")
		
		outMxFileName <- paste(inFolder, "//", scen, "_Rasterdescription.csv", sep="")
		
		periodList <- list.files(inFolder)
	   
		for (period in periodList) {
		  
		  gcmList <- list.files(paste(inFolder, "//", period, sep=""), pattern="_")
		  
		  for (GCM in gcmList) {
			
			cat("Model", GCM, "\n")
			cat("Period", period, "\n")
			cat("Scenario", scen, "\n")
			
			inGCMFolder <- paste(inFolder, "//", period, "//", GCM, sep="")
			
			for (m in 1:12) {
			  
			  cat("Month", m, "\n")
			  
			  varName <- c("tmax", "tmin", "tmean", "prec")
			  
			  for (variable in varName) {
				  gridName <- paste(inGCMFolder, "//", variable, "_", m, ".asc", sep="")
				  
				  rs <- raster(gridName)
				  rsData <- rs[1:ncell(rs)]
				  rsData <- rsData[which(!is.na(rsData[]))]
				  
				  outMx[saveCounter, 1] <- GCM
				  outMx[saveCounter, 2] <- period
				  outMx[saveCounter, 3] <- paste(variable, "_", m, sep="")
				  outMx[saveCounter, 4] <- ncell(rs)
				  outMx[saveCounter, 5] <- mean(rsData)
				  outMx[saveCounter, 6] <- sd(rsData)
				  outMx[saveCounter, 7] <- max(rsData)
				  outMx[saveCounter, 8] <- min(rsData)
				  
				  rm(rs)
				  
				  saveCounter <- saveCounter + 1
				}
			  
			}
			
			for (b in 1:19) {
				
				cat("Bio", b, "\n")
				
				gridName <- paste(inGCMFolder, "//", "bio_", b, ".asc", sep="")
				
				rs <- raster(gridName)
				rsData <- rs[1:ncell(rs)]
				rsData <- rsData[which(!is.na(rsData[]))]
				
				outMx[saveCounter, 1] <- GCM
				outMx[saveCounter, 2] <- period
				outMx[saveCounter, 3] <- paste("bio_", b, sep="")
				outMx[saveCounter, 4] <- ncell(rs)
				outMx[saveCounter, 5] <- mean(rsData)
				outMx[saveCounter, 6] <- sd(rsData)
				outMx[saveCounter, 7] <- max(rsData)
				outMx[saveCounter, 8] <- min(rsData)
				
				rm(rs)
				
				saveCounter <- saveCounter + 1
			}
			
		  }
		}
		
		outfile <- write.csv(outMx, outMxFileName, row.names=F, quote=F)
	}
	return("Done!")
}