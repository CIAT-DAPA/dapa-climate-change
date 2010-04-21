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
		outAVOID <- paste(inFolder, "//", GCM sep="")
		
		cat("Model", GCM, "\n")
		
		inGCMFolder <- paste(inFolder, "//", GCM, sep="")
		
		periodList <- list.files(inFolder)
		
		for (period in periodList) {
			
			outFolder <- paste(outAVOID, "//", period, sep="")
			
			for (m in 1:12) {
			  
			  cat("Month", m, "\n")
			  
				for (procVar in varList) {
					
					
					
				}
				
				if (!file.exists(paste(outFolder, "//tmean_", m, ".asc", sep=""))) {
					precCopy <- file.copy(paste(inGCMFolder, "//pre_", m, ".asc", sep=""), paste(outFolder, "//prec_", m, ".asc", sep=""))
					
					dtrRaster <- raster(paste(inGCMFolder, "//dtr_", m, ".asc", sep=""))
					tmpRaster <- raster(paste(inGCMFolder, "//tmp_", m, ".asc", sep=""))
					 
					tminRaster <- (tmpRaster - (dtrRaster / 2)) * 10
					tminRaster <- writeRaster(tminRaster, paste(outFolder, "//tmin_", m, ".asc", sep=""))
					 
					tmaxRaster <- (tmpRaster + (dtrRaster / 2)) * 10
					tmaxRaster <- writeRaster(tmaxRaster, paste(outFolder, "//tmax_", m, ".asc", sep=""))
					  
					tmpRaster <- tmpRaster * 10
					tmpRaster <- writeRaster(tmpRaster, paste(outFolder, "//tmean_", m, ".asc", sep=""))
					  
					rm(dtrRaster)
					rm(tmpRaster)
					rm(tminRaster)
					rm(tmaxRaster)
				}
			}
		}
		
		bioclim <- BioCalc(outFolder, ext=".asc", format='ascii')
		
		rm(bioclim)
		
	}
	
	return("Done!")
}