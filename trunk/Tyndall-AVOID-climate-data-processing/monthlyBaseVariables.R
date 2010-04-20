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
cat("monthlyCalc(inputAVOIDScenDIR, outptAVOIDScenDIR, AVOIDScenario) \n")
cat("\n")

monthlyCalc <- function(inputAVOIDScen, outptAVOIDScen, scen) {

	if (!file.exists(outptAVOIDScen)) {
		dir.create(outptAVOIDScen)
	}

	#for (scen in AVOIDList) {
	  inFolder <- paste(inputAVOIDScen, "//", scen, sep="")
	  cat("Processing scenario", scen, "\n")
	  
	  periodList <- list.files(inFolder)
	  
	  outAVOID <- paste(outptAVOIDScen, "//", scen, sep="")
	  
	  if (!file.exists(outAVOID)) {
		  dir.create(outAVOID) 
		}
	   
	   for (period in periodList) {
		   
			if (!file.exists(paste(outAVOID, "//", period, sep=""))) {
				dir.create(paste(outAVOID, "//", period, sep=""))
			}
		  
		  gcmList <- list.files(paste(inFolder, "//", period, sep=""), pattern="_")
		  
		  for (GCM in gcmList) {
			
			cat("Model", GCM, "\n")
			
			inGCMFolder <- paste(inFolder, "//", period, "//", GCM, sep="")
			outFolder <- paste(outptAVOIDScen, "//", scen, "//", period, "//", GCM, sep="")
			
			if (!file.exists(outFolder)) {
			  dir.create(outFolder) 
			}
			
			for (m in 1:12) {
			  
			  cat("Month", m, "\n")
			  
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
			
			bioclim <- BioCalc(outFolder, ext=".asc", format='ascii')
			
			rm(bioclim)
			
		  }
		}
	#}
	return("Done!")
}