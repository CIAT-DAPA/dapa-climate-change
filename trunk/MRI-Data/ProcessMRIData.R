# This R script processes the MRI data using gdal and other stuff
#Available periods are SP0A, SF0A, and SN0A

require(rgdal)
require(raster)

cat("Available periods are SP0A, SF0A, and SN0A \n")
cat("\n")
cat("Run as procMRIData(baseDir, tmpDir, outDir, period)\n")

procMRIData <- function(baseDir, tmpDir, outDir, period) {
	
	periodPath <- paste(baseDir, "/", period, sep="")
	if (!file.exists(periodPath)) {
		stop("The folder", periodPath, "does not exist \n")
	}
	
	tmpPeriodDir <- paste(tmpDir, "/", period, sep="")
	
	if (!file.exists(tmpPeriodDir)) {
		dir.create(tmpPeriodDir, recursive=T)
	}
	
	outPeriodDir <- paste(outDir, "/", period, sep="")
	
	if (!file.exists(outDir)) {
		dir.create(outPeriodDir, recursive=T)
	}
	
	#Listing folders in the input folder
	
	dateList <- list.files(periodPath, pattern="OUT_")
	nDates <- length(dateList)
	
	cat("Processing", nDates, "dates \n")
	
	for (doy in dateList) {
		dte <- strsplit(doy, "_")[[1]][2]
		
		verFile <- paste(outPeriodDir, "/", doy, "/done.txt", sep="")
		if (file.exists(verFile)) {
			cat("Date", dte, "done! \n")
		} else {
			
			inDateDir <- paste(periodPath, "/", doy, sep="")
			
			outDateDir <- paste(outPeriodDir, "/", doy, sep="")
			if (!file.exists(outDateDir)) {
				dir.create(outDateDir, recursive=T)
			}
			
			year <- substr(dte, 1, 4)
			month <- substr(dte, 5, 6)
			day <- substr(dte, 7, 8)
			
			ncFileList <- list.files(inDateDir, pattern=".nc")
			
			for (fileName in ncFileList) {
				
				fileNoExt <- strsplit(fileName, ".n")[[1]][1]
				
				prefix <- substr(fileNoExt, 1, 13)
				dom <- substr(fileNoExt, 15, 16)
				
				cat("Processing", fileName, "\n")
				
				cat("GDAL translate \n")
				
				inFile <- paste(inDateDir, "/", fileName, sep="")
				
				if (prefix == "0_sfc_max_day") {
					outVarType <- "temptr"
					outFile <- paste(outDateDir, "/tmax_", dom, ".asc", sep="")
				} else if (prefix == "1_sfc_max_day") {
					outVarType <- "windsp"
					outFile <- paste(outDateDir, "/wsmax_", dom, ".asc", sep="")
				} else if (prefix == "2_sfc_max_day") {
					outVarType <- "precip"
					outFile <- paste(outDateDir, "/prmax_", dom, ".asc", sep="")
				} else if (prefix == "0_sfc_min_day") {
					outVarType <- "temptr"
					outFile <- paste(outDateDir, "/tmin_", dom, ".asc", sep="")
				} else if (prefix == "0_sfc_avr_day") {
					outVarType <- "precip"
					outFile <- paste(outDateDir, "/prec_", dom, ".asc", sep="")
				} else if (prefix == "0_sfc_avr_day") {
					outVarType <- "temptr"
					outFile <- paste(outDateDir, "/tmean_", dom, ".asc", sep="")
				}
				
				gzFileName <- paste(outFile, ".gz", sep="")
				if (!file.exists(gzFileName)) {
					
					if (file.exists(outFile)) {
						file.remove(outFile)
					}
					
					tmpFile <- paste(tmpPeriodDir, "/temp.asc", sep="")
					tmpFileCom <- paste(tmpPeriodDir, "/temp.asc.aux.xml", sep="")
					
					if (file.exists(tmpFile)) {
						file.remove(tmpFile)
					}
					
					if (file.exists(tmpFileCom)) {
						file.remove(tmpFileCom)
					}
					
					system(paste("gdal_translate", "-of", "AAIGrid", inFile, tmpFile))
					
					cat("Loading the data \n")
					
					loadData <- scan(tmpFile, skip=7)
					
					rs <- raster(nrow=960, ncol=1920, xmn=0, xmx=360)
					rs[] <- loadData
					
					rm(loadData)
					
					rs <- flip(rs, 'y')
					rs <- rotate(rs)
					
					if (outVarType == "temptr") {
						rs <- rs - 272.15
						rs <- writeRaster(rs, outFile, overwrite=T, format='ascii')
						file.remove(tmpFile)
						file.remove(tmpFileCom)
					} else {
						rs <- writeRaster(rs, outFile, overwrite=T, format='ascii')
						file.remove(tmpFile)
						file.remove(tmpFileCom)
					}
					
					rm(rs)
					system(paste("gzip", outFile))
				} else {
					cat("The file", gzFileName, "already exists \n")
				}
			}
			
			con <- file(verFile, "w")
			textToWrite <- paste("These files were processed on", date())
			writeLines(textToWrite, con)
			close(con)
			
		}
		
	}
	
	
}