# This R script processes the MRI data using gdal and other stuff

#Available periods are SP0A, SF0A, and SN0A

procMRIData <- function(baseDir, tmpDir, outDir, period) {
	
	periodPath <- paste(basedir, "//", period, sep="")
	if (!file.exists(periodPath)) {
		error("The folder", periodPath, "does not exist \n")
	}
	
	if (!file.exists(tmpDir)) {
		dir.create(tmpDir, recursive=T)
	}
	
	if (!file.exists(outDir)) {
		dir.create(outDir, recursive=T)
	}
	
	#Listing folders in the input folder
	
	dateList <- list.files(periodPath, pattern="OUT_")
	nDates <- length(dateList)
	
	cat("Processing", nDates, "dates \n")
	
	for (doy in dateList) {
		dte <- strsplit(doy, "_")[[1]][2]
		
		verFile <- paste(outDir, "//", doy, "//done.txt", sep="")
		if (file.exists(verFile)) {
			cat("Date", dte, "done! \n")
		} else {
			
			inDateDir <- paste(periodPath, "//", dte, sep="")
			
			outDateDir <- paste(outDir, "//", doy, sep="")
			if (file.exists(outDateDir)) {
				dir.create(outDateDir)
			}
			
			year <- substr(dte, 1, 4)
			month <- substr(dte, 5, 2)
			day <- substr(dte, 7, 2)
			
			ncFileList <- list.files(inDateDir, pattern=".nc")
			
			for (fileName in ncFileList) {
				
				fileNoExt <- strsplit(fileName, ".n")[[1]][1]
				
				prefix <- substr(fileNoExt, 1, 13)
				dom <- substr(fileNoExt, 15, 2)
				
				cat("Processing", fileName, "\n")
				
				cat("GDAL translate \n")
				
				inFile <- paste(inDateDir, "//", fileName, sep="")
				outFile <- paste(outDateDir, "//", fileName, sep="")
				
				system(paste("gdal_translate", "-of", "AAIGrid", inFile, outFile))
				
				cat("Loading the data \n")
				
				
				
				
				
			}
			
		}
		
	}
	
	
}