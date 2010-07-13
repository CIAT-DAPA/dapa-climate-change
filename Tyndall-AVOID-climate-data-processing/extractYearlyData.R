#Generic function to extract data for a set of years using a mask, from CRU and derived downscaled GCM datasets
#Julian Ramirez, June 2010

require(sp)
require(rgdal)
require(raster)

extractYearlyData <- function(indir, msk, statsdir, iniYear, finYear, writeOutputs=T) {
	
	#Verifying that the stats dir exists
	if (!file.exists(statsdir)) {
		dir.create(statsdir)
	}
	
	#Extract the name of the mask, and loading it as a raster
	zName <- unlist(strsplit(msk, "/", fixed=T))
	zName <- zName[length(zName)]
	zName <- unlist(strsplit(zName, ".", fixed=T))[1]
	msk <- raster(msk)
	
	#Get the name of the file from the input directory (such that it becomes specific and unique)
	fnm <- gsub("/", "_", indir)
	fnm <- substr(fnm, 4, nchar(fnm))
	
	#Extract the coordinates from which the data is to be extracted
	coords <- xyFromCell(msk, which(!is.na(msk[])))
	
	#List the variables
	vars <- c("dtr","tmp","pre")
	
	#If writeOutputs then create the output folder
	if (writeOutputs) {
		oFolder <- paste(statsdir, "/", fnm, sep="")
		if (!file.exists(oFolder)) {
			dir.create(oFolder)
		}
	}
	
	#Cycle through years
	ctr <- 1
	for (yr in iniYear:finYear) {
		
		cat("\n Year", yr, "\n")
		
		#If writeOutputs then create the year-specific output folder
		if (writeOutputs) {
			outYrFolder <- paste(oFolder, "/", yr, sep="")
			if (!file.exists(outYrFolder)) {
				dir.create(outYrFolder)
			}
		}
		
		#Cycle through variables (dtr, tmp, pre)
		for (procVar in vars) {
			cat("Variable", procVar, "\n")
			
			#Cycle through months
			for (m in 1:12) {
				
				#Read the raster
				cat("...Reading raster", m, "\n")
				
				fileName <- paste(indir, "/", yr, "/", procVar, "_", m,".asc", sep="")
				rs <- raster(fileName)
				
				#If writeOutputs is selected then mask the raster and write it
				if (writeOutputs) {
					
					#Cropping and masking the raster
					rsc <- crop(rs, msk@extent)
					rsc <- mask(rsc, msk)
					
					#Writing the output raster
					rname <- paste(outYrFolder, "/", procVar, "_", m, ".asc", sep="")
					rsc <- writeRaster(rsc, rname, format='ascii', overwrite=T)
					rm(rsc)
				}
				
				#Extract raster values from loaded monthly raster
				resVals <- xyValues(rs, coords)
				resVals <- resVals[which(!is.na(resVals))]
				
				#Comprising results (mean, sd, max, min) onto a row
				resRow <- c(zName, yr, m, procVar, mean(resVals), sd(resVals), max(resVals), min(resVals))
				
				#Creating a matrix of results
				if (ctr == 1) {
					resmx <- resRow
				} else {
					resmx <- rbind(resmx, resRow)
				}
				
				ctr <- ctr+1
			}
		}
	}
	
	#Writing the result matrix
	row.names(resmx) <- c(1:nrow(resmx))
	resmx <- as.data.frame(resmx)
	names(resmx) <- c("Site", "Year", "Month", "Variable", "MEAN", "STDEV", "MAX", "MIN")
	otstats <- paste(statsdir, "/", zName, "_", fnm, "_", iniYear, "_", finYear, ".csv", sep="")
	write.csv(resmx, otstats, row.names=F, quote=F)
}