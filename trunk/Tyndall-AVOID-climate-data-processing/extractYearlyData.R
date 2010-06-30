require(sp)
require(raster)

extractYearlyData <- function(indir, otdir, msk, iniYear, finYear) {
	
	#indir <- "T:/input/20C3M/yearly_asciis/CRU3_TS"
	#otdir <- "F:/Workspace/Thailand-mike-laure/out-yearly/20C3M/CRU3_TS" #"T:/out-yearly/20C3M/CRU3_TS"

	#msk <- "F:/Workspace/Thailand-mike-laure/masks/tha.asc"
	zName <- unlist(strsplit(msk, "/", fixed=T))
	zName <- zName[length(zName)]
	zName <- unlist(strsplit(zName, ".", fixed=T))[1]
	msk <- raster(msk)

	coords <- xyFromCell(msk, which(!is.na(msk[])))

	#iniYear <- 1901
	#finYear <- 1905 #2005

	vars <- c("dtr","tmp","pre")
	ctr <- 1

	for (yr in iniYear:finYear) {
		
		outFolder <- paste(otdir, "/", yr, sep="")
		if (!file.exists(outFolder)) {
			dir.create(outFolder)
		}
		
		cat("\n Year", yr, "\n")
		
		for (procVar in vars) {
			cat("Variable", procVar, "\n")
			
			cat("...Reading input file \n")
			
			fileName <- paste(indir, "/", yr, "/", procVar, "_all.asc", sep="")
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
			
			for (m in 1:12) {
				finrs <- raster(rs)
				finrs[] <- outMergeOrd[,(m+6)]
				outName <- paste(outFolder, "/", procVar, "_", m, ".asc", sep="")
				finrs <- writeRaster(finrs, outName, format='ascii', overwrite=T)
				
				resVals <- xyValues(finrs, coords)
				resVals <- resVals[which(!is.na(resVals))]
				
				resRow <- c(zName, yr, m, procVar, mean(resVals), sd(resVals), max(resVals), min(resVals))
				
				if (ctr == 1) {
					resmx <- resRow
				} else {
					resmx <- rbind(resmx, resRow)
				}
				
				ctr <- ctr+1
			}
		}
	}
	
	row.names(resmx) <- c(1:nrow(resmx))
	resmx <- as.data.frame(resmx)
	names(resmx) <- c("Site", "Year", "Month", "Variable", "MEAN", "STDEV", "MAX", "MIN")
	otstats <- paste("F:/Workspace/Thailand-mike-laure/", zName, ".csv", sep="")
	write.csv(resmx, otstats, row.names=F, quote=F)
}