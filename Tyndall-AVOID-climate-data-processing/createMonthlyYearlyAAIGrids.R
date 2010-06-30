require(sp)
require(raster)

createMonthlyAsciis <- function(indir, otdir, iniYear, finYear) {

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
				ctr <- ctr+1
			}
		}
	}
}