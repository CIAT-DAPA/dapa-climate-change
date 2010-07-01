require(sp)
require(raster)

extractYearlyData <- function(indir, msk, statsdir, iniYear, finYear) {
	
	zName <- unlist(strsplit(msk, "/", fixed=T))
	zName <- zName[length(zName)]
	zName <- unlist(strsplit(zName, ".", fixed=T))[1]
	msk <- raster(msk)
	
	fnm <- gsub("/", "_", indir)
	fnm <- substr(fnm, 4, nchar(fnm))
	
	coords <- xyFromCell(msk, which(!is.na(msk[])))
	
	vars <- c("dtr","tmp","pre")
	ctr <- 1
	
	for (yr in iniYear:finYear) {
		
		cat("\n Year", yr, "\n")
		
		for (procVar in vars) {
			cat("Variable", procVar, "\n")
			
			for (m in 1:12) {
				
				cat("...Reading raster", m, "\n")
				
				fileName <- paste(indir, "/", yr, "/", procVar, "_", m,".asc", sep="")
				rs <- raster(fileName)
				
				resVals <- xyValues(rs, coords)
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
	otstats <- paste(statsdir, "/", zName, "_", fnm, "_", iniYear, "_", finYear, ".csv", sep="")
	write.csv(resmx, otstats, row.names=F, quote=F)
}