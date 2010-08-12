#####################################################################
# Testing with a small square of the data (worldclim stations)
#####################################################################

# gcmint <- GCMSmoothIntp(scenario='SRES_A1B', ts='2010_2039', model='bccr_bcm2_0')

require(fields)
require(raster)
require(rgdal)

xAbsMin <- -180
xAbsMax <- 180
yAbsMin <- -60
yAbsMax <- 90

#intZonesDir <- "F://climateChangeInterpolations"
#baseDir <- "X://climate_change//IPCC_CMIP3"

cat("\n")
cat("GCMSmoothIntp(intZonesDir, baseDir, outputDir, nCols=3, nRows=3, scenario='SRES_A1B', ts='2010_2039', model='bccr_bcm2_0') \n")
cat("\n")

GCMSmoothIntp <- function(intZonesDir, baseDir, outputDir, nCols=3, nRows=3, scenario='SRES_A1B', ts='2010_2039', model='bccr_bcm2_0') {
	
	if (!file.exists(outputDir)) {
		dir.create(outputDir)
	}
	
	cat("\n")
	cat("MODEL ", model, " UNDER SCENARIO ", scenario, " AND FOR ", ts, "\n")
	
	emiScenario <- scenario
	scenDir <- paste(baseDir, "//", emiScenario, sep="")
	
	outScenDir <- paste(outputDir, "//", emiScenario, sep="")
	if (!file.exists(outScenDir)) {
		dir.create(outScenDir)
		dir.create(paste(outScenDir, "//interpolations", sep=""))
	}
	
	procModel <- model
	inModelDir <- paste(scenDir, "//updated//", procModel, sep="")
	
	outModelDir <- paste(outScenDir, "//interpolations//", procModel, sep="")
	if (!file.exists(outModelDir)) {
		dir.create(outModelDir)
	}
	
	period <- ts
	inPeriodDir <- paste(inModelDir, "//", period, sep="")
	
	outPeriodDir <- paste(outModelDir, "//", period, sep="")
	if (!file.exists(outPeriodDir)) {
		dir.create(outPeriodDir)
	}
	
	spacingX <- 360 / nCols
	spacingY <- 150 / nRows
	
	for (rowNr in 1:nRows) {
		
		#rowNr <- 1
		
		for (colNr in 1:nCols) {
			#colNr <- 1
			
			cat("Processing for row ", rowNr, " and column ", colNr, " \n")
			
			boxXMin <- xAbsMin + ((colNr - 1) * spacingX)
			boxXMax <- xAbsMin + (colNr * spacingX)
			boxYMin <- yAbsMax - (rowNr * spacingY)
			boxYMax <- yAbsMax - ((rowNr - 1) * spacingY)
			
			xMin <- boxXMin - 15
			if (xMin < xAbsMin) {
				xMin <- xAbsMin
			}
			
			xMax <- boxXMax + 15
			if (xMax > xAbsMax) {
				xMax <- xAbsMax
			}
			
			yMin <- boxYMin - 15
			if (yMin < yAbsMin) {
				yMin <- yAbsMin
			}
			
			yMax <- boxYMax + 15
			if (yMax > yAbsMax) {
				yMax <- yAbsMax
			}
			
			cat(xMin, " to ", xMax, "in LON and  ", yMin, " to ", yMax, " in LAT", "\n")
			
			altGridName <- paste(intZonesDir, "//", "alt_", rowNr, "_", colNr, ".asc", sep="")
			mskGridName <- paste(intZonesDir, "//", "mask_", rowNr, "_", colNr, ".asc", sep="")
			
			altGrid <- raster(altGridName)
			altGrid <- readAll(altGrid)
			
			msk <- raster(mskGridName)
			msk <- readAll(msk)
			
			varList <- c("prec","tmin","tmax","tmean")
			
			for (varName in varList) {
				
				stationDataFileName <- paste(inPeriodDir, "//", varName, ".csv", sep="")
				wcData <- read.csv(stationDataFileName)
				
				wcData <- wcData[which(wcData$Lon <= xMax & wcData$Lon >= xMin & wcData$Lat >= yMin & wcData$Lat <= yMax),]
				
				monList <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
				nMth <- 1
				
				for (mth in monList) {
					
					cat("Processing month ", mth, " \n")
					
					outGridName <- paste(outPeriodDir, "//", varName, "_", nMth, "_", rowNr, "_", colNr, ".asc", sep="")
					
					if (!file.exists(outGridName)) {
						indvars <- matrix(nrow=nrow(wcData), ncol=3)
						indvars[,1] <- wcData$Lon #Lon
						indvars[,2] <- wcData$Lat #Lat
						indvars[,3] <- wcData$Alt #Alt
						
						depvar <- wcData[,(nMth+4)]
						
						cat("Fitting the surface \n")
						
						fit <- Tps(indvars, depvar, m=2)
						
						result <- interpolate(altGrid, fit, xyOnly=F, progress="text")
						result <- writeRaster(result, outGridName, format="ascii", overwrite=T)
						
					}
					
					nMth <- nMth + 1
				}
			}
		}
	}
	return("Done!")	
}
