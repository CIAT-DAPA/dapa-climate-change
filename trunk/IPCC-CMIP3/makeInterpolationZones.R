#####################################################################
# Crop altitude and mask
#####################################################################

require(raster)
require(rgdal)

xAbsMin <- -180
xAbsMax <- 180
yAbsMin <- -60
yAbsMax <- 90

maxLon <- 360
maxLat <- 150

#nRowCuts <- 3
#nColCuts <- 4

cat("\n")
cat("makeIntZones(inputDir, outputDir, nRowCuts=3, nColCuts=4) \n")
cat("\n")

makeIntZones <- function(inputDir, outputDir, nRowCuts=3, nColCuts=4) {
	
	rowSize <- maxLat / nRowCuts
	colSize <- maxLon / nColCuts
	
	if (!file.exists(outputDir)) {
		dir.create(outputDir, recursive=T)
	}
	
	for (rowNr in 1:nRowCuts) {
		
		#rowNr <- 1
		
		for (colNr in 1:nColCuts) {
			#colNr <- 1
			
			cat("Processing for row ", rowNr, " and column ", colNr, " \n")
			
			boxXMin <- xAbsMin + ((colNr - 1) * colSize)
			boxXMax <- xAbsMin + (colNr * colSize)
			boxYMin <- yAbsMax - (rowNr * rowSize)
			boxYMax <- yAbsMax - ((rowNr - 1) * rowSize)
			
			xMin <- boxXMin - 20
			if (xMin < xAbsMin) {
				xMin <- xAbsMin
			}
			
			xMax <- boxXMax + 20
			if (xMax > xAbsMax) {
				xMax <- xAbsMax
			}
			
			yMin <- boxYMin - 20
			if (yMin < yAbsMin) {
				yMin <- yAbsMin
			}
			
			yMax <- boxYMax + 20
			if (yMax > yAbsMax) {
				yMax <- yAbsMax
			}
			
			cat("Bounding box goes from ", xMin, " to ", xMax, "in LONG and from ", yMin, " to ", yMax, " in LAT", "\n")
			
			jpeg(paste(outputDir, "//fig_", rowNr, "_", colNr, ".jpeg", sep=""))
			par(mfrow=c(2,1))
			
			bb <- extent(xMin, xMax, yMin, yMax)
			rs <- raster(paste(inputDir, "//alt.asc", sep=""))
			rs <- crop(rs, bb)
			rs <- writeRaster(rs, paste(outputDir, "//alt_", rowNr, "_", colNr, ".asc", sep=""), format="ascii", overwrite=T)
			
			plot(rs)
			
			rs <- raster(paste(inputDir, "//mask.asc", sep=""))
			rs <- crop(rs, bb)
			rs <- writeRaster(rs, paste(outputDir, "//mask_", rowNr, "_", colNr, ".asc", sep=""), format="ascii", overwrite=T)
			
			plot(rs)
			dev.off()
			
		}
	}
	return("Done!")
}