#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: Adds a field indicating whether the record is unique or not, and returns the same input dataset
require(raster)

getUniqueCoord <- function(dataset, fields=c(2,3), resol=0.083333) {
	if (length(fields) != 2) {
		stop("The fields argument MUST be a vector of length 2 (lon, lat)")
	}
	
	if (!is.numeric(dataset[,fields[1]]) | !is.numeric(dataset[,fields[2]])) {
		stop("Longitude and latitude fields MUST be numeric")
	}
	
	onlyLatLon <- cbind(dataset[,fields[1]], dataset[,fields[2]])
	
	if (max(onlyLatLon[,1]) > 180 | min(onlyLatLon[,1] < -180)) {
		stop("Longitude field seems to contain invalid values")
	}
	
	if (max(onlyLatLon[,2]) > 90 | min(onlyLatLon[,2] < -60)) {
		stop("Latitude field seems to contain invalid values")
	}
	
	#Now create the raster (global coverage for uniformity)
	rs <- raster(nrow=round(360/resol,0), ncol=round(180/resol,0))
	rs[] <- 1:ncell(rs)
	
	vals <- xyValues(rs, onlyLatLon[,1:2]) 
	onlyLatLon <- cbind(onlyLatLon, rep(NA,times=nrow(onlyLatLon)))
	onlyLatLon[,3] <- vals
	
	rm(rs)
	
	onlyLatLon <- cbind(onlyLatLon, duplicated(onlyLatLon))
	urows <- which(onlyLatLon[,4] == 0)
	drows <- which(onlyLatLon[,4] == 1)
	
	dataset$UID <- 1:nrow(dataset)
	dataset$IS_UNIQUE <- NA
	
	dataset$IS_UNIQUE[urows] <- TRUE
	dataset$IS_UNIQUE[drows] <- FALSE
	
	return(dataset)
}
