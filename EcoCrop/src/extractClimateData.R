#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: Extract monthly max, min, mean, temp, and total precip

require(raster)
require(rgdal)

extractMonthlyData <- function(wd="./data/climate", variable="prec", ext=".asc", dataset, fields=c(2,3), verbose=T) {
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
	
	
	if (verbose) {
		cat("\n")
		cat("Variable:", variable, "\n")
	}
	for (m in 1:12) {
		if (verbose) cat(".",m," ",sep="")
		rs <- raster(paste(wd, "/", variable, "_", m, ext, sep=""))
		vec <- xyValues(rs, onlyLatLon)
		dataset$VEC <- vec
		names(dataset)[length(names(dataset))] <- paste(toupper(variable),m,sep="")
	}
	if (verbose) cat("\n")
	return(dataset)
}