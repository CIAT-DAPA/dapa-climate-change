# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

gapAreas <- function(pointdens, gthresh=10, evdist, ethresh=10, outfile='') {
	
	if (outfile == '') {
		stop('Please provide a valid name for your output file')
	}
	
	if (class(gthresh) != "numeric") {
		stop('Radius must be a number')
	} else if (gthresh < 0) {
		stop('Radius must greater than or equal to 0')
	}
	
	if (class(ethresh) != "numeric") {
		stop('Radius must be a number')
	} else if (ethresh < 0) {
		stop('Radius must greater than or equal to 0')
	}
	
	if (class(pointdens) != "RasterLayer" && !file.exists(pointdens)) {
		stop('The file or object corresponding to point densities does not exist')
	} else {
		if (class(pointdens) == "character") {
			pointdens <- raster(pointdens)
		}
	}
	
	if (class(evdist) != "RasterLayer" && !file.exists(evdist)) {
		stop('The file or object corresponding to environmental distances does not exist')
	} else {
		if (class(evdist) == "character") {
			evdist <- raster(evdist)
		}
	}
	
	if (!canProcessInMemory(pointdens, n=3)) {
		stop('Cannot allocate the rasters in memory')
	} else {
		
		pointdens <- readAll(pointdens)
		
		msk <- pointdens
		pointdens[which(values(pointdens) >= gthresh)] <- NA
		pointdens[which(values(!is.na(pointdens)))] <- 1
		pointdens[which(values(is.na(pointdens)))] <- 0
		pointdens <- mask(pointdens, msk)
		rm(msk)
		
		evdist <- readAll(evdist)
		
		msk <- evdist
		evdist[which(values(evdist) <= ethresh)] <- NA
		evdist[which(values(!is.na(evdist)))] <- 2
		evdist[which(values(is.na(evdist)))] <- 0
		evdist <- mask(evdist, msk)
		rm(msk)
		
		rslt <- pointdens + evdist
		rslt <- writeRaster(rslt, outfile, overwrite=TRUE)
		return(rslt)
	}
}
