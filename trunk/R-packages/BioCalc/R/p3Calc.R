# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p3Calc <- function(p2, p7, outfile, format='') {
	
	if (trim(class(p2)) != "RasterLayer") {
		stop('First element should be a RasterLayer [bio_2]')
	} else if (trim(class(p7)) != "RasterLayer") {
		stop('Second element should be a RasterLayer [bio_7]')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Isothermality (P3)", "\n")
			
			if (!canProcessInMemory(p2,3)) {
				b3fun <- function(x,y) {return(round(100 * x / y))}
				p3 <- overlay(p2, p7, fun=b3fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			} else {
				p2 <- readAll(p2)
				p7 <- readAll(p7)
				
				p3 <- round(100 * p2 / p7)
				p3 <- writeRaster(p3, outfile, format=format, overwrite=TRUE)
			}
		} else {
			cat("", "\n", "File bio_3 already exists, skipping calculation, but loading", "\n")
			p3 <- raster(outfile)
		}
		return(p3)
}
