# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p7Calc <- function(p5, p6, outfile, format='') {
	
	if (trim(class(p5)) != "RasterLayer") {
		stop('First element should be a RasterLayer [bio_5]')
	} else if (trim(class(p6)) != "RasterLayer") {
		stop('Second element should be a RasterLayer [bio_6]')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Annual temperature range (P7)", "\n")
			
			if (!canProcessInMemory(p5,3)) {
				b7fun <- function(x,y) {return(x-y)}
				p7 <- overlay(p5, p6, fun=b7fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			} else {
				p5 <- readAll(p5)
				p6 <- readAll(p6)
				
				p7 <- p5 - p6
				p7 <- writeRaster(p7, outfile, format=format, overwrite=TRUE)
			}
		} else {
			cat("", "\n", "File bio_7 already exists, skipping calculation, but loading", "\n")
			p7 <- raster(outfile)
		}
		return(p7)
}
