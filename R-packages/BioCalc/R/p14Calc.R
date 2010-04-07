# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p14Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of driest month (P14)", "\n")
			tmpstack <- stack(rlist)
			b14fun <- function(x) {min(x)}
			p14 <- calc(tmpstack, b14fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_14 already exists, skipping calculation, but loading", "\n")
			p14 <- raster(outfile)
		}
		return(p14)
}
