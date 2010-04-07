# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p5Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (tmax)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Maximum temperature of warmest month (P5)", "\n")
			tmpstack <- stack(rlist)
			b5fun <- function(x) {max(x)}
			p5 <- calc(tmpstack, b5fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_5 already exists, skipping calculation, but loading", "\n")
			p5 <- raster(outfile)
		}
		return(p5)
}
