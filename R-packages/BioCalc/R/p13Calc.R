# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p13Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of wettest month (P13)", "\n")
			tmpstack <- stack(rlist)
			b13fun <- function(x) {max(x)}
			p13 <- calc(tmpstack, b13fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_13 already exists, skipping calculation, but loading", "\n")
			p13 <- raster(outfile)
		}
		return(p13)
}
