# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p6Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (tmin)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Minimum temperature of coldest month (P6)", "\n")
			tmpstack <- stack(rlist)
			b6fun <- function(x) {min(x)}
			p6 <- calc(tmpstack, b6fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_6 already exists, skipping calculation, but loading", "\n")
			p6 <- raster(outfile)
		}
		return(p6)
}
