# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p4Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Temperature seasonality (P4)", "\n")
			b4fun <- function(x) {round(100 * sqrt((sd(x)^2) * 11 / 12))}
			tmpstack <- stack(rlist)
			p4 <- calc(tmpstack, b4fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_4 already exists, skipping calculation, but loading", "\n")
			p4 <- raster(outfile)
		}
		return(p4)
}
