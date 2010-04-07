# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p15Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation seasonality (P15)", "\n")
			
			tmpstack <- stack(rlist)
			b15fun <- function(x) {round((100 * sqrt((sd(x)^2) * 11 / 12)) / (1 + sum(x) / 12))}
			p15 <- calc(tmpstack, b15fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
			rm(tmpstack)
			
		} else {
			cat("", "\n", "File bio_15 already exists, skipping calculation, but loading", "\n")
			p15 <- raster(outfile)
		}
		return(p15)
}