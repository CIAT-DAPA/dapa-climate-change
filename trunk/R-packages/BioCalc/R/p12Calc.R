# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p12Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Total annual rainfall (P12)", "\n")
			
			tmpstack <- stack(rlist)
			rs <- raster(tmpstack, 1)
			
			if (!canProcessInMemory(rs, 2)) {
				b12fun <- function(x) {sum(x)}
				p12 <- calc(tmpstack, b12fun, progress="text", filename=outfile, format=format, overwrite=TRUE)
				rm(tmpstack)
			} else {
				pb <- pbCreate(12, type='text', style=3)
				
				for (i in 1:12) {
					if (i == 1) { 
						p12 <- readAll(raster(tmpstack, i))
					} else {
						p12 <- p12 + readAll(raster(tmpstack, i))
					}
					pbStep(pb, i)
				}
				pbClose(pb)
				
				p12 <- writeRaster(p12, outfile, format=format, overwrite=TRUE)
				
			}
		} else {
			cat("", "\n", "File bio_12 already exists, skipping calculation, but loading", "\n")
			p12 <- raster(outfile)
		}
		return(p12)
}
