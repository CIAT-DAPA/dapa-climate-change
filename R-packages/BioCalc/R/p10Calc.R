# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p10Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Mean Temperature of Warmest Quarter (P10)", "\n")
			
			TaStack <- stack(rlist)
			
			p10fun <- function(TaDataPixel) {
				if(is.na(TaDataPixel[1])) {
					return(NA)
				} else {
					
					t1 <- -9999
					hot12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("t", wm, sep=""), TaDataPixel[i] + TaDataPixel[j] + TaDataPixel[k])
					}
					
					hot1 <- t1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("hot", j, sep=""), if (get(paste("t", j, sep="")) > get(paste("hot", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("hot", wm, sep=""))})
					}
					res <- round(hot12 / 3)
					return(res)
				}
			}
			
			p10 <- raster(TaStack, 0)
			filename(p10) <- outfile
			
			pb <- pbCreate(nrow(TaStack), type='text', style=3)
			for (rw in 1:nrow(TaStack)) {
				rowVals <- getValues(TaStack, rw)
				
				RasVals <- apply(rowVals, 1, p10fun)
				p10 <- setValues(p10, RasVals, rw)
				p10 <- writeRaster(p10, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(TaStack)
		} else {
			cat("", "\n", "File bio_10 already exists, skipping calculation, but loading", "\n")
			p10 <- raster(outfile)
		}
		return(p10)
}
