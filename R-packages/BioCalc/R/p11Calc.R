# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p11Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Mean Temperature of Coldest Quarter (P11)", "\n")
			
			TaStack <- stack(rlist)
			
			p11fun <- function(TaDataPixel) {
				if(is.na(TaDataPixel[1])) {
					return(NA)
				} else {
					
					t1 <- -9999
					cld12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("t", wm, sep=""), TaDataPixel[i] + TaDataPixel[j] + TaDataPixel[k])
					}
					
					cld1 <- t1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("cld", j, sep=""), if (get(paste("t", j, sep="")) < get(paste("cld", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("cld", wm, sep=""))})
					}
					res <- round(cld12 / 3)
					return(res)
				}
			}
			
			p11 <- raster(TaStack, 0)
			filename(p11) <- outfile
			
			pb <- pbCreate(nrow(TaStack), type='text', style=3)
			for (rw in 1:nrow(TaStack)) {
				rowVals <- getValues(TaStack, rw)
				
				RasVals <- apply(rowVals, 1, p11fun)
				p11 <- setValues(p11, RasVals, rw)
				p11 <- writeRaster(p11, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(TaStack)
		} else {
			cat("", "\n", "File bio_11 already exists, skipping calculation, but loading", "\n")
			p11 <- raster(outfile)
		}
		return(p11)
}
