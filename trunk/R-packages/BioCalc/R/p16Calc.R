# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p16Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of wettest quarter (P16)", "\n")
			
			PpStack <- stack(rlist)
			
			p16fun <- function(PptDataPixel) {
				if(is.na(PptDataPixel[1])) {
					return(NA)
				} else {
					
					q1 <- -9999
					wet12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("q", wm, sep=""), PptDataPixel[i] + PptDataPixel[j] + PptDataPixel[k])
					}
					
					wet1 <- q1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("wet", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("wet", wm, sep=""))})
					}
					return(wet12)
				}
			}
			
			p16 <- raster(PpStack, 0)
			filename(p16) <- outfile
			
			pb <- pbCreate(nrow(PpStack), type='text', style=3)
			for (rw in 1:nrow(PpStack)) {
				rowVals <- getValues(PpStack, rw)
				
				RasVals <- apply(rowVals, 1, p16fun)
				p16 <- setValues(p16, RasVals, rw)
				p16 <- writeRaster(p16, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			
			rm(PpStack)
		} else {
			cat("", "\n", "File bio_15 already exists, skipping calculation, but loading", "\n")
			p16 <- raster(outfile)
		}
		return(p16)
}
