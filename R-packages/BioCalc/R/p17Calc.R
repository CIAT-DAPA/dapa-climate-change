# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p17Calc <- function(rlist, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of driest quarter (P17)", "\n")
			
			PpStack <- stack(rlist)
			
			p17fun <- function(PptDataPixel) {
				if(is.na(PptDataPixel[1])) {
					return(NA)
				} else {
					
					q1 <- -9999
					dry12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("q", wm, sep=""), PptDataPixel[i] + PptDataPixel[j] + PptDataPixel[k])
					}
					
					dry1 <- q1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("dry", j, sep=""), if (get(paste("q", j, sep="")) < get(paste("dry", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("dry", wm, sep=""))})
					}
					return(dry12)
				}
			}
			
			p17 <- raster(PpStack, 0)
			filename(p17) <- outfile
			
			pb <- pbCreate(nrow(PpStack), type='text', style=3)
			for (rw in 1:nrow(PpStack)) {
				rowVals <- getValues(PpStack, rw)
				
				RasVals <- apply(rowVals, 1, p17fun)
				p17 <- setValues(p17, RasVals, rw)
				p17 <- writeRaster(p17, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(PpStack)
		} else {
			cat("", "\n", "File bio_17 already exists, skipping calculation, but loading", "\n")
			p17 <- raster(outfile)
		}
		return(p17)
}

