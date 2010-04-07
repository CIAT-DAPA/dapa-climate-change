# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p9Calc <- function(rlist, rlist2, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	} else if (!is.list(rlist2)) {
		stop('Second argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Mean Temperature of Driest Quarter (P9)", "\n")
			
			PpTaStack <- stack(c(rlist, rlist2))
			
			p9fun <- function(DataPixel) {
				if(is.na(DataPixel[1])) {
					return(NA)
				} else {
					
					q1 <- -9999
					mnt12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						PptDataPixel <- DataPixel[1:12]
						TavDataPixel <- DataPixel[13:24]
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("q", wm, sep=""), PptDataPixel[i] + PptDataPixel[j] + PptDataPixel[k])
						assign(paste("t", wm, sep=""), TavDataPixel[i] + TavDataPixel[j] + TavDataPixel[k])
					}
					
					mnt1 <- 1
					dry1 <- q1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("mnt", j, sep=""), if (get(paste("q", j, sep="")) < get(paste("dry", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
						assign(paste("dry", j, sep=""), if (get(paste("q", j, sep="")) < get(paste("dry", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("dry", wm, sep=""))})
					}
					drym <- mnt12
					
					for (wm in 1:12) {
						assign(paste("yy", wm, sep=""), if (drym == wm) {get(paste("t", wm, sep=""))} else {-9999})
					}
					res <- round(max(yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12) / 3)
					return(res)
				}
			}
			
			p9 <- raster(PpTaStack, 0)
			filename(p9) <- outfile
			
			pb <- pbCreate(nrow(PpTaStack), type='text', style=3)
			for (rw in 1:nrow(PpTaStack)) {
				rowVals <- getValues(PpTaStack, rw)
				
				RasVals <- apply(rowVals, 1, p9fun)
				p9 <- setValues(p9, RasVals, rw)
				p9 <- writeRaster(p9, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(PpTaStack)
		} else {
			cat("", "\n", "File bio_9 already exists, skipping calculation, but loading", "\n")
			p9 <- raster(outfile)
		}
		return(p9)
}
