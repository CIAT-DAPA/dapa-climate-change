# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p18Calc <- function(rlist, rlist2, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	} else if (!is.list(rlist2)) {
		stop('Second argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of Warmest Quarter (P18)", "\n")
			
			PpTaStack <- stack(c(rlist, rlist2))
			
			p18fun <- function(DataPixel) {
				if(is.na(DataPixel[1])) {
					return(NA)
				} else {
					
					t1 <- -9999
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
					hot1 <- t1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("mnt", j, sep=""), if (get(paste("t", j, sep="")) > get(paste("hot", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
						assign(paste("hot", j, sep=""), if (get(paste("t", j, sep="")) > get(paste("hot", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("hot", wm, sep=""))})
					}
					hotm <- mnt12
					
					for (wm in 1:12) {
						assign(paste("xx", wm, sep=""), if (hotm == wm) {get(paste("q", wm, sep=""))} else {-9999})
					}
					res <- max(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12)
					return(res)
				}
			}
			
			p18 <- raster(PpTaStack, 0)
			filename(p18) <- outfile
			
			pb <- pbCreate(nrow(PpTaStack), type='text', style=3)
			for (rw in 1:nrow(PpTaStack)) {
				rowVals <- getValues(PpTaStack, rw)
				
				RasVals <- apply(rowVals, 1, p18fun)
				p18 <- setValues(p18, RasVals, rw)
				p18 <- writeRaster(p18, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(PpTaStack)
		} else {
			cat("", "\n", "File bio_18 already exists, skipping calculation, but loading", "\n")
			p18 <- raster(outfile)
		}
		return(p18)
}
