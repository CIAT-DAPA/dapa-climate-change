# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p19Calc <- function(rlist, rlist2, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	} else if (!is.list(rlist2)) {
		stop('Second argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Precipitation of Coldest Quarter (P19)", "\n")
			
			PpTaStack <- stack(c(rlist, rlist2))
			
			p19fun <- function(DataPixel) {
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
					cld1 <- t1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("mnt", j, sep=""), if (get(paste("t", j, sep="")) < get(paste("cld", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
						assign(paste("cld", j, sep=""), if (get(paste("t", j, sep="")) < get(paste("cld", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("cld", wm, sep=""))})
					}
					cldm <- mnt12
					
					for (wm in 1:12) {
						assign(paste("yy", wm, sep=""), if (cldm == wm) {get(paste("q", wm, sep=""))} else {-9999})
					}
					res <- max(yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12)
					return(res)
				}
			}
			
			p19 <- raster(PpTaStack, 0)
			filename(p19) <- outfile
			
			pb <- pbCreate(nrow(PpTaStack), type='text', style=3)
			for (rw in 1:nrow(PpTaStack)) {
				rowVals <- getValues(PpTaStack, rw)
				
				RasVals <- apply(rowVals, 1, p19fun)
				p19 <- setValues(p19, RasVals, rw)
				p19 <- writeRaster(p19, outfile, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(PpTaStack)
		} else {
			cat("", "\n", "File bio_19 already exists, skipping calculation, but loading", "\n")
			p19 <- raster(outfile)
		}
		return(p19)
}
