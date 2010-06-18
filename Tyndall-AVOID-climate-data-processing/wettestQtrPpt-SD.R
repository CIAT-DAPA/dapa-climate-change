# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

wetQtrRain <- function(rlist, outfile, outfileSD, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	}
	
	if (!file.exists(outfile) | !file.exists(outfileSD)) {
			cat("", "\n", "Precipitation of wettest quarter (P16) and STD", "\n")
			
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
			
			p16funsd <- function(PptDataPixel) {
				if(is.na(PptDataPixel[1])) {
					return(NA)
				} else {
					
					q1 <- -9999
					s1 <- -9999
					wet12 <- -9999
					wsd12 <- -9999
					
					for (wm in 1:12) {
						i <- wm
						j <- wm + 1
						k <- wm + 2
						
						if (j > 12) {j <- j-12}
						if (k > 12) {k <- k-12}
						
						assign(paste("q", wm, sep=""), PptDataPixel[i] + PptDataPixel[j] + PptDataPixel[k])
						assign(paste("s", wm, sep=""), sd(c(PptDataPixel[i],PptDataPixel[j],PptDataPixel[k])))
					}
					
					wet1 <- q1
					wsd1 <- s1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("wet", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("wet", wm, sep=""))})
						assign(paste("wsd", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("s", j, sep=""))} else {get(paste("wsd", wm, sep=""))})
					}
					return(wsd12)
				}
			}
			
			p16 <- raster(PpStack, 0)
			p16sd <- raster(PpStack, 0)
			
			filename(p16) <- outfile
			filename(p16sd) <- outfileSD
			
			pb <- pbCreate(nrow(PpStack), type='text', style=3)
			for (rw in 1:nrow(PpStack)) {
				rowVals <- getValues(PpStack, rw)
				
				RasVals <- apply(rowVals, 1, p16fun)
				p16 <- setValues(p16, RasVals, rw)
				p16 <- writeRaster(p16, outfile, format=format, overwrite=TRUE)
				
				RasVals <- apply(rowVals, 1, p16funsd)
				p16sd <- setValues(p16sd, RasVals, rw)
				p16sd <- writeRaster(p16sd, outfileSD, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			
			rm(PpStack)
		} else {
			cat("", "\n", "Files already exist, skipping calculation, but loading", "\n")
			p16 <- raster(outfile)
		}
		return(p16)
}
