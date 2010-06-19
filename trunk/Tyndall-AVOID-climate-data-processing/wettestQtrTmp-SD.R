# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

wetQtrTmp <- function(rlist, rlist2, outfile, outfileSD, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (prec)')
	} else if (!is.list(rlist2)) {
		stop('Second argument should be a list or rasters (tmean)')
	}
	
	if (!file.exists(outfile) | !file.exists(outfileSD)) {
			cat("", "\n", "Mean temperature of wettest quarter (P8) and STD", "\n")
			
			PpTaStack <- stack(c(rlist, rlist2))
			
			p8fun <- function(DataPixel) {
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
					wet1 <- q1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("mnt", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
						assign(paste("wet", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("wet", wm, sep=""))})
					}
					wetm <- mnt12
					
					for (wm in 1:12) {
						assign(paste("xx", wm, sep=""), if (wetm == wm) {get(paste("t", wm, sep=""))} else {-9999})
					}
					res <- round(max(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12) / 3)
					return(res)
				}
			}
			
			p8funsd <- function(DataPixel) {
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
						assign(paste("t", wm, sep=""), sd(c(TavDataPixel[i],TavDataPixel[j],TavDataPixel[k])))
					}
					
					mnt1 <- 1
					wet1 <- q1
					
					for (wm in 1:11) {
						j <- wm + 1
						assign(paste("mnt", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
						assign(paste("wet", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("wet", wm, sep=""))})
					}
					wetm <- mnt12
					
					for (wm in 1:12) {
						assign(paste("xx", wm, sep=""), if (wetm == wm) {get(paste("t", wm, sep=""))} else {-9999})
					}
					res <- round(max(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12))
					return(res)
				}
			}
			
			p8 <- raster(PpTaStack, 0)
			p8sd <- raster(PpTaStack, 0)
			
			filename(p8) <- outfile
			filename(p8sd) <- outfileSD
			
			pb <- pbCreate(nrow(PpTaStack), type='text', style=3)
			for (rw in 1:nrow(PpTaStack)) {
				rowVals <- getValues(PpTaStack, rw)
				
				RasVals <- apply(rowVals, 1, p8fun)
				p8 <- setValues(p8, RasVals, rw)
				p8 <- writeRaster(p8, outfile, format=format, overwrite=TRUE)
				
				RasVals <- apply(rowVals, 1, p8funsd)
				p8sd <- setValues(p8sd, RasVals, rw)
				p8sd <- writeRaster(p8sd, outfileSD, format=format, overwrite=TRUE)
				
				pbStep(pb, rw)
			}
			pbClose(pb)
			rm(PpTaStack)
		} else {
			cat("", "\n", "Files already exist, skipping calculation, but loading", "\n")
			p8 <- raster(outfile)
		}
		return(p8)
}
