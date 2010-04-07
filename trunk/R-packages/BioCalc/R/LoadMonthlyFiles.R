# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

LoadMonthlyFiles <- function(folder, ext='', varbl='prec', format='ascii') {
	
	flist <- writeFormats()[,1]
	
	if (format %in% flist) {
		ext <- ext
		fmt <- format
	} else {
		stop('Invalid grid type')
	}
	
	if (tolower(varbl) %in% c("prec", "tmax", "tmin", "tmean")) {
		if (tolower(varbl) == 'prec') {
			prefix <- "prec_"
		} else if (tolower(varbl) == 'tmin') {
			prefix <- "tmin_"
		} else if (tolower(varbl) == 'tmax') {
			prefix <- "tmax_"
		} else if (tolower(varbl) == 'tmean') {
			prefix <- "tmean_"
		}
	} else {
		stop('Invalid variable name')
	}
	
	if (!file.exists(folder)) {
		stop('the specified directory does not exist')
	}
	
	pb <- pbCreate(12, type='text', style=3)
	cat("\n", "Loading", varbl, "rasters", "\n")
	
	for (i in 1:12) {
		
		if (tolower(varbl) == 'tmean') {
			if (!file.exists(paste(folder, "//", prefix, i, ext, sep=""))) {
				#cat("Calculating tmean for month ", i)
				
				tmn <- raster(paste(folder, "//", "tmin_", i, ext, sep=""))
				tmx <- raster(paste(folder, "//", "tmax_", i, ext, sep=""))
				
				if (!canProcessInMemory(tmn, n=3)) {
					
					tmpstack <- stack(tmn, tmx)
					tmfun <- function(x) {round(mean(x))}
					
					assign(paste(tolower(varbl), i, sep="_"), calc(tmpstack, tmfun, filename=paste(folder, "//", prefix, i, ext, sep=""), format=fmt, overwrite=TRUE))
					
					rm(tmpstack)
				} else {
					
					assign(paste(tolower(varbl), i, sep="_"), round((tmn + tmx) * 0.5))
					
					assign(paste(tolower(varbl), i, sep="_"), writeRaster(get(paste(tolower(varbl), i, sep="_")), paste(folder, "//", prefix, i, ext, sep=""), overwrite=TRUE, format=fmt))
				}
				
				rm(tmn)
				rm(tmx)
				rm(tmpstack)
				
			} else {
				assign(paste(tolower(varbl), i, sep="_"), raster(paste(folder, "//", prefix, i, ext, sep="")))
			}
		} else {
			if (!file.exists(paste(folder, "//", prefix, i, ext, sep=""))) {
				stop('Incomplete set of rasters (check your folder)')
			} else {
				assign(paste(tolower(varbl), i, sep="_"), raster(paste(folder, "//", prefix, i, ext, sep="")))
			}
		}
		
		if (i == 1) {
				MonList <- get(paste(varbl, i, sep="_"))
			} else {
				MonList <- c(MonList, get(paste(varbl, i, sep="_")))
			}
		pbStep(pb, i)
	}
	pbClose(pb)
	return(MonList)
}

# listt <- LoadMonthlyFiles("C://CIAT_work//_tools//BioCalc_test", ext='.asc', varbl='tmean', format='ascii')