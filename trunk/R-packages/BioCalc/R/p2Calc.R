# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

p2Calc <- function(rlist, rlist2, outfile, format='') {
	
	if (!is.list(rlist)) {
		stop('First argument should be a list or rasters (tmax)')
	} else if (!is.list(rlist2)) {
		stop('Second argument should be a list or rasters (tmin)')
	}
	
	if (!file.exists(outfile)) {
			cat("", "\n", "Mean diurnal temperature range (P2)", "\n")
			tmpstack <- stack(c(rlist, rlist2))
			
			b2fun <- function(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24){return(round(((t1-t13)+(t2-t14)+(t3-t15)+(t4-t16)+(t5-t17)+(t6-t18)+(t7-t19)+(t8-t20)+(t9-t21)+(t10-t22)+(t11-t23)+(t12-t24))/12))}
			
			outfileNoExt <- substr(outfile, 1, (nchar(outfile)-4))
			
			p2 <- overlay(tmpstack, fun=b2fun, progress="text", filename=outfileNoExt, format='raster', overwrite=TRUE)
			p2 <- writeRaster(p2, filename=outfile, format='ascii', overwrite=TRUE)
			
			file.remove(paste(outfileNoExt, ".grd", sep=""))
			file.remove(paste(outfileNoExt, ".gri", sep=""))
			
			rm(tmpstack)
		} else {
			cat("", "\n", "File bio_2 already exists, skipping calculation, but loading", "\n")
			p2 <- raster(outfile)
		}
		return(p2)
}
