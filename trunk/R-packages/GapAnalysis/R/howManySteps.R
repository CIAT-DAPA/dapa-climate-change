# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

howManySteps <- function(ncells) {
	
	if (class(ncells) != "numeric") {
		stop('Object ', ncells, ' is not a number')
	} else if (ncells <= 0) {
		stop('Object ', ncells, ' must be greater than zero')
	}
	
	ncls <- 1000
	nrws <- round(ncells / ncls + 0.5)
	
	rs <- raster(ncol=ncls, nrow=nrws)
	
	if (canProcessInMemory(rs, n=1)) {
		return(c(ncells, 1))
	} else {
		dv <- 2
		while (!canProcessInMemory(rs, n=1)) {
			ncll <- ncells / dv
			nrws <- round(ncll / ncls + 0.5)
			
			rs <- raster(ncol=ncls, nrow=nrws)
			dv <- dv + 1
		}
		ncmem <- ncell(rs)
		nstep <- round(ncells/ncmem)
		
		return(nstep)
	}
}