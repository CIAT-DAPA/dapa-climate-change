# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

extractBackground <- function(msk, variables, ext='', n=100, outfile='') {
	
	if (outfile == '') {
		stop('Please provide a valid name for your output file')
	}
	
	if (trim(class(variables)) != "RasterStack") {
		if (!file.exists(variables)) {
			stop('The desired folder or RasterStack ', variables, ' does not exist')
		} else {
		  if (ext == '') {
		    stop('You need to provide a valid extension')
		  }
			flist <- list.files(paste(variables, "//", sep=""), pattern=ext)
		  varstk <- stack(flist)
		  nlay <- nlayers(varstk)
		}
	} else {
	 varstk <- variables
	 nlay <- nlayers(varstk)
	}
	
	if (trim(class(msk)) != "RasterLayer") {
		if (!file.exists(msk)) {
			stop('The desired mask file or object does not exist')
		} else {
			msk <- raster(msk)
		}
	}
	
	if (class(n) != "numeric") {
		if (tolower(n) != "all") {
		  stop('n must be all or a number')
		} else {
		  n <- ncell(msk)
		}
	} else if (n <= 0) {
		stop('n must greater than 0')
	}
	
	if (n > ncell(msk)) {
    n <- ncell(msk)
	}
	
	rs <- raster(msk)
	rs[] <- 1:ncell(rs)
	rs <- mask(rs, msk)
	
  rcells <- sampleRandom(rs, n, na.rm=TRUE)
	xyvls <- xyFromCell(rs, rcells)
	
	omx <- matrix(ncol=nlay+4, nrow=n)
	omx[,1] <- 1:n
  omx[,2] <- rcells
	omx[,3] <- xyvls[,1]
	omx[,4] <- xyvls[,2]
	rm(rs)
	rm(rcells)
	
	cat("\n", "Now extracting", "\n")
	pb <- pbCreate(nlay, type='text', style=3)
	for (i in 1:nlay) {
    rs <- raster(varstk, i)
    if (canProcessInMemory(rs, 2)) {
      rs <- readAll(rs)
      ptvals <- xyValues(rs, xyvls)
      omx[,i+4] <- ptvals
    } else {
      ptvals <- xyValues(rs, xyvls)
      omx[,i+4] <- ptvals
    }
   pbStep(pb, i) 
	}
	pbClose(pb)
	
	omx <- as.data.frame(omx)
	names(omx)[1] <- "idrow"
	names(omx)[2] <- "idcell"
	names(omx)[3] <- "X"
	names(omx)[4] <- "Y"
	
	for (i in 5:ncol(omx)) {
	 names(omx)[i] <- paste("var_", i-4, sep="")
	}
	
	out <- write.csv(omx, outfile, quote=FALSE, row.names=FALSE)
	return(omx)
	
}
