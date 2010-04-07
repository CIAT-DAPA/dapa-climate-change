# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

extractStoreValues <- function(variables, ext='', accfile, outfile='') {
	
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
	
	if (!file.exists(accfile)) {
		stop('The desired accessions file does not exist') 
	} else {
		pnts <- read.csv(accfile)
		
		if (ncol(pnts) != 4) {
			stop('The accessions file should have 4 columns: rowid, accid, lon, lat')
		} else {
			xydata <- pnts[,3:4] #Suppossing this data is rowid,accid,lon,lat
		}
	}
	
	omx <- matrix(ncol=nlay+4, nrow=nrow(xydata))
	omx[,1] <- pnts[,1]
  omx[,2] <- pnts[,2]
	omx[,3] <- xydata[,1]
	omx[,4] <- xydata[,2]
	rm(pnts)
	
	cat("\n", "Now extracting", "\n")
	pb <- pbCreate(nlay, type='text', style=3)
	for (i in 1:nlay) {
    rs <- raster(varstk, i)
    if (canProcessInMemory(rs, 2)) {
      rs <- readAll(rs)
      ptvals <- xyValues(rs, xydata)
      omx[,i+4] <- ptvals
    } else {
      ptvals <- xyValues(rs, xydata)
      omx[,i+4] <- ptvals
    }
   pbStep(pb, i) 
	}
	pbClose(pb)
	
	omx <- as.data.frame(omx)
	names(omx)[1] <- "idrow"
	names(omx)[2] <- "idacc"
	names(omx)[3] <- "X"
	names(omx)[4] <- "Y"
	
	for (i in 5:ncol(omx)) {
	 names(omx)[i] <- paste("var_", i-4, sep="")
	}
	
	out <- write.csv(omx, outfile, quote=FALSE, row.names=FALSE)
	return(omx)
	
}
