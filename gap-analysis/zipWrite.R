#Julian Ramirez
#Opens a connection over a zip or gzfile and reads the data

require(raster)

zipWrite <- function(rs, path, fname) {
	infile <- paste(path, "/", fname, sep="")
	
	#automatically detect the file type (zip or gzip)
	
	splt <- unlist(strsplit(fname, ".", fixed=T))
	ext <- splt[length(splt)]
	
	infname <- substring(fname, 1, nchar(fname)-nchar(ext)-1)

	if (ext == "gz") {
		zz <- gzfile(infile, "w")
	} else {
		stop("Not supported file type")
	}
	
	#Getting the properties to write
	nc <- ncol(rs)
	nr <- nrow(rs)
	xll <- rs@extent@xmin
	yll <- rs@extent@ymin
	cz <- (rs@extent@xmax - rs@extent@xmin) / nc
	nas <- -9999
	
	#Writing header
	cat("ncols         ", as.character(nc), '\n', sep="", file=zz)
    cat("nrows         ", as.character(nr), '\n', sep="", file=zz)
    cat("xllcorner     ", as.character(xll), '\n', sep="", file=zz)
    cat("yllcorner     ", as.character(yll), '\n', sep="", file=zz)
    cat("cellsize      ", as.character(cz), '\n', sep="", file=zz)
    cat("NODATA_value  ", as.character(nas), '\n', sep="", file=zz)
	
	#Change NA to nas string
	rs[is.na(rs)] <- -9999
	
	#Get the values
	printValues <- matrix(rs[], ncol=nc, nrow=nr, byrow=F)
	lastRow <- rep("\n", nc)
	printValues <- rbind(printValues, lastRow)
	
	#Print the values
	cat(printValues, file=zz)
	
	#Close the connection
	close(zz)
	
	return(infile)
}