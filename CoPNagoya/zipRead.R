#Julian Ramirez
#Opens a connection over a zip or gzfile and reads the data

require(raster)

zipRead <- function(path, fname) {
	infile <- paste(path, "/", fname, sep="")

	#automatically detect the file type (zip or gzip)

	splt <- unlist(strsplit(fname, ".", fixed=T))
	ext <- splt[length(splt)]

	infname <- substring(fname, 1, nchar(fname)-nchar(ext)-1)

	if (tolower(ext) == "zip") {
		zz <- unz(infile, infname, "r")
	} else if (ext == "gz") {
		zz <- gzfile(infile, "r")
	} else {
		stop("Not supported file type")
	}

	#Reading the data from the ascii, and then closing the connection
	nc <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
	nr <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
	xll <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
	yll <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
	cz <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
	nas <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])

	dta <- scan(zz, na.string=paste(nas), quiet=T)
	close(zz)
	
	#Drawing the extent 
	xur <- xll + cz*nc
	yur <- yll + cz*nr

	#Creating the raster and filling it with data
	rs <- raster(ncol=nc, nrow=nr, xmn=xll, xmx=xur, ymn=yll, ymx=yur)
	rs[] <- dta
	
	return(rs)
}