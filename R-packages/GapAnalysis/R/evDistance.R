# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

evDistance <- function(spamsurf, variables, ext='', acclist, smplfile, oper='min', nacc=10, outfile='') {
	
	if (outfile == '') {
		stop('Please provide a valid name for your output file')
	}
	
	if (!tolower(oper) %in% c("min", "max", "mean", "meanmax", "meanmin")) {
		stop('Not supported operation in argument oper')
	}
	
	if (trim(class(variables)) != "RasterStack") {
		if (!file.exists(variables)) {
			stop('There is no', variables, 'directory in your system')
		} else {
			cat("", "\n", "Loading variables", "\n")
			
			VarList <- list.files(paste(variables, "//", sep=""), pattern=ext)
			rsstack <- stack(VarList)
			
		}
	} else {
		rsstack <- variables
	}
	
	nlay <- nlayers(rsstack)
	ncls <- nlay+4
		
	for (i in 1:nlay) {
		assign(paste("var_", i, sep=""), raster(rsstack, i))
	}
	
	if (trim(class(spamsurf)) != "RasterLayer") {
		if (!file.exists(spamsurf)) {
			stop('The file or object corresponding to the iSPAM surface does not exist')
		} else {
			spamsurf <- raster(spamsurf)
		}
	}
	
	if (!file.exists(acclist)) {
		stop('The specified accessions list does not exist')
	} else {
		acclist <- read.csv(acclist)
		if (ncol(acclist) != ncls) {
			stop('The file only contains ', ncol(acclist), ' fields and needs', 4+nlay)
		} else {
			accBC <- acclist[,5:ncls] #idrow,idacc,lon,lat,var1...varn
		}
	}
	
	if (!file.exists(smplfile)) {
		stop('The specified accessions list does not exist')
	} else {
		smplfile <- read.csv(smplfile)
		if (ncol(smplfile) != ncls) {
			stop('The file only contains ', ncol(smplfile), ' fields and needs', 4+nlay)
		} else {
			smplfile <- smplfile[,5:ncls]
		}
	}
	
	Sx <- cov(smplfile)
	rm(smplfile)
	
	minMah <- function(dataPixel) {
		if (is.na(dataPixel[1])) {
			return(NA)
		} else {
			
			if (tolower(oper) == "min") {
				result <- min(mahalanobis(accBC, dataPixel, Sx))
				return(result)
			} else if (tolower(oper) == "mean") {
				result <- mean(mahalanobis(accBC, dataPixel, Sx))
				return(result)
			} else if (tolower(oper) == "max") {
				result <- max(mahalanobis(accBC, dataPixel, Sx))
				return(result)
			} else if (tolower(oper) == "meanmax") {
				
				if (class(nacc) != "numeric") {
					stop('nacc must be a numer')
				} else if (length(nacc) != 1) {
					stop('nacc must be a single numer')
				} else if (nacc <= 1) {
					stop('nacc must be greater than 1')
				} else if (nacc >= nrow(accBC)) {
					stop('nacc must be lower than the number of accessions')
				} else {
					result <- mahalanobis(accBC, dataPixel, Sx)
					result <- sort(result, decreasing=TRUE)
					result <- mean(result[1:nacc])
					return(result)
				}
				
			} else if (tolower(oper) == "meanmin") {
				
				if (class(nacc) != "numeric") {
					stop('nacc must be a numer')
				} else if (length(nacc) != 1) {
					stop('nacc must be a single numer')
				} else if (nacc <= 1) {
					stop('nacc must be greater than 1')
				} else if (nacc >= nrow(accBC)) {
					stop('nacc must be lower than the number of accessions')
				} else {
					result <- mahalanobis(accBC, dataPixel, Sx)
					result <- sort(result, decreasing=FALSE)
					result <- mean(result[1:nacc])
					return(result)
				}
			}
		}
	}
	
	outraster <- raster(rsstack, 0)
	filename(outraster) <- outfile
	outraster <- clearValues(outraster)
	
	if (!canProcessInMemory(get(paste("var_", 1, sep="")), n=nlay+20)) {
		cat("\n", "Cannot process in memory, taking the long way", "\n")
		pb <- pbCreate(nrow(rsstack), type='text', style=3)
		
		for(i in 1:nrow(rsstack)) {
			
			RowVals <- getValues(rsstack, i)
			OutVals <- apply(RowVals, 1, minMah)
			outraster <- setValues(outraster, OutVals, i)
			outraster <- writeRaster(outraster, outfile, overwrite=TRUE)
			
			pbStep(pb, i)
		}
		pbClose(pb)
		
		outraster <- mask(outraster, spamsurf)
		return(outraster)
	} else {
		cat("\n", "Processing in memory", "\n")
		
		ncl <- ncell(get(paste("var_", 1, sep="")))
		inmx <- matrix(ncol=nlay, nrow=ncl)
		
		cat("\n", "Extracting values for calculation", "\n")
		pb <- pbCreate(nlay, type='text', style=3)
		for (i in 1:nlay) {
			inmx[,i] <- cellValues(get(paste("var_", i, sep="")), 1:ncell(get(paste("var_", i, sep=""))))
			pbStep(pb, i)
		}
		pbClose(pb)
		
		cat("\n", "Now calculating...", "\n")
		OutVals <- apply(inmx, 1, minMah)
		rm(inmx)
		outraster <- setValues(outraster, OutVals)
		outraster <- writeRaster(outraster, outfile, overwrite=TRUE)
		
		return(outraster)
	}
}
