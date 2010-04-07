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
		return(2)
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

pointDensity <- function(msk, accfile, radius=3, type='', outfile='') {
	
	if (class(radius) != "numeric") {
		stop('Radius must be a number')
	} else if (radius < 0) {
		stop('Radius must greater than or equal to 0')
	}
	
	if (trim(class(msk)) != "RasterLayer") {
		if (!file.exists(msk)) {
			stop('The desired mask file or object does not exist')
		} else {
			msk <- raster(msk)
		}
	}
	
	if (!file.exists(accfile)) {
		stop('The desired accessions file does not exist') 
	} else {
		pnts <- read.csv(accfile)
		
		if (ncol(pnts) != 4) {
			stop('The accessions file should have 4 columns: rowid, accid, lon, lat')
		} else {
			xydata <- pnts[,3:4] #Suppossing this data is rowid,accid,lon,lat
			rm(pnts)
		}
	}
	
	if (tolower(type) %in% c("simple", "garea", "parea")) {
		if (tolower(type) == 'simple') {
			dv <- 1
			dv2 <- 1
			tp <- "Euclidean"
		} else if (tolower(type) == 'garea') {
			dv <- 2*pi*radius^2
			dv2 <- 1
			tp <- "Euclidean"
		} else {
			dv2 <- 1000
			radius <- pointDistance(c(0,0), c(0,radius), type='GreatCircle') / dv2
			dv <- 2*pi*radius^2
			tp <- "GreatCircle"
		}
	} else {
		stop('Invalid output type')
	}
	
	CountAccFun <- function(dataPixel) {
		if (is.na(dataPixel[1])) {
			return(NA)
		} else {
			cll <- c(dataPixel[2], dataPixel[3])
			dst <- pointDistance(cll, xydata, type=tp) / dv2
			outmtrx <- matrix(nrow=length(dst), ncol=1)
			outmtrx[,1] <- dst
			rm(dst)
			return(length(which(outmtrx[,1] <= radius)) / dv)
		}
	}
	
	outr <- msk
	outr <- clearValues(outr)
	
	if (!canProcessInMemory(msk, n=20)) {
		cat("\n", "Cannot process in memory, taking the long way", "\n")
		
		nstep <- howManySteps(ncell(msk)*4)
		cperstep <- round(ncell(msk) / nstep)
		rowperstep <- round(cperstep / ncol(msk))
		residual <- round((cperstep / ncol(msk) - rowperstep) * nstep)
		
		totr <- rowperstep*nstep + residual
		
		if (totr != nrow(msk)) {
			stop('Stepped calculation failed')
		}
		
		pb <- pbCreate(nstep, type='text', style=3)
		for (i in 1:nstep) {
			inirow <- i*rowperstep - rowperstep + 1
			inicell <- inirow * ncol(msk) - ncol(msk) + 1
			
			if (i == nstep) {
				finrow <- i*rowperstep + residual
				anrow <- rowperstep + residual
			} else {
				finrow <- i*rowperstep
				anrow <- rowperstep
			}
			
			fincell <- finrow * ncol(msk)
			
			rowvals <- cellValues(msk, inicell:fincell)
			
			locat <- xyFromCell(msk, inicell:fincell)
			
			inmtx <- matrix(nrow=nrow(locat), ncol=3)
			inmtx[,1] <- rowvals
			inmtx[,2] <- locat[,1]
			inmtx[,3] <- locat[,2]
			
			compvals <- apply(inmtx, 1, CountAccFun)
			outr <- setValuesRows(outr, compvals, inirow, anrow)
			outr <- writeRaster(outr, outfile, overwrite=TRUE)
			
			rm(inmtx)
			pbStep(pb, i)
			}
		pbClose(pb)
		return(outr)
	} else {
		cat("\n", "Processing in memory", "\n")
		msk <- readAll(msk)
		rowvals <- cellValues(msk, 1:ncell(msk))
		locat <- xyFromCell(msk, 1:ncell(msk))
		
		inmtx <- matrix(nrow=ncell(msk), ncol=3)
		inmtx[,1] <- rowvals
		inmtx[,2] <- locat[,1]
		inmtx[,3] <- locat[,2]
		
		compvals <- apply(inmtx, 1, CountAccFun)
		outr <- setValues(outr, compvals)
		outr <- writeRaster(outr, outfile, overwrite=TRUE)
		return(outr)
	}
}
