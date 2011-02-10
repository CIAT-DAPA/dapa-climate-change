require(rgdal)
require(raster)

#Create stack, then a function to calculate per pixel the average and the probability
#Uncertainty metrics for a given stack (likely a set of previously loaded GCMs)

#This part is temporarily unavailable as intends loading monthly raw GCM data by putting them 
#in one single resolution rather than having a mixture of different resolutions
# msk <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, ncol=320, nrow=160); msk[] <- 1
# xy <- xyFromCell(msk, 1:ncell(msk))

#Basic function to calculate eveything per pixel
uncertain <- function(x) {
	if (is.na(x[1])) {
		return(c(NA,NA,NA,NA,NA,NA,NA,NA))
	} else {
		#stippling
		x <- x[which(!is.na(x))]
		if (mean(x) < 0) {
			st <- length(which(x < 0))
		} else if (mean(x) > 0) {
			st <- length(which(x > 0))
		} else  if (mean(x) == 0) {
			st <- length(which(x == 0))
		} else {st <- NA}
		
		#splitted histogram
		posmod <- x[which(x > 0)]
		negmod <- x[which(x < 0)]
		zmod <- x[which(x == 0)]
		if (length(negmod) == 0 & length(posmod) == 0) {pm <- Inf} else {pm <- length(posmod) / length(negmod)}
		
		#if (is.na(pm) | is.na(length(posmod))) {print(x)}
		if (pm == Inf & length(posmod) == 0) {
			pm <- -1
		} else if (pm == Inf & length(posmod != 0)) {
			pm <- length(posmod)
		}
		
		if (pm == -1) {
			certainty <- 1
		} else if (pm >= 0 & pm < 1) {
			certainty <- length(negmod) / (length(negmod) + length(posmod))
		} else if (pm == 1) {
			certainty <- 0.5
		} else if (pm > 1) {
			certainty <- length(posmod) / (length(negmod) + length(posmod))
		}
		
		return(c(mean(x),sd(x),st,st/length(x),mean(posmod),mean(negmod),pm,certainty))
	}
}


#Block calculation
#Final grid naming and creation

uncertainties <- function(instack, outFolder="F:/EcoCrop-development/testing") {
	if (!file.exists(outFolder)) {dir.create(outFolder)}
	
	#creating rasters from scratch
	rsmean <- raster(gcmstack, 0)
	rssd <- raster(gcmstack, 0)
	rsst <- raster(gcmstack, 0)
	rsstp <- raster(gcmstack, 0)
	rspom <- raster(gcmstack, 0)
	rsnem <- raster(gcmstack, 0)
	rspm <- raster(gcmstack, 0)
	certainty <- raster(gcmstack, 0)
	
	#Looping through chunks of data
	bs <- blockSize(gcmstack, n=41, minblocks=2)
	cat("(", bs$n, " chunks) \n", sep="")
	pb <- pbCreate(bs$n, type='text', style=3)
	for (b in 1:bs$n) {
		#extracting data and running the function
		rowVals <- getValues(gcmstack, row=bs$row[b], nrows=bs$nrow[b])
		rasVals <- apply(rowVals, 1, uncertain)
		#extracting the outcome of the function
		avgVec <- rasVals[1,]
		sdVec <- rasVals[2,]
		stVec <- rasVals[3,]
		stpVec <- rasVals[4,]
		pomVec <- rasVals[5,]
		nemVec <- rasVals[6,]
		pmVec <- rasVals[7,]
		cerVec <- rasVals[8,]
		rm(rasVals)
		#assigning outcome to corresponding cells
		iniCell <- 1+(bs$row[b]-1)*ncol(rsmean)
		finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(rsmean)
		rsmean[iniCell:finCell] <- avgVec
		rssd[iniCell:finCell] <- sdVec
		rsst[iniCell:finCell] <- stVec
		rsstp[iniCell:finCell] <- stpVec
		rspom[iniCell:finCell] <- pomVec
		rsnem[iniCell:finCell] <- nemVec
		rspm[iniCell:finCell] <- pmVec
		certainty[iniCell:finCell] <- cerVec
		pbStep(pb, b)
	}
	pbClose(pb)

	#Setting NA uncertain areas for each side of the changes
	rspmc <- rspm
	rspmc[which(rspm[] < 0)] <- 0 #when all predictions say NO CHANGE
	rspmc[which(rspm[] >= 0 & rspm[] < 1)] <- 1 #when majority of models report negative changes
	rspmc[which(rspm[] == 1)] <- 2 #when number of models are equal for positive and negative changes
	rspmc[which(rspm[] > 1)] <- 3 #when majority of models report negative changes
	rspomna <- rspom; rspomna[which(rspmc[] != 3)] <- NA
	rsnemna <- rsnem; rsnemna[which(rspmc[] != 1)] <- NA

	#Writing data
	avgName <- paste(outFolder, "/mean.asc", sep=""); rsmean <- writeRaster(rsmean, avgName, format='ascii', overwrite=TRUE)
	sdName <- paste(outFolder, "/sd.asc", sep=""); rssd <- writeRaster(rssd, sdName, format='ascii', overwrite=TRUE)
	stName <- paste(outFolder, "/agreement.asc", sep=""); rsst <- writeRaster(rsst, stName, format='ascii', overwrite=TRUE)
	stpName <- paste(outFolder, "/agreement-percent.asc", sep=""); rsstp <- writeRaster(rsstp, stpName, format='ascii', overwrite=TRUE)
	posmName <- paste(outFolder, "/mean-positive.asc", sep=""); rspom <- writeRaster(rspom, posmName, format='ascii', overwrite=TRUE)
	negmName <- paste(outFolder, "/mean-negative.asc", sep=""); rsnem <- writeRaster(rsnem, negmName, format='ascii', overwrite=TRUE)
	pmName <- paste(outFolder, "/pos-neg-proportion.asc", sep=""); rspm <- writeRaster(rspm, pmName, format='ascii', overwrite=TRUE)
	pmcName <- paste(outFolder, "/pos-neg-classes.asc", sep=""); rspmc <- writeRaster(rspmc, pmcName, format='ascii', overwrite=TRUE)
	pomnaName <- paste(outFolder, "/mean-positive-cut.asc", sep=""); rspomna <- writeRaster(rspomna, pomnaName, format='ascii', overwrite=TRUE)
	nemnaName <- paste(outFolder, "/mean-negative-cut.asc", sep=""); rsnemna <- writeRaster(rsnemna, nemnaName, format='ascii', overwrite=TRUE)
	certName <- paste(outFolder, "/pos-neg-certainty.asc", sep=""); certainty <- writeRaster(certainty, certName, format='ascii', overwrite=TRUE)
	
	return("Done!")
}

# plot(density(x),ylim=c(0,0.41),col="red",lwd=2)
# lines(density(posmod),col="blue",lwd=2); lines(density(negmod), col="black",lwd=2)
# lines(c(mean(x),mean(x)),c(0,1),col="red",lwd=1)
# lines(c(mean(posmod),mean(posmod)),c(0,1),col="blue",lwd=1)
# lines(c(mean(negmod),mean(negmod)),c(0,1),col="black",lwd=1)
