require(rgdal)
require(raster)

#Create stack, then a function to calculate per pixel the average and the probability
#Uncertainty metrics for a given stack (likely a set of previously loaded GCMs)

#Calculate average, standard deviation, average of first 25%, average of last 25%, agreement (n), agreement (%)

# #Creating the stack
# gcmDir <- "G:/climate_change/IPCC_CMIP3/SRES_A1B/anomalies"
# gcmList <- list.files(gcmDir)
# period <- "2020_2049"
# rsn <- "prec_1"

# for (gcm in gcmList) {
	# cat(gcm, "\n")
	# rDir <- paste(gcmDir, "/", gcm, "/", period, sep="")
	# rs <- raster(paste(rDir, "/", rsn, ".asc", sep=""))
	# v <- extract(rs, xy)
	# assign(gcm, raster(msk)); assign(gcm, setValues(get(gcm), v))
	
	# if (gcm == gcmList[1]) {
		# gcmstack <- c(get(gcm))
	# } else {
		# gcmstack <- c(gcmstack, get(gcm))
	# }
# }
# gcmstack <- stack(gcmstack)

#Basic function to calculate eveything per pixel
uncertain <- function(x) {
	if (is.na(x[1])) {
		return(c(NA,NA,NA,NA,NA,NA))
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
		
		#Get 25% quantile
		q25 <- quantile(x,probs=0.25)
		#Average below that value
		bot25 <- mean(x[which(x<=q25)])
		
		#Get 75% quantile
		q75 <- quantile(x,probs=0.75)
		#Average above that value
		top25 <- mean(x[which(x>=q75)])
		
		#Return values
		return(c(mean(x),sd(x),bot25,top25,st,st/length(x)))
	}
}


#Block calculation
#Final grid naming and creation

uncertainties <- function(instack, outFolder="F:/EcoCrop-development/testing") {
	if (!file.exists(outFolder)) {dir.create(outFolder)}
	
	#creating rasters from scratch
	rsmean <- raster(gcmstack, 0)
	rssd <- raster(gcmstack, 0)
	rsb25 <- raster(gcmstack, 0)
	rst25 <- raster(gcmstack, 0)
	rsst <- raster(gcmstack, 0)
	rsstp <- raster(gcmstack, 0)
	
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
		b25Vec <- rasVals[3,]
		t25Vec <- rasVals[4,]
		stVec <- rasVals[5,]
		stpVec <- rasVals[6,]
		rm(rasVals)
		#assigning outcome to corresponding cells
		iniCell <- 1+(bs$row[b]-1)*ncol(rsmean)
		finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(rsmean)
		rsmean[iniCell:finCell] <- avgVec
		rssd[iniCell:finCell] <- sdVec
		rsb25[iniCell:finCell] <- b25Vec
		rst25[iniCell:finCell] <- t25Vec
		rsst[iniCell:finCell] <- stVec
		rsstp[iniCell:finCell] <- stpVec
		pbStep(pb, b)
	}
	pbClose(pb)

	#Writing data
	avgName <- paste(outFolder, "/mean.asc", sep=""); rsmean <- writeRaster(rsmean, avgName, format='ascii', overwrite=TRUE)
	sdName <- paste(outFolder, "/sd.asc", sep=""); rssd <- writeRaster(rssd, sdName, format='ascii', overwrite=TRUE)
	b25Name <- paste(outFolder, "/mean-bottom25p.asc", sep=""); rsb25 <- writeRaster(rsb25, b25Name, format='ascii', overwrite=TRUE)
	t25Name <- paste(outFolder, "/mean-top25p.asc", sep=""); rst25 <- writeRaster(rst25, t25Name, format='ascii', overwrite=TRUE)
	stName <- paste(outFolder, "/agreement.asc", sep=""); rsst <- writeRaster(rsst, stName, format='ascii', overwrite=TRUE)
	stpName <- paste(outFolder, "/agreement-percent.asc", sep=""); rsstp <- writeRaster(rsstp, stpName, format='ascii', overwrite=TRUE)
}

# plot(density(x),ylim=c(0,0.41),col="red",lwd=2)
# lines(density(posmod),col="blue",lwd=2); lines(density(negmod), col="black",lwd=2)
# lines(c(mean(x),mean(x)),c(0,1),col="red",lwd=1)
# lines(c(mean(posmod),mean(posmod)),c(0,1),col="blue",lwd=1)
# lines(c(mean(negmod),mean(negmod)),c(0,1),col="black",lwd=1)
