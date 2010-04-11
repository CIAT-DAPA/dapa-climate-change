#This R script computes the EcoCrop suitability index based on a set of parameters

#out <- suitCalc(climPath='C:/CIAT_work/_tools/BioCalc_test', Gmin=90,Gmax=90,Tkmp=0,Tmin=10,Topmin=16,Topmax=25,Tmax=35,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600, outfolder='C:/CIAT_work/_tools/EcoCrop_test', cropname='bean')

makeLogFile <- function(filePathName, climPath, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax) {
	con <- file(filePathName, "w")
	writeLines("CLIMATE_FILES:", climPath)
	writeLines("CROP:", cropname)
	writeLines("GMIN:", Gmin)
	writeLines("GMAX:", Gmax)
	writeLines("TKMP:", Tkmp)
	writeLines("TMIN:", Tmin)
	writeLines("TOPMIN:", Topmin)
	writeLines("TOPMAX:", Topmax)
	writeLines("TMAX:", Tmax)
	writeLines("RMIN:", Rmin)
	writeLines("ROPMIN:", Ropmin)
	writeLines("ROPMAX:", Ropmax)
	writeLines("RMAX:", Rmax)
	close(con)
}

suitCalc <- function(climPath='', Gmin=90,Gmax=90,Tkmp=0,Tmin=10,Topmin=16,Topmax=25,Tmax=35,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600, outfolder='', cropname='') {
	minAdapt <- 0
	maxAdapt <- 1
	
	#Checking climPath folder for consistency
	
	if (!file.exists(climPath)) {
		stop("The specified folder where the climate files should be does not exist, please check...")
	}
	
	#Checking climate files for existence
	
	for (i in 1:12) {
		if (!file.exists(paste(climPath, "//tmean_", i, ".asc", sep=""))) {
			stop("Error mean temperature for month ", i, ": file does not exist")
		} else if (!file.exists(paste(climPath, "//tmin_", i, ".asc", sep=""))) {
			stop("Error min temperature for month ", i, ": file does not exist")
		} else if (!file.exists(paste(climPath, "//prec_", i, ".asc", sep=""))) {
			stop("Error precipitation for month ", i, ": file does not exist")
		}
	}
	
	cat("Input file verification successful \n")
	
	#Creating the log file
	
	logFileName <- paste(outfolder, "//parameters.model", sep="")
	createLog <- makeLogFile(logFileName, climPath, cropname, Gmin, Gmax, Tkmp, Tmin, Topmin, Topmax, Tmax, Rmin, Ropmin, Ropmax, Rmax)
	
	#Creating the stack of the whole list of variables
	
	TaStack <- stack(paste(climPath, "//tmean_", c(1:12), sep=""))
	TnStack <- stack(paste(climPath, "//tmin_", c(1:12), sep=""))
	PrStack <- stack(paste(climPath, "//tmax_", c(1:12), sep=""))
	
	climateStack <- stack(TaStack, TnStack, PrStack)
	
	#Multiplying temperatures by 10
	
	Tkmp <- Tkmp * 10
	Tmin <- Tmin * 10
	Topmin <- Topmin * 10
	Topmax <- Topmax * 10
	Tmax <- Tmax * 10
	
	#Calculating regression models between Rmin-Ropmin and Ropmax-Rmax
	
	rainLeftReg <- lsfit(x=c(Rmin,Ropmin), y=c(0,1))
	rainLeftM <- rainLeftReg$coefficients[2]
	rainLeftB <- rainLeftReg$coefficients[1]
	
	rainRightReg <- lsfit(x=c(Ropmax,Rmax), y=c(1,0))
	rainRightM <- rainRightReg$coefficients[2]
	rainRightB <- rainRightReg$coefficients[1]
	
	Gavg <- round(mean(c(Gmin, Gmax)) / 30)
	Tkill <- Tkmp + 40
	
	cat("Growing season is", Gavg, "\n")
	
	#This is the function that evaluates the suitability in a pixel basis
	
	suitFun <- function(dataPixel) {
		if(is.na(dataPixel[1])) {
			return(c(NA,NA,NA))
		} else {
			
			TavDataPixel <- dataPixel[1:12]
			TnDataPixel <- dataPixel[13:24]
			PptDataPixel <- dataPixel[25:36]
			
			tSuit <- rep(NA, 12)
			pSuit <- rep(NA, 12)
			cumPpt <- rep(NA, 12)
			
			for (i in 1:12) {
				start.month <- i
				end.month <- i + Gavg - 1
				
				#Temp. iteration
				
				if (TnDataPixel[i] < Tkill) {
					tSuit[i] <- 0
				} else if (TavDataPixel[i] < Tmin) {
					tSuit[i] <- 0
				} else if (TavDataPixel[i] < Topmin) {
					tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
				} else if (TavDataPixel[i] < Topmax) {
					tSuit[i] <- 1
				} else if (TavDataPixel[i] < Tmax) {
					tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
				} else {
					tSuit[i] <- 0
				}
				
				#Ppt growing season
				
				end.mth.p <- end.month
				if (end.mth.p > 12) {
					end.mth.p <- end.mth.p - 12
				}
				
				cumPpt[i] <- sum(PptDataPixel[start.month:end.mth.p])
				
				#Precipitation iteration
				
				if (cumPpt[i] < Rmin) {
					pSuit[i] <- 0
				} else if (cumPpt[i] >= Rmin && cumPpt[i] <= Ropmin) {
					pSuit[i] <- (rainLeftM) * cumPpt[i] + (rainLeftB)
				} else if (cumPpt[i] > Ropmin && cumPpt[i] < Ropmax) {
					pSuit[i] <- 1
				} else if (cumPpt[i] >= Ropmax &&  cumPpt[i] <= Rmax) {
					pSuit[i] <- (rainRightM) * cumPpt[i] + (rainRightB)
				} else if (cumPpt[i] > Rmax) {
					pSuit[i] <- 0
				} else {
					pSuit[i] <- NA
				}
			}
			
			#Minimum cumulated temperature and rainfall suitability
			
			ecotf <- rep(NA, 12)
			ecopf <- rep(NA, 12)
			
			for (i in 1:12) {
				start.month <- i
				end.month <- i + Gavg - 1
				
				ecot <- rep(NA, Gavg)
				ecop <- rep(NA, Gavg)
				
				ecot[1] <- 1
				ecop[1] <- 0
				
				mthCounter <- 1
				for (j in start.month:end.month) {
					r.end.mth <- j
					if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
					r.nxt.mth <- r.end.mth + 1
					
					nxtCounter <- mthCounter + 1
					
					if (tSuit[r.end.mth] < ecot[mthCounter]) {
						ecot[nxtCounter] <- tSuit[r.end.mth]
					} else {
						ecot[nxtCounter] <- ecot[mthCounter]
					}
					
					if (pSuit[r.end.mth] > ecop[mthCounter]) {
						ecop[nxtCounter] <- pSuit[r.end.mth]
					} else {
						ecop[nxtCounter] <- ecop[mthCounter]
					}
					
					mthCounter <- mthCounter + 1
				}
				
				ecot <- ecot[which(!is.na(ecot[]))]
				ecop <- ecop[which(!is.na(ecot[]))]
				
				ecotf[i] <- min(ecot)
				ecopf[i] <- max(ecop)
			}
			
			precFinSuit <- round(max(ecopf * 100))
			tempFinSuit <- round(max(ecotf * 100))
			finSuit <- round((max(ecopf) * max(ecotf)) * 100)
			
			res <- c(precFinSuit, tempFinSuit, finSuit)
			return(res)
		}
	}
	
	#Final grid naming and creation
	
	pSuitName <- paste(outfolder, "//", cropname, "_psuitability.grd", sep="")
	tSuitName <- paste(outfolder, "//", cropname, "_tsuitability.grd", sep="")
	fSuitName <- paste(outfolder, "//", cropname, "_suitability.grd", sep="")
	
	pSuitRaster <- raster(climateStack, 0)
	filename(pSuitRaster) <- pSuitName
	
	tSuitRaster <- raster(climateStack, 0)
	filename(tSuitRaster) <- tSuitName
	
	fSuitRaster <- raster(climateStack, 0)
	filename(fSuitRaster) <- fSuitName
	
	pb <- pbCreate(nrow(climateStack), type='text', style=3)
	for (rw in 1:nrow(climateStack)) {
		rowVals <- getValues(climateStack, rw)
		
		rasVals <- apply(rowVals, 1, suitFun)
		precVecSuit <- rasVals[1,]
		tempVecSuit <- rasVals[2,]
		finlVecSuit <- rasVals[3,]
		
		rm(rasVals)
		
		pSuitRaster <- setValues(pSuitRaster, precVecSuit, rw)
		pSuitRaster <- writeRaster(pSuitRaster, pSuitName, format='raster', overwrite=TRUE)
		
		tSuitRaster <- setValues(tSuitRaster, tempVecSuit, rw)
		tSuitRaster <- writeRaster(tSuitRaster, tSuitName, format='raster', overwrite=TRUE)
		
		fSuitRaster <- setValues(fSuitRaster, finlVecSuit, rw)
		fSuitRaster <- writeRaster(fSuitRaster, fSuitName, format='raster', overwrite=TRUE)
		
		pbStep(pb, rw)
	}
	pbClose(pb)
	
	return(stack(pSuitRaster, tSuitRaster, fSuitRaster))
}
