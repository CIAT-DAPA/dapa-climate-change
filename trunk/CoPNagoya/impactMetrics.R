require(rgdal)
require(raster)

source("zipRead.R")

#To calculate impact metrics:
#1. Change in maximum probability (using the same point as detected in current)
#2. Change in maximum probability (even if the point moves)
#3. Change in area
	#a. For NullAdap only decrease would be possible
	#b. For FullAdap both increase and decrease would be possible, and overall change
		#For increase it would be those pixels being == 0 in current and == 1 in the future
		#For decrease it would be those pixels being == 1 in current and == 0 in the future
		#For overall change you just need the number of pixels in current, and those in the future

#spp <- 100841
#idir <- "C:/CIAT_work/COP_CONDESAN"

impactMetrics <- function(spp, idir) {
	mxoDir <- paste(idir, "/mxe_outputs", sep="")
	fdName <- paste("sp-", spp, sep="")
	spFolder <- paste(mxoDir, "/", fdName, sep="")
		
	if (file.exists(spFolder)) {
		
		cat("Taxon", spp, "\n")
		
		threList <- c("Prevalence", "TenPercentile")
		sresList <- c("SRES_A1B", "SRES_A2")
		tsList <- c("2010_2039", "2040_2069")
		migrList <- c("FullAdap", "NullAdap")
		
		rsFolder <- paste(spFolder, "/projections", sep="")
		
		scc <- 1
		#Cycle through SRES
		for (sres in sresList) {
			
			#Cycle through time-slices
			for (tsl in tsList) {
				
				cat("Loading prob. rasters \n")
				#Loading probability rasters
				#Read the baseline raster
				BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000.asc.zip", sep="")
				if (!file.exists(paste(rsFolder, "/", BLfName, sep=""))) {
					BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000.asc.gz", sep="")
				}
				BLrs <- zipRead(rsFolder, BLfName)
				
				#Read the future raster
				fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, ".asc.zip", sep="")
				if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
					fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, ".asc.gz", sep="")
				}
				rs <- zipRead(rsFolder, fName)
				
				#Cycle through thresholds
				for (threshold in threList) {
					
					cat("Loading binned baseline rasters \n")
					#Read the baseline raster
					BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.zip", sep="")
					if (!file.exists(paste(rsFolder, "/", BLfName, sep=""))) {
						BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.gz", sep="")
					}
					BLrst <- zipRead(rsFolder, BLfName)
					
					#Multiply baseline probabilities with binned surface
					
					BLrs <- BLrs * BLrst
					
					#Extract maximum probability
					maxProbCurr <- max(BLrs[which(!is.na(BLrs[]))])
					maxProbCell <- which(BLrs[] == maxProbCurr)
					maxProbCellX <- xFromCell(BLrs, maxProbCell)[1]
					maxProbCellY <- yFromCell(BLrs, maxProbCell)[1]
					
					#Count number of cells (current)
					areaCurr <- length(which(BLrst[] == 1))
					
					#Cycle through migration scenarios
					for (mig in migrList) {
						
						cat("Calculating:", paste(sres, "-", tsl, "-", threshold, "-", mig, sep=""), "\n")
						
						#Loading the future raster
						fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, "_", threshold, "_", mig, ".asc.zip", sep="")
						if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
							fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, "_", threshold, "_", mig, ".asc.gz", sep="")
						}
						rst <- zipRead(rsFolder, fName)
						
						rs <- rst * rs
						
						#Extract maximum probability
						maxProbFutu <- max(rs[which(!is.na(rs[]))])
						maxProbCorr <- mean(rs[maxProbCell])
						
						#Count number of cells (future)
						areaFut <- length(which(rst[] == 1))
						
						#Increase in area npix(curr == 0 & futu == 1)
						areaInc <- length(which(BLrst[] == 0 & rst[] == 1))
						
						#Decrease in area npix(curr == 1 & futu == 0)
						areaDec <- length(which(BLrst[] == 1 & rst[] == 0))
						
						rm(rst)
						
						resRow <- data.frame(SPID=spp, SRES=sres, PERIOD=tsl, THRESH=threshold, MIGSCEN=mig, MPBaseline=maxProbCurr, MPCell=maxProbCell, MPCellX=maxProbCellX, MPCellY=maxProbCellY, MPFuture=maxProbFutu, MPFutureCorresp=maxProbCorr, AreaBaseline=areaCurr, AreaFuture=areaFut, AreaIncrease=areaInc, AreaDecrease=areaDec)
						if (scc == 1) {
							res <- resRow
						} else {
							res <- rbind(res, resRow)
						}
						scc <- scc+1
					}
					rm(BLrst)
				}
				rm(BLrs)
				rm(rs)
			}
		}
		
		metFolder <- paste(spFolder, "/metrics", sep="")
		oFile <- paste(metFolder, "/ImpactMetrics.csv", sep="")
		write.csv(res, oFile, quote=F, row.names=F)
		return(res)
	} else {
		cat("The species", spp, "was not modeled \n")
		return(NA)
	}
}














