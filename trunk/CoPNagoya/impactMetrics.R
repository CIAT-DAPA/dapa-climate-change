#Julian Ramirez, July 11 2010
require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXX IMPACT METRICS CALCULATION XXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

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
#4. Suitable area under all scenarios (use the calculated area *_area asciis in ./masks/AAIGrids)

impactMetrics <- function(spp, idir, overwrite=T) {
	mxoDir <- paste(idir, "/mxe_outputs", sep="")
	fdName <- paste("sp-", spp, sep="")
	spFolder <- paste(mxoDir, "/", fdName, sep="")
		
	if (file.exists(spFolder)) {
		
		cat("Taxon", spp, "\n")
		
		metFolder <- paste(spFolder, "/metrics", sep="")
		oFile <- paste(metFolder, "/ImpactMetrics.csv", sep="")
		
		if (!file.exists(oFile) | overwrite) {
			
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
						maxProbCell <- which(BLrs[] == maxProbCurr)[1]
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
			
			write.csv(res, oFile, quote=F, row.names=F)
			return(res)
		} else {
			cat("The species", spp, "was already calculated, use overwrite=T, to re-calculate it \n")
		}
	} else {
		cat("The species", spp, "was not modeled \n")
		return(NA)
	}
}


############################################################################
#idir <- "C:/CIAT_work/COP_CONDESAN"
#ini <- 600
#fin <- 610

impactProcess <- function(idir, ini, fin) {
	ufile <- paste(idir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
	ufile <- read.csv(ufile)
	nspp <- nrow(ufile)
	
	#Checking consistency of initial and final nspp
	if (fin <= ini) {
		stop("Final number of species is less than or equal to initial, please correct")
	}

	if (fin > nspp) {
		cat("Final number of species is greater than total number of species, using #SPP instead \n")
		fin <- nspp
	}
	
	spList <- ufile$IDSpecies[ini:fin]
	sppC <- 1
	
	for (sp in spList) {
		cat("\n")
		cat("...Species", sp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
		out <- impactMetrics(sp, idir, overwrite=T)
		sppC <- sppC + 1
	}
	return("Done!")
}
