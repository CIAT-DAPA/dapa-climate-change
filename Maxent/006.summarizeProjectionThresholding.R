# Calculating averages of all rasters projections
require(raster)
source("D:/Maxent_Nicaragua/_scripts/000.zipRead.R")

summarize <- function(spID, inputDir, outFolder, outName, NADir) {
	
	cat(" \n")
	cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
	cat("XXXXXXXX SUMARIZE AND THRESHOLDING XXXXXXXXX \n")
	cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
	
	cat("Summarize projections \n")
	####Thresholding Projections#####

	###Listing Ascii Projections
	ascList <- list.files(paste(outName, "/projections", sep=""), pattern=".asc")
	setwd(paste(outName, "/projections", sep=""))

	cat("Calculating and writing mean probability of all raster \n")
	distMean <- mean(stack(paste(ascList)))
	distMean <- writeRaster(distMean, paste(outName, "/projections/averages/", spID, "_EMN.asc", sep=""), format="ascii", overwrite=T)

	cat("Calculating and writing std probability of all raster \n")
	fun <- function(x) { sd(x) }
	distStdv <- calc(stack(ascList), fun)
	distStdv <- writeRaster(distStdv, paste(outName, "/projections/averages/", spID, "_ESD.asc", sep=""), format="ascii", overwrite=T)

	#Read the thresholds file
	threshFile <- paste(outName, "/metrics/thresholds.csv", sep="")
	threshData <- read.csv(threshFile)
	thslds <- c("UpperLeftROC")

	thrNames <- names(threshData)
	thePos <- which(thrNames == thslds)
	theVal <- threshData[1,thePos]

	cat("Thresholding averages projections... \n")
	distMeanPR <- distMean
	distMeanPR[which(distMeanPR[] < theVal)] <- NA
	distMeanPA <- distMean
	distMeanPA[which(distMeanPA[] < theVal)] <- 0
	distMeanPA[which(distMeanPA[] != 0)] <- 1

	distStdvPR <- distStdv * distMeanPA

	#Now cut to native areas
	#Verify if the native area exists, else create one using the buffered convex hull
	NAGridName <- paste(NADir, "/", spID, "/narea.asc.gz", sep="")
	if (!file.exists(NAGridName)) {
		cat("The native area does not exist, generating one \n")
		NAGrid <- chullBuffer(inputDir, occFile, paste(NADir, "/", spID, sep=""), 500000)
	} else {
		cat("The native area exists, using it \n")
		NAGrid <- zipRead(paste(NADir, "/", spID, sep=""), "narea.asc.gz")
	}

	distMeanPA <- distMeanPA * NAGrid
	distMeanPR <- distMeanPR * NAGrid
	distStdvPR <- distStdvPR * NAGrid

	#Writing these rasters
	# distMeanPA <- writeRaster(distMeanPA, paste(outName, "/projections/averages/", spID, "_pjr_EMN_PA.asc", sep=""), format='ascii', overwrite=T)
	distMeanPR <- writeRaster(distMeanPR, paste(outName, "/projections/summarize/", spID, "_pjr_EMN_PR.asc", sep=""), format='ascii', overwrite=T)
	distStdvPR <- writeRaster(distStdvPR, paste(outName, "/projections/summarize/", spID, "_pjr_ESD_PR.asc", sep=""), format='ascii', overwrite=T)



	####Thresholding Baseline#####
	###Listing Ascii Projections

	currMean <- raster(paste(outName, "/crossval/", spID, "_avg.asc", sep=""))
	currStdv <- raster(paste(outName, "/crossval/", spID, "_stddev.asc", sep=""))

	cat("Thresholding current distance... \n")

	currMeanPR <- currMean
	currMeanPR[which(currMeanPR[] < theVal)] <- NA

	currMeanPA <- currMean
	currMeanPA[which(currMeanPA[] < theVal)] <- 0
	currMeanPA[which(currMeanPA[] != 0)] <- 1

	currStdvPR <- currStdv * currMeanPA

	currMeanPA <- currMeanPA * NAGrid
	currMeanPR <- currMeanPR * NAGrid
	currStdvPR <- currStdvPR * NAGrid

	
	#Writing these rasters
	cat("Calculating changes... \n")
	currMeanPA <- writeRaster(currMeanPA, paste(outName, "/crossval/", spID, "_bsl_EMN_PA.asc", sep=""), format='ascii', overwrite=T)
	currMeanPR <- writeRaster(currMeanPR, paste(outName, "/projections/summarize/", spID, "_bsl_EMN_PR.asc", sep=""), format='ascii', overwrite=T)
	currStdvPR <- writeRaster(currStdvPR, paste(outName, "/projections/summarize/", spID, "_bsl_ESD_PR.asc", sep=""), format='ascii', overwrite=T)


	#####Calculating Changes######
	if (!file.exists(paste(outName, "/projections/changes", sep=""))) {
	  dir.create(paste(outName, "/projections/changes", sep=""))
	}

	for(i in ascList){
		chgRaster <- raster(paste(outName, "/projections/", i, sep=""), pattern=".asc") - currMeanPR
		chgRaster <- writeRaster(chgRaster, paste(outName, "/projections/changes/", i, sep=""), format='ascii', overwrite=T)
		}	

		

	###Listing Ascii Changes
	chgList <- list.files(paste(outName, "/projections/changes", sep=""), pattern=".asc")
	setwd(paste(outName, "/projections/changes", sep=""))

	cat("Calculating and writing mean probability of all raster \n")
	chgMean <- mean(stack(paste(chgList)))
	chgMean <- writeRaster(chgMean, paste(outName, "/projections/summarize/", spID, "_chg_EMN.asc", sep=""), format="ascii", overwrite=T)

	cat("Calculating and writing std probability of all raster \n")
	fun <- function(x) { sd(x) }
	chgStdv <- calc(stack(chgList), fun)
	chgStdv <- writeRaster(chgStdv, paste(outName, "/projections/summarize/", spID, "_chg_ESD.asc", sep=""), format="ascii", overwrite=T)



	cat("Calculating raster directions")
	if (!file.exists(paste(outName, "/projections/changes/direction", sep=""))) {
	  dir.create(paste(outName, "/projections/changes/direction", sep=""))
	}

	for(i in chgList){
		direction <- raster(paste(outName, "/projections/changes/", i, sep=""), pattern=".asc") * chgMean
		direction[which(direction[] >=0)] <- 1
		direction[which(direction[] <0)] <- 0
		direction[is.na(direction)] <- 0 
		# direction[which(direction[] is.na NA)] <- 0
		direction <- writeRaster(direction, paste(outName, "/projections/changes/direction/", i, sep=""), format="ascii", overwrite=T)
	}


	dirChgList <- list.files(paste(outName, "/projections/changes/direction", sep=""), pattern=".asc")
	setwd(paste(outName, "/projections/changes/direction", sep=""))
	dirChgStack <- sum(stack(paste(dirChgList)))
	dirChgStack[which(dirChgStack[] ==0)] <- NA
	dirChgStack <- writeRaster(dirChgStack, paste(outName, "/projections/summarize/", spID, "_dir_chg.asc", sep=""), format="ascii", overwrite=T)

	dirChg <- raster(paste(outName, "/projections/summarize/coffea_arabica_dir_chg.asc", sep="")) / 475 * 100

	dirChg <- writeRaster(dirChg, paste(outName, "/projections/summarize/coffea_arabica_dir_chg_per.asc", sep=""), format="ascii", overwrite=T)

	}