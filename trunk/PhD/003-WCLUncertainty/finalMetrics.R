#Harvest train and test data for all tiles for each part, fold, variable and month and calculate R2 and RMSE, store that value in a file

#Folder structure:
#/climate-data-assessment/wcl-uncertainties/outputs/cross-validation/$variable/part-$part/fold-$fold/tile-$tile

#Files:
#rain_fitted-values.csv
#rain_test-values.csv

#File structure:
#ID,LONG,LAT,PRED.JAN,JAN,PRED.FEB,FEB,PRED.MAR,MAR,PRED.APR,APR,PRED.MAY,MAY,PRED.JUN,JUN,PRED.JUL,JUL,PRED.AUG,AUG,PRED.SEP,SEP,PRED.OCT,OCT,PRED.NOV,NOV,PRED.DEC,DEC

#1. read all tiles file and add them into a whole thing matrix
#2. calculate required metrics and store them into a matrix

source("accuracyFinal.R")

finalMetrics <- function(bDir="", variable="rain", ntiles=5) {
	#Input data details
	dataDir <- paste(bDir, "/climate-data-assessment/wcl-uncertainties/outputs/cross-validation/", variable, sep="")
	
	#Output directory
	outDir <- paste(bDir, "/climate-data-assessment/wcl-uncertainties/outputs", sep="")
	counter <- 1
	
	#Looping through the 100 folds
	for (part in 1:10) {
		partDir <- paste(dataDir, "/part-", part, sep="")
		for (fold in 1:10) {
			foldDir <- paste(partDir, "/fold-", fold, sep="")
			
			cat("\n")
			cat("Processing fold", counter, "out of 100 \n")
			
			#Loading tiled data for metrics calculation
			cat("Loading tile data \n")
			for (tile in 1:ntiles) {
				tileDir <- paste(foldDir, "/tile-", tile, sep="")
				fitFile <- paste(tileDir, "/", variable, "_fitted-values.csv", sep="")
				tstFile <- paste(tileDir, "/", variable, "_test-values.csv", sep="")
				
				fitData <- read.csv(fitFile)
				tstData <- read.csv(tstFile)
				
				if (tile == 1) {
					foldFitData <- fitData
					foldTstData <- tstData
				} else {
					foldFitData <- rbind(foldFitData, fitData)
					foldTstData <- rbind(foldTstData, tstData)
				}
			}
			
			#Calculating monthly metrics
			cat("Calculating accuracy metrics \n")
			acc <- accuracy(trainMx=foldFitData, testMx=foldTstData, variable=variable)
			
			#Creating output data frames
			cat("Putting everything together \n")
			if (counter == 1) {
				metrix <- acc$METRICS
				allFitData <- acc$FITTED
				allTstData <- acc$TEST
			} else {
				metrix <- rbind(metrix, acc$METRICS)
				allFitData <- rbind(allFitData, acc$FITTED)
				allTstData <- rbind(allTstData, acc$TEST)
			}
			counter <- counter+1
		}
	}

	#Write output matrix and data
	outMetrixFile <- paste(outDir, "/", variable, "_overall-metrics.csv", sep="")
	outFitFile <- paste(outDir, "/", variable, "_overall-fitted-values.csv", sep="")
	outTstFile <- paste(outDir, "/", variable, "_overall-test-values.csv", sep="")

	write.csv(metrix, outMetrixFile, quote=F, row.names=F)
	write.csv(allFitData, outFitFile, quote=F, row.names=F)
	write.csv(allTstData, outTstFile, quote=F, row.names=F)
}


#Here implement the density plot figures on a monthly and variable basis