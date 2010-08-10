require(rgdal)
require(raster)

#1. Load species data
#2. Select background area
#3. Train the maxent model
#4. Getting the metrics of the species into a single file
#   4.1  Total number of samples
#   4.2  Training samples
#   4.3  Test samples
#   4.4  Training AUC
#   4.5  Test AUC
#   4.6  Rsquare (test and background)
#   4.7  Logistic deviation on test data
#   4.8  Root mean square difference on test data
#   4.9 Thresholds (7 different taken from Liu et al. 2005)
#5. Project maxent model into current
#6. Project maxent model into all future scenarios
#7. Create the buffer area (500km)
#8. Buffer the distributions
#9. Threshold the distributions
#10. Calculate and write presence/absence surfaces

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXX GAP ANALYSIS MODELING XXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")
cat("...Sourcing scripts \n")
cat(" \n")
cat(" \n")

###############################################################################################
###############################################################################################
#The buffer function
###############################################################################################
###############################################################################################

source("bufferSpecies.R")

###############################################################################################
###############################################################################################
# The evaluation metrics and threshold calculation function
###############################################################################################
###############################################################################################

source("getMetrics.R")

###############################################################################################
###############################################################################################
#Creating the big function
###############################################################################################
###############################################################################################

inputDir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
destDir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
spID <- "Phaseolus_acutifolius"
OSys <- "nt"


theEntireProcess <- function(spID, OSys, inputDir, destDir) {
	
	verFile <- paste(destDir, "/mxe_outputs/sp-", spID, "/ps-", spID, ".run", sep="")
	
	OSys <- tolower(OSys)
	
	if (!file.exists(verFile)) {
		
		if (file.exists(paste(inputDir, "/mxe_outputs/sp-", spID, sep=""))) {
			cat("Removing previous stuff ... \n")
			system(paste("rm", "-r", paste(inputDir, "/mxe_outputs/sp-", spID, sep="")))
		}
		
		inProjClimDir <- paste(inputDir, "/climate_data", sep="")
		maxentApp <- paste(inputDir, "/maxent333a/maxent.jar", sep="")
		mskDir <- paste(inputDir, "/maskData/AAIGrids", sep="")
		backoutdir <- paste(inputDir, "/background_selection", sep="")
		
		cat("Taxon ", spID, "\n")
	  
		#1. Load species data
	  
		occFile <- paste(inputDir, "/occurrence_files/", spID, ".csv", sep="")
		
		if (file.exists(occFile)) {
		
			#1.1 Load the data
			
			inData <- read.csv(occFile)
			nOcc <- nrow(inData)
			
			if (nOcc >= 0) {
			
				#3. Get background file name
				
				backFileSwd <- paste(backoutdir, "/background_swd.csv", sep="")
				
				#4. Train the maxent model
				
				outFileName <- occFile
				
				outFolder <- paste(inputDir, "/mxe_outputs", sep="")
				if (!file.exists(outFolder)) {
				  dir.create(outFolder)
				}
				
				outName <- paste(outFolder, "/sp-", spID, sep="")
				if (!file.exists(outName)) {
					dir.create(outName)
					dir.create(paste(outName, "/crossval", sep=""))
					dir.create(paste(outName, "/projections", sep=""))
					dir.create(paste(outName, "/metrics", sep=""))
				}
				
				cat("Crossvalidating the model... \n")
				system(paste("java", "-mx512m", "-jar", maxentApp, "-s", outFileName, "-e", backFileSwd, "-o", paste(outName, "/crossval", sep=""), "-P", "replicates=25", "replicatetype=crossvalidate", "nowarnings", "-a", "-z"), wait=TRUE)
				
				if (file.exists(paste(outName, "/crossval/", spID,".html", sep=""))) {
					cat("Model done successfully!", "\n")
					procSwitch <- T
					
					#Determine how many folds were finally performed
					datafile <- read.csv(paste(outName, "/crossval/maxentResults.csv", sep=""))
					nFolds <- nrow(datafile) - 1
					
				} else {
					cat("Error in computing... erasing the folder \n")
					system(paste("rm", "-rv", outName))
					procSwitch <- F
				}
				
				#5. Getting the metrics
				
				if (procSwitch) {
					out <- getMetrics(paste(outName, "/crossval", sep=""), paste(spID), 25, paste(outName, "/metrics", sep=""))
				}
				
				#Read the thresholds file
				threshFile <- paste(outName, "/metrics/thresholds.csv", sep="")
				threshData <- read.csv(threshFile)
				
				#7. Projecting the model into the 21 future scenarios
				
				projectionList <- c("WorldClim-2_5min-bioclim")
				
				cat("Projecting the model...", "\n")
				
				prjCount <- 1
				
				for (prj in projectionList) {
					
					cat("Performing ", prj, "\n")
					
					#Project each fold and then calculate the average and standard deviation
					#Then threshold
					
					projLayers <- paste(inProjClimDir, "/", prj, sep="")
					suffix <- gsub("/", "_", prj)
					
					for (fd in 1:nFolds) {
						cat(prjCount,".",sep="")
						fdID <- fd-1
						
						outGrid <- paste(outName, "/projections/", spID, "_", suffix, "_f", fd, sep="")
						lambdaFile <- paste(outName, "/crossval/", spID, "_", fdID, ".lambdas", sep="")
						system(paste("java", "-mx512m", "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
						
						assign(paste("prjRaster-", fd,sep=""), raster(paste(outName, "/projections/", spID, "_", suffix, "_f", fd, ".asc", sep="")))
						
						#Creating the list for the stack
						if (fd == 1) {
							otList <- get(paste("prjRaster-", fd,sep=""))
						} else {
							otList <- c(otList, get(paste("prjRaster-", fd,sep="")))
						}
					}
					cat("\n")
					fun <- function(x) { sd(x) }
					distMean <- mean(stack(otList))
					distStdv <- calc(stack(otList), fun)
					
					#Writing this two rasters
					distMean <- writeRaster(distMean, paste(outName, "/projections/", spID, "_", suffix, "_EMN.asc", sep=""), format="ascii", overwrite=T)
					distStdv <- writeRaster(distMean, paste(outName, "/projections/", spID, "_", suffix, "_ESD.asc", sep=""), format="ascii", overwrite=T)
					
					#Thresholding and cutting to native areas
					
					thslds <- c("UpperLeftROC")
					
					thrNames <- names(threshData)
					thePos <- which(thrNames == thslds)
					theVal <- threshData[1,thePos]
					
					cat("Thresholding... \n")
					
					distMeanPR <- distMean
					distMeanPR[which(distMeanPR[] < theVal)] <- NA
					
					distMeanPA <- distMean
					distMeanPA[which(distMeanPA[] < theVal] <- 0
					distMeanPA[which(distMeanPA[] != 0] <- 1
					
					distStdvPR <- distStdv * distMeanPA
					
					#Now cut to native areas
					
					#Writing these two rasters
					
					
					
					if (file.exists(paste(outGrid, ".asc", sep=""))) {
						cat("Projection is OK!", "\n")
					} else {
						cat("Error in projecting", "\n")
					}
				  
					rm(prjRaster)
					prjCount <- prjCount + 1
				}
				
				#Compressing everything within the projection dir
				
				ftoZIP <- list.files(paste(outName, "/projections/", sep=""), pattern=".asc")
				for (fz in ftoZIP) {
					fName <- paste(outName, "/projections/", fz, sep="")
					if (OSys == "linux") {
						system(paste("gzip", fName))
					} else {
						system(paste("7za", "a", "-tgzip", paste(fName, ".zip", sep=""), fName))
						file.remove(fName)
					}
				}
				
				#Run verification file
				verFile <- paste(outName, "/ps-", spID, ".run", sep="")
				opnFile <- file(verFile, open="w")
				cat("Modeled on", date(), file=opnFile)
				close.connection(opnFile)
				
				#Now copy the files
				if (OSys == "linux") {
					destName <- paste(destDir, "/mxe_outputs/.", sep="")
					system(paste("cp", "-rvf", outName, destName))
					system(paste("rm", "-rvf", outName))
				} else {
					destName <- paste(destDir, "/mxe_outputs/sp-", spID, sep="")
					idir <- gsub("/", "\\\\", outName)
					odir <- gsub("/", "\\\\", destName)
					system(paste("xcopy", "/E", "/I", idir, odir))
					system(paste("rm", "-r", outName))
				}
				
				return("Done")
			} else {
				cat("Species with less than 10 datapoints, not to be modeled \n")
			}
		} else {
			cat("The occurrence file does not exist! \n")
		}
	} else {
		cat("The species was already modeled \n")
	}
}

#Initial stuff

#setOptions(overwrite=T)
#idir <- "C:/CIAT_work/COP_CONDESAN"
#ddir <- "/mnt/GeoData/COP_CONDESAN"
#outp <- NagoyaProcess(idir, ddir, 1, 10, OSys="NT")
#setOptions(overwrite=T)

NagoyaProcess <- function(inputDir, destDir, ini, fin, OSys="LINUX") {
	
	ufile <- paste(inputDir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
	ufile <- read.csv(ufile)
	
	if (fin > nrow(ufile)) {
		cat("Final number is greater than nrow, using nrows instead \n")
		fin <- nrow(ufile)
	}
	
	spList <- ufile$IDSpecies[ini:fin]
	sppC <- 1
	
	for (sp in spList) {
		cat("\n")
		cat("...Species", sp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
		out <- theEntireProcess(sp, OSys, inputDir, destDir)
		sppC <- sppC + 1
	}
	
	return("Done!")
	
}