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
cat("XXXXXXXX CoP 10 at Nagoya (Japan) XXXXXXXXXX \n")
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
# The select background area function
###############################################################################################
###############################################################################################

source("selectBackgroundArea.R")

###############################################################################################
###############################################################################################
# The select background area function
###############################################################################################
###############################################################################################

source("getMetrics.R")

###############################################################################################
###############################################################################################
#Creating the big function
###############################################################################################
###############################################################################################

theEntireProcess <- function(spID, OSys, inputDir) {
  
	backgroundDir <- paste(inputDir, "/backgrounds", sep="")
	inProjClimDir <- paste(inputDir, "/climateData/andes", sep="")
	maxentApp <- paste(inputDir, "/maxent332/maxent.jar", sep="")
	mskDir <- paste(inputDir, "/maskData/AAIGrids", sep="")
	
	cat("Taxon ", spID, "\n")
  
  #1. Load species data
  
  occFile <- paste(inputDir, "/occurrences/splitted-occurrence-files/", spID, ".csv", sep="")
  
  if (file.exists(occFile)) {
	
	#1.1 Load the data
    
	inData <- read.csv(occFile)
	nOcc <- nrow(inData)
	
    #3. Select background area
    
	msk <- paste(mskDir, "/and0_ctr_30s.asc", sep="")
	backoutdir <- paste(inputDir, "/background-files", sep="")
	
	if (!file.exists(backoutdir)) {
		dir.create(backoutdir)
	}
	
    backFile <- paste(backoutdir, "/bg-", spID, ".csv", sep="")
    backGround <- selectBack(occFile, backFile, msk, backgroundDir)
    backFileSwd <- paste(backoutdir, "/bg-", spID, ".csv", sep="")
	
    #4. Train the maxent model
    
	outFileName <- occFile
	
    outFolder <- paste(inputDir, "/mxe_outputs", sep="")
    if (!file.exists(outFolder)) {
      dir.create(outFolder)
    }
    
    outName <- paste(outFolder, "/sp-", spID, sep="")
    if (!file.exists(outName)) {
      dir.create(outName)
      dir.create(paste(outName, "/model", sep=""))
      dir.create(paste(outName, "/crossval", sep=""))
      dir.create(paste(outName, "/projections", sep=""))
      dir.create(paste(outName, "/metrics", sep=""))
    }
    
    if (nOcc >= 40) {
		cat("Fitting the model... (10 VAR) \n")
		system(paste("java", "-mx512m", "-jar", maxentApp, "-s", outFileName, "-e", backFileSwd, "-o", paste(outName, "/model", sep=""), "-P", "-X", "0", "nowarnings", "-a", "-z"), wait=TRUE)
		
		cat("Crossvalidating the model...(10 VAR) \n")
		system(paste("java", "-mx512m", "-jar", maxentApp, "-s", outFileName, "-e", backFileSwd, "-o", paste(outName, "/crossval", sep=""), "-P", "replicates=10", "replicatetype=crossvalidate", "nowarnings", "-a", "-z"), wait=TRUE)
	} else {
		cat("Fitting the model... (6 VAR)\n")
		system(paste("java", "-mx512m", "-jar", maxentApp, "-s", outFileName, "-e", backFileSwd, "-o", paste(outName, "/model", sep=""), "-N bio_5 -N bio_6 -N bio_16 -N bio_17", "-P", "-X", "0", "nowarnings", "-a", "-z"), wait=TRUE)
		
		cat("Crossvalidating the model... (6 VAR)\n")
		system(paste("java", "-mx512m", "-jar", maxentApp, "-s", outFileName, "-e", backFileSwd, "-o", paste(outName, "/crossval", sep=""), "-N bio_5 -N bio_6 -N bio_16 -N bio_17", "-P", "replicates=10", "replicatetype=crossvalidate", "nowarnings", "-a", "-z"), wait=TRUE)
	}
    
    if (file.exists(paste(outName, "/model/", spID,".lambdas", sep=""))) {
      cat("Model done successfully!", "\n")
    } else {
      cat("Error in computing \n")
    }
    
    #5. Getting the metrics
    
    out <- getMetrics(paste(outName, "/crossval", sep=""), paste(spID), 10, paste(outName, "/model", sep=""), paste(outName, "/metrics", sep=""))
    
    #9. Create the buffer area
    msk <- paste(mskDir, "/and0_25m.asc", sep="")
    bufferOutGrid <- paste(outName, "/projections/buf-", spID, ".asc", sep="")
    bfo <- createBuffers(occFile, bufferOutGrid, 300000, msk)
    bufferRaster <- raster(bufferOutGrid)
    
    threshFile <- paste(outName, "/metrics/thresholds.csv", sep="")
    threshData <- read.csv(threshFile)
    
    #7. Projecting the model into the 21 future scenarios
    
    projectionList <- c("baseline/20C3M/WorldClim-2_5min-bioclim/1950_2000","future/SRES_A1B/disaggregated/2010_2039","future/SRES_A1B/disaggregated/2040_2069","future/SRES_A2/disaggregated/2010_2039","future/SRES_A2/disaggregated/2040_2069")
    
    cat("Projecting the model...", "\n")
    
    prjCount <- 1
    
    for (prj in projectionList) {
      
      cat("Performing ", prj, "\n")
      
      projLayers <- paste(inProjClimDir, "/", prj, sep="")
      
      suffix <- gsub("/", "_", prj)
      outGrid <- paste(outName, "/projections/", spID, "_", suffix, sep="")
      
      lambdaFile <- paste(outName, "/model/", spID, ".lambdas", sep="")
      
      system(paste("java", "-mx512m", "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
       
      thslds <- c("TenPercentile_1", "Prevalence_3")
      
      prjRaster <- raster(paste(outName, "/projections/", spID, "_", suffix, ".asc", sep=""))
      
      cat("Thresholding and buffering... \n")
      
      procThr <- 1
      for(thr in thslds) {
        
        theName <- strsplit(thr, "_")[[1]][1]
        thePos <- as.numeric(strsplit(thr, "_")[[1]][2])
          
        theVal <- threshData[1,thePos]
        
        #Multi threshold PA surfaces for baseline
        if (prjCount == 1) {
			cat("...", theName, "\n")
			
			theRaster <- prjRaster
			theRaster <- theRaster * bufferRaster
          
			theRaster[which(theRaster[] < theVal)] <- 0
			theRaster[which(theRaster[] != 0)] <- 1
			
			outRsName <- paste(outName, "/projections/", spID, "_", suffix, "_", theName, ".asc", sep="")
			theRaster <- writeRaster(theRaster, outRsName, overwrite=T, format='ascii')
			rm(theRaster)
          
        } else {
        #Multi threshold PA surfaces for future scenarios (two mig. scenarios)
          
          #Null adaptation
          
          cat("...", theName, "\n")
          
          theRaster <- prjRaster
          theRaster <- theRaster * bufferRaster
          
          theRaster[which(theRaster[] < theVal)] <- 0
    		  theRaster[which(theRaster[] != 0)] <- 1
    		  
    		  outRsName <- paste(outName, "/projections/", spID, "_", suffix, "_", theName, "_NullAdap.asc", sep="")
    		  theRaster <- writeRaster(theRaster, outRsName, overwrite=T, format='ascii')
    		  rm(theRaster)
    		  
    		  #Full adaptation
    		  
    		  theRaster <- prjRaster
    		  theRaster[which(theRaster[] < theVal)] <- 0
    		  theRaster[which(theRaster[] != 0)] <- 1
    		  
    		  outRsName <- paste(outName, "/projections/", spID, "_", suffix, "_", theName, "_FullAdap.asc", sep="")
    		  theRaster <- writeRaster(theRaster, outRsName, overwrite=T, format='ascii')
    		  rm(theRaster)
        }
        procThr <- procThr + 1
      }
      
      if (file.exists(paste(outGrid, ".asc", sep=""))) {
        cat("Projection is OK!", "\n")
      } else {
        cat("Error in projecting", "\n")
      }
      
      rm(prjRaster)
      prjCount <- prjCount + 1
    }
	
	#Compressing everything within the projection dir
	
	OSys <- tolower(OSys)
	
	ftoZIP <- list.files(paste(outName, "/projections/", sep=""), pattern=".asc")
	for (fz in ftoZIP) {
		fName <- paste(outName, "/projections/", fz, sep="")
		if (OSys == "linux") {
			system(paste("gzip", fName))
		} else {
			system(paste("7za", "a", "-tzip", paste(fName, ".zip", sep=""), fName))
			file.remove(fName)
		}
	}
    
    return("Done")
  } else {
    cat("The occurrence file does not exist! \n")
  }
}

#Initial stuff

#idir <- "C:/CIAT_work/COP_CONDESAN"
#outp <- NagoyaProcess(idir, 1, 10)

NagoyaProcess <- function(inputDir, ini, fin) {

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
		out <- theEntireProcess(sp, "LINUX", inputDir)
		sppC <- sppC + 1
	}
	
	#Now move the data
	
	return("Done!")
	
}