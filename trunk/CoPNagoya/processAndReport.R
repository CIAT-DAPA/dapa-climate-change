require(rgdal)
require(raster)
require(maptools)
gpclibPermit()

source("zipRead.R")
wdir <- getwd()

#Creating the function

report <- function(idir, spID, shp) {
	cat("\n")
	
	#Defining and checking the runsDir
	runsDir <- paste(idir, "/mxe_outputs", sep="")
	if (!file.exists(runsDir)) {
		stop("Invalid input directory, please check")
	}
	
	#Defining and checking the species dir
	sppDir <- paste(runsDir, "/sp-", spID, sep="")
	
	if (file.exists(sppDir)) {
		cat("Processing species", spID, "\n")
		#Defining and creating output folder
		
		odir <- paste(sppDir, "/report", sep="")
		
		if (file.exists(paste(odir, "/report.pdf", sep=""))) {
			system(paste("rm", "-rf", odir))
		}
		
		if (!file.exists(odir)) {
			dir.create(odir)
		}

		cat("Building metrics and thresholds files... \n")
		
		#Read the list of species to gather names

		sppList <- read.csv(paste(idir, "/occurrences/modeling-data/speciesListToModel.csv", sep=""))
		spName <- sppList$Species[which(sppList$IDSpecies == spID)]

		#First read and Organize the metrics file to get only AUCs and number of samples

		metrics <- read.csv(paste(sppDir, "/metrics/metrics.csv", sep=""))

		outMetrics <- data.frame(ID=spID, TAXON=spName)
		outMetrics$TOTAL.SAMPLES = metrics$NSamples
		outMetrics$TRAIN.SAMPLES = metrics$TrainSamples
		outMetrics$TEST.SAMPLES = metrics$TestSamples
		outMetrics$TRAIN.AUC = metrics$TrainAUC
		outMetrics$TRAIN.AUC.SD = metrics$TrainAUCSD
		outMetrics$TEST.AUC = metrics$TestAUC
		outMetrics$TEST.AUC.SD = metrics$TestAUCSD
		outMetrics$AUC = metrics$AUC

		rnms <- names(outMetrics)
		outMetrics <- as.data.frame(t(outMetrics))
		row.names(outMetrics) <- rnms
		names(outMetrics) <- c("VALUE")
		write.csv(outMetrics, paste(odir, "/metrics.csv", sep=""), quote=F, row.names=T)

		#Read the thresholds file

		thresholds <- read.csv(paste(sppDir, "/metrics/thresholds.csv", sep=""))
		rnms <- names(thresholds)
		thresholds <- as.data.frame(t(thresholds))
		row.names(thresholds) <- rnms
		names(thresholds) <- c("VALUE")
		write.csv(thresholds, paste(odir, "/thresholds.csv", sep=""), quote=F, row.names=T)

		#Now organize the impact metrics file

		imetrics <- read.csv(paste(sppDir, "/metrics/ImpactMetrics.csv", sep=""))

		for (i in 1:nrow(imetrics)) {
			scen <- paste(imetrics$SRES[i], "-", gsub("_", "-", imetrics$PERIOD[i]), "-", imetrics$THRESH[i], "-", imetrics$MIGSCEN[i], sep="")
			
			areaChg <- (imetrics$AreaFuture[i] - imetrics$AreaBaseline[i]) / (imetrics$AreaBaseline[i]) * 100
			areaInc <- imetrics$AreaIncrease[i] / imetrics$AreaBaseline[i] * 100
			areaDec <- imetrics$AreaDecrease[i] / imetrics$AreaBaseline[i] * 100
			
			probChg <- (imetrics$MPFutureCorresp[i] - imetrics$MPBaseline[i]) / (imetrics$MPBaseline[i]) * 100
			
			resRow <- data.frame(SCENARIO=scen, ARC=areaChg, RI=areaInc, RD=areaDec, PC=probChg)
			if (i == 1) {
				outIM <- resRow
			} else {
				outIM <- rbind(outIM, resRow)
			}
		}

		write.csv(outIM, paste(odir, "/impactMetrics.csv", sep=""), quote=F, row.names=F)

		#Read the occurrence points

		occPts <- read.csv(paste(sppDir, "/model/", spID, "_samplePredictions.csv", sep=""))

		#Read the shapefile (Andean countries)

		cat("Reading world shapefile... \n")
		worldshapefile <- shp
		sh <- readShapePoly(worldshapefile)

		#Read the current dist and the current buffered dists (prev. and tenP.)

		cat("Plotting current distributions... \n")

		ext <- "zip"
		fName <- paste(spID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000.asc.", ext, sep="")
		if (!file.exists(paste(sppDir, "/projections/", fName, sep=""))) {
			ext <- "gz"
			fName <- paste(spID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000.asc.", ext, sep="")
		}

		currProbs <- zipRead(paste(sppDir, "/projections", sep=""), fName)

		png(paste(odir, "/Current-probability.png", sep=""), width=((currProbs@extent@xmax - currProbs@extent@xmin)*20), height=((currProbs@extent@ymax - currProbs@extent@ymin)*20), pointsize=15)
		plot(currProbs, col=c('gray', colorRampPalette(c("yellow", "red"))(100)), zlim=c(0,1))
		points(occPts$X, occPts$Y, col='black', pch=20, cex=0.8)
		plot(sh, add=T)
		dev.off()

		fName <- paste(spID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_Prevalence.asc.", ext, sep="")
		binPrev <- zipRead(paste(sppDir, "/projections", sep=""), fName)
		PrevProbs <- currProbs * binPrev
		png(paste(odir, "/Current-probability-prev.png", sep=""), width=((currProbs@extent@xmax - currProbs@extent@xmin)*20), height=((currProbs@extent@ymax - currProbs@extent@ymin)*20), pointsize=15)
		plot(PrevProbs, col=c('gray', colorRampPalette(c("yellow", "red"))(100)), zlim=c(0,1))
		plot(sh, add=T)
		dev.off()

		fName <- paste(spID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_TenPercentile.asc.", ext, sep="")
		binTenp <- zipRead(paste(sppDir, "/projections", sep=""), fName)
		TenpProbs <- currProbs * binTenp
		png(paste(odir, "/Current-probability-tenp.png", sep=""), width=((currProbs@extent@xmax - currProbs@extent@xmin)*20), height=((currProbs@extent@ymax - currProbs@extent@ymin)*20), pointsize=15)
		plot(TenpProbs, col=c('gray', colorRampPalette(c("yellow", "red"))(100)), zlim=c(0,1))
		plot(sh, add=T)
		dev.off()

		#Read and plot the future distributions (probabilities and both thresholds, for A2 and A1B, 2020, and 2050)

		listMatrix = expand.grid(SCN=c('SRES_A2','SRES_A1B'),YR=c("2010_2039","2040_2069"),THR=c("Prevalence", "TenPercentile"),MIG=c("FullAdap", "NullAdap"))
		listMatrix <- paste("future_", listMatrix$SCN, "_disaggregated_", listMatrix$YR, "_", listMatrix$THR, "_", listMatrix$MIG, sep="")

		cat("Plotting future distributions... \n")
		for (scn in listMatrix) {
			probsName <- unlist(strsplit(scn, "_", fixed=T))[1:6]
			probsName <- paste(probsName[1], probsName[2], probsName[3], probsName[4], probsName[5], probsName[6], sep="_")
			probsFileName <- paste(spID, "_", probsName, ".asc.", ext, sep="")
			
			probs <- zipRead(paste(sppDir, "/projections", sep=""), probsFileName)
			
			png(paste(odir, "/", probsName, ".png", sep=""), width=((probs@extent@xmax - probs@extent@xmin)*20), height=((probs@extent@ymax - probs@extent@ymin)*20), pointsize=15)
			plot(probs, col=c('gray', colorRampPalette(c("yellow", "red"))(100)), zlim=c(0,1))
			plot(sh, add=T)
			dev.off()
			
			fName <- paste(spID, "_", scn, ".asc.", ext, sep="")
			rs <- zipRead(paste(sppDir, "/projections", sep=""), fName)
			rs <- rs * probs
			png(paste(odir, "/", scn, ".png", sep=""), width=((rs@extent@xmax - rs@extent@xmin)*20), height=((rs@extent@ymax - rs@extent@ymin)*20), pointsize=15)
			plot(rs, col=c('gray', colorRampPalette(c("yellow", "red"))(100)), zlim=c(0,1))
			plot(sh, add=T)
			dev.off()
		}

		cat("Sweaving and pdflatex... \n")
		file.copy("report.Rnw", paste(odir, "/report.Rnw", sep=""))
		setwd(odir)
		Sweave("report.Rnw")
		system(paste("R", "CMD", "pdflatex", "-quiet", "report.tex"))
		setwd(wdir)
	} else {
		cat("The species was found to not exist, check or try again, or both?")
	}
	return(paste(odir, "/report.pdf", sep=""))
}