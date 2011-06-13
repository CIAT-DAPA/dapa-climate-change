#Summarise metrics at the country level

#folder structure:
#/climate-data-assessment/comparisons/results/$dataset-vs-gcm/$period/$iso/$model

#Dataset: ghcn|gsod|cru|wcl|wclst
#Period: ANNUAL|JJA|DJF
#Iso: ETH | TZA | KEN | UGA | GHA | SEN | MLI | NER | BFA | IND | BGD | NPL
#Model: bccr_bcm2_0 | cccma_cgcm3_1_t47 | cccma_cgcm3_1_t63 | ...

#Files:
#metrics-$variable.csv
#plotData-$variable.csv

#File structure (metrics-$variable.csv):
#MONTH,VALUE,N,R2.FORCED,ADJ.R2.FORCED,P.VALUE.FORCED,SLOPE.FORCED,INTERCEPT.FORCED,F.STAT.FORCED,R2,ADJ.R2,P.VALUE,SLOPE,INTERCEPT,F.STAT,ERROR
#From this file the values will be harvested to get a file where all GCMs and regions are stored so that a boxplot can be done

#File structure (plotData-$variable.csv)
#MONTH,GCM,CL.M,CL.X,CL.N
#This file should be used to get regional metrics by joining countries (WAF, EAF, IGP, ALL)

#Lists of countries
isoALL <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
isoWAF <- c("GHA", "SEN", "MLI", "NER", "BFA")
isoEAF <- c("ETH", "KEN", "TZA", "UGA")
isoIGP <- c("IND", "BGD", "NPL")

#Various input parameters
#From JJA and DJF take totals, and from ANNUAL take all, so first load ANNUAL and then JJA and DJF
#Loop into all countries to get the metrics into a single file

#This is a function to gather metrics into a single file

####################################################
#With the first function you should be able to get all metrics to do a boxplot where the boxes are countries and the values are the 
#R2, Slope, Pvalue, and RMSE

####################################################
#With the second function you should be able to get values for a X-Y dispersion diagram using all totals, or for a single month
#Also, do a dispersion diagram for a particular country, or for a set of countries

metricsSummary <- function(bDir, dataset, variable) {
	periodList <- c("ANNUAL","JJA","DJF")
	datasetDir <- paste(bDir, "/", dataset, "-vs-gcm", sep="")
	outDir <- paste(datasetDir, "/summary", sep=""); if (!file.exists(outDir)) dir.create(outDir)
	
	#Loop through periods
	counter <- 1
	for (period in periodList) {
		periodDir <- paste(datasetDir, "/", period, sep="")
		
		#Loop through isos
		for (iso in isoALL) {
			isoDir <- paste(periodDir, "/", iso, sep="")
			
			#Listing and looping through models
			mList <- list.files(isoDir)
			for (model in mList) {
				mDir <- paste(isoDir, "/", model, sep="")
				
				cat("Summarising for", model, iso, period, "\n")
				
				#Open file, conditinally select all rows, or only totals (JJA/DJF cases)
				cat("Reading in data \n")
				metData <- read.csv(paste(mDir, "/metrics-", variable, ".csv", sep=""))
				
				if (period != "ANNUAL") {
					metData <- metData[which(metData$MONTH == "total"),]
				}
				
				#Adding identification fields
				cat("Adding identification fields \n")
				metData <- cbind(PERIOD=rep(period,nrow(metData)), ISO=rep(iso,nrow(metData)), MODEL=rep(model,nrow(metData)),metData)
				
				cat("Rbinding \n")
				if (counter == 1) {
					summaryData <- metData
				} else {
					summaryData <- rbind(summaryData, metData)
				}
				
				counter <- counter+1
			}
		}
	}

	outFile <- paste(outDir, "/", variable, "-", dataset, "-vs-gcm-summaryMetrics.csv", sep="")
	write.csv(summaryData,outFile,row.names=F,quote=F)
}

#This function gathers all the data that was used for getting the metrics in a single file 
#(all GCMs and countries in the same file for further plotting)

dataSummary <- function(bDir, dataset, variable) {
	periodList <- c("ANNUAL","JJA","DJF")
	datasetDir <- paste(bDir, "/", dataset, "-vs-gcm", sep="")
	outDir <- paste(datasetDir, "/summary", sep=""); if (!file.exists(outDir)) dir.create(outDir)
	
	#Loop through periods
	counter <- 1
	for (period in periodList) {
		periodDir <- paste(datasetDir, "/", period, sep="")
		
		#Loop through isos
		for (iso in isoALL) {
			isoDir <- paste(periodDir, "/", iso, sep="")
			
			#Listing and looping through models
			mList <- list.files(isoDir)
			for (model in mList) {
				mDir <- paste(isoDir, "/", model, sep="")
				
				cat("Summarising for", model, iso, period, "\n")
				
				#Open file, conditinally select all rows, or only totals (JJA/DJF cases)
				cat("Reading in data \n")
				plotData <- read.csv(paste(mDir, "/plotData-", variable, ".csv", sep=""))
				
				if (period != "ANNUAL") {
					plotData <- plotData[which(plotData$MONTH == "total"),]
				}
				
				#Adding identification fields
				cat("Adding identification fields \n")
				plotData <- cbind(PERIOD=rep(period,nrow(plotData)), ISO=rep(iso,nrow(plotData)), MODEL=rep(model,nrow(plotData)),plotData)
				
				cat("Rbinding \n")
				if (counter == 1) {
					summaryData <- plotData
				} else {
					summaryData <- rbind(summaryData, plotData)
				}
				
				counter <- counter+1
			}
		}
	}

	outFile <- paste(outDir, "/", variable, "-", dataset, "-vs-gcm-allPlotData.csv", sep="")
	write.csv(summaryData,outFile,row.names=F,quote=F)
	return(summaryData)
}

#Now the function to get the same metrics over a set of countries for a particular period or month

source("accuracy.R")

regionalMetrics <- function(bDir="", region="WAF", dataset="wcl", variable="prec", period="ANNUAL", month="TOTAL") {
	summaryDir <- paste(bDir, "/summary", sep="")
	inFile <- paste(summaryDir, "/", variable, "-", dataset, "-vs-gcm-allPlotData.csv", sep="")
	
	cat("Reading in data \n")
	dataAll <- read.csv(inFile)
	
	isoList <- get(paste("iso", region, sep=""))
	
	#Selecting period and month
	cat("Reselecting month and period \n")
	dataSelected <- dataAll[which(dataAll$PERIOD == period & dataAll$MONTH == month),]
	
	#Selecting countries
	cat("Sub-selecting countries \n")
	for (iso in isoList) {
		
		dataIsoSel <- dataSelected[which(dataSelected$ISO == iso),]
		if (iso == isoList[1]) {
			isoOutData <- dataIsoSel
		} else {
			isoOutData <- rbind(isoOutData, dataIsoSel)
		}
	}
	
	#Now calculate the metrics for each GCM
	cat("Calculating gcm metrics \n")
	gcmList <- unique(isoOutData$MODEL)
	for (gcm in gcmList) {
		gcmInData <- isoOutData[which(isoOutData$MODEL == gcm),]
		outMetrics <- accuracy(gcmInData, verbose=T, plotit=F, plotName=NA, plotDir=NA)
		outMetrics <- cbind(PERIOD=rep(period,nrow(outMetrics)), ISO=rep(region,nrow(outMetrics)), MODEL=rep(gcm,nrow(outMetrics)),outMetrics)
		
		if (gcm == gcmList[1]) {
			metData <- outMetrics
		} else {
			metData <- rbind(metData, outMetrics)
		}
	}
	
	cat("Writing output file \n")
	outFile <- paste(summaryDir, "/", variable, "-", region, "-", dataset, "-vs-gcm-metrics.csv", sep="")
	write.csv(metData, outFile, quote=F, row.names=F)
}
