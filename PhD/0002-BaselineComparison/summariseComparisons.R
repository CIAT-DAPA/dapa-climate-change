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
        if (file.exists(paste(mDir,"/metrics-",variable,".csv",sep=""))) {
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
	}

	outFile <- paste(outDir, "/", variable, "-", dataset, "-vs-gcm-summaryMetrics.csv", sep="")
	write.csv(summaryData,outFile,row.names=F,quote=F)
  return(summaryData)
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
        if (file.exists(paste(mDir, "/plotData-", variable, ".csv", sep=""))) {
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


#Boxplots
boxplots.cru_wcl.ISO <- function(fDir="C:/CIAT_work/CCAFS/baselineComparison/metrics", variable="prec") {
	inFile.cru <- paste(fDir, "/", variable, "-cru-vs-gcm-summaryMetrics.csv", sep="")
	inFile.wcl <- paste(fDir, "/", variable, "-wcl-vs-gcm-summaryMetrics.csv", sep="")

	cruValues <- read.csv(inFile.cru)
	wclValues <- read.csv(inFile.wcl)
	allValues <- rbind(cruValues, wclValues)
  
  if (!file.exists(paste(fDir,"/figures",sep=""))) {dir.create(paste(fDir,"/figures",sep=""))}
  if (!file.exists(paste(fDir,"/figures/merged",sep=""))) {dir.create(paste(fDir,"/figures/merged",sep=""))}
  
	values.ann <- allValues[which(allValues$PERIOD == "ANNUAL" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
	values.djf <- allValues[which(allValues$PERIOD == "DJF" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
	values.jja <- allValues[which(allValues$PERIOD == "JJA" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]

	for (period in c("ann","djf","jja")) {
		values <- get(paste("values.", period, sep=""))
		
		tiff(paste(fDir, "/figures/merged/fig-ISO-",variable, "-", period, "-cru_wcl-vs-gcm_N.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2)
		boxplot(values$N~values$ISO, ylab="Number of observations", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-ISO-",variable, "-", period, "-cru_wcl-vs-gcm_R2.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2)
		boxplot(values$R2.FORCED~values$ISO, ylab="R-square", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-ISO-",variable, "-", period, "-cru_wcl-vs-gcm_SLOPE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2)
		boxplot(values$SLOPE.FORCED~values$ISO, ylab="Slope", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-ISO-",variable, "-", period, "-cru_wcl-vs-gcm_RMSE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2)
		boxplot(values$ERROR~values$ISO, ylab="RMSE", pch=20, col='white')
		dev.off()
	}
}

#Boxplots for CRU and WCL comparisons merged
boxplots.cru_wcl.GCM <- function(fDir="C:/CIAT_work/CCAFS/baselineComparison/metrics", variable="prec") {
	inFile.cru <- paste(fDir, "/", variable, "-cru-vs-gcm-summaryMetrics.csv", sep="")
	inFile.wcl <- paste(fDir, "/", variable, "-wcl-vs-gcm-summaryMetrics.csv", sep="")
  
  if (!file.exists(paste(fDir,"/figures",sep=""))) {dir.create(paste(fDir,"/figures",sep=""))}
  if (!file.exists(paste(fDir,"/figures/merged",sep=""))) {dir.create(paste(fDir,"/figures/merged",sep=""))}
  
	cruValues <- read.csv(inFile.cru)
	wclValues <- read.csv(inFile.wcl)
	allValues <- rbind(cruValues, wclValues)

	values.ann <- allValues[which(allValues$PERIOD == "ANNUAL" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
	values.djf <- allValues[which(allValues$PERIOD == "DJF" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
	values.jja <- allValues[which(allValues$PERIOD == "JJA" & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]

	for (period in c("ann","djf","jja")) {
		values <- get(paste("values.", period, sep=""))
		
		tiff(paste(fDir, "/figures/merged/fig-GCM-",variable, "-", period, "-cru_wcl-vs-gcm_N.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2, mar=c(11, 5, 1, 1))
		boxplot(values$N~values$MODEL, ylab="Number of observations", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-GCM-",variable, "-", period, "-cru_wcl-vs-gcm_R2.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2, mar=c(11, 5, 1, 1))
		boxplot(values$R2.FORCED~values$MODEL, ylab="R-square", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-GCM-",variable, "-", period, "-cru_wcl-vs-gcm_SLOPE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2, mar=c(11, 5, 1, 1))
		boxplot(values$SLOPE.FORCED~values$MODEL, ylab="Slope", pch=20, col='white')
		dev.off()

		tiff(paste(fDir, "/figures/merged/fig-GCM-",variable, "-", period, "-cru_wcl-vs-gcm_RMSE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
		par(las=2, mar=c(11, 5, 1, 1))
		boxplot(values$ERROR~values$MODEL, ylab="RMSE", pch=20, col='white')
		dev.off()
	}
}


#Boxplot where countries are x-axis
boxplots.ISO <- function(fDir="", variable="prec", dataset="wcl", period="ANNUAL") {
	#Reading in data
	inFile <- paste(fDir, "/", variable, "-", dataset, "-vs-gcm-summaryMetrics.csv", sep="")
	allValues <- read.csv(inFile)
  
  if (!file.exists(paste(fDir,"/figures",sep=""))) {dir.create(paste(fDir,"/figures",sep=""))}
  
	#Sub-selecting desired data
	values <- allValues[which(allValues$PERIOD == period & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
	
	#Plotting
	tiff(paste(fDir, "/figures/fig-ISO-",variable, "-", period, "-", dataset, "-vs-gcm_N.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2)
	boxplot(values$N~values$ISO, ylab="Number of observations", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-ISO-",variable, "-", period, "-", dataset, "-vs-gcm_R2.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2)
	boxplot(values$R2.FORCED~values$ISO, ylab="R-square", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-ISO-",variable, "-", period, "-", dataset, "-vs-gcm_SLOPE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2)
	boxplot(values$SLOPE.FORCED~values$ISO, ylab="Slope", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-ISO-",variable, "-", period, "-", dataset, "-vs-gcm_RMSE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2)
	boxplot(values$ERROR~values$ISO, ylab="RMSE", pch=20, col='white')
	dev.off()
}

#Boxplot where GCMs are x-axis
boxplots.GCM <- function(fDir="", variable="prec", dataset="wcl", period="ANNUAL") {
	#Reading in data
	inFile <- paste(fDir, "/", variable, "-", dataset, "-vs-gcm-summaryMetrics.csv", sep="")
	allValues <- read.csv(inFile)
  
  if (!file.exists(paste(fDir,"/figures",sep=""))) {dir.create(paste(fDir,"/figures",sep=""))}
  
	#Sub-selecting desired data
	values <- allValues[which(allValues$PERIOD == period & allValues$MONTH == "total" & allValues$VALUE == "MEAN"),]
  
	#Plotting
	tiff(paste(fDir, "/figures/fig-GCM-",variable, "-", period, "-", dataset, "-vs-gcm_N.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2, mar=c(11, 5, 1, 1))
	boxplot(values$N~values$MODEL, ylab="Number of observations", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-GCM-",variable, "-", period, "-", dataset, "-vs-gcm_R2.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2, mar=c(11, 5, 1, 1))
	boxplot(values$R2.FORCED~values$MODEL, ylab="R-square", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-GCM-",variable, "-", period, "-", dataset, "-vs-gcm_SLOPE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2, mar=c(11, 5, 1, 1))
	boxplot(values$SLOPE.FORCED~values$MODEL, ylab="Slope", pch=20, col='white')
	dev.off()
	
	tiff(paste(fDir, "/figures/fig-GCM-",variable, "-", period, "-", dataset, "-vs-gcm_RMSE.tif",sep=""), pointsize=6, width=900, height=1000, units="px", compression="lzw",bg="white",res=300)
	par(las=2, mar=c(11, 5, 1, 1))
	boxplot(values$ERROR~values$MODEL, ylab="RMSE", pch=20, col='white')
	dev.off()
}

generateBoxplots <- function(fd="C:/CIAT_work/CCAFS/baselineComparison/metrics") {
	for (vr in c("prec","tmean","dtr")) {
		for (dset in c("wcl","cru","wclst","ghcn","gsod")) {
      if (file.exists(paste(fd,"/",vr,"-",dset,"-vs-gcm-summaryMetrics.csv",sep=""))) {
  			for (prd in c("ANNUAL","JJA","DJF")) {
  				boxplots.ISO(fDir=fd, variable=vr, dataset=dset, period=prd)
  				boxplots.GCM(fDir=fd, variable=vr, dataset=dset, period=prd)
  			}
      }
		}
	}
}

createColoured <- function(fDir="C:/CIAT_work/CCAFS/baselineComparison/metrics", variable="prec", dataset="wclst", month="total", period="ANNUAL", metric="R2.FORCED") {
	inFile <- paste(fDir, "/", variable, "-", dataset, "-vs-gcm-summaryMetrics.csv", sep="")
	allValues <- read.csv(inFile)
	col.metric <- which(names(allValues) == metric)
	
	#Sub-selecting desired data
	values <- allValues[which(allValues$PERIOD == period & allValues$MONTH == month & allValues$VALUE == "MEAN"),]
	for (iso in unique(values$ISO)) {
		values.iso <- values[which(values$ISO == iso),]
		values.iso <- data.frame(X=values.iso$MODEL, Y=values.iso[,col.metric])
		names(values.iso) <- c("GCM",paste(iso))
		
		if (iso == unique(values$ISO)[1]) {
			output <- values.iso
		} else {
			output <- merge(output,values.iso)
		}
	}

	write.csv(output, paste(fDir,"/",variable,"-",dataset,"-vs-gcm-coloured.csv",sep=""), row.names=F, quote=F)
}
