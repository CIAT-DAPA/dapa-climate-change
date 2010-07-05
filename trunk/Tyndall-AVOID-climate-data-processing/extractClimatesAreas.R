#This was done for the megaprogram stuff that Rao and Steve needed

require(sp)
require(rgdal)
require(raster)
require(BioCalc)

source("wettestQtrPpt-SD.R")
source("wettestQtrTmp-SD.R")
source("driestQtrPpt-SD.R")
source("driestQtrTmp-SD.R")

#E:\AVOID_scenarios\outputBio\20C3M\1925s\CRU3_TS
#E:\AVOID_scenarios\outputBio\A1Bp50\2020s\cccma_cgcm31

#Prepare the masks: BRA(C,N,S), ETH(N,S), MEX(C,N,S), MOZ(C,N,S), MWI(N,S), TZA(N,S), UGA(N,SW)

baseDir <- "C:/CIAT_work/cc_impact_crops/beans/megaprogram-meeting" #"E:/AVOID_scenarios/outputBio"
outDir <- "C:/CIAT_work/cc_impact_crops/beans/megaprogram-meeting/climates" #"E:/megaprogram/climates"

samDir <- paste(baseDir, "/country_region_samples", sep="")

scen <- c("20C3M", "A1Bp50")

for (sc in scen) {
	scDir <- paste(baseDir, "/", sc, sep="")
	scDirOut <- paste(outDir, "/", sc, sep="")
	
	if (!file.exists(scDirOut)) {
		dir.create(scDirOut)
	}

	yrList <- list.files(scDir)

	for (yr in yrList) {
		yrDir <- paste(scDir, "/", yr, sep="")
		yrDirOut <- paste(scDirOut, "/", yr, sep="")
		
		if (!file.exists(yrDirOut)) {
			dir.create(yrDirOut)
		}
		
		modList <- list.files(yrDir, pattern="_")
		
		for (GCM in modList) {
			cat("\n")
			cat(sc, yr, GCM, "\n")
			
			GCMDir <- paste(yrDir, "/", GCM, sep="")
			GCMDirOut <- paste(yrDirOut, "/", GCM, sep="")
			
			if (!file.exists(GCMDirOut)) {
				dir.create(GCMDirOut)
			}
			
			mthRasPpt <- LoadMonthlyFiles(folder=GCMDir, ext=".asc", varbl="prec", format='ascii')
			mthRasTav <- LoadMonthlyFiles(folder=GCMDir, ext=".asc", varbl="tmean", format='ascii')
			
			#Functions
			#wetQtrTmp, driQtrTmp
			rasterName <- paste(GCMDirOut, "/wetQtrTmp-AV.asc", sep="")
			rasterNameSD <- paste(GCMDirOut, "/wetQtrTmp-SD.asc", sep="")
			outrs <- wetQtrTmp(mthRasPpt, mthRasTav, rasterName, rasterNameSD, format='ascii')
			
			rasterName <- paste(GCMDirOut, "/driQtrTmp-AV.asc", sep="")
			rasterNameSD <- paste(GCMDirOut, "/driQtrTmp-SD.asc", sep="")
			outrs <- driQtrTmp(mthRasPpt, mthRasTav, rasterName, rasterNameSD, format='ascii')
			
			rm(mthRasTav)
			
			#wetQtrRain, driQtrRain
			rasterName <- paste(GCMDirOut, "/wetQtrPpt-AV.asc", sep="")
			rasterNameSD <- paste(GCMDirOut, "/wetQtrPpt-SD.asc", sep="")
			outrs <- wetQtrRain(mthRasPpt, rasterName, rasterNameSD, format='ascii')
			
			rasterName <- paste(GCMDirOut, "/driQtrPpt-AV.asc", sep="")
			rasterNameSD <- paste(GCMDirOut, "/driQtrPpt-SD.asc", sep="")
			outrs <- driQtrRain(mthRasPpt, rasterName, rasterNameSD, format='ascii')
			
			arList <- list.files(samDir, pattern=".txt")
			arCount <- 1
			
			for (area in arList) {
				arFileName <- paste(samDir, "/", area, sep="")
				arName <- substring(area, 1, nchar(area)-3)
				
				cat("Extracting for ", arName, "\n")
				
				loc <- read.table(arFileName, sep="\t", header=T)
				loc <- as.matrix(cbind(loc$x, loc$y))
				
				rasList <- list.files(GCMDirOut, pattern=".asc")
				rasCount <- 1
				
				for (ras in rasList) {
					rasName <- paste(GCMDirOut, "/", ras, sep="")
					rs <- raster(rasName)
					
					zoneValues <- xyValues(rs, loc)
					zoneValues <- zoneValues[which(!is.na(zoneValues))]
					
					rowV <- c(area, ras, mean(zoneValues), sd(zoneValues), max(zoneValues), min(zoneValues))
					
					if (rasCount == 1 & arCount ==1) {
						resDF <- rowV
					} else {
						resDF <- rbind(resDF, rowV)
					}
					rasCount <- rasCount+1
				}
				arCount <- arCount+1
			}
			
			resDF <- as.data.frame(resDF)
			names(resDF) <- c("Zone", "Raster", "Mean", "SD", "Max", "Min")
			
			resDFName <- paste(GCMDirOut, "/Results.csv", sep="")
			
			write.csv(resDF, resDFName, quote=F, row.names=F)
		}
	}
}

