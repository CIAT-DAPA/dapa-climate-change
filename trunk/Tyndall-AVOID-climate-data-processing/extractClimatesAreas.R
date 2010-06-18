require(sp)
require(rgdal)
require(raster)
require(BioCalc)

source("wettestQtrPpt.R")
source("wettestQtrTmp.R")
source("driestQtrPpt.R")
source("driestQtrTmp.R")

#E:\AVOID_scenarios\outputBio\20C3M\1925s\CRU3_TS
#E:\AVOID_scenarios\outputBio\A1Bp50\2020s\cccma_cgcm31

#Prepare the masks: BRA(C,N,S), ETH(N,S), MEX(C,N,S), MOZ(C,N,S), MWI(N,S), TZA(N,S), UGA(N,SW)

baseDir <- "E:/AVOID_scenarios/outputBio"
outDir <- "E:/megaprogram/climates"

scen <- c("20C3M", "A1Bp50")

scDir <- paste(baseDir, "/", scen, sep="")
scDirOut <- paste(outDir, "/", scen, sep="")

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
		GCMDir <- paste(yrDir, "/", GCM, sep="")
		GCMDirOut <- paste(yrDirOut, "/", GCM, sep="")
		
		if (!file.exists(yrDirOut)) {
			dir.create(yrDirOut)
		}
		
		mthRasPpt <- LoadMonthlyData(folder=GCMDir, ext=".asc", varbl="prec", format='ascii')
		mthRasTav <- LoadMonthlyData(folder=GCMDir, ext=".asc", varbl="tmean", format='ascii')
		
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
	}
}



