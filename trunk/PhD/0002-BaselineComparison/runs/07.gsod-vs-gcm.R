#################################################################################
#################################################################################
#Compare stations timeseries
#################################################################################
#################################################################################

setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison/")
source("compareGSODRaster-TS.R")

#Specify data location
wd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/gsod-weather-stations/"
ad <- "F:/Administrative_boundaries/SHP_files"
gd <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
od <- "F:/PhD-work/climate-data-assessment/comparisons/results/gsod-vs-gcm-ts"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
for (ctry in cList) {
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which=ALL, aDir=ad, var.in="rain", var.out="prec", iso.ctry=ctry, time.series=c(1961:1990))
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which=ALL, aDir=ad, var.in="tmean", var.out="tmean", iso.ctry=ctry, time.series=c(1961:1990))
}


#################################################################################
#################################################################################

library(rgdal)
setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison")
source("compareGSODRaster.R")

#################################################################################
#################################################################################
#GCM vs. GHCN weather stations
#################################################################################
#################################################################################
md <- "F:/climate_change/IPCC_CMIP3/20C3M/filled"
gcmList <- list.files(md)
cd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/gsod-weather-stations/all-years"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- "F:/PhD-work/climate-data-assessment/comparisons/results/gsod-vs-gcm/JJA"
djf <- "F:/PhD-work/climate-data-assessment/comparisons/results/gsod-vs-gcm/DJF"
ann <- "F:/PhD-work/climate-data-assessment/comparisons/results/gsod-vs-gcm/ANNUAL"
for (ctry in cList) {
  for (mod in gcmList) {
  	for (vr in c("tmean", "prec", "tmin", "tmax")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(6,7,8), outDir=jja, verbose=T)
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(12,1,2), outDir=djf, verbose=T)
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(1:12), outDir=ann, verbose=T)
		}
	}
}
