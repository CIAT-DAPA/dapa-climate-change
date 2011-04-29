#################################################################################
#################################################################################
#Compare stations timeseries
#################################################################################
#################################################################################

setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison/")
source("compareGHCNRaster-TS.R")

#Specify data location
wd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/"
ad <- "F:/Administrative_boundaries/SHP_files"
gd <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
od <- "F:/PhD-work/climate-data-assessment/comparisons/results/ghcn-vs-gcm-ts"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
for (ctry in cList) {
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which=ALL, aDir=ad, var.in="rain", var.out="prec", iso.ctry=ctry, time.series=c(1961:1990))
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which=ALL, aDir=ad, var.in="tmean", var.out="tmean", iso.ctry=ctry, time.series=c(1961:1990))
}


#################################################################################
#################################################################################

library(rgdal)
setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison")
source("compareGHCNRaster.R")

#################################################################################
#################################################################################
#GCM vs. GHCN weather stations
#################################################################################
#################################################################################
md <- "F:/climate_change/IPCC_CMIP3/20C3M/filled"
gcmList <- list.files(md)
cd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/organized-data"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- "F:/PhD-work/climate-data-assessment/comparisons/results/ghcn-vs-gcm/JJA"
djf <- "F:/PhD-work/climate-data-assessment/comparisons/results/ghcn-vs-gcm/DJF"
ann <- "F:/PhD-work/climate-data-assessment/comparisons/results/ghcn-vs-gcm/ANNUAL"
for (ctry in cList) {
  for (mod in gcmList) {
  	for (vr in c("tmean", "prec", "tmin", "tmax")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			outp <- compareGHCNR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(6,7,8), outDir=jja, verbose=T)
			outp <- compareGHCNR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(12,1,2), outDir=djf, verbose=T)
			outp <- compareGHCNR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(1:12), outDir=ann, verbose=T)
		}
	}
}
