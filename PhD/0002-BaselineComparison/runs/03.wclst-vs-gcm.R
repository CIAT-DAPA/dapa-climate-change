library(rgdal)
setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison")
source("compareWSRaster.R")

#################################################################################
#################################################################################
#GCM vs. WCL weather stations
#################################################################################
#################################################################################
md <- "F:/PhD-work/cru-wcl-gcm-comparison/gcm-data/20C3M/1961_1990"
gcmList <- list.files(md)
cd <- "F:/PhD-work/cru-wcl-gcm-comparison/input-data/wcl-weather-stations"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- "F:/PhD-work/comparisons/results/wclst-vs-gcm/JJA"
djf <- "F:/PhD-work/comparisons/results/wclst-vs-gcm/DJF"
ann <- "F:/PhD-work/comparisons/results/wclst-vs-gcm/ANNUAL"
for (ctry in cList) {
  for (mod in gcmList) {
		for (vr in c("tmean", "prec", "tmin", "tmax")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			res <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(6,7,8), outDir=jja, verbose=T)
			res <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(12,1,2), outDir=djf, verbose=T)
			res <- compareWSR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(1:12), outDir=ann, verbose=T)
		}
	}
}
