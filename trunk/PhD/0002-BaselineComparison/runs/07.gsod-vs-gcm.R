#################################################################################
#################################################################################
#Compare stations timeseries
#################################################################################
#################################################################################

library(rgdal)

repoDir <- "D:/_tools"
srcDir <- paste(repoDir, "/dapa-climate-change/trunk/PhD/0002-BaselineComparison", sep="")
setwd(srcDir)
source("compareGSODRaster-TS.v2.R")

#Specify data location
mDataDir <- "F:/PhD-work"
wd <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/gsod-weather-stations/", sep="")
ad <- "F:/Administrative_boundaries/SHP_files"
gd <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
od <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm-ts", sep="")

ctry <- "GHA"

vList <- c("rain","tmean")
for (vrin in vList) {
  if (vrin == "rain") {vrout <- "prec"} else {vrout <- vrin}
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which="ALL", aDir=ad, var.in=vrin, var.out=vrout, iso.ctry=ctry, time.series=c(1961:1990))
}



########### LINUX RUN

#"ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL"

library(rgdal)

repoDir <- "/home/jramirez"
srcDir <- paste(repoDir, "/dapa-climate-change/PhD/0002-BaselineComparison", sep="")
setwd(srcDir)
source("compareGSODRaster-TS.v2.R")

#Specify data location
mDataDir <- "/mnt/GIS-HD717/CCAFS"
wd <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/gsod-weather-stations/", sep="")
ad <- "/mnt/GIS-HD716/Administrative_boundaries/SHP_files"
gd <- "/mnt/GIS-HD716/climate_change/IPCC_CMIP3/20C3M/original-data"
od <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm-ts", sep="")

ctry <- "BGD" #SEN MLI NER BFA IND BGD

vList <- c("rain","tmean")
for (vrin in vList) {
  if (vrin == "rain") {vrout <- "prec"} else {vrout <- vrin}
  pcws <- processCompareWS(work.dir=wd, out.dir=od, gcmdir=gd, which="ALL", aDir=ad, var.in=vrin, var.out=vrout, iso.ctry=ctry, time.series=c(1961:1990))
}


#################################################################################
#################################################################################

library(rgdal)

repoDir <- "D:/_tools"
srcDir <- paste(repoDir, "/dapa-climate-change/trunk/PhD/0002-BaselineComparison", sep="")

setwd(srcDir)
source("compareGSODRaster.R")

#################################################################################
#################################################################################
#GCM vs. GHCN weather stations (DTR)
#################################################################################
#################################################################################
mDataDir <- "F:/PhD-work"
md <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/gcm-data/20C3M/1961_1990", sep="")
gcmList <- list.files(md)[-c(10,23)]

cd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/gsod-weather-stations/all-years"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/JJA", sep="")
djf <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/DJF", sep="")
ann <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/ANNUAL", sep="")
for (ctry in cList) {
  for (mod in gcmList) {3
    cat("Processing", ctry, mod, "dtr \n")
		outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable="dtr", divide=T, months=c(6,7,8), outDir=jja, verbose=T)
		outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable="dtr", divide=T, months=c(12,1,2), outDir=djf, verbose=T)
		outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable="dtr", divide=T, months=c(1:12), outDir=ann, verbose=T)
	}
}

#################################################################################
#################################################################################
#GCM vs. GHCN weather stations (RAIN, TMEAN)
#################################################################################
#################################################################################
mDataDir <- "F:/PhD-work"
md <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/gcm-data/20C3M/1961_1990", sep="")
gcmList <- list.files(md)

cd <- "S:/CCAFS/climate-data-assessment/comparisons/input-data/gsod-weather-stations/all-years"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/JJA", sep="")
djf <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/DJF", sep="")
ann <- paste(mDataDir, "/climate-data-assessment/comparisons/results/gsod-vs-gcm/ANNUAL", sep="")
for (ctry in cList) {
  for (mod in gcmList) {
  	for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(6,7,8), outDir=jja, verbose=T)
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(12,1,2), outDir=djf, verbose=T)
			outp <- compareGSODR(gcmDir=md, gcm=mod, shpDir=shd, stationDir=cd, country=ctry, variable=vr, divide=dv, months=c(1:12), outDir=ann, verbose=T)
		}
	}
}
