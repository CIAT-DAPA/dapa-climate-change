library(rgdal)

repoDir <- "D:/_tools"
srcDir <- paste(repoDir, "/dapa-climate-change/trunk/PhD/0002-BaselineComparison", sep="")

setwd(srcDir)
source("compareRasterRaster.R")

#################################################################################
#################################################################################
#GCM vs. CRU grids
#################################################################################
#################################################################################
mDataDir <- "F:/PhD-work"
md <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/gcm-data/20C3M/1961_1990", sep="")
gcmList <- list.files(md)
cd <- paste(mDataDir, "/climate-data-assessment/comparisons/input-data/cru-data", sep="")
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- paste(mDataDir, "/climate-data-assessment/comparisons/results/cru-vs-gcm/JJA", sep="")
djf <- paste(mDataDir, "/climate-data-assessment/comparisons/results/cru-vs-gcm/DJF", sep="")
ann <- paste(mDataDir, "/climate-data-assessment/comparisons/results/cru-vs-gcm/ANNUAL",sep="")
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "tmean") {dv <- T} else {dv <- F}
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8), verbose=T)
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2), verbose=T)
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12), verbose=T)
		}
	}
}
