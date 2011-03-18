library(rgdal)
setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison")
source("compareRasterRaster.R")

#################################################################################
#################################################################################
#GCM vs. WCL grids
#################################################################################
#################################################################################
md <- "F:/PhD-work/cru-wcl-gcm-comparison/gcm-data/20C3M/1961_1990"
gcmList <- list.files(md)
cd <- "F:/PhD-work/cru-wcl-gcm-comparison/input-data/wcl-data"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "BFA", "IND", "BGD", "NPL")
jja <- "F:/PhD-work/comparisons/results/wcl-vs-gcm/JJA"
djf <- "F:/PhD-work/comparisons/results/wcl-vs-gcm/DJF"
ann <- "F:/PhD-work/comparisons/results/wcl-vs-gcm/ANNUAL"
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8), verbose=T)
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2), verbose=T)
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12), verbose=T)
		}
	}
}

