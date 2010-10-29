library(rgdal)
source("compareRasterRaster.R")

#################################################################################
#################################################################################
#WCL
#################################################################################
#################################################################################
md <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/gcm-data/20C3M/1961_1990"
gcmList <- list.files(md)
cd <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-data"
shd <- "G:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "MLI", "BFA")
jja <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/JJA"
djf <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/DJF"
ann <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/ANNUAL"

for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "prec") {dv <- F} else {dv <- T}
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12))
		}
	}
}

#################################################################################
#################################################################################
#CRU
#################################################################################
#################################################################################
md <- "G:/climate_change/IPCC_CMIP3/20C3M/filled"
cd <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/cru-data"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "MLI", "BFA")
jja <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/cru-results/JJA"
djf <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/cru-results/DJF"
ann <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/cru-results/ANNUAL"
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			cat("Processing", ctry, mod, vr, "\n")
			if (vr == "tmean") {dv <- T} else {dv <- F}
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12))
		}
	}
}
