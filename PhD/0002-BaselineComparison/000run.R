source("compareRasterRaster.R")

#################################################################################
#################################################################################
#WCL
#################################################################################
#################################################################################
md <- "G:/climate_change/IPCC_CMIP3/20C3M/filled"
gcmList <- list.files(md)
cd <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-data"
shd <- "F:/Administrative_boundaries/SHP_files"

cList <- c("ETH", "KEN", "TZA", "UGA", "GHA", "SEN", "MLI", "NER", "MLI", "BFA")
jja <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/JJA"
djf <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/DJF"
ann <- "C:/Workspace/PhD-work/cru-wcl-gcm-comparison/wcl-results/ANNUAL"
for (ctry in cList) {
	for (mod in gcmList) {
		for (vr in c("tmean", "prec")) {
			if (vr == "tmean") {dv <- T} else {dv <- F}
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
			if (vr == "tmean") {dv <- T} else {dv <- F}
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=jja, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(6,7,8))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=djf, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(12,1,2))
			res <- compareRR(gcmDir=md, gcm=mod, wclDir=cd, shpDir=shd, outDir=ann, vn=vr, divide=dv, ext=".asc", country=ctry, monthList=c(1:12))
		}
	}
}
