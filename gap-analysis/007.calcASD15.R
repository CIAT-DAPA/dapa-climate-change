require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")

# Script to calculate proportion of the dist. range with SD above 0.15 (ASD15)

idir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
spID <- "Phaseolus_acutifolius"

calcASD15 <- function(idir, spID) {
	cat("Taxon", spID, "\n")
	spFolder <- paste(idir, "/mxe_outputs/sp-", spID, sep="")
	projFolder <- paste(spFolder, "/projections", sep="")
	
	esdCpt <- paste(spID, "_WorldClim-2_5min-bioclim_ESD.asc.gz", sep="")
	esdThr <- paste(spID, "_WorldClim-2_5min-bioclim_ESD_PR.asc.gz", sep="")
	
	dumm <- paste(spID, "_WorldClim-2_5min-bioclim_EMN.asc.gz", sep="")
	
	cat("..Reading raster files \n")
	dumm <- zipRead(projFolder, dumm)
	esdCpt <- zipRead(projFolder, esdCpt)
	esdThr <- zipRead(projFolder, esdThr)
	
	esdCpt[which(dumm[] < 0.001)] <- NA
	
	rm(dumm)
	
	esdThr[which(esdThr[] == 0)] <- NA
	
	cat("..Calculating \n")
	szCpt <- length(which(esdCpt[] >= 0))
	szCptUncertain <- length(which(esdCpt[] >= 0.15))
	rateCpt <- szCptUncertain / szCpt * 100
	
	szThr <- length(which(esdThr[] >= 0))
	szThrUncertain <- length(which(esdThr[] >= 0.15))
	rateThr <- szThrUncertain / szThr * 100
	
	cat("..Writing results \n")
	dfOut <- data.frame(taxon=spID, sizeComplete=szCpt, sizeCompleteUncertain=szCptUncertain, rateComplete=rateCpt, sizeThresholded=szThr, sizeThresholdedUncertain=szThrUncertain, rateThresholded=rateThr)
	
	oFile <- paste(spFolder, "/metrics/ASD15.csv", sep="")
	write.csv(dfOut, oFile, quote=F, row.names=F)
	
	return(dfOut)
}