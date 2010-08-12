require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")

# Script to calculate proportion of the dist. range with SD above 0.15 (ASD15)

idir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"

spID <- "Phaseolus_acutifolius"

spFolder <- paste(idir, "/mxe_outputs/sp-", spID, sep="")
projFolder <- paste(spFolder, "/projections", sep="")

esdCpt <- "Phaseolus_acutifolius_WorldClim-2_5min-bioclim_ESD.asc.gz"
esdThr <- "Phaseolus_acutifolius_WorldClim-2_5min-bioclim_ESD_PR.asc.gz"

dumm <- "Phaseolus_acutifolius_WorldClim-2_5min-bioclim_EMN.asc.gz"

dumm <- zipRead(projFolder, dumm)
esdCpt <- zipRead(projFolder, esdCpt)
esdCpt[which(dumm[] < 0.001)] <- NA

rm(dumm)

esdThr <- zipRead(projFolder, esdThr)
esdThr[which(esdThr[] == 0)] <- NA

szCpt <- length(which(esdCpt[] >= 0))
szCptUncertain <- length(which(esdCpt[] >= 0.15))
rateCpt <- szCptUncertain / szCpt * 100

szThr <- length(which(esdThr[] >= 0))
szThrUncertain <- length(which(esdCpt[] >= 0.15))


