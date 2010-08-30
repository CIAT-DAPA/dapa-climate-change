#Julian Ramirez, July 11 2010
require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXX CUT TO NEW DOMAIN XXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

source("zipRead.R")
source("zipWrite.R")

#Need to cut all the grids within the projection folder to a mask raster

#bdir <- "F:/COP_CONDESAN"

#Basic cut function for presence/absence grid
cutPAGrid <- function(rs, msk) {
	rs[which(is.na(msk[]) & !is.na(rs[]))] <- 0
	return(rs)
}

#Basic cut function for probability grid
cutPRGrid <- function(rs, msk) {
	rs[which(is.na(msk[]))] <- NA
	return(rs)
}

#Cut baseline layers to domain
cutDomainBL <- function(ID, fdir, thresh=NA, msk, writedir) {
	ext <- ".asc.gz"
	lsf <- list.files(fdir, pattern=ext)
	if (length(lsf) == 0) {
		ext <- ".asc.zip"
	}
	
	if (is.na(thresh)) {
		fn <- paste(ID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000", ext, sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPRGrid(rs, msk)
	} else {
		fn <- paste(ID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", thresh, ext, sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPAGrid(rs, msk)
	}
	dumm <- zipWrite(rs, writedir, fn)
	return(rs)
}

#Cut future layers to domain function
cutDomainFT <- function(ID, fdir, sres="SRES_A2", period="2010_2039", thresh=NA, mig=NA, msk, writedir) {
	ext <- ".asc.gz"
	lsf <- list.files(fdir, pattern=ext)
	if (length(lsf) == 0) {
		ext <- ".asc.zip"
	}
	
	if (is.na(thresh)) {
		fn <- paste(ID, "_future_", sres, "_disaggregated_", period, ext, sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPRGrid(rs, msk)
	} else {
		fn <- paste(ID, "_future_", sres, "_disaggregated_", period, "_", thresh, "_", mig, ext, sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPAGrid(rs, msk)
	}
	dumm <- zipWrite(rs, writeDir, fn)
	return(rs)
}

#Verification file creation
createVerFile <- function(wrd="Nothing", vfile) {
	opnFile <- file(vfile, open="w")
	cat(wrd, date(), file=opnFile)
	close.connection(opnFile)
	return(TRUE)
}

#Cut a single species
cutDomainSpp <- function(ID, bdir) {
	
	mxdir <- paste(bdir, "/mxe_outputs", sep="")
	spDir <- paste(mxdir, "/sp-", ID, sep="")
	projDir <- paste(spDir, "/projections", sep="")
	corrDir <- paste(spDir, "/projections/_newDomain", sep="")
	
	if (!file.exists(corrDir)) {
		dir.create(corrDir)
	}
	
	msk <- raster(paste(bdir, "/maskData/AAIGrids/and0_25m_area.asc", sep=""))
	
	sresList <- c("SRES_A1B", "SRES_A2")
	periodList <- c("2010_2039", "2040_2069")
	migList <- c("FullAdap", "NullAdap")
	threshList <- c(NA, "Prevalence", "TenPercentile")
	
	#Performing for baseline (probability raster)
	for (thr in threshList) {
		cat("Baseline:", thr, "\n")
		rs <- cutDomainBL(ID, projDir, thresh=thr, msk, corrDir)
	}
	
	#Performing for future
	for (sr in sresList) {
		for (per in periodList) {
			for (thr in threshList) {
				if (is.na(thr)) {
					cat("Future:", sr, per, thr, "\n")
					rs <- cutDomainFT(ID, projDir, sres=sr, period=per, thresh=thr, mig=NA, msk, corrDir)
				} else {
					for (mg in migList) {
						cat("Future:", sr, per, thr, mg, "\n")
						rs <- cutDomainFT(ID, projDir, sres=sr, period=per, thresh=thr, mig=mg, msk, corrDir)
					}
				}
			}
		}
	}
	
	#Create verification file
	theText <- "Cut made on"
	vf <- paste(spDir, "/ct-", ID, ".run", sep="")
	isDone <- createVerFile(theText, vf)
}

#This is the entire process function
cutDomain <- function(bdir, ini, fin, OSys="LINUX") {
	
	ufile <- paste(bdir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
	ufile <- read.csv(ufile)
	
	if (fin > nrow(ufile)) {
		cat("Final number is greater than nrow, using nrows instead \n")
		fin <- nrow(ufile)
	}
	
	spList <- ufile$IDSpecies[ini:fin]
	sppC <- 1
	
	for (sp in spList) {
		cat("\n")
		cat("...Species", sp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
		out <- cutDomainSpp(sp, OSys, inputDir, destDir)
		sppC <- sppC + 1
	}
	
	return("Done!")
	
}
