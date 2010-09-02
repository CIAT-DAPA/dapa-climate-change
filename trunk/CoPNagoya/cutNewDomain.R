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
		fno <- paste(ID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000", ".asc.gz", sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPRGrid(rs, msk)
	} else {
		fn <- paste(ID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", thresh, ext, sep="")
		fno <- paste(ID, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", thresh, ".asc.gz", sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPAGrid(rs, msk)
	}
	dumm <- zipWrite(rs, writedir, fno)
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
		fno <- paste(ID, "_future_", sres, "_disaggregated_", period, ".asc.gz", sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPRGrid(rs, msk)
	} else {
		fn <- paste(ID, "_future_", sres, "_disaggregated_", period, "_", thresh, "_", mig, ext, sep="")
		fno <- paste(ID, "_future_", sres, "_disaggregated_", period, "_", thresh, "_", mig, ".asc.gz", sep="")
		rs <- zipRead(fdir, fn)
		rs <- cutPAGrid(rs, msk)
	}
	dumm <- zipWrite(rs, writedir, fno)
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
cutDomainSpp <- function(ID, bdir, ldir, OSys="linux", ow=F) {
	
	mxdir <- paste(bdir, "/mxe_outputs", sep="")
	spDir <- paste(mxdir, "/sp-", ID, sep="")
	projDir <- paste(spDir, "/projections", sep="")
	corrDir <- paste(spDir, "/projections/_newDomain", sep="")
	
	vf <- paste(spDir, "/ct-", ID, ".run", sep="")
	
	if (ow) {
		file.remove(vf)
	}
	
	if (file.exists(spDir)) {
		if (!file.exists(vf)) {
			
			if (file.exists(corrDir)) {
				cat("Erasing previous stuff \n")
				system(paste("rm", "-rf", corrDir))
			}
			
			if (!file.exists(corrDir)) {
				dir.create(corrDir)
			}
			
			spTmpDir <- paste("sp-", ID, sep="")
			if (file.exists(spTmpDir)) {
				cat("Erasing temporal stuff \n")
				system(paste("rm", "-rf", spTmpDir))
			}
			
			if (!file.exists(spTmpDir)) {
				dir.create(spTmpDir)
				dir.create(paste(spTmpDir, "/_newDomain", sep=""))
			}
			
			msk <- raster(paste(ldir, "/maskData/AAIGrids/andes_finalDomain_25m.asc", sep=""))
			
			sresList <- c("SRES_A1B", "SRES_A2")
			periodList <- c("2010_2039", "2040_2069")
			migList <- c("FullAdap", "NullAdap")
			threshList <- c(NA, "Prevalence", "TenPercentile")
			
			#Performing for baseline (probability raster)
			for (thr in threshList) {
				cat("Baseline:", thr, "\n")
				rs <- cutDomainBL(ID, projDir, thresh=thr, msk, paste(spTmpDir, "/_newDomain", sep=""))
			}
			
			#Performing for future
			for (sr in sresList) {
				for (per in periodList) {
					for (thr in threshList) {
						if (is.na(thr)) {
							cat("Future:", sr, per, thr, "\n")
							rs <- cutDomainFT(ID, projDir, sres=sr, period=per, thresh=thr, mig=NA, msk, paste(spTmpDir, "/_newDomain", sep=""))
						} else {
							for (mg in migList) {
								cat("Future:", sr, per, thr, mg, "\n")
								rs <- cutDomainFT(ID, projDir, sres=sr, period=per, thresh=thr, mig=mg, msk, paste(spTmpDir, "/_newDomain", sep=""))
							}
						}
					}
				}
			}
			
			#Now copy the files
			if (OSys == "linux") {
				destName <- paste(projDir, "/.", sep="")
				system(paste("cp", "-rf", paste(spTmpDir, "/_newDomain", sep=""), destName))
				system(paste("rm", "-rf", spTmpDir))
			} else {
				destName <- corrDir
				origindir <- paste(spTmpDir, "\\_newDomain", sep="") #gsub("/", "\\\\", )
				destindir <- gsub("/", "\\\\", destName)
				system(paste("xcopy", "/E", "/I", origindir, destindir))
				system(paste("rm", "-rf", spTmpDir))
			}
			
			#Create verification file
			theText <- "Cut made on"
			isDone <- createVerFile(theText, vf)
		} else {
			cat("The taxon is already processed \n")
		}
	} else {
		cat("The taxon was never modeled \n")
	}
}

#This is the entire process function
cutDomain <- function(bdir, ldir, ini, fin, OSys="LINUX", overwrite=F) {
	
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
		out <- cutDomainSpp(sp, bdir, ldir, OSys=tolower(OSys), ow=overwrite)
		sppC <- sppC + 1
	}
	
	return("Done!")
}
