require(rgdal)
require(raster)

source("zipRead.R")

#To correct the null adaptation scenario

#Baseline:		[SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH].[FORMAT].[EXT]
#Future:		[SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH]_[MIG].[FORMAT].[EXT]

#[SPID] 		...any
#[CONDITION] 	baseline, future
#[SRES] 		20C3M, SRES_A1B, SRES_A2
#[STRING] 		WorldClim-2_5min-bioclim, disaggregated
#[TS] 			1950_2000, 2010_2039, 2040_2069
#[THRESH] 		Prevalence, TenPercentile
#[MIG] 			FullAdap, NullAdap
#[FORMAT]		asc
#[EXT] 			zip, gz

threList <- c("Prevalence", "TenPercentile")

correctSpecies <- function(spp, OSys, idir) {
	
	OSys <- tolower(OSys)
	
	fdName <- paste("sp-", spp, sep="")
	spFolder <- paste(idir, "/mxe_outputs/", fdName, sep="")
	
	#Performing only for existing folders
	if (file.exists(spFolder)) {
		cat("...Taxon", spp, "\n")
		
		verFile <- paste(spFolder, "/cr-", spp, ".run", sep="")
		if (!file.exists(verFile)) {
			
			rsFolder <- paste(spFolder, "/projections", sep="")
			
			sresList <- c("SRES_A1B", "SRES_A2")
			tsList <- c("2010_2039", "2040_2069")
			
			#Cycle through the SRES list
			for (sres in sresList) {
				#cat("Performing for scenario", sres, "\n")
				
				#Cycle through the time-slices
				for (tsl in tsList) {
					#cat("Performing for timeslice", tsl, "\n")
					
					#Cycle through thresholds
					for (threshold in threList) {
						#cat("Performing for threshold", threshold, "\n")
						
						#Now correct the NullAdap scenario
						cat("Correcting:", paste("future-", sres, "-", tsl, "-", threshold, "-NullAdap", sep=""), "\n")
						
						#Filename: [SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH]_[MIG].[FORMAT].[EXT]
						fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, "_", threshold, "_NullAdap.asc.zip", sep="")
						if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
							fName <- paste(spp, "_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_NullAdap.asc.gz", sep="")
						}
						
						#Loading the raster
						rs <- zipRead(rsFolder, fName)
						
						#Correct the NullAdap raster (if NullAdap, read baseline and multiply, 
						#then write again the ascii, zip it, copy it and erase the ascii)
						
						cat("Correcting the raster \n")
						
						#Getting the name of the baseline raster
						BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.zip", sep="")
						if (!file.exists(paste(rsFolder, "/", BLfName, sep=""))) {
							BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.gz", sep="")
						}
						
						#Reading the baseline raster
						BLrs <- zipRead(rsFolder, BLfName)
						
						#Multiplying them both
						rs <- rs * BLrs
						rm(BLrs)
						
						#Writing new raster
						rfileName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, "_", threshold, "_NullAdap.asc", sep="")
						rs <- writeRaster(rs, rfileName, overwrite=T, format='ascii')
						rm(rs)
						
						#Compressing it and moving
						if (OSys == "nt") {
							system(paste("7za", "a", "-tzip", "-bd", paste(rfileName, ".zip", sep=""), rfileName))
							file.remove(rfileName)
							file.copy(paste(rfileName, ".zip", sep=""), paste(rsFolder, "/", rfileName, ".zip", sep=""), overwrite=T)
							file.remove(paste(rfileName, ".zip", sep=""))
						} else {
							system(paste("gzip", rfileName))
							file.copy(paste(rfileName, ".gz", sep=""), paste(rsFolder, "/", rfileName, ".gz", sep=""), overwrite=T)
							file.remove(paste(rfileName, ".zip", sep=""))
						}
						
					}
					
				}
				
			}
					
			#Run correction verification file writing
			opnFile <- file(verFile, open="w")
			cat("Corrected on", date(), file=opnFile)
			close.connection(opnFile)
		} else {
			cat("The species", spp, "was already corrected \n")
		}
	} else {
		cat("The species", spp, "was not modeled", "\n")
	}
}


##############################################################
#idir <- "C:/CIAT_work/COP_CONDESAN"

correctNullAdap <- function(idir, ini, fin, OSys="LINUX") {
	
	ufile <- paste(idir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
	ufile <- read.csv(ufile)
	nspp <- nrow(ufile)
	
	#Checking consistency of initial and final nspp
	if (fin <= ini) {
		stop("Final number of species is less than or equal to initial, please correct")
	}
	if (fin > nspp) {
		cat("Final number of species is greater than total number of species, using #SPP instead \n")
		fin <- nspp
	}
	if (fin > nrow(ufile)) {
		cat("Final number is greater than nrow, using nrows instead \n")
		fin <- nrow(ufile)
	}
	
	#Subselecting the species
	
	spList <- ufile$IDSpecies[ini:fin]
	sppC <- 1
	
	for (sp in spList) {
		cat("\n")
		cat("...Species", sp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
		out <- correctSpecies(sp, OSys, idir)
		sppC <- sppC + 1
	}
	
	return("Done!")
	
}