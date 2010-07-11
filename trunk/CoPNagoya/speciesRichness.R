require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXX SPECIES RICHNESS CALCULATION XXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

source("zipRead.R")

#Over a set of species, for all threshold types, and both current and future conditions (both timeslices and both migration scenarios)
#Calculate the sum of species (species richness)

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

#idir <- "C:/CIAT_work/COP_CONDESAN"

speciesRichness <- function(idir, type="plants", ini, fin) {
	#Checking if type is within the expected
	if (!tolower(type) %in% c("plants", "aves")) {
		stop("Type of species is not supported")
	}
	
	condList <- c("baseline", "future")
	threList <- c("Prevalence", "TenPercentile")
	migrList <- c("FullAdap", "NullAdap")
	
	spFile <- paste(idir, "/occurrences/modeling-data/", tolower(type) "-speciesListToModel.csv", sep="")
	spListComplete <- read.csv(spFile)
	
	nspp <- nrow(spListComplete)

	#Checking consistency of initial and final nspp
	if (fin <= ini) {
		stop("Final number of species is less than or equal to initial, please correct")
	}
	
	if (fin > nspp) {
		cat("Final number of species is greater than total number of species, using #SPP instead \n")
		fin <- nspp
	}
	
	spi <- spListComplete$IDSpecies[ini]
	spf <- spListComplete$IDSpecies[fin]
	
	#Subselecting the species
	spList <- spListComplete$IDSpecies[ini:fin]
	
	odir <- paste(idir, "/summaries", sep="")
	if (!file.exists(odir)) {
		dir.create(odir)
	}
	
	richdir <- paste(odir, "/", tolower(type), "-richness-", spi, "-", spf, sep="")
	if (!file.exists(richdir)) {
		dir.create(richdir)
	}

	sppC <- 1
	sppCC <- 1
	for (spp in spList) {
		fdName <- paste("sp-", spp, sep="")
		spFolder <- paste(idir, "/mxe_outputs/", fdName, sep="")
		
		#Performing only for existing folders
		if (file.exists(spFolder)) {
			cat("\n")
			cat("...Processing species", spp, paste("...",round(sppCC/length(spList)*100,2),"%",sep=""), "\n")
			
			rsFolder <- paste(spFolder, "/projections", sep="")
			
			#Cycle through conditions
			for (condition in condList) {
				#cat("Performing for", condition, "\n")
				
				#Defining stuff to cycle after
				if (condition == "baseline") {
					sresList <- c("20C3M")
					string <- "WorldClim-2_5min-bioclim"
					tsList <- c("1950_2000")
				} else {
					sresList <- c("SRES_A1B", "SRES_A2")
					string <- "disaggregated"
					tsList <- c("2010_2039", "2040_2069")
				}
				
				#Cycle through the SRES list
				for (sres in sresList) {
					#cat("Performing for scenario", sres, "\n")
					
					#Cycle through the time-slices
					for (tsl in tsList) {
						#cat("Performing for timeslice", tsl, "\n")
						
						#Cycle through thresholds
						for (threshold in threList) {
							#cat("Performing for threshold", threshold, "\n")
							
							#Cycle through migration scenarios if future, else just load and comprise
							if (condition == "baseline") {
								
								#Filename: [SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH].[FORMAT].[EXT]
								fName <- paste(spp, "_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, ".asc.zip", sep="")
								if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
									fName <- paste(spp, "_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, ".asc.gz", sep="")
								}
								
								#Loading the raster
								cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, sep=""), "\n")
								rs <- zipRead(rsFolder, fName)
								
								#if sppC equals 1 then define this richness, else sum
								if (sppC == 1) {
									#cat("Calculating richness [1] \n")
									assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep=""), rs)
									rm(rs)
								} else {
									#cat("Calculating richness \n")
									assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep=""), rs+get(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep="")))
									rm(rs)
								}
								
							} else {
								
								#Now cycle through migration scenarios
								for (mig in migrList) {
									#cat("Performing for mig. scenario", mig, "\n")
									
									#Filename: [SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH]_[MIG].[FORMAT].[EXT]
									fName <- paste(spp, "_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, ".asc.zip", sep="")
									if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
										fName <- paste(spp, "_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, ".asc.gz", sep="")
									}
									
									#Loading the raster
									cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, "-", mig, sep=""), "\n")
									rs <- zipRead(rsFolder, fName)
									
									#if sppC equals 1 then define this richness, else sum with previous richness
									if (sppC == 1) {
										#cat("Calculating richness [1] \n")
										assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, sep=""), rs)
										rm(rs)
									} else {
										#cat("Calculating richness \n")
										assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, sep=""), rs+get(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, sep="")))
										rm(rs)
									}
									
								}
								
							}
							
						}
						
					}
					
				}
				
			}
			sppC <- sppC+1
		} else {
			cat("The species", spp, "was not modeled", paste("...",round(sppCC/length(spList)*100,2),"%",sep=""), "\n")
		}
		sppCC <- sppCC+1
	}

	objList <- ls(pattern="richness")

	cat("\n")
	cat("Writing summary rasters \n")
	for (obj in objList) {
		rName <- paste(richdir, "/", obj, sep="")
		assign(obj, writeRaster(get(obj), rName, format='ascii', overwrite=T))
		png(paste(rName, ".png", sep=""))
		plot(get(obj))
		dev.off()
	}

	cat("\n")
	cat("Done! \n")
}