#Julian Ramirez, July 11 2010
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
#Calculate the sum of species (species richness), over a set of genera

#Create a list of genera, and a list of species with a column being genera
#Create an ID for each genus, do this separately for AVES and PLANTS (to avoid confussion with names of genera)

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


speciesRichness <- function(idir, type, OSys="LINUX") {
	
	#Reading the list of genera
	gnFile <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToModel.csv", sep="")
	gnListComplete <- read.csv(gnFile)
	ngen <- nrow(gnListComplete)
	
	#Listing looping items
	condList <- c("baseline", "future")
	threList <- c("Prevalence", "TenPercentile")
	migrList <- c("FullAdap", "NullAdap")
	
	oRichFolder <- paste(idir, "/summaries/richness-", tolower(type), "/all-genera", sep="")
	if (!file.exists(oRichFolder)) {
		dir.create(oRichFolder)
	}
				
	gnList <- gnListComplete$IDGenus
	
	cat("Processing", paste(length(gnList)), "genera \n")
	
	genC <- 1
	for (gen in gnList) {
		fdGenName <- paste("gn-", gen, sep="")
		iGenFolder <- paste(idir, "/summaries/richness-", tolower(type), "/", fdGenName, sep="")
		
		#Performing only for existing folders
		if (file.exists(iGenFolder)) {
			cat("Genus", gen, paste("...",round(genC/length(gnList)*100,2),"%",sep=""), "\n")
			
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
								fName <- paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, ".asc.gz", sep="")
								
								#Loading the raster
								#cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, sep=""), "\n")
								rs <- zipRead(iGenFolder, fName)
								
								#if sppC equals 1 then define this richness, else sum
								if (genC == 1) {
									#cat("Calculating richness [1] \n")
									assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep=""), rs)
									rm(rs)
								} else {
									#cat("Calculating richness \n")
									assign(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep=""), rs+get(paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, sep="")))
									rm(rs)
								}
								
							} else {
								cat(condition, ".", sep="")
								#Now cycle through migration scenarios
								for (mig in migrList) {
									#cat("Performing for mig. scenario", mig, "\n")
									
									#Filename: [SPID]_[CONDITION]_[SRES]_[STRING]_[TS]_[THRESH]_[MIG].[FORMAT].[EXT]
									fName <- paste("richness_", condition, "_", sres, "_", string, "_", tsl, "_", threshold, "_", mig, ".asc.gz", sep="")
									
									#Loading the raster
									#cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, "-", mig, sep=""), "\n")
									rs <- zipRead(iGenFolder, fName)
									
									#if sppC equals 1 then define this richness, else sum with previous richness
									if (genC == 1) {
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
			genC <- genC+1
		} else {
			cat("The genus", gen, "was not modeled \n")
		}
	}

	objList <- ls(pattern="richness")
	
	#Writing ascii files
	cat("Writing summary rasters \n")
	for (obj in objList) {
		rName <- paste(oRichFolder, "/", obj, sep="")
		assign(obj, writeRaster(get(obj), rName, format='ascii', overwrite=T))
		png(paste(rName, ".png", sep=""))
		plot(get(obj))
		dev.off()
	}
	
	#Compressing them
	cat("Compressing summary rasters \n")
	ftoZIP <- list.files(oRichFolder, pattern=".asc")
	for (fz in ftoZIP) {
		fName <- paste(oRichFolder, "/", fz, sep="")
		if (tolower(OSys) == "linux") {
			system(paste("gzip", fName))
		} else {
			system(paste("7za", "a", "-bd", "-tgzip", paste(fName, ".gz", sep=""), fName))
			file.remove(fName)
		}
	}
	
}

################################################################

richnessChange <- function(richDir, type, sres, period, threshold, adapescen, OSys="LINUX") {
	if (!tolower(type) %in% c("plants", "aves")) {
		stop("Type is not supported, use plants or aves")
	}
	
	if (!toupper(sres) %in% c("SRES_A1B", "SRES_A2")) {
		stop("SRES emission scenario not supported")
	}
	
	if (!period %in% c("2010_2039", "2040_2069")) {
		stop("Period not supported")
	}
	
	if (!threshold %in% c("Prevalence", "TenPercentile")) {
		stop("Invalid threshold name")
	}
	
	if (!adapscen %in% c("FullAdap", "NullAdap")) {
		stop("Invalid adaptation scenario name")
	}
	
	cat("Loading and calculating... \n")
	
	curRich <- zipRead(richDir, paste("richness_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.gz", sep=""))
	futRich <- zipRead(richDir, paste("richness_future_", sres, "_disaggregated_", period, "_", threshold, "_", adapscen, ".asc.gz", sep=""))
	
	absChg <- futRich - curRich
	perChg <- absRich / curRich * 100
	
	rsName <- paste(richDir, "/richness_abschg_", sres, "_", period, "_", threshold, "_", adapscen, ".asc", sep="")
	absChg <- writeRaster(absChg, rsName, format='ascii', overwrite=T)
	
	if (OSys == "linux") {
		system(paste("gzip", rsName))
	} else {
		system(paste("7za", "a", "-bd", "-tgzip", paste(rsName, ".gz", sep=""), rsName))
		file.remove(rsName)
	}
	
	rsName <- paste(richDir, "/richness_perchg_", sres, "_", period, "_", threshold, "_", adapscen, ".asc", sep="")
	perChg <- writeRaster(perChg, rsName, format='ascii', overwrite=T)
	
	if (tolower(OSys) == "linux") {
		system(paste("gzip", rsName))
	} else {
		system(paste("7za", "a", "-bd", "-tgzip", paste(rsName, ".gz", sep=""), rsName))
		file.remove(rsName)
	}
	return("Done!")
}

#idir <- "L:/COP_CONDESAN"

richnessChangeCalc <- function(idir) {
	typeList <- c("aves", "plants")
	threList <- c("Prevalence", "TenPercentile")
	migrList <- c("FullAdap", "NullAdap")
	sresList <- c("SRES_A1B", "SRES_A2")
	tsList <- c("2010_2039", "2040_2069")
	
	for (type in typeList) {
		for (sres in sresList) {
			for (tsl in tsList) {
				for (threshold in threList) {
					for (mig in migrList) {
						cat(".Performing for:", type, sres, tsl, threshold, mig)
						richdir <- paste(idir, "/summaries/richness-", tolower(type), "/all-genera", sep="")
						ot <- richnessChange(richdir, type, sres, tsl, threshold, mig, OSys='linux')
					}
				}
			}
		}
	}
	
}













