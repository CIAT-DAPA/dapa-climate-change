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


speciesRichness <- function(bdir, idir, genID, OSys="LINUX") {
	
	#Reading the list of species
	spFile <- paste(bdir, "/occurrences/modeling-data/", tolower(type), "-speciesListToModel.csv", sep="")
	spListComplete <- read.csv(spFile)
	nspp <- nrow(spListComplete)
	
	#Listing looping items
	condList <- c("baseline", "future")
	threList <- c("Prevalence", "TenPercentile")
	migrList <- c("FullAdap", "NullAdap")
	
	fdGenName <- paste("gn-", genID, sep="")
	if (!file.exists(fdGenName)) {
		dir.create(fdGenName)
	}
	
	oGenFolder <- paste(idir, "/", fdGenName, sep="")
	verF <- paste(oGenFolder, "/ps-", genID, ".run", sep="")
	
	if (!file.exists(verF)) {
	
		if (file.exists(oGenFolder)) {
			cat("Removing previous stuff ... \n")
			system(paste("rm", "-r", oGenFolder))
		}
		
		spList <- spListComplete$IDSpecies[which(spListComplete$IDGenus == genID)]
		
		cat("Processing", paste(length(spList)), "species \n")
		
		sppC <- 1
		for (spp in spList) {
			fdName <- paste("sp-", spp, sep="")
			spFolder <- paste(bdir, "/mxe_outputs/", fdName, sep="")
			
			#Performing only for existing folders
			if (file.exists(spFolder)) {
				cat("Species", spp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
				
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
									#cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, sep=""), "\n")
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
										#cat("Calculating:", paste(condition, "-", sres, "-", tsl, "-", threshold, "-", mig, sep=""), "\n")
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
				cat("The species", spp, "was not modeled \n")
			}
		}

		objList <- ls(pattern="richness")
		
		#Writing ascii files
		cat("\n")
		cat("Writing summary rasters \n")
		for (obj in objList) {
			rName <- paste(fdGenName, "/", obj, sep="")
			assign(obj, writeRaster(get(obj), rName, format='ascii', overwrite=T))
			png(paste(rName, ".png", sep=""))
			plot(get(obj))
			dev.off()
		}
		
		#Compressing them
		cat("\n")
		cat("Compressing summary rasters \n")
		ftoZIP <- list.files(fdGenName, pattern=".asc")
		for (fz in ftoZIP) {
			fName <- paste(fdGenName, "/", fz, sep="")
			if (OSys == "linux") {
				system(paste("gzip", fName))
			} else {
				system(paste("7za", "a", "-bd", "-tgzip", paste(fName, ".gz", sep=""), fName))
				file.remove(fName)
			}
		}
		
		#Run verification file
		verFile <- paste(fdGenName, "/ps-", genID, ".run", sep="")
		opnFile <- file(verFile, open="w")
		cat("Calculated on", date(), file=opnFile)
		close.connection(opnFile)
		
		#Now copy the files
		if (OSys == "linux") {
			destName <- paste(idir, "/.", sep="")
			system(paste("cp", "-rvf", fdGenName, destName))
			system(paste("rm", "-rvf", fdGenName))
		} else {
			destName <- oGenFolder
			origindir <- fdGenName #gsub("/", "\\\\", )
			destindir <- gsub("/", "\\\\", destName)
			system(paste("xcopy", "/E", "/I", origindir, destindir))
			system(paste("rm", "-r", fdGenName))
		}
	} else {
		cat("The genus was already modeled \n")
	}
}

################################################################
#idir <- "C:/CIAT_work/COP_CONDESAN"
#type <- "plants"
#ini <- 1
#fin <- 5

richnessProcess <- function(idir, type, ini, fin, OSys="LINUX") {
	
	OSys <- tolower(OSys)
	
	#Checking if type is within the expected
	if (!tolower(type) %in% c("plants", "aves")) {
		stop("Type of species is not supported")
	}
	
	#Reading the list of genera
	grList <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToModel.csv", sep="")
	grListComplete <- read.csv(grList)
	ngen <- nrow(grListComplete)
	
	#Checking consistency of initial and final ngen
	if (fin <= ini) {
		stop("Final number of species is less than or equal to initial, please correct")
	}
	
	if (fin > ngen) {
		cat("Final number of genera is greater than total number of species, using #SPP instead \n")
		fin <- ngen
	}
	
	gri <- grListComplete$IDGenus[ini]
	grf <- grListComplete$IDGenus[fin]
	
	#Subselecting the genera
	grList <- grListComplete$IDGenus[ini:fin]
	
	odir <- paste(idir, "/summaries", sep="")
	if (!file.exists(odir)) {
		dir.create(odir)
	}
	
	richdir <- paste(odir, "/richness-", tolower(type), sep="")
	if (!file.exists(richdir)) {
		dir.create(richdir)
	}
	genC <- 1
	
	for (gen in grList) {
		cat("\n")
		cat("...Processing genus", gen, paste("...",round(genC/length(grList)*100,2),"%",sep=""), "\n")
		
		ot <- speciesRichness(idir, richdir, gen, OSys=OSys)
		genC <- genC+1
	}
}