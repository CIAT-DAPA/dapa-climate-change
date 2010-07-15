#Julian Ramirez, July 11 2010
require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXX SPECIES TURNOVER CALCULATION XXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

source("zipRead.R")

#1. Cycle through SRES
#2. Cycle through time-slices
#3. Cycle through thresholds
#4. Open current species binned distribution
#5. Perform the conditional and perform richness style calc

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

speciesTurnover <- function(idir, type, OSys="LINUX") {
	
	#Reading the list of genera
	gnFile <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToModel.csv", sep="")
	gnListComplete <- read.csv(gnFile)
	ngen <- nrow(gnListComplete)
	
	#Listing looping items
	threList <- c("Prevalence", "TenPercentile")
	sresList <- c("SRES_A1B", "SRES_A2")
	tsList <- c("2010_2039", "2040_2069")
	items <- c("gain", "loss", "stab", "unst")
	
	oTurnFolder <- paste(idir, "/summaries/turnover-", tolower(type), "/all-genera", sep="")
	if (!file.exists(oTurnFolder)) {
		dir.create(oTurnFolder)
	}
	
	gnList <- gnListComplete$IDGenus[1:3]
	
	cat("Processing", paste(length(gnList)), "genera \n")
	
	genC <- 1
	for (gen in gnList) {
		fdGenName <- paste("gn-", gen, sep="")
		iGenFolder <- paste(idir, "/summaries/turnover-", tolower(type), "/", fdGenName, sep="")
		
		#Performing only for existing folders
		if (file.exists(iGenFolder)) {
			cat("\n")
			cat("Genus", gen, paste("...",round(genC/length(gnList)*100,2),"%",sep=""), "\n")
			
			#Cycle through SRES
			for (sres in sresList) {
				
				#Cycle through time-slices
				for (tsl in tsList) {
					
					#Cycle through thresholds
					for (threshold in threList) {
						
						for (item in items) {
							#Assign raster name
							fName <- paste("turnover_", item, "_", sres, "_", tsl, "_", threshold, ".asc.gz", sep="")
							
							#Reading the compressed raster file
							rs <- zipRead(iGenFolder, fName)
							
							#Calculating sums for each type of calculation
							if (genC == 1) {
								#cat("Calculating sums [1] \n")
								assign(paste("turnover_", item, "_", sres, "_", tsl, "_", threshold, sep=""), rs)
								rm(rs)
							} else {
								#cat("Calculating sums \n")
								assign(paste("turnover_", item, "_", sres, "_", tsl, "_", threshold, sep=""), rs+get(paste("turnover_", item, "_", sres, "_", tsl, "_", threshold, sep="")))
								rm(rs)
							}
						}
						cat("\n")
					}
				}
			}
			
			genC <- genC+1
		} else {
			cat("The genus", gen, "was not modeled \n")
		}
	}
	objList <- ls(pattern="turnover")
	
	#Writing ascii files
	cat("Writing summary rasters \n")
	for (obj in objList) {
		rName <- paste(oTurnFolder, "/", obj, sep="")
		assign(obj, writeRaster(get(obj), rName, format='ascii', overwrite=T))
		png(paste(rName, ".png", sep=""))
		plot(get(obj))
		dev.off()
	}
	
	#Compressing them
	cat("Compressing summary rasters \n")
	ftoZIP <- list.files(oTurnFolder, pattern=".asc")
	for (fz in ftoZIP) {
		fName <- paste(oTurnFolder, "/", fz, sep="")
		if (OSys == "linux") {
			system(paste("gzip", fName))
		} else {
			system(paste("7za", "a", "-bd", "-tgzip", paste(fName, ".gz", sep=""), fName))
			file.remove(fName)
		}
	}
}

#Turnover function

turnover <- function(turnDir, richDir, type, sres, period, threshold, OSys="LINUX") {
	
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
	
	cat("Loading and calculating... \n")
	
	richness <- zipRead(richDir, paste("richness_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.gz", sep=""))
	gain <- zipRead(turnDir, paste("turnover_gain_", sres, "_", period, "_", threshold, ".asc.gz", sep=""))
	loss <- zipRead(turnDir, paste("turnover_gain_", sres, "_", period, "_", threshold, ".asc.gz", sep=""))
	
	tover <- 100*(gain-loss)/(richness - gain)
	
	oName <- paste(turnDir, "/turnover_", sres, "_", period, "_", threshold, ".asc", sep="")
	tover <- writeRaster(tover, paste(oName), overwrite=T)
	
	if (tolower(OSys) == "linux") {
		system(paste("gzip", oName))
	} else {
		system(paste("7za", "a", "-bd", "-tgzip", paste(oName, ".gz", sep=""), oName))
		file.remove(oName)
	}
	
	cat("Finished!")
	return("Done!")
}

#idir <- "L:/COP_CONDESAN"

turnoverCalc <- function(idir) {
	typeList <- c("aves", "plants")
	threList <- c("Prevalence", "TenPercentile")
	migrList <- c("FullAdap", "NullAdap")
	sresList <- c("SRES_A1B", "SRES_A2")
	tsList <- c("2010_2039", "2040_2069")
	
	for (type in typeList) {
		for (sres in sresList) {
			for (tsl in tsList) {
				for (threshold in threList) {
					cat(".Performing for:", type, sres, tsl, threshold)
					turnDir <- paste(idir, "/summaries/turnover-", tolower(type), "/all-genera", sep="")
					richDir <- paste(idir, "/summaries/richness-", tolower(type), "/all-genera", sep="")
					ot <- turnover(turnDir, richDir, type, sres, tsl, threshold, OSys='linux')
				}
			}
		}
	}
	
}



