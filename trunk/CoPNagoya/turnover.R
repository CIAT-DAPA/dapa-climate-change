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

#To calculate turnover you need to calculate the species gain in each pixel (number of species arriving) 
#and the species loss in each pixel (number of species leaving). Use a different script for each type of calculation

#This is done only for the FullAdap migration scenario
#Calculate for each species: 

#IF curr==1 & fut==0, loss=1, else loss=0 ........ this happens in a pixel when a species is lost
#IF curr==0 & fut==1, gain=1, else gain=0 ........ this happens in a pixel when a species is gained
#IF curr==1 & fut==1, stable=1, else stable=0 .... this happens in a pixel when a species remains there
#IF curr==0 & fut==0, unsuit=1, else unsuit=0 .... this happens in a pixel when a species never arrives

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

speciesTurnover <- function(idir, type="plants", ini, fin) {
	
	#Checking if type is within the expected
	if (!tolower(type) %in% c("plants", "aves")) {
		stop("Type of species is not supported")
	}
	
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

	turndir <- paste(odir, "/turnover-", spi, "-", spf, sep="")
	if (!file.exists(turndir)) {
		dir.create(turndir)
	}

	threList <- c("Prevalence", "TenPercentile")
	sresList <- c("SRES_A1B", "SRES_A2")
	tsList <- c("2010_2039", "2040_2069")

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
			
			#Cycle through SRES
			for (sres in sresList) {
				
				#Cycle through time-slices
				for (tsl in tsList) {
					
					#Cycle through thresholds
					for (threshold in threList) {
						
						cat("Calculating:", paste(sres, "-", tsl, "-", threshold, sep=""), "\n")
						
						cat("Loading... ")
						#Read the baseline raster
						BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.zip", sep="")
						if (!file.exists(paste(rsFolder, "/", BLfName, sep=""))) {
							BLfName <- paste(spp, "_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_", threshold, ".asc.gz", sep="")
						}
						BLrs <- zipRead(rsFolder, BLfName)
						
						#Loading the future raster
						fName <- paste(spp, "_future_", sres, "_disaggregated_", tsl, "_", threshold, "_FullAdap.asc.zip", sep="")
						if (!file.exists(paste(rsFolder, "/", fName, sep=""))) {
							fName <- paste(spp, "_", condition, "_", sres, "_disaggregated_", tsl, "_", threshold, "_FullAdap.asc.gz", sep="")
						}
						
						#Loading the raster
						rs <- zipRead(rsFolder, fName)
						
						#IF curr==1 & fut==0, loss=1, else loss=0 ........ this happens in a pixel when a species is lost
						cat("Calculating... Loss... ")
						loss <- rs
						loss[which(!is.na(loss[]))] <- 0
						if (length(which(BLrs[] == 1 & rs[] == 0)) != 0) {
							loss[which(BLrs[] == 1 & rs[] == 0)] <- 1
						}
						
						#IF curr==0 & fut==1, gain=1, else gain=0 ........ this happens in a pixel when a species is gained
						cat("Gain... ")
						gain <- rs
						gain[which(!is.na(gain[]))] <- 0
						if (length(which(BLrs[] == 0 & rs[] == 1)) != 0) {
							gain[which(BLrs[] == 0 & rs[] == 1)] <- 1
						}
						
						#IF curr==1 & fut==1, stable=1, else stable=0 .... this happens in a pixel when a species remains there
						cat("Stable... ")
						stab <- rs
						stab[which(!is.na(stab[]))] <- 0
						if (length(which(BLrs[] == 1 & rs[] == 1)) != 0) {
							stab[which(BLrs[] == 1 & rs[] == 1)] <- 1
						}
						
						#IF curr==0 & fut==0, unsuit=1, else unsuit=0 .... this happens in a pixel when a species never arrives
						cat("Unsuitable \n")
						unst <- rs
						unst[which(!is.na(unst[]))] <- 0
						if (length(which(BLrs[] == 0 & rs[] == 0)) != 0) {
							unst[which(BLrs[] == 0 & rs[] == 0)] <- 1
						}
						
						rm(rs)
						rm(BLrs)
						#Calculating sum of species
						if (sppC == 1) {
							assign(paste("turnover_loss_", sres, "_", tsl, "_", threshold, sep=""), loss)
							rm(loss)
							
							assign(paste("turnover_gain_", sres, "_", tsl, "_", threshold, sep=""), gain)
							rm(gain)
							
							assign(paste("turnover_stab_", sres, "_", tsl, "_", threshold, sep=""), stab)
							rm(stab)
							
							assign(paste("turnover_unst_", sres, "_", tsl, "_", threshold, sep=""), unst)
							rm(unst)
						} else {
							assign(paste("turnover_loss_", sres, "_", tsl, "_", threshold, sep=""), loss+get(paste("turnover_loss_", sres, "_", tsl, "_", threshold, sep="")))
							rm(loss)
							
							assign(paste("turnover_gain_", sres, "_", tsl, "_", threshold, sep=""), gain+get(paste("turnover_gain_", sres, "_", tsl, "_", threshold, sep="")))
							rm(gain)
							
							assign(paste("turnover_stab_", sres, "_", tsl, "_", threshold, sep=""), stab+get(paste("turnover_stab_", sres, "_", tsl, "_", threshold, sep="")))
							rm(stab)
							
							assign(paste("turnover_unst_", sres, "_", tsl, "_", threshold, sep=""), unst+get(paste("turnover_unst_", sres, "_", tsl, "_", threshold, sep="")))
							rm(unst)
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

	cat("\n")
	cat("Writing summary rasters \n")

	objList <- ls(pattern="turnover")

	for (obj in objList) {
		rName <- paste(turndir, "/", obj, sep="")
		assign(obj, writeRaster(get(obj), rName, format='ascii', overwrite=T))
		png(paste(rName, ".png", sep=""))
		plot(get(obj))
		dev.off()
	}

	cat("\n")
	cat("Done! \n")
}