#Julian Ramirez, July 11 2010
require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXX SPECIES VERIFICATION  XXXXXXXXXXX \n")
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

verifySpecies <- function(spID, idir, fNameList, overwrite=T) {
	
	#Name of species folder
	spFolderName <- paste("sp-", spID, sep="")
	spFolder <- paste(idir, "/mxe_outputs/", spFolderName, sep="")
	
	#if the species was modeled then process it 
	if (file.exists(spFolder)) {
		cat("Verifying species", spID, "\n")
		prjFolder <- paste(spFolder, "/projections", sep="")
		
		metFolder <- paste(spFolder, "/metrics", sep="")
		oFile <- paste(metFolder, "/rasterVerification.csv", sep="")
		
		if (!file.exists(oFile) | overwrite) {
			#Loop the filenames
			fcounter <- 1
			for (f in fNameList$FILENAME) {
				cat(fcounter, ".", sep="")
				fName <- paste(spID, "_", f, ".asc.zip", sep="")
				if (f == "buf-") {
					fName <- paste(f, spID, ".asc.zip", sep="")
				}
				
				#If it's not a zip file then use gz extension
				if (!file.exists(paste(prjFolder, "/", fName, sep=""))) {
					fName <- paste(spID, "_", f, ".asc.gz", sep="")
					if (f == "buf-") {
						fName <- paste(f, spID, ".asc.gz", sep="")
					}
				}
				
				#Read the zip/gz file
				rs <- zipRead(prjFolder, fName)
				
				#Describe the raster
				zmx <- max(rs[which(!is.na(rs[]))])
				zmn <- min(rs[which(!is.na(rs[]))])
				men <- mean(rs[which(!is.na(rs[]))])
				std <- sd(rs[which(!is.na(rs[]))])
				
				#Create an output data frame
				resRow <- data.frame(SPID=spID, FILE=fcounter, NCL=ncol(rs), NRW=nrow(rs), ZMAX=zmx, ZMIN=zmn, ZMEAN=men, ZSTD=std)
				if (fcounter == 1) {
					res <- resRow
				} else {
					res <- rbind(res, resRow)
				}
				
				fcounter <- fcounter+1
			}
			cat("\n")
			
			#Write the output data-frame
			write.csv(res, oFile, quote=F, row.names=F)
			return(res)
			
		} else {
			cat("The species was previously verified \n")
		}
	
	} else {
		cat("The species was not modeled \n")
	}
}

################################################################
#idir <- "C:/CIAT_work/COP_CONDESAN"
#ini <- 1
#fin <- 5

verificationProcess <- function(idir, ini, fin) {
	
	fNameList <- read.table(paste(idir, "/summaries/standard-file-list-species.txt", sep=""), header=T)
	
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
	
	spList <- ufile$IDSpecies[ini:fin]
	sppC <- 1
	
	for (sp in spList) {
		cat("\n")
		cat("...Species", sp, paste("...",round(sppC/length(spList)*100,2),"%",sep=""), "\n")
		out <- verifySpecies(sp, idir, fNameList, overwrite=F)
		sppC <- sppC + 1
	}
	return("Done!")
}