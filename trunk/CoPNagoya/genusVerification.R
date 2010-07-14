#Julian Ramirez, July 11 2010
require(rgdal)
require(raster)

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXX GENERA VERIFICATION  XXXXXXXXXXX \n")
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

verifyGenus <- function(genID, idir, fNameList, type='plants', calctype='richness', overwrite=T) {
	
	#Name of genus folder
	gnFolderName <- paste("gn-", genID, sep="")
	gnFolder <- paste(idir, "/summaries/", tolower(calctype), "-", tolower(type), "/", gnFolderName, sep="")
	
	#if the genus was modeled then process it 
	if (file.exists(gnFolder)) {
		cat("Verifying genus", genID, "\n")
		
		oFile <- paste(gnFolder, "/rasterVerification.csv", sep="")
		
		if (!file.exists(oFile) | overwrite) {
			#Loop the filenames
			fcounter <- 1
			for (f in fNameList$FILENAME) {
				cat(fcounter, ".", sep="")
				fName <- f
				
				#Read the zip/gz file
				rs <- zipRead(gnFolder, fName)
				
				#Describe the raster
				zmx <- max(rs[which(!is.na(rs[]))])
				zmn <- min(rs[which(!is.na(rs[]))])
				men <- mean(rs[which(!is.na(rs[]))])
				std <- sd(rs[which(!is.na(rs[]))])
				
				#Create an output data frame
				resRow <- data.frame(GENID=genID, FILE=fcounter, NCL=ncol(rs), NRW=nrow(rs), ZMAX=zmx, ZMIN=zmn, ZMEAN=men, ZSTD=std)
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
			cat("The genus was previously verified \n")
		}
	
	} else {
		cat("The genus was not modeled \n")
	}
}

################################################################
#idir <- "C:/CIAT_work/COP_CONDESAN"
#ini <- 1
#fin <- 5

verificationProcess <- function(idir, type='plants', calctype='richness', ini, fin) {
	
	fNameList <- read.table(paste(idir, "/summaries/", tolower(calctype), "-file-list-genus.txt", sep=""), header=T)
	
	ufile <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToModel.csv", sep="")
	ufile <- read.csv(ufile)
	ngen <- nrow(ufile)
	
	#Checking consistency of initial and final ngen
	if (fin <= ini) {
		stop("Final number of species is less than or equal to initial, please correct")
	}

	if (fin > ngen) {
		cat("Final number of species is greater than total number of species, using #SPP instead \n")
		fin <- ngen
	}
	
	genList <- ufile$IDGenus[ini:fin]
	genC <- 1
	
	for (gen in genList) {
		cat("\n")
		cat("...Genus", gen, paste("...",round(genC/length(genList)*100,2),"%",sep=""), "\n")
		out <- verifyGenus(gen, idir, fNameList, type=tolower(type), calctype=tolower(calctype), overwrite=F) 
		genC <- genC + 1
	}
	return("Done!")
}