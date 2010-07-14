#Extract the AUC evaluation statistics to select species with relatively high AUC (i.e. >.65 or .7)
#Test and train AUCs should be extracted from cross validated runs

cat(" \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXX SUMMARIZE METRICS SCRIPT XXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")

cat(" \n")
cat(" \n")

summarizeMetrics <- function(idir="C:/CIAT_work/COP_CONDESAN", ini, fin) {

	#idir <- "C:/CIAT_work/COP_CONDESAN"
	
	#Setting the output directory
	odir <- paste(idir, "/summaries", sep="")

	if (!file.exists(odir)) {
		dir.create(odir)
	}

	spFile <- paste(idir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
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

	#Subselecting the species
	spList <- spListComplete$IDSpecies[ini:fin]
	
	sppC <- 1
	sppCC <- 1
	for (spp in spList) {
		fdName <- paste("sp-", spp, sep="")
		spFolder <- paste(idir, "/mxe_outputs/", fdName, sep="")
		
		#Performing only for existing folders
		if (file.exists(spFolder)) {
			
			cat("Processing species", spp, paste("...",round(sppCC/length(spList)*100,2),"%",sep=""), "\n")
			
			#Loading metrics files and adding one more field (SPID)
			metricsFile <- paste(spFolder, "/metrics/ImpactMetrics.csv", sep="")
			metrics <- read.csv(metricsFile)
			
			#Comprising everything onto a matrix
			if (sppC == 1) {
				finRes <- metrics
				rm(metrics)
			} else {
				finRes <- rbind(finRes, metrics)
				rm(metrics)
			}
			
			sppC <- sppC + 1
		} else {
			cat("The species", spp, "was not modeled", paste("...",round(sppCC/length(spList)*100,2),"%",sep=""), "\n")
		}
		sppCC <- sppCC + 1
	}

	#Now writing the outputs
	cat("\n")
	cat("Writing outputs... \n")
	oFile <- paste(odir, "/ImpactMetrics.csv", sep="")
	write.csv(finRes, oFile, quote=F, row.names=F)
	
	#Return the metrics data-frame
	return(finRes)
}