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

summarizeMetrics <- function(idir="C:/CIAT_work/COP_CONDESAN", type='plants', calctype='richness', ini, fin) {
	
	#Checking if type is within the expected
	if (!tolower(type) %in% c("plants", "aves")) {
		stop("Type of species is not supported")
	}
	
	#Checking if calctype is within the expected
	if (!tolower(calctype) %in% c("richness", "turnover")) {
		stop("Type of calculation is not supported")
	}
	
	#Setting the output directory
	odir <- paste(idir, "/summaries", sep="")
	if (!file.exists(odir)) {
		dir.create(odir)
	}

	gnFile <- paste(idir, "/occurrences/modeling-data/", tolower(type), "-generaListToModel.csv", sep="")
	gnListComplete <- read.csv(gnFile)

	ngen <- nrow(gnListComplete)

	#Checking consistency of initial and final ngen
	if (fin <= ini) {
		stop("Final number of genera is less than or equal to initial, please correct")
	}

	if (fin > ngen) {
		cat("Final number of genera is greater than total number of genera, using #GEN instead \n")
		fin <- ngen
	}

	#Subselecting the genera
	gnList <- gnListComplete$IDGenus[ini:fin]

	genC <- 1
	genCC <- 1
	for (gen in gnList) {
		fdName <- paste("gn-", gen, sep="")
		gnFolder <- paste(idir, "/summaries/", tolower(calctype), "-", tolower(type), "/", fdName, sep="")
		
		#Performing only for existing folders
		if (file.exists(gnFolder)) {
			
			cat("Processing genus", gen, paste("...",round(genCC/length(gnList)*100,2),"%",sep=""), "\n")
			
			#Loading metrics files and adding one more field (SPID)
			metricsFile <- paste(gnFolder, "/rasterVerification.csv", sep="")
			metrics <- read.csv(metricsFile)
			
			#Comprising everything onto a matrix
			if (genC == 1) {
				finRes <- metrics
				rm(metrics)
			} else {
				finRes <- rbind(finRes, metrics)
				rm(metrics)
			}
			
			genC <- genC + 1
		} else {
			cat("The genus", gen, "was not modeled", paste("...",round(genCC/length(gnList)*100,2),"%",sep=""), "\n")
		}
		genCC <- genCC + 1
	}

	#Now writing the outputs
	cat("\n")
	cat("Writing outputs... \n")
	oFile <- paste(odir, "/", tolower(calctype), "-", tolower(type), "-GenusVerification.csv", sep="")
	write.csv(finRes, oFile, quote=F, row.names=F)
	
	#Return the metrics data-frame
	return(finRes)
}