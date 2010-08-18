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

summarizeMetrics <- function(idir="C:/CIAT_work/COP_CONDESAN") {

	#idir <- "/mnt/GeoData/Gap-analysis/cwr-gap-analysis/gap-phaeolus/modeling_data"
	
	#Setting the output directory
	odir <- paste(idir, "/summary-files", sep="")
	
	if (!file.exists(odir)) {
		dir.create(odir)
	}
	
	spList <- list.files(paste(idir, "/occurrence_files", sep=""))
	nspp <- nrow(spList)
	
	sppC <- 1
	sppCC <- 1
	for (spp in spList) {
		spp <- unlist(strsplit(spp, ".", fixed=T))[1]
		fdName <- paste("sp-", spp, sep="")
		spFolder <- paste(idir, "/mxe_outputs/", fdName, sep="")
		
		#Performing only for existing folders
		if (file.exists(spFolder)) {
			
			cat("Processing species", spp, paste("...",round(sppCC/length(spList)*100,2),"%",sep=""), "\n")
			
			#Loading metrics files and adding one more field (SPID)
			metricsFile <- paste(spFolder, "/metrics/metrics.csv", sep="")
			metrics <- read.csv(metricsFile)
			metrics <- cbind(SPID=spp, metrics)
			
			#Loading thresholds files and adding one more field (SPID)
			threshFile <- paste(spFolder, "/metrics/thresholds.csv", sep="")
			thresholds <- read.csv(threshFile)
			thresholds <- cbind(SPID=spp, thresholds)
			
			#Comprising everything onto a matrix
			if (sppC == 1) {
				finRes <- metrics
				finThr <- thresholds
				rm(thresholds)
				rm(metrics)
			} else {
				finRes <- rbind(finRes, metrics)
				finThr <- rbind(finThr, thresholds)
				rm(thresholds)
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
	oFile <- paste(odir, "/accuracy.csv", sep="")
	write.csv(finRes, oFile, quote=F, row.names=F)
	
	oFile <- paste(odir, "/thresholds.csv", sep="")
	write.csv(finThr, oFile, quote=F, row.names=F)
	
	#Return the metrics data-frame
	return(finRes)
}