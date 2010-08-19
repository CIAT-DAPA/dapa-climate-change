require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")
source("000.bufferPoints.R")

#Calculate the size of the DR, of the convexhull in km2, of the native area, and of the herbarium samples
#based on the area of the cells

#bdir <- "F:/gap_analysis_publications/gap_phaseolus"
#spID <- "Phaseolus_acutifolius"

edistDR <- function(bdir, spID) {
	
	idir <- paste(bdir, "/modeling_data", sep="")
	ddir <- paste(bdir, "/samples_calculations", sep="")
	pcdir <- paste(bdir, "/pca_data", sep="")
	
	#Creating the directories
	if (!file.exists(ddir)) {
		dir.create(ddir)
	}
	
	spOutFolder <- paste(ddir, "/", spID, sep="")
	if (!file.exists(spOutFolder)) {
		dir.create(spOutFolder)
	}
	
	#Read the thresholded raster (PA), multiply it by the area raster and sum up those cells that are != 0
	cat("Taxon", spID, "\n")
	spFolder <- paste(idir, "/mxe_outputs/sp-", spID, sep="")
	projFolder <- paste(spFolder, "/projections", sep="")
	
	cat("Loading principal components \n")
	pc1 <- zipRead(pcdir, "bio_pcac1_r.asc.gz")
	pc2 <- zipRead(pcdir, "bio_pcac2_r.asc.gz")
	
	#Edist of the DR
	cat("Reading presence/absence surface \n")
	grd <- paste(spID, "_WorldClim-2_5min-bioclim_EMN_PA.asc.gz", sep="")
	if (file.exists(paste(projFolder, "/", grd, sep=""))) {
		grd <- zipRead(projFolder, grd)
		
		cat("Env. distribution of the DR \n")
		grda <- grd * pc1 #PC1
		edistDR1 <- unique(grda[])
		edistDR1 <- edistDR1[which(edistDR1 != 0 & !is.na(edistDR1))]
		rm(grda)
		
		grda <- grd * pc2 #PC2
		edistDR2 <- unique(grda[])
		edistDR2 <- edistDR2[which(edistDR2 != 0 & !is.na(edistDR2))]
		rm(grda)
		
		rm(grd)
	} else {
		edistDR1 <- NULL
		edistDR2 <- NULL
	}
	
	#Edist of the convex-hull
	if (file.exists(paste(ddir, "/", spID, "/convex-hull.asc.gz", sep=""))) {
		cat("Reading convex hull \n")
		grd <- zipRead(paste(ddir, "/", spID, sep=""), "convex-hull.asc.gz")
		
		cat("Env. distribution of the convex hull \n")
		grda <- grd * pc1
		edistCH1 <- unique(grda[])
		edistCH1 <- edistCH1[which(edistCH1 != 0 & !is.na(edistCH1))]
		rm(grda)
		
		grda <- grd * pc2
		edistCH2 <- unique(grda[])
		edistCH2 <- edistCH2[which(edistCH2 != 0 & !is.na(edistCH2))]
		
		rm(grda)
		rm(grd)
	} else {
		edistCH1 <- NULL
		edistCH2 <- NULL
	}
	
	#Edist of the native area
	naFolder <- paste(idir, "/native-areas/asciigrids/", spID, sep="")
	if (file.exists(paste(naFolder, "/narea.asc.gz", sep=""))) {
		cat("Reading native area \n")
		grd <- zipRead(naFolder, "narea.asc.gz")
		
		cat("Env. distribution of the native area \n")
		grda <- grd * pc1
		edistNA1 <- unique(grda[])
		edistNA1 <- edistNA1[which(edistNA1 != 0 & !is.na(edistNA1))]
		rm(grda)
		
		grda <- grd * pc2
		edistNA2 <- unique(grda[])
		edistNA2 <- edistNA2[which(edistNA2 != 0 & !is.na(edistNA2))]
		
		rm(grda)
		rm(grd)
	} else {
		edistNA1 <- NULL
		edistNA2 <- NULL
	}
	
	#Edist of the herbarium samples CA50
	cat("Reading h-samples buffer \n")
	if (file.exists(paste(ddir, "/", spID, "/hsamples-buffer.asc.gz", sep=""))) {
		grd <- zipRead(paste(ddir, "/", spID, sep=""), "hsamples-buffer.asc.gz")
		
		cat("Env. distribution of h-samples buffer \n")
		grda <- grd * pc1
		edistHB1 <- unique(grda[])
		edistHB1 <- edistHB1[which(edistHB1 != 0 & !is.na(edistHB1))]
		rm(grda)
		
		grda <- grd * pc2
		edistHB2 <- unique(grda[])
		edistHB2 <- edistHB2[which(edistHB2 != 0 & !is.na(edistHB2))]
		
		rm(grda)
		rm(grd)
	} else {
		edistHB1 <- NULL
		edistHB2 <- NULL
	}
	
	#Size of the germplasm samples CA50
	cat("Reading g-samples buffer \n")
	if (file.exists(paste(ddir, "/", spID, "/gsamples-buffer.asc.gz", sep=""))) {
		grd <- zipRead(paste(ddir, "/", spID, sep=""), "gsamples-buffer.asc.gz")
		
		cat("Env. distribution of g-samples buffer \n")
		grda <- grd * pc1
		edistGB1 <- unique(grda[])
		edistGB1 <- edistGB1[which(edistGB1 != 0 & !is.na(edistGB1))]
		rm(grda)
		
		grda <- grd * pc2
		edistGB2 <- unique(grda[])
		edistGB2 <- edistGB2[which(edistGB2 != 0 & !is.na(edistGB2))]
		
		rm(grda)
		rm(grd)
	} else {
		edistGB1 <- NULL
		edistGB2 <- NULL
	}
	#Writing results
	outDF <- data.frame(DRDist.PC1=length(edistDR1), DRDist.PC2=length(edistDR2), CHDist.PC1=length(edistCH1), CHDist.PC2=length(edistCH2), NADist.PC1=length(edistNA1), NADist.PC2=length(edistNA2), HBDist.PC1=length(edistHB1), HBDist.PC2=length(edistHB2), GBDist.PC1=length(edistGB1), GBDist.PC2=length(edistGB2))
	
	write.csv(outDF, paste(spOutFolder, "/edist.csv", sep=""), quote=F, row.names=F)
	return(outDF)
}


summarizeDR <- function(idir) {
	
	ddir <- paste(idir, "/samples_calculations", sep="")
	
	odir <- paste(idir, "/modeling_data/summary-files", sep="")
	if (!file.exists(odir)) {
		dir.create(odir)
	}
	
	spList <- list.files(paste(idir, "/modeling_data/occurrence_files", sep=""))
	
	sppC <- 1
	for (spp in spList) {
		spp <- unlist(strsplit(spp, ".", fixed=T))[1]
		fdName <- paste("sp-", spp, sep="")
		spFolder <- paste(idir, "/modeling_data/mxe_outputs/", fdName, sep="")
		spOutFolder <- paste(ddir, "/", spp, sep="")
		
		if (file.exists(spFolder)) {
			
			res <- edistDR(idir, spp)
			
			metFile <- paste(spOutFolder, "/edist.csv", sep="")
			metrics <- read.csv(metFile)
			metrics <- cbind(taxon=spp, metrics)
			
			if (sppC == 1) {
				outSum <- metrics
			} else {
				outSum <- rbind(outSum, metrics)
			}
			sppC <- sppC + 1
		} else {
			cat("The taxon was never modeled \n")
		}
	}
	
	outFile <- paste(odir, "/edist.csv", sep="")
	write.csv(outSum, outFile, quote=F, row.names=F)
	return(outSum)
}