require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")
source("000.bufferPoints.R")

#bdir <- "F:/gap_analysis_publications/gap_phaseolus"

#For HPTs with invalid models use the Hsamples buffer, if hsamples do not exist 

gapRichness <- function(bdir) {
	idir <- paste(bdir, "/modeling_data", sep="")
	ddir <- paste(bdir, "/samples_calculations", sep="")
	
	outFolder <- paste(bdir, "/gap_richness", sep="")
	if (!file.exists(outFolder)) {
		dir.create(outFolder)
	}
	
	spList <- read.csv(paste(idir, "/HPTaxa.csv", sep=""))
	spList <- spList[which(spList$Class == "HPS"),]
	
	cat("\n")
	cat("Processing", nrow(spList), "HP Taxa \n")
	
	allOcc <- read.csv(paste(bdir, "/samples/phaseolus_all.csv", sep=""))
	
	sppC <- 1
	rcount <- 1
	scount <- 1
	for (spp in spList$Taxon) {
		
		cat("Processing taxon", paste(spp), "\n")
		
		isValid <- spList$ValidModel[which(spList$Taxon == paste(spp))]
		
		sppFolder <- paste(idir, "/mxe_outputs/sp-", spp, sep="")
		projFolder <- paste(sppFolder, "/projections", sep="")
		
		#Size of the herbarium samples CA50
		cat("Calculating h-samples buffer \n")
		tallOcc <- allOcc[which(allOcc$Taxon == paste(spp)),]
		hOcc <- tallOcc[which(tallOcc$Sampletype == "H"),]
		if (nrow(hOcc) != 0) {
			spOutFolder <- paste(ddir, "/", spp, sep="")
			
			if (!file.exists(paste(spOutFolder, "/hsamples-buffer.asc.gz", sep=""))) {
				if (!file.exists(spOutFolder)) {
					dir.create(spOutFolder)
				}
				hOcc <- as.data.frame(cbind(as.character(hOcc$Taxon), hOcc$Longitude, hOcc$Latitude))
				names(hOcc) <- c("taxon", "lon", "lat")
				
				write.csv(hOcc, paste(spOutFolder, "/hsamples.csv", sep=""), quote=F, row.names=F)
				rm(hOcc)
				grd <- createBuffers(paste(spOutFolder, "/hsamples.csv", sep=""), spOutFolder, "hsamples-buffer.asc", 50000, paste(idir, "/masks/mask.asc", sep=""))
			} else {
				grd <- zipRead(spOutFolder, "hsamples-buffer.asc.gz")
			}
		}
		
		hbuffFile <- paste(spOutFolder, "/hsamples-buffer.asc.gz", sep="")
		
		#Size of the genebank accessions CA50
		cat("Calculating g-samples buffer \n")
		gOcc <- tallOcc[which(tallOcc$Sampletype == "G"),]
		if (nrow(gOcc) != 0) {
			spOutFolder <- paste(ddir, "/", spp, sep="")
			
			if (!file.exists(paste(spOutFolder, "/gsamples-buffer.asc.gz", sep=""))) {
				if (!file.exists(spOutFolder)) {
					dir.create(spOutFolder)
				}
				gOcc <- as.data.frame(cbind(as.character(gOcc$Taxon), gOcc$Longitude, gOcc$Latitude))
				names(gOcc) <- c("taxon", "lon", "lat")
				
				write.csv(gOcc, paste(spOutFolder, "/gsamples.csv", sep=""), quote=F, row.names=F)
				rm(hOcc)
				grd.ga <- createBuffers(paste(spOutFolder, "/gsamples.csv", sep=""), spOutFolder, "gsamples-buffer.asc", 50000, paste(idir, "/masks/mask.asc", sep=""))
			} else {
				grd.ga <- zipRead(spOutFolder, "gsamples-buffer.asc.gz")
			}
		}
		
		gbuffFile <- paste(spOutFolder, "/gsamples-buffer.asc.gz", sep="")
		
		#Presence absence surfaces
		
		if (isValid == 1) {
			cat("Presence/absence surf. exists, using it \n")
			pagrid <- paste(spp, "_WorldClim-2_5min-bioclim_EMN_PA.asc.gz", sep="")
			pagrid <- zipRead(projFolder, pagrid)
			
			if (file. exists(gbuffFile)) {
				pagrid[which(grd.ga[] == 1)] <- 0
			}
			
			assign(paste("sdgrid",sppC,sep=""), paste(spp, "_WorldClim-2_5min-bioclim_ESD_PR.asc.gz", sep=""))
			assign(paste("sdgrid",sppC,sep=""), zipRead(projFolder, get(paste("sdgrid",sppC,sep=""))))
			
			assign(paste("dpgrid",sppC,sep=""), zipRead(spOutFolder, "pop-dist.asc.gz"))
			assign(paste("dpgrid",sppC,sep=""), get(paste("dpgrid",sppC,sep="")) * pagrid)
		} else if (file.exists(hbuffFile)) {
			cat("Presence/absence surf. does not exist or is not reliable, using hsamples instead \n")
			pagrid <- grd
			
			if (file. exists(gbuffFile)) {
				pagrid[which(grd.ga[] == 1)] <- 0
			}
			
			assign(paste("dpgrid",sppC,sep=""), zipRead(spOutFolder, "pop-dist.asc.gz"))
			assign(paste("dpgrid",sppC,sep=""), get(paste("dpgrid",sppC,sep="")) * pagrid)
		} else {
			cat("No PA surface, no HSamples, cannot map it out \n")
		}
		
		#Gap richness
		if (rcount == 1) {
			if (isValid == 1 | file.exists(hbuffFile)) {
				sprich <- pagrid
				rm(pagrid)
				dplist <- get(paste("dpgrid",sppC,sep=""))
				rcount <- rcount + 1
			}
		} else {
			if (isValid == 1 | file.exists(hbuffFile)) {
				sprich <- sprich + pagrid
				rm(pagrid)
				dplist <- c(dplist, get(paste("dpgrid",sppC,sep="")))
				rcount <- rcount + 1
			}
		}
		
		#Aggregated SD
		if (scount == 1) {
			if (isValid == 1) {
				sdlist <- get(paste("sdgrid",sppC,sep=""))
				scount <- scount + 1
			}
		} else {
			if (isValid == 1) {
				sdlist <- c(sdlist, get(paste("sdgrid",sppC,sep="")))
				scount <- scount + 1
			}
		}
		
		sppC <- sppC + 1
	}

	cat("Writing richness raster \n")
	dumm <- zipWrite(sprich, outFolder, "gap-richness.asc.gz")
	
	cat("Calculating mean pdist raster \n")
	dpmean <- sum(stack(dplist)) / sprich
	cat("Writing \n")
	dumm <- zipWrite(dpmean, outFolder, "gap-richness-dpmean.asc.gz")
	
	cat("Calculating max pdist raster \n")
	dpmax <- max(stack(dplist))
	cat("Writing \n")
	dumm <- zipWrite(dpmax, outFolder, "gap-richness-dpmax.asc.gz")
	
	cat("Calculating min pdist raster \n")
	dpmin <- min(stack(dplist))
	cat("Writing \n")
	dumm <- zipWrite(dpmin, outFolder, "gap-richness-dpmin.asc.gz")
	
	cat("Calculating mean sd raster \n")
	sdmean <- sum(stack(sdlist)) / sprich
	cat("Writing \n")
	dumm <- zipWrite(sdmean, outFolder, "gap-richness-sdmean.asc.gz")

	cat("Calculating max sd raster \n")
	sdmax <- max(stack(sdlist))
	cat("Writing \n")
	dumm <- zipWrite(sdmax, outFolder, "gap-richness-sdmax.asc.gz")
	
	cat("Done! \n")

	return(stack(sprich,sdmean,sdmax,dpmean,dpmax,dpmin))
}