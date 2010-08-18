require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")

#bdir <- "F:/gap_analysis_publications/gap_phaseolus"

speciesRichness <- function(bdir) {
	idir <- paste(bdir, "/modeling_data", sep="")
	
	spList <- read.csv(paste(idir, "/taxaForRichnes.csv", sep=""))

	sppC <- 1
	for (spp in spList$taxon) {
		
		cat("Processing taxon", spp, "\n")
		
		outFolder <- paste(bdir, "/species_richness", sep="")
		if (!file.exists(outFolder)) {
			dir.create(outFolder)
		}
		
		sppFolder <- paste(idir, "/mxe_outputs/sp-", spp, sep="")
		projFolder <- paste(sppFolder, "/projections", sep="")
		
		pagrid <- paste(spp, "_WorldClim-2_5min-bioclim_EMN_PA.asc.gz", sep="")
		pagrid <- zipRead(projFolder, pagrid)
		
		assign(paste("sdgrid",sppC,sep=""), paste(spp, "_WorldClim-2_5min-bioclim_ESD_PR.asc.gz", sep=""))
		assign(paste("sdgrid",sppC,sep=""), zipRead(projFolder, get(paste("sdgrid",sppC,sep=""))))
		
		if (sppC == 1) {
			sprich <- pagrid
			rm(pagrid)
			
			sdlist <- get(paste("sdgrid",sppC,sep=""))
		} else {
			sprich <- sprich + pagrid
			rm(pagrid)
			
			sdlist <- c(sdlist, get(paste("sdgrid",sppC,sep="")))
		}
		
		sppC <- sppC + 1
	}

	cat("Writing richness raster \n")
	dumm <- zipWrite(sprich, outFolder, "species-richness.asc.gz")

	cat("Calculating mean sd raster \n")
	sdmean <- sum(sdlist) / sprich
	cat("Writing \n")
	dumm <- zipWrite(sdmean, outFolder, "species-richness-sdmean.asc.gz")

	cat("Calculating max sd raster \n")
	sdmax <- max(sdlist)
	cat("Writing \n")
	dumm <- zipWrite(sdmax, outFolder, "species-richness-sdmax.asc.gz")

	cat("Calculating min sd raster \n")
	sdmin <- min(sdlist)
	cat("Writing \n")
	dumm <- zipWrite(sdmin, outFolder, "species-richness-sdmin.asc.gz")

	cat("Done! \n")

	return(stack(sprich,sdmean,sdmax,sdmin))
}