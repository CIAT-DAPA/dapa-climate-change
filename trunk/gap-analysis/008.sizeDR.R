require(rgdal)
require(raster)

source("000.zipRead.R")
source("000.zipWrite.R")

#Calculate the size of the DR, of the convexhull in km, of the native area, and of the herbarium samples
#based on the area of the cells

#idir <- "F:/gap_analysis_publications/gap_phaseolus/modeling_data"
#spID <- "Phaseolus_acutifolius"

sizeDR <- function(indir, spID) {
	#Read the thresholded raster (PA), multiply it by the area raster and sum up those cells that are != 0
	
	cat("Taxon", spID, "\n")
	spFolder <- paste(idir, "/mxe_outputs/sp-", spID, sep="")
	projFolder <- paste(spFolder, "/projections", sep="")
	
	mskArea <- paste(idir, "/masks/cellArea.asc", sep="")
	mskArea <- raster(mskArea)
	msk <- paste(idir, "/masks/mask.asc", sep="")
	msk <- raster(msk)
	
	#Size of the DR
	cat("Reading raster files \n")
	paGrid <- paste(spID, "_WorldClim-2_5min-bioclim_EMN_PA.asc.gz", sep="")
	paGrid <- zipRead(projFolder, paGrid)
	
	cat("Size of the DR \n")
	paGrid <- paGrid * mskArea
	areaDR <- sum(paGrid[which(paGrid[] != 0)])
	
	#Size of the convexhull
	cat("Reading occurrences \n")
	occ <- read.csv(paste(indir, "/occurrence_files/", spID, ".csv", sep=""))
	
	cat("Creating the convex hull \n")
	ch <- occ[chull(cbind(occ$lon, occ$lat)),2:3]
	ch <- rbind(ch, ch[1,])
	
	cat("Transforming to polygons \n")
	pol <- SpatialPolygons(list(Polygons(list(Polygon(ch)), 1)))
	chGrid <- polygonsToRaster(pol, msk)
	
	cat("Final fixes \n")
	chGrid[which(!is.na(chGrid[]))] <- 1
	chGrid[which(is.na(chGrid[]) & msk[] == 1)] <- 0
	chGrid[which(is.na(msk[]))] <- NA
	
	cat("Writing convex hull \n")
	chName <- zipWrite(chGrid, projFolder, paste(spID, "_chull.asc.gz", sep=""))
	
	cat("Size of the convex hull \n")
	
	#Size of the native area
	
	#Size of the herbarium samples CA50
	
	#Size of the 
}