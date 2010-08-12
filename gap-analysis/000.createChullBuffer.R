require(maptools)
require(raster)
require(rgdal)
require(SDMTools)

gpclibPermit()

source("000.zipWrite.R")

chullBuffer <- function(inDir, occFile, outFolder, buffDist) {
	
	if (!file.exists(outFolder)) {
		dir.create(outFolder)
	}
	
	#Reading the occurrences
	cat("Reading occurrences \n")
	occ <- read.csv(occFile)

	#Create the convex hull
	cat("Creating the convex hull \n")
	ch <- occ[chull(cbind(occ$lon, occ$lat)),2:3]
	chClosed <- rbind(ch, ch[1,])
	
	#Buffering convex hull vertices
	cat("Buffering the convex hull \n")
	lons = lats = NULL #objects to store points buffering hull points
	for (bear in seq(0,length=360,by=1)) { #cycle through 360 directions and get points at 200 km from hull points
		tt = destination(ch$lat,ch$lon,bearing = bear,distance = buffDist)
		lons = c(lons,tt$lon2); lats = c(lats,tt$lat2)
	}
	hull.buff = chull(cbind(lats,lons)) #get the convex hull of the buffered points
	hull.buff = data.frame(lon = lons[hull.buff], lat = lats[hull.buff]) #get lat/long of the buffered convex hull
	hull.buff = rbind(hull.buff, hull.buff[1,])
	
	#now transform it into a polygon and then a raster
	cat("Transforming to polygons \n")
	msk <- raster(paste(inDir, "/masks/mask.asc", sep=""))
	pol <- SpatialPolygons(list(Polygons(list(Polygon(hull.buff)), 1)))
	pa <- polygonsToRaster(pol, msk)

	cat("Final calculations \n")
	pa[which(!is.na(pa[]))] <- 1
	pa[which(is.na(pa[]) & msk[] == 1)] <- 0
	pa[which(is.na(msk[]))] <- NA
	
	cat("Writing output \n")
	paName <- zipWrite(pa, outFolder, "narea.asc.gz")
	
	return(pa)
}
