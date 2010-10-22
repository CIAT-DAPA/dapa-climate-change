#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

require(SDMTools)
require(rgdal)
require(maptools)
require(raster)
gpclibPermit()

#Input a vector (lon, lat) and returns a raster with 1 in buffered areas and 0 otherwise. buffDist MUST be in meters

createBuffers <- function(x, msk, buffDist=50000, method=1, verbose=F) {
	occ <- as.data.frame(x)
	names(occ) <- c("lon", "lat")
	if (method == 1) {msk <- raster(msk); msk[which(!is.na(msk[]))] <- 0}
	if (verbose) cat("Buffering the points \n")
	if (verbose) {pb <- pbCreate(nrow(occ), "text", style=3)}
	for (pnt in 1:nrow(occ)) {
		#cat("Point", pnt, "out of", nrow(occ), "\n")
		lons = lats = NULL #objects to store points buffering hull points
		for (bear in seq(0,length=360,by=1)) { #cycle through 360 directions and get points at 50 km from hull points
			tt = destination(occ$lat[pnt],occ$lon[pnt],bearing = bear,distance = buffDist)
			lons = c(lons,tt$lon2); lats = c(lats,tt$lat2)
		}
		buff <- cbind(lons, lats)
		buff <- rbind(buff, buff[1,])
		if (method == 1) {
			#modification
			res <- (msk@extent@xmax - msk@extent@xmin) / (msk@ncols)
			nc <- ((max(lons)+res*2) - (min(lons)-res*2)) / res; nr <- ((max(lats)+res*2) - (min(lats)-res*2)) / res
			polrs <- raster(xmn=(min(lons)-res*nc*0.1), xmx=(max(lons)+res*nc*0.1), ymn=(min(lats)-res*nc*0.1), ymx=(max(lats)+res*nc*0.1), ncol=nc, nrow=nr)
			polrs[] <- 1
			pols <- Polygons(list(Polygon(buff)), pnt)
			polrs <- polygonsToRaster(SpatialPolygons(list(pols)), polrs, silent=T)
			xy <- xyFromCell(polrs, which(polrs[] == 1))
			msk[cellFromXY(msk,xy)] <- 1
		} else {
			#original
			assign(paste("pol",pnt, sep=""), Polygons(list(Polygon(buff)), pnt))
			if (pnt == 1) {
				polgrp <- c(get(paste("pol",pnt,sep="")))
			} else {
				polgrp <- c(polgrp, get(paste("pol",pnt,sep="")))
			}
		}
		if (verbose) {pbStep(pb, pnt)}
	}
	if (verbose) {pbClose(pb)}
	if (method != 1) {
		if (verbose) cat("Creating the raster \n")
		msk <- raster(msk); msk[which(!is.na(msk[]))] <- 1
		pa <- polygonsToRaster(SpatialPolygons(polgrp), msk)
		pa[which(!is.na(pa[]))] <- 1
		pa[which(is.na(pa[]) & msk[] == 1)] <- 0
		pa[which(is.na(msk[]))] <- NA
		return(pa)
	} else {
		return(msk)
	}
	
}