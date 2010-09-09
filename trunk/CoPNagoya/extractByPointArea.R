require(maptools)
require(rgdal)
require(raster)

gpclibPermit()

#Generic function to extract values from a grid using a set of points, a set of polygons (shapefile), a single polygon, or a grid (mask)
#Apply that function to richness and turnover rasters

#rs <- raster("C:/CIAT_work/COP_CONDESAN/test.extract/grid.asc")
#otp <- valueByPolygon(rs=rs, datafile="C:/CIAT_work/COP_CONDESAN/test.extract/wepas_new.shp")

valueByPoint <- function(rs, datafile) {
	splt <- unlist(strsplit(datafile, ".", fixed=T))
	ext <- splt[length(splt)]
	
	if (!ext %in% c("shp", "csv")) {
		stop("Not supported file type")
	}
	
	if (ext == "shp") {
		shp <- readShapePoints(datafile)
		coords <- shp@coords
	} else {
		csv <- read.csv(datafile)
		coords <- csv[,2:3]
	}
	
	values <- xyValues(rs, coords)
	coords <- as.data.frame(cbind(coords, values))
	names(coords) <- c("X","Y","VALUE")
	
	return(coords)
}


valueByPolygon <- function(rs, datafile) {
	splt <- unlist(strsplit(datafile, ".", fixed=T))
	ext <- splt[length(splt)]
	
	if (!ext %in% c("shp")) {
		stop("Not supported file type, a shapefile should be provided")
	}
	
	shp <- readShapePoly(datafile)
	shpdata <- shp@data
	
	pol <- unlist(shp@polygons)
	npol <- length(pol)
	
	cat("\n")
	cat("Found", npol, "polygons \n")
	
	polcounter <- 1
	for (np in 1:npol) {
		
		Pol <- pol[[np]]
		nPol <- length(Pol@Polygons)
		
		for (nP in 1:nPol) {
			
			cat("\n")
			cat("Subpol", nP, "from Pol", np, "\n")
			
			p <- Pol@Polygons[nP]
			coords <- p[[1]]@coords
			
			#bbox is min/max +/- 5% of the range
			xmx <- max(coords[,1]) + (abs(max(coords[,1]) - min(coords[,1])) * 0.05)
			xmn <- min(coords[,1]) - (abs(max(coords[,1]) - min(coords[,1])) * 0.05)
			ymx <- max(coords[,2]) + (abs(max(coords[,2]) - min(coords[,2])) * 0.05)
			ymn <- min(coords[,2]) - (abs(max(coords[,2]) - min(coords[,2])) * 0.05)
			
			#ncols is fixed to 50
			cat("  .Create dummy grid \n")
			nc <- 25
			resol <- abs(xmx - xmn) / nc
			nr <- round(abs(ymx - ymn) / resol, 0)
			
			msk <- raster(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, ncol=nc, nrow=nr)
			msk[] <- 1
			
			#plot(msk)
			#lines(coords[,1], coords[,2])
			
			cat("  .Create grid from polygon \n")
			
			grd <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)))
			grd <- polygonsToRaster(grd, msk, silent=T)
			
			cat("  .Extract the values \n")
			
			coords <- xyFromCell(msk, which(!is.na(grd[])))
			values <- xyValues(rs, coords)
			values <- values[which(!is.na(values))]
			
			#points(coords, pch=20, cex=0.3)
			
			cat("  .Finalising \n")
			
			res <- c(mean(values), max(values), min(values), sd(values))
			
			if (polcounter == 1) {
				resdata <- res
			} else {
				resdata <- rbind(resdata, res)
			}
			
			polcounter <- polcounter + 1
		}
	}
	
	cat("\n")
	cat("Appending to shapefile database \n")
	shpdata <- cbind(shpdata, resdata)
	
	return(shpdata)
}