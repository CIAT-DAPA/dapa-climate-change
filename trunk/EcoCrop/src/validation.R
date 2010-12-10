#Julian Ramirez
#eejarv@leeds.ac.uk
#Dec 2010

#Script to validate presence and absence of the crop against suitability rating at different administrative levels
#1. input should be a shapefile and a suitability raster
#2. then extract all the values from the shapefile corresponding to the particular field that 
#   indicates presence of absence
#3. create a data-frame that has 
	#a. all the features in the shapefile,
	#b. the suitability rating (max, min, mean, sd) in one column (and derived presence/absence)
	#b1. percent suitable 
	#c. the feature presence, absence or NA corresponding value
	#d. calculate accuracy metrics
		#i. false negative rate: percent of features being predicted as if the crop was not suitable, but being
		#	marked as with the crop in national statistics, to the total number of available features to assess
		#ii. true positive rate: percent of features being predicted as suitable and also marked as suitable 
		#	 to the total number of available features to assess
		#iii. true negative rate: if possible, using absences, calculate percent of features being predicted to
		#	  not have the crop, and in fact not having the crop to the percent of features to assess
		#iv. false positive rate cannot be calculated since this is a climate-based potential suitability map

require(maptools)
require(raster)
require(rgdal)

shp <- readShapePoly("world-adm0-sorghum.shp")
rsl <- raster("D:/_tools/dapa-climate-change/trunk/EcoCrop/data/runs/selected/MEAN-sorghum-merged_suitability.asc")
field <- "ISPRES"
naValue <- -9999
extractFromShape <- function(shp, field, naValue=-9999, rsl) {
	#Extract data from shapefile
	shpData <- shp@data
	#Select analysis field and create output data frame
	anField <- grep(field, colnames(shpData))
	outmx <- cbind(as.numeric(rownames(shpData)), as.numeric(shpData[,anField])); outmx <- as.data.frame(outmx); names(outmx) <- c("ID",field)
	outmx[which(outmx[,2] == naValue),2] <- NA
	outmx$ME <- rep(NA,times=nrow(outmx))
	outmx$SD <- rep(NA,times=nrow(outmx))
	outmx$MX <- rep(NA,times=nrow(outmx))
	outmx$MN <- rep(NA,times=nrow(outmx))
	outmx$PS <- rep(NA,times=nrow(outmx))
	#Calculate resolution of rasterLayer
	res <- (rsl@extent@xmax - rsl@extent@xmin)/(rsl@ncols)
	#Process each polygon as a raster to extract suitability values
	nPol <- length(shp@polygons)
	for (p in 1:nPol) {
		cat("Pol", p, "\n")
		pol <- shp@polygons[p] #extract single polygon
		sh <- SpatialPolygons(pol) #create SP object from extracted feature
		rs <- createMask(sh, res) #create a raster from the SP object
		a <- area(rs); a <- a*rs #calculate physical area per pixel
		xy <- xyFromCell(rs, which(!is.na(rs[]))) #extract xy values from raster cells
		if (nrow(xy) == 0) {
			outmx[p,3:7] <- c(NA,NA,NA,NA,NA)
		} else {
			av <- extract(a, xy)
			vl <- extract(rsl, xy)
			sv <- (vl * 0.01) * av
			av <- av[which(!is.na(av))]; vl <- vl[which(!is.na(vl))]; sv <- sv[which(!is.na(sv))] #get rid of NAs
			if (length(vl) == 0) {outmx[p,3:7] <- c(NA,NA,NA,NA,NA)}
			else {
				phyA <- sum(av) #calculate country physical area
				suiA <- sum(sv) #calculate country suitable area
				fraA <- suiA/phyA
				outmx[p,3:7] <- c(mean(vl),sd(vl),max(vl),min(vl), fraA)
			}
		}
	}
	return(outmx)
}

createMask <- function(shp, res) { #Function to create a mask from the shapefile
	xn <- shp@bbox[1,1] - 2*res; if (xn < -180) {xn <- -180}
	xx <- shp@bbox[1,2] + 2*res; if (xx > 180) {xx <- 180}
	yn <- shp@bbox[2,1] - 2*res; if (yn < -90) {yn <- -90}
	yx <- shp@bbox[2,2] + 2*res; if (yx > 90) {yx <- 90}
	nc <- round((xx - xn) / res); xx <- (nc * res) + xn #calculating and readjusting ncols and xmax
	nr <- round((yx - yn) / res); yx <- (nr * res) + yn #calculating and readjusting nrows and ymax
	rs <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr); rs[] <- 1
	rs <- rasterize(shp, rs, silent=T, getCover=T) #getCover is TRUE to get the percent each cell is occupied by the polygon
	rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
	return(rs)
}
