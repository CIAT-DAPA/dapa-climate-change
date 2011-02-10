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

extractFromShape <- function(shp, field, naValue=-9999, rsl) {
	#Extract data from shapefile
	shpData <- shp@data
	#Calculate physical area per pixel
	a <- area(rsl); a[which(is.na(rsl[]))] <- NA
	#Select analysis field and create output data frame
	anField <- grep(field, colnames(shpData))
	outmx <- cbind(as.numeric(rownames(shpData)), as.numeric(shpData[,anField])); outmx <- as.data.frame(outmx); names(outmx) <- c("ID",field)
	outmx[which(outmx[,2] == naValue),2] <- NA
	outmx$ME <- rep(NA,times=nrow(outmx)) #average
	outmx$MZ <- rep(NA,times=nrow(outmx)) #average without zeros
	outmx$SD <- rep(NA,times=nrow(outmx)) #standard deviation
	outmx$MX <- rep(NA,times=nrow(outmx)) #maximum
	outmx$MN <- rep(NA,times=nrow(outmx)) #minimum
	outmx$SM <- rep(NA,times=nrow(outmx)) #sum
	outmx$PA <- rep(NA,times=nrow(outmx)) #physical area
	outmx$SA <- rep(NA,times=nrow(outmx)) #suitable area
	outmx$PS <- rep(NA,times=nrow(outmx)) #fraction suitable area
	#Calculate resolution of rasterLayer
	res <- (rsl@extent@xmax - rsl@extent@xmin)/(rsl@ncols)
	#Process each polygon as a raster to extract suitability values
	nPol <- length(shp@polygons)
	for (p in 1:nPol) {
		cat("Pol", p, "\n")
		pol <- shp@polygons[p] #extract single polygon
		sh <- SpatialPolygons(pol) #create SP object from extracted feature
		rs <- createMask(sh, res) #create a raster from the SP object
		xy <- xyFromCell(rs, which(!is.na(rs[]))) #extract xy values from raster cells
		if (nrow(xy) == 0) {
			outmx[p,3:11] <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
		} else {
			av <- extract(a, xy)
			vl <- extract(rsl, xy)
			sv <- (vl * 0.01) * av
			av <- av[which(!is.na(av))]; vl <- vl[which(!is.na(vl))]; sv <- sv[which(!is.na(sv))] #get rid of NAs
			if (length(vl) == 0) {outmx[p,3:11] <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA)}
			else {
				vl2 <- vl[which(vl != 0)]
				if (length(vl2) == 0) {vl2m <- NA} else {vl2m <- mean(vl2)}
				phyA <- sum(av) #calculate country physical area
				suiA <- sum(sv) #calculate country suitable area
				fraA <- suiA/phyA #fraction suitable
				outmx[p,3:11] <- c(mean(vl),vl2m,sd(vl),max(vl),min(vl),sum(vl),phyA,suiA,fraA)
				rm(rs); rm(sh); rm(pol); rm(xy); rm(av); rm(vl); rm(sv)
				rm(phyA); rm(suiA); rm(fraA); rm(vl2); rm(vl2m)
				gc()
			}
		}
	}
	outmx.final <- cbind(outmx,shpData)
	rm(shpData); rm(a); rm(rsl); gc()
	return(outmx.final)
}

valMetrics <- function(mx, pres.field) {
	anField <- grep(pres.field, colnames(mx))
	if (length(anField != 1)) {anField <- anField[1]}
	#sub-table for more specific metrics
	met <- cbind(mx[,anField],mx$PS)
	met <- met[which(!is.na(met[,1])),]; met <- met[which(!is.na(met[,2])),] #get rid of NAs
	#true positive rate
	ntp <- length(which(met[,2] > 0 & met[,1] == 1))
	tpr <- ntp/length(which(met[,1] == 1))
	#false negative rate
	nfp <- length(which(met[,2] == 0 & met[,1] == 1))
	fpr <- nfp/length(which(met[,1] == 1))
	#true negative rate (if absences are available)
	if (length(which(met[,1] == 0)) != 0) {
		ntn <- length(which(met[,2] > 0 & met[,1] == 0))
		tnr <- ntn / length(which(met[,1] == 0))
	} else {tnr <- NA}
	#object to return
	rm(mx); rm(met); gc()
	met.final <- data.frame(TPR=tpr, FPR=fpr, TNR=tnr)
	return(met.final)
}
