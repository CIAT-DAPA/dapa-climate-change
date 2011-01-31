#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Impact assessment metrics

require(rgdal)
require(raster)

suitchg <- function(x, y, oDir) { #x current, y future
	rs <- y - x
	#cells currently > 0
	a <- which(x[] == 0)
	#cells future > 0, unlimited mig
	b <- which(y[] == 0)
	ab <- unique(c(a,b))
	rs[ab] <- NA
	rs <- writeRaster(rs, paste(oDir, "/suitability-change.asc", sep=""), format="ascii", overwrite=T)
	rm(a); rm(b); rm(ab); gc()
	return(rs)
}

suitclass <- function(x, y) { #calculate metrics per suitability class, x=current, y=future	
	rs <- x #current suitability
	nm <- y; nm[which(rs[] == 0)] <- 0 #future no migration
	um <- y #future with migration
	a <- area(rs); a[which(is.na(rs[]))] <- NA #area in km2
	#area of each class, each 10 units from 0 to 100
	classes <- seq(0,100,by=10); classes <- cbind(classes, classes+10)
	classes <- rbind(c(0,0),classes); classes[12,2] <- 100
	classes <- cbind(classes,rep(NA,12),rep(NA,12),rep(NA,12))
	for (p in 1:nrow(classes)) {
		cat("Between", classes[p,1], "and", classes[p,2], "\n")
		if (p == 11) {
			b <- sum(a[which(rs[] > classes[p,1] & rs[] < classes[p,2])])
			n <- sum(a[which(nm[] > classes[p,1] & nm[] < classes[p,2])])
			u <- sum(a[which(um[] > classes[p,1] & um[] < classes[p,2])])
		} else if (p == 1 | p == 12) {
			b <- sum(a[which(rs[] == classes[p,1])])
			n <- sum(a[which(nm[] == classes[p,1])])
			u <- sum(a[which(um[] == classes[p,1])])
		} else {
			b <- sum(a[which(rs[] > classes[p,1] & rs[] <= classes[p,2])])
			n <- sum(a[which(nm[] > classes[p,1] & nm[] <= classes[p,2])])
			u <- sum(a[which(um[] > classes[p,1] & um[] <= classes[p,2])])
		}
		classes[p,3] <- b; classes[p,4] <- n; classes[p,5] <- u
		#area gain and loss per suit class
		k <- rs; k[] <- NA
		if (p == 11) {
			j <- which(rs[] > classes[p,1] & rs[] < classes[p,2])
			l <- which(um[] > classes[p,1] & um[] < classes[p,2])
		} else if (p == 1 | p == 12) {
			j <- which(rs[] == classes[p,1])
			l <- which(um[] == classes[p,1])
		} else {
			j <- which(rs[] > classes[p,1] & rs[] <= classes[p,2])
			l <- which(um[] > classes[p,1] & um[] <= classes[p,2])
		}
		m <- c(j,l); m <- m[which(duplicated(m) == T)]
		k[j] <- 1; k[l] <- 3; k[m] <- 2
		gain <- sum(a[which(k[] == 3)])
		loss <- sum(a[which(k[] == 1)]) 
		stab <- sum(a[which(k[] == 2)])
		
		if (p == 1) {out <- c(gain,loss,stab)} else {out <- rbind(out, c(gain,loss,stab))}
	}
	classes <- as.data.frame(classes); names(classes) <- c("lb","ub","current","future.nm","future.um")
	classes$gain <- out[,1]; classes$loss <- out[,2]; classes$stab <- out[,3]
	return(classes)
}

impact <- function(x, y, threshold=0) { #suitability and suitable-area change	
	rs <- x #current
	um <- y #future
	nm <- y; nm[which(rs[] == 0)] <- 0 #future no migration
	chg <- y-x #change
	as <- area(chg); as[which(is.na(rs[]))] <- NA #calculate pixel area
	#change in area suitable (>threshold)
	#cells currently > threshold
	a <- which(!is.na(rs[]) & rs[] > threshold)
	#cells future > threshold, unlimited mig
	b <- which(!is.na(um[]) & um[] > threshold)
	#cells future > threshold, no mig
	d <- which(!is.na(nm[]) & nm[] > threshold)
	#cells within the mask, unlimited mig
	ab <- unique(c(a,b))
	#cells within the mask, no mig
	ad <- unique(c(a,d))
	#cells increasing suitability
	e <- which(!is.na(chg[]) & chg[] > 0)
	#cells within mask and with positive change
	abe <- c(ab,e); abe <- abe[which(duplicated(abe) == T)] #unlimited mig
	ade <- c(ad,e); ade <- ade[which(duplicated(ade) == T)] #no mig
	#cells decreasing suitability
	f <- which(!is.na(chg[]) & chg[] < 0)
	#cells within mask and with negative change
	abf <- c(ab,f); abf <- abf[which(duplicated(abf) == T)] #unlimited mig
	adf <- c(ad,f); adf <- adf[which(duplicated(adf) == T)] #no mig
	#suitability metrix, mask >Threshold, unlimited mig
	th.csum <- mean(chg[ab]) #change in suitability
	th.arum <- sum(as[ab]) #total area in >threshold mask
	th.inc.csum <- mean(chg[abe]) #change in suitability, areas increasing
	th.inc.arum <- sum(as[abe]) #total area in mask with increases in suitability
	th.dec.csum <- mean(chg[abf]) #change in suitability, areas decreasing
	th.dec.arum <- sum(as[abf]) #total area in mask with decreases in suitability
	r1 <- c("un.mig",threshold,th.csum,th.arum,th.inc.csum,th.inc.arum,th.dec.csum,th.dec.arum)
	#suitability metrix, mask >Threshold, no mig
	th.csnm <- mean(chg[ad]) #change in suitability
	th.arnm <- sum(as[ad]) #total area in >threshold mask
	th.inc.csnm <- mean(chg[ade]) #change in suitability, areas increasing
	th.inc.arnm <- sum(as[ade]) #total area in mask with increases in suitability
	th.dec.csnm <- mean(chg[adf]) #change in suitability, areas decreasing
	th.dec.arnm <- sum(as[adf]) #total area in mask with decreases in suitability
	r2 <- c("no.mig",threshold,th.csnm,th.arnm,th.inc.csnm,th.inc.arnm,th.dec.csnm,th.dec.arnm)
	#return object
	res <- rbind(r1,r2); res <- as.data.frame(res)
	names(res) <- c("SCEN","THRESHOLD","AV.SUIT.CHG","AREA.SUIT","AV.SUIT.INC","AREA.SUIT.INC","AV.SUIT.DEC","AREA.SUIT.DEC")
	return(res)
}

#function to take countries and make mask and run all the above
require(raster); require(maptools)
source("D:/_tools/dapa-climate-change/trunk/EcoCrop/src/createMask.R")
shp <- readShapePoly("F:/EcoCrop-development/agricultural-data/FAOSTAT/world-adm0-sorghum.shp")
oDir <- "F:/EcoCrop-development/testing/impacts"
csr <- raster("curr-suit.asc"); fsr <- raster("futu-suit.asc")

iMetrix <- function(csr, fsr, shp, oDir) {
	res <- (csr@extent@xmax - csr@extent@xmin)/(csr@ncols) #Resolution
	#Looping polygons
	nPol <- length(shp@polygons)
	for (p in 1:nPol) {
		cat("Pol", p, "\n")
		cname <- shp@data[p]
		pol <- shp@polygons[p] #extract single polygon
		sh <- SpatialPolygons(pol) #create SP object from extracted feature
		rs <- createMask(sh, res) #create a raster from the SP object
		xy <- xyFromCell(rs, 1:ncell(rs)) #extract xy values from raster cells
		cv <- extract(csr, xy)
		fv <- extract(fsr, xy)
		cu <- rs; cu[] <- cv; rm(cv); gc()
		fu <- rs; fu[] <- fv; rm(fv); gc()
		#running impact functions
		op <- suitclass(cu, fu) #suitability classes
		im.nz <- impact(cu, fu, threshold=0) #impact for g0 mask
		im.th <- impact(cu, fu, threshold=50) #impact for g50 mask
		chg <- suitchg(cu, fu, oDir)
		rm(cu); rm(fu); rm(rs); rm(pol); rm(sh); rm(xy);rm(chg); gc()
		
		if (p == 1) {
			
		} else {
			
		}
		
		}
	}
	
}
