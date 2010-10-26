#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

require(raster)

gcmDir <- "F:/climate_change/IPCC_CMIP3/20C3M/filled"
gcm <- "bccr_bcm2_0"
gcmrsDir <- paste(gcmDir, "/", gcm, "/1961_1990", sep="")
monthList <- c(5:7)

wclDir <- "F:/Clim_30s/10min_wcl_asciis" #"C:/CIAT_work/_tools/dapa-climate-change/trunk/EcoCrop/data/climate"

#Load GCM & WorldClim data
#Load each raster and also compute the total

#Define a mask or domain where the calculations are to take place this should be at original gcm resolution
bb <- c(-15, 17, 0, 18)
mk <- raster(paste(gcmrsDir, "/prec_01.asc", sep="")); mk <- crop(mk, bb)

vn <- "prec"
ext <- ".asc"
for (m in monthList) {
	if (m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
	rsa <- raster(paste(wclDir, "/", vn, "_", m, ext, sep=""))
	rsb <- raster(paste(gcmrsDir, "/", vn, "_", mth, ext, sep=""))
	cp <- compareAndPlot(mk, rsb, rsa, plotit=T, plotDir=".", plotName="dummy")
}

createMask <- function(shp, msk) {
	
	shp <- polygonToRaster()
}

compareAndPlot <- function(msk, gcmrs, wclrs, plotit=T, plotDir=".", plotName="dummy") {
	cszGCM <- (gcmrs@extent@xmax - gcmrs@extent@xmin) / gcmrs@ncols
	coords <- xyFromCell(msk, which(!is.na(msk[]))); coords <- as.matrix(cbind(coords[,1], coords[,2]))
	res <- apply(coords, 1, extractGCMCell, wclrs, cszGCM)
	
	#Now create a raster for each parameter
	wclrs.m <- raster(msk); wclrs.m[] <- res[1,]
	wclrs.x <- raster(msk); wclrs.x[] <- res[2,]
	wclrs.n <- raster(msk); wclrs.n[] <- res[3,]
	gcmrs.mod <- msk; gcmrs.mod[] <- xyValues(gcmrs, coords)
	gcmrs.mod[which(is.na(wclrs.m[]))] <- NA
	compMatrix <- cellValues(stack(gcmrs.mod, wclrs.m, wclrs.x, wclrs.n), which(!is.na(wclrs.m[])))
	compMatrix <- as.data.frame(compMatrix); names(compMatrix) <- c("GCM", "WCL.M", "WCL.X", "WCL.N")
	lims <- c(min(compMatrix), max(compMatrix))
	
	#Fit and plot mean
	fit.mf <- lm(compMatrix$WCL.M ~ compMatrix$GCM - 1)
	pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
	fit.m <- lm(compMatrix$WCL.M ~ compMatrix$GCM)
	pd <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd <- cbind(lims, pd)
	#Fit and plot max
	fit.xf <- lm(compMatrix$WCL.X ~ compMatrix$GCM - 1)
	pd.xf <- lims*fit.xf$coefficients; pd.xf <- cbind(lims, pd.xf); #plot(compMatrix$GCM, compMatrix$WCL.X,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
	fit.x <- lm(compMatrix$WCL.X ~ compMatrix$GCM)
	pd.m <- lims*fit.x$coefficients[2] + fit.x$coefficients[1]; pd.m <- cbind(lims, pd.m)
	#Fit and plot min
	fit.nf <- lm(compMatrix$WCL.N ~ compMatrix$GCM - 1)
	pd.nf <- lims*fit.nf$coefficients; pd.nf <- cbind(lims, pd.nf); #plot(compMatrix$GCM, compMatrix$WCL.N,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
	fit.n <- lm(compMatrix$WCL.N ~ compMatrix$GCM)
	pd.n <- lims*fit.n$coefficients[2] + fit.n$coefficients[1]; pd.n <- cbind(lims, pd.n)
	
	if (plotit) {
		jpeg(paste(plotDir, "/", plotName, ".jpg", sep=""))
		plot(compMatrix$GCM, compMatrix$WCL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
		lines(pd.mf); #lines(pd, lty=2)
		lines(pd.xf, col="red", lty=3); #lines(pd.m, lty=2); #abline(0,1,lty=2)
		lines(pd.nf, col="red", lty=3); #lines(pd.n, lty=2); #abline(0,1,lty=2)
		for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$WCL.N[i], compMatrix$WCL.X[i]))}
		abline(0,1,lty=2)
		dev.off()
	}
	
	#Calculate RMSQError, rsquare (0,0), rsquare (unforced) (y ~ x - 1 is a line through the origin, or y ~ x + 0)
	rsq <- c(summary(fit.mf)$r.squared, summary(fit.xf)$r.squared, summary(fit.nf)$r.squared)
	adj.rsq <- c(summary(fit.mf)$adj.r.squared, summary(fit.xf)$adj.r.squared, summary(fit.nf)$adj.r.squared)
	rmsqe <- c(sqrt(sum((compMatrix$WCL.M - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$WCL.X - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$WCL.N - compMatrix$GCM) ^ 2) / nrow(compMatrix)))
	f <- c(summary(fit.mf)$fstatistic[1], summary(fit.xf)$fstatistic[1], summary(fit.nf)$fstatistic[1])
	m <- data.frame(VALUE=c("MEAN","MAX","MIN"), R2=rsq, ADJ.R2=adj.rsq, ERROR=rmsqe, F.STAT=f)
	return(m)
}

extractGCMCell <- function(x, rs, gcmRes) {
	xn <- x[1] - (gcmRes/2)
	xx <- x[1] + (gcmRes/2)
	yn <- x[2] - (gcmRes/2)
	yx <- x[2] + (gcmRes/2)
	wclRes <- (rs@extent@xmax - rs@extent@xmin) / rs@ncols
	nc <- round((xx - xn) / wclRes); nr <- round((yx - yn) / wclRes)
	bx <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr)
	xy <- xyFromCell(bx, 1:ncell(bx))
	wclVals <- xyValues(rs, xy)
	wclVals <- wclVals[which(!is.na(wclVals))]
	if (length(wclVals) == 0) {res <- c(NA, NA, NA)} else {res <- c(mean(wclVals), max(wclVals), min(wclVals))}
}

