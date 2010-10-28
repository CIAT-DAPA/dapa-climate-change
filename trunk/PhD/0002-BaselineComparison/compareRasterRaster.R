#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

require(raster)
require(maptools)
gpclibPermit()

#Input a GCM and another baseline raster and compare them for a particular region, defined by a shapefile and return a metrics matrix, and a chart
#The script is composed by these functions:
#Create mask uses an input shapefile and a value of resolution

compareRR <- function(gcmDir, gcm, wclDir, shpDir, outDir, vn="prec", divide=T, ext=".asc", country="ETH", monthList=c(6,7,8)) {
	#Create country folder
	outDir <- paste(outDir, "/", country, sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
	outDir <- paste(outDir, "/", gcm, sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
	lgname <- paste("log-", vn, sep="")
	if (!file.exists(paste(outDir, "/", lgname, sep=""))) {
		#Loading basic data
		gcmrsDir <- paste(gcmDir, "/", gcm, sep="")
		sh <- readShapePoly(paste(shpDir, "/", country, "_adm/", country, "0.shp", sep=""))
		mk <- raster(paste(gcmrsDir, "/prec_01.asc", sep=""))
		#Creating the mask
		mkRes <- (mk@extent@xmax - mk@extent@xmin) / mk@ncols
		mk <- createMask(sh, mkRes); cat("Mask done! \n")
		#Looping months
		mcounter <- 1
		for (m in monthList) {
			cat("Month", m, "\n")
			if (m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
			#Loading the data (high resol baseline & GCM baseline)
			rsa <- raster(paste(wclDir, "/", vn, "_", m, ext, sep=""))
			rsb <- raster(paste(gcmrsDir, "/", vn, "_", mth, ext, sep="")); cat("Rasters loaded! \n")
			#Performing the statistical comparison
			cp <- compareAndPlot(mk, rsb, rsa, plotit=T, plotDir=outDir, plotName=paste(vn,"-",m,sep="")); cat("Comparison done! \n")
			#Plotting the whole rasterstack
			#par(mfrow=c(2,2))
			#lims <- c(cp$RasterLayers@layers[[1]][], cp$RasterLayers@layers[[2]][], cp$RasterLayers@layers[[2]][], cp$RasterLayers@layers[[3]][], cp$RasterLayers@layers[[4]][])
			#lims <- lims[which(!is.na(lims))]; lims <- c(min(lims), max(lims))
			#cat("Writing rasters \n")
			for (l in 1:length(cp$RasterLayers@layers)) {
				ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",vn,"_",m,"-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
				rm(ro)
				#plot(raster(cp$RasterLayers,l), zlim=lims)
				#plot(sh, add=T)
			}
			mt <- cbind(MONTH=rep(m, times=nrow(cp$Metrics)), cp$Metrics)
			pdo <- cbind(MONTH=rep(m, times=nrow(cp$plotData)), cp$plotData)
			if (length(monthList) > 1) {
				if (mcounter == 1) {
					#cat("Summarising (1) \n")
					rsd <- rsb
					rsc <- rsa
					om <- mt
					pdt <- pdo
				} else {
					#cat("Summarising (1+m) \n")
					rsd <- rsd + rsb
					rsc <- rsc + rsa
					om <- rbind(om, mt)
					pdt <- rbind(pdt, pdo)
				}
			}
			mcounter <- mcounter+1
		}
		#cat("Summary \n")
		if (divide) {
			#cat("Averaging \n")
			rsc <- rsc / length(monthList)
			rsd <- rsd / length(monthList)
		}
		#Performing statistical comparison
		cp <- compareAndPlot(mk, rsd, rsc, plotit=T, plotDir=outDir, plotName=paste(vn,"-total",sep="")); cat("Comparison done! \n")
		for (l in 1:length(cp$RasterLayers@layers)) {
			ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",vn,"_total-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
			rm(ro)
		}
		mt <- cbind(MONTH=rep("total", times=nrow(cp$Metrics)), cp$Metrics); om <- rbind(om, mt)
		pdo <- cbind(MONTH=rep("total", times=nrow(cp$plotData)), cp$plotData); pdt <- rbind(pdt, pdo)
		#Writing metrics matrix
		write.csv(om, paste(outDir,"/metrics-", vn, ".csv",sep=""), row.names=F)
		write.csv(pdt, paste(outDir, "/plotData-", vn, ".csv", sep=""), row.names=F)
		createLog(outDir, lgname)
		return(om)
	} else {return("Task completed previously")}
}

createLog <- function(folder, name) {
	zz <- file(paste(folder, "/", name, sep=""), open="w")
	cat("Done on", date(), "\n", file=zz)
	cat("Platform", version$platform, "\n", file=zz)
	cat("Version", version$version.string, "\n", file=zz)
	close(zz)
}

createMask <- function(shp, res) { #Function to create a mask
	xn <- shp@bbox[1,1] - 2*res
	xx <- shp@bbox[1,2] + 2*res
	yn <- shp@bbox[2,1] - 2*res
	yx <- shp@bbox[2,2] + 2*res
	nc <- round((xx - xn) / res); nr <- round((yx - yn) / res)
	xr <- (xx-xn)/nc; yr <- (yx-yn)/nr
	if (xr != yr) {yx <- xx-xn+yn} #readjust yx if horizontal and vertical grids are not equal
	rs <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr); rs[] <- 1
	rs <- polygonsToRaster(shp, rs, silent=T, getCover=T)
	rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
	return(rs)
}

extractGCMCell <- function(x, rs, gcmRes) { #Function to extract average values @gcmCells from a smaller cellsize grid
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

compareAndPlot <- function(msk, gcmrs, wclrs, plotit=T, plotDir=".", plotName="dummy") { #Main function for the comparison
	#cat("...Extracting data \n")
	cszGCM <- (gcmrs@extent@xmax - gcmrs@extent@xmin) / gcmrs@ncols
	coords <- xyFromCell(msk, which(!is.na(msk[]))); coords <- as.matrix(cbind(coords[,1], coords[,2]))
	#cat("...Apply function over cells \n")
	res <- apply(coords, 1, extractGCMCell, wclrs, cszGCM)
	#Now create a raster for each parameter
	#cat("...Raster of each par \n")
	wclrs.m <- raster(msk); wclrs.m[which(!is.na(msk[]))] <- res[1,]; wclrs.m@title <- "MEAN"
	wclrs.x <- raster(msk); wclrs.x[which(!is.na(msk[]))] <- res[2,]; wclrs.x@title <- "MAX"
	wclrs.n <- raster(msk); wclrs.n[which(!is.na(msk[]))] <- res[3,]; wclrs.n@title <- "MIN"
	gcmrs.mod <- msk; gcmrs.mod[which(!is.na(msk[]))] <- xyValues(gcmrs, coords)
	gcmrs.mod[which(is.na(wclrs.m[]))] <- NA; gcmrs.mod@title <- "GCM"
	#cat("...Stack \n")
	#cat(ncell(wclrs.m), ncell(wclrs.x), ncell(wclrs.n), ncell(gcmrs.mod), "\n")
	stk <- stack(gcmrs.mod, wclrs.m, wclrs.x, wclrs.n); stk@layernames <- c("GCM", "MEAN", "MAX", "MIN")
	compMatrix <- cellValues(stack(gcmrs.mod, wclrs.m, wclrs.x, wclrs.n), which(!is.na(wclrs.m[])))
	compMatrix <- as.data.frame(compMatrix); names(compMatrix) <- c("GCM", "CL.M", "CL.X", "CL.N")
	lims <- c(min(compMatrix), max(compMatrix))
	
	#Fit and plot mean
	#cat("...Fitting linear regs \n")
	fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1)
	pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
	fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM)
	pd <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd <- cbind(lims, pd)
	#Fit and plot max
	fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1)
	pd.xf <- lims*fit.xf$coefficients; pd.xf <- cbind(lims, pd.xf); #plot(compMatrix$GCM, compMatrix$CL.X,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
	fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM)
	pd.m <- lims*fit.x$coefficients[2] + fit.x$coefficients[1]; pd.m <- cbind(lims, pd.m)
	#Fit and plot min
	fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1)
	pd.nf <- lims*fit.nf$coefficients; pd.nf <- cbind(lims, pd.nf); #plot(compMatrix$GCM, compMatrix$CL.N,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
	fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM)
	pd.n <- lims*fit.n$coefficients[2] + fit.n$coefficients[1]; pd.n <- cbind(lims, pd.n)
	
	#cat("...Plotting charts \n")
	if (plotit) {
		jpeg(paste(plotDir, "/", plotName, ".jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
		plot(compMatrix$GCM, compMatrix$CL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
		lines(pd.mf); #lines(pd, lty=2)
		lines(pd.xf, col="red", lty=3); #lines(pd.m, lty=2); #abline(0,1,lty=2)
		lines(pd.nf, col="red", lty=3); #lines(pd.n, lty=2); #abline(0,1,lty=2)
		for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
		abline(0,1,lty=2)
		dev.off()
	}
	
	#Calculate RMSQError, rsquare (0,0), rsquare (unforced) (y ~ x - 1 is a line through the origin, or y ~ x + 0)
	#cat("...Metrics \n")
	rsq <- c(summary(fit.mf)$r.squared, summary(fit.xf)$r.squared, summary(fit.nf)$r.squared)
	adj.rsq <- c(summary(fit.mf)$adj.r.squared, summary(fit.xf)$adj.r.squared, summary(fit.nf)$adj.r.squared)
	rmsqe <- c(sqrt(sum((compMatrix$CL.M - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.X - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.N - compMatrix$GCM) ^ 2) / nrow(compMatrix)))
	f <- c(summary(fit.mf)$fstatistic[1], summary(fit.xf)$fstatistic[1], summary(fit.nf)$fstatistic[1])
	m <- data.frame(VALUE=c("MEAN","MAX","MIN"), R2=rsq, ADJ.R2=adj.rsq, ERROR=rmsqe, F.STAT=f)
	return(list(Metrics=m, RasterLayers=stk, plotData=compMatrix))
}
