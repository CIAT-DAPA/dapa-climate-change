#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

require(raster)
require(maptools)
gpclibPermit()

source("createMask.R") #Create mask uses an input shapefile and a value of resolution
source("createLog.R") #Load function to create a log file to monitor the runs

#Input a GCM and another baseline raster and compare them for a particular region, defined by a shapefile and return a metrics matrix, and a chart

#Function to extract average values @gcmCells from a smaller cellsize grid
extractGCMCell <- function(x, rs, gcmRes) {
  xn <- x[1] - (gcmRes/2)
	xx <- x[1] + (gcmRes/2)
	yn <- x[2] - (gcmRes/2)
	yx <- x[2] + (gcmRes/2)
	wclRes <- (rs@extent@xmax - rs@extent@xmin) / rs@ncols
	nc <- round((xx - xn) / wclRes); nr <- round((yx - yn) / wclRes)
	bx <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr)
	xy <- xyFromCell(bx, 1:ncell(bx))
	wclVals <- extract(rs, xy)
	wclVals <- wclVals[which(!is.na(wclVals))]
	if (length(wclVals) == 0) {res <- c(NA, NA, NA)} else {res <- c(mean(wclVals), max(wclVals), min(wclVals))}
}

#Second order function to compare a raster and the higher resolution raster
compareAndPlot <- function(msk, gcmrs, wclrs, plotit=T, plotDir=".", plotName="dummy", verbose=F) { #Main function for the comparison
	#Extracting data
	cszGCM <- (gcmrs@extent@xmax - gcmrs@extent@xmin) / gcmrs@ncols
	coords <- xyFromCell(msk, which(!is.na(msk[]))); coords <- as.matrix(cbind(coords[,1], coords[,2]))
	
  #Apply function over cells
	res <- apply(coords, 1, extractGCMCell, wclrs, cszGCM)
  if (verbose) cat("Function applied \n")
  
	#Now create a raster for each parameter
	wclrs.m <- raster(msk); wclrs.m[which(!is.na(msk[]))] <- res[1,]; wclrs.m@title <- "MEAN"
	wclrs.x <- raster(msk); wclrs.x[which(!is.na(msk[]))] <- res[2,]; wclrs.x@title <- "MAX"
	wclrs.n <- raster(msk); wclrs.n[which(!is.na(msk[]))] <- res[3,]; wclrs.n@title <- "MIN"
	gcmrs.mod <- msk; gcmrs.mod[which(!is.na(msk[]))] <- xyValues(gcmrs, coords)
	gcmrs.mod[which(is.na(wclrs.m[]))] <- NA; gcmrs.mod@title <- "GCM"
	stk <- stack(gcmrs.mod, wclrs.m, wclrs.x, wclrs.n); stk@layernames <- c("GCM", "MEAN", "MAX", "MIN")
  if (verbose) cat("Rasters for assessment created \n")
  
  #Extract data from stack for metrics calculation
	compMatrix <- extract(stk, which(!is.na(wclrs.m[])))
	compMatrix <- as.data.frame(compMatrix); names(compMatrix) <- c("GCM", "CL.M", "CL.X", "CL.N")
	lims <- c(min(compMatrix), max(compMatrix))
	
  #Check if compMatrix has any of its columns with full zeros
  nz.GCM <- length(which(compMatrix$GCM == 0))
  nz.CL.M <- length(which(compMatrix$CL.M == 0))
  nz.CL.X <- length(which(compMatrix$CL.X == 0))
  nz.CL.N <- length(which(compMatrix$CL.N == 0))
  
  if (nz.GCM == nrow(compMatrix) | nz.CL.M == nrow(compMatrix)) {
    fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.mf <- NA
    fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM) #Fit normal (unforced)
    pval.m <- NA
    plot.M <- F
  } else {
  	#Fit and plot mean
  	fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1) #Fit forced to origin
  	pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
    pval.mf <- pf(summary(fit.mf)$fstatistic[1],summary(fit.mf)$fstatistic[2],summary(fit.mf)$fstatistic[3],lower.tail=F)
  	fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM) #Fit normal (unforced)
  	pd.m <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd.m <- cbind(lims, pd.m)
    pval.m <- pf(summary(fit.m)$fstatistic[1],summary(fit.m)$fstatistic[2],summary(fit.m)$fstatistic[3],lower.tail=F)
    plot.M <- T
  }
  
  if (nz.GCM == nrow(compMatrix) | nz.CL.X == nrow(compMatrix)) {
    fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.xf <- NA
    fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM) #Fit normal (unforced)
    pval.x <- NA
    plot.X <- F
  } else {
  	#Fit and plot max
  	fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1) #Fit forced to origin
  	pd.xf <- lims*fit.xf$coefficients; pd.xf <- cbind(lims, pd.xf); #plot(compMatrix$GCM, compMatrix$CL.X,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
    pval.xf <- pf(summary(fit.xf)$fstatistic[1],summary(fit.xf)$fstatistic[2],summary(fit.xf)$fstatistic[3],lower.tail=F)
  	fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM) #Fit normal (unforced)
  	pd.x <- lims*fit.x$coefficients[2] + fit.x$coefficients[1]; pd.x <- cbind(lims, pd.x)
    pval.x <- pf(summary(fit.x)$fstatistic[1],summary(fit.x)$fstatistic[2],summary(fit.x)$fstatistic[3],lower.tail=F)
    plot.X <- T
  }
  
  if (nz.GCM == nrow(compMatrix) | nz.CL.X == nrow(compMatrix)) {
    fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.nf <- NA
    fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM) #Fit normal (unforced)
    pval.n <- NA
    plot.N <- F
  } else {
  	#Fit min
  	fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1) #Fit forced to origin
  	pd.nf <- lims*fit.nf$coefficients; pd.nf <- cbind(lims, pd.nf); #plot(compMatrix$GCM, compMatrix$CL.N,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
    pval.nf <- pf(summary(fit.nf)$fstatistic[1],summary(fit.nf)$fstatistic[2],summary(fit.nf)$fstatistic[3],lower.tail=F)
  	fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM) #Fit normal (unforced)
  	pd.n <- lims*fit.n$coefficients[2] + fit.n$coefficients[1]; pd.n <- cbind(lims, pd.n)
    pval.n <- pf(summary(fit.n)$fstatistic[1],summary(fit.n)$fstatistic[2],summary(fit.n)$fstatistic[3],lower.tail=F)
    plot.N <- T
  }
  
	if (verbose) cat("Linear regressions fitted \n")
  
	if (plotit) {
    #Forced to origin
		jpeg(paste(plotDir, "/", plotName, "-forced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
		plot(compMatrix$GCM, compMatrix$CL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
		if (plot.M) {lines(pd.mf)}; #lines(pd, lty=2)
		if (plot.X) {lines(pd.xf, col="red", lty=3)}; #lines(pd.m, lty=2); #abline(0,1,lty=2)
		if (plot.N) {lines(pd.nf, col="red", lty=3)}; #lines(pd.n, lty=2); #abline(0,1,lty=2)
		for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
		abline(0,1,lty=2)
		dev.off()
    #Not forced to origin
    jpeg(paste(plotDir, "/", plotName, "-unforced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
    plot(compMatrix$GCM, compMatrix$CL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
    if (plot.M) {lines(pd.m)}
    if (plot.X) {lines(pd.x, col="red", lty=3)}
    if (plot.N) {lines(pd.n, col="red", lty=3)}
    for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
    abline(0,1,lty=2)
    dev.off()
    if (verbose) cat("Plots done \n")
	}
	
	#Calculate RMSQError, rsquare (0,0), rsquare (unforced) (y ~ x - 1 is a line through the origin, or y ~ x + 0)
  #Forced stuff
  p.value.f <- c(pval.mf,pval.xf,pval.nf)
  rsq.f <- c(summary(fit.mf)$r.squared, summary(fit.xf)$r.squared, summary(fit.nf)$r.squared)
  adj.rsq.f <- c(summary(fit.mf)$adj.r.squared, summary(fit.xf)$adj.r.squared, summary(fit.nf)$adj.r.squared)
  slp.f <- c(fit.mf$coefficients, fit.xf$coefficients, fit.nf$coefficients)
  intc.f <- c(0,0,0)
  f.f <- c(summary(fit.mf)$fstatistic[1], summary(fit.xf)$fstatistic[1], summary(fit.nf)$fstatistic[1])
  if (length(f.f) == 0) {f.f <- c(NA,NA,NA)}
  
  #Unforced stuff
  p.value <- c(pval.m,pval.x,pval.n)
  rsq <- c(summary(fit.m)$r.squared, summary(fit.x)$r.squared, summary(fit.n)$r.squared)
  adj.rsq <- c(summary(fit.m)$adj.r.squared, summary(fit.x)$adj.r.squared, summary(fit.n)$adj.r.squared)
  slp <- c(fit.m$coefficients[2], fit.x$coefficients[2], fit.n$coefficients[2])
  intc <- c(fit.m$coefficients[1], fit.x$coefficients[1], fit.n$coefficients[1])
  f <- c(summary(fit.m)$fstatistic[1], summary(fit.x)$fstatistic[1], summary(fit.n)$fstatistic[1])
  if (length(f) == 0) {f <- c(NA,NA,NA)}
  
  #Error and number of points
  npts <- rep(nrow(compMatrix),times=3)
  rmsqe <- c(sqrt(sum((compMatrix$CL.M - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.X - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.N - compMatrix$GCM) ^ 2) / nrow(compMatrix)))
  if (verbose) cat("Metrics done \n")
  
  #Final output data-frame
  m <- data.frame(VALUE=c("MEAN","MAX","MIN"), N=npts, R2.FORCED=rsq.f, ADJ.R2.FORCED=adj.rsq.f, P.VALUE.FORCED=p.value.f, SLOPE.FORCED=slp.f, INTERCEPT.FORCED=intc.f, F.STAT.FORCED=f.f, R2=rsq, ADJ.R2=adj.rsq, P.VALUE=p.value, SLOPE=slp, INTERCEPT=intc, F.STAT=f, ERROR=rmsqe)
  return(list(Metrics=m, RasterLayers=stk, plotData=compMatrix))
}

#Final primary function to be applied over directories and etc
compareRR <- function(gcmDir, gcm, wclDir, shpDir, outDir, vn="prec", divide=T, ext=".asc", country="ETH", monthList=c(6,7,8), verbose=T) {
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
		mk <- createMask(sh, mkRes)
    if (verbose) cat("Mask done! \n")
    
		#Looping months
		mcounter <- 1
		for (m in monthList) {
			if (verbose) cat("Month", m, "\n")
			if (m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
      
			#Loading the data (high resol baseline & GCM baseline)
			rsa <- raster(paste(wclDir, "/", vn, "_", m, ext, sep=""))
			rsb <- raster(paste(gcmrsDir, "/", vn, "_", mth, ext, sep=""))
      if (verbose) cat("Rasters loaded! \n")
      
			#Performing the statistical comparison
			cp <- compareAndPlot(mk, rsb, rsa, plotit=T, plotDir=outDir, plotName=paste(vn,"-",m,sep=""), verbose=verbose)
      if (verbose) cat("Monthly comparison done! \n")
      
			#Writing rasters
			for (l in 1:length(cp$RasterLayers@layers)) {
				ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",vn,"_",m,"-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
				rm(ro)
			}
      
      #Summarising for calculating total or average
			mt <- cbind(MONTH=rep(m, times=nrow(cp$Metrics)), cp$Metrics)
			pdo <- cbind(MONTH=rep(m, times=nrow(cp$plotData)), cp$plotData)
			if (length(monthList) > 1) {
				if (mcounter == 1) {
					rsd <- rsb
					rsc <- rsa
					om <- mt
					pdt <- pdo
				} else {
					rsd <- rsd + rsb
					rsc <- rsc + rsa
					om <- rbind(om, mt)
					pdt <- rbind(pdt, pdo)
				}
			}
			mcounter <- mcounter+1
		}
		if (divide) {
			rsc <- rsc / length(monthList)
			rsd <- rsd / length(monthList)
		}
    
		#Performing statistical comparison
		cp <- compareAndPlot(mk, rsd, rsc, plotit=T, plotDir=outDir, plotName=paste(vn,"-total",sep="")); cat("Comparison done! \n")
    if (verbose) cat("Comparison over total done \n")
    
    #Write those rasterLayers
		for (l in 1:length(cp$RasterLayers@layers)) {
			ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",vn,"_total-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
			rm(ro)
		}
    
    #Metrics on total
		mt <- cbind(MONTH=rep("total", times=nrow(cp$Metrics)), cp$Metrics); om <- rbind(om, mt)
		pdo <- cbind(MONTH=rep("total", times=nrow(cp$plotData)), cp$plotData); pdt <- rbind(pdt, pdo)
    
		#Writing metrics matrix and log creation
		write.csv(om, paste(outDir,"/metrics-", vn, ".csv",sep=""), row.names=F)
		write.csv(pdt, paste(outDir, "/plotData-", vn, ".csv", sep=""), row.names=F)
		createLog(outDir, lgname)
		return(om)
	} else {
    if (verbose) cat("Task completed previously \n")
    return(NA)
  }
}