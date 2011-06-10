#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

#Script to compare WorldClim weather station data with GCM cells

require(raster)
require(maptools)
require(foreign)

source("createMask.R")
source("createLog.R")

#List of months
mList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

#Basic core function to extract average values @gcmCells from the set of stations
st.extractGCMCell <- function(x, stations, mnth="JAN", gcmRes) {
  xn <- x[1] - (gcmRes/2)
  xx <- x[1] + (gcmRes/2)
	yn <- x[2] - (gcmRes/2)
	yx <- x[2] + (gcmRes/2)
	#Select stations within the cell's range
  comp.st <- stations[which(stations$LONG >= xn & stations$LONG <= xx & stations$LAT >= yn & stations$LAT <= yx),]
  m.pos <- which(names(comp.st) == mnth)
  comp.data <- comp.st[,m.pos] * 0.1
  if (length(comp.data) == 0) {resp <- c(NA,NA,NA)} else {resp <- c(mean(comp.data,na.rm=T),max(comp.data,na.rm=T),min(comp.data,na.rm=T))}
  return(resp)
}

#Second order function to compare a raster and the set of stations
st.compareAndPlot <- function(msk, gcmrs, sel.st, mName, plotit=F, plotDir=NULL, plotName=NULL, verbose=T) {
  #GCM cell size
  cszGCM <- (gcmrs@extent@xmax - gcmrs@extent@xmin) / gcmrs@ncols
  #Within-country GCM cells coordinates
  coords <- xyFromCell(msk, which(!is.na(msk[]))); coords <- as.matrix(cbind(coords[,1], coords[,2]))
  
  #Apply function to extract data from GCM cell
  st.vals <- apply(coords, 1, st.extractGCMCell, sel.st, mnth=mName, cszGCM)
  if (verbose) cat("Function applied \n")
  
  #Now create a raster for each parameter
  strs.m <- raster(msk); strs.m[which(!is.na(msk[]))] <- st.vals[1,]; strs.m@title <- "MEAN"
  strs.x <- raster(msk); strs.x[which(!is.na(msk[]))] <- st.vals[2,]; strs.x@title <- "MAX"
  strs.n <- raster(msk); strs.n[which(!is.na(msk[]))] <- st.vals[3,]; strs.n@title <- "MIN"
  gcmrs.mod <- msk; gcmrs.mod[which(!is.na(msk[]))] <- extract(gcmrs, coords)
  gcmrs.mod[which(is.na(strs.m[]))] <- NA; gcmrs.mod@title <- "GCM"
  stk <- stack(gcmrs.mod, strs.m, strs.x, strs.n); stk@layernames <- c("GCM", "MEAN", "MAX", "MIN")
  if (verbose) cat("Rasters for assessment created \n")
  
  #Matrix for assessment and limits for plotting
  compMatrix <- extract(stk, which(!is.na(strs.m[])))
  compMatrix <- as.data.frame(compMatrix); names(compMatrix) <- c("GCM", "CL.M", "CL.X", "CL.N")
  #print(compMatrix)
  #plot(msk); plot(gcmrs,add=T); points(coords,pch=20)
  lims <- c(min(compMatrix), max(compMatrix))
  
  #Check if compMatrix has any of its columns with full zeros
  nz.GCM <- length(which(compMatrix$GCM == 0))
  nz.CL.M <- length(which(compMatrix$CL.M == 0))
  nz.CL.X <- length(which(compMatrix$CL.X == 0))
  nz.CL.N <- length(which(compMatrix$CL.N == 0))
  
  #Fit mean
  if (nz.GCM == nrow(compMatrix) | nz.CL.M == nrow(compMatrix) | nrow(compMatrix) == 1) {
    fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.mf <- NA
    fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM) #Fit normal (unforced)
    pval.m <- NA
    plot.M <- F
  } else {
	#Fit mean
    fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1) #Fit forced to origin
    pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
    pval.mf <- pf(summary(fit.mf)$fstatistic[1],summary(fit.mf)$fstatistic[2],summary(fit.mf)$fstatistic[3],lower.tail=F)
    fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM) #Fit normal (unforced)
    pd.m <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd.m <- cbind(lims, pd.m)
    pval.m <- pf(summary(fit.m)$fstatistic[1],summary(fit.m)$fstatistic[2],summary(fit.m)$fstatistic[3],lower.tail=F)
    plot.M <- T
  }
 
  #Fit max
  if (nz.GCM == nrow(compMatrix) | nz.CL.X == nrow(compMatrix) | nrow(compMatrix) == 1) {
    fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.xf <- NA
    fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM) #Fit normal (unforced)
    pval.x <- NA
    plot.X <- F
  } else {
  	fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1) #Fit forced to origin
  	pd.xf <- lims*fit.xf$coefficients; pd.xf <- cbind(lims, pd.xf) #plot(compMatrix$GCM, compMatrix$CL.X,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
  	pval.xf <- pf(summary(fit.xf)$fstatistic[1],summary(fit.xf)$fstatistic[2],summary(fit.xf)$fstatistic[3],lower.tail=F)
  	fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM) #Fit normal (unforced)
  	pd.x <- lims*fit.x$coefficients[2] + fit.x$coefficients[1]; pd.x <- cbind(lims, pd.x)
  	pval.x <- pf(summary(fit.x)$fstatistic[1],summary(fit.x)$fstatistic[2],summary(fit.x)$fstatistic[3],lower.tail=F)
  	plot.X <- T
  }
 
  #Fit min
  if (nz.GCM == nrow(compMatrix) | nz.CL.N == nrow(compMatrix) | nrow(compMatrix) == 1) {
  fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1) #Fit forced to origin
    pval.nf <- NA
    fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM) #Fit normal (unforced)
    pval.n <- NA
    plot.N <- F
  } else {
	fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1) #Fit forced to origin
	pd.nf <- lims*fit.nf$coefficients; pd.nf <- cbind(lims, pd.nf) #plot(compMatrix$GCM, compMatrix$CL.N,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
	pval.nf <- pf(summary(fit.nf)$fstatistic[1],summary(fit.nf)$fstatistic[2],summary(fit.nf)$fstatistic[3],lower.tail=F)
	fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM) #Fit normal (unforced)
	pd.n <- lims*fit.n$coefficients[2] + fit.n$coefficients[1]; pd.n <- cbind(lims, pd.n)
	pval.n <- pf(summary(fit.n)$fstatistic[1],summary(fit.n)$fstatistic[2],summary(fit.n)$fstatistic[3],lower.tail=F)
	plot.N <- T
  }
 
  cat("Linear regressions fitted \n")
  
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
    if (plot.M) {lines(pd.m)}; #lines(pd, lty=2)
    if (plot.X) {lines(pd.x, col="red", lty=3)} #lines(pd.m, lty=2); #abline(0,1,lty=2)
    if (plot.N) {lines(pd.n, col="red", lty=3)} #lines(pd.n, lty=2); #abline(0,1,lty=2)
    for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
    abline(0,1,lty=2)
    dev.off()
    cat("Plots done \n")
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
  #Error and number of data points
  npts <- rep(nrow(compMatrix),times=3)
  rmsqe <- c(sqrt(sum((compMatrix$CL.M - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.X - compMatrix$GCM) ^ 2) / nrow(compMatrix)), sqrt(sum((compMatrix$CL.N - compMatrix$GCM) ^ 2) / nrow(compMatrix)))
  if (verbose) {cat("Metrics done \n")}
  
  #Final output data-frame
  m <- data.frame(VALUE=c("MEAN","MAX","MIN"), N=npts, R2.FORCED=rsq.f, ADJ.R2.FORCED=adj.rsq.f, P.VALUE.FORCED=p.value.f, SLOPE.FORCED=slp.f, INTERCEPT.FORCED=intc.f, F.STAT.FORCED=f.f, R2=rsq, ADJ.R2=adj.rsq, P.VALUE=p.value, SLOPE=slp, INTERCEPT=intc, F.STAT=f, ERROR=rmsqe)
  return(list(Metrics=m, RasterLayers=stk, plotData=compMatrix))
}

#Main function to compare
compareGHCNR <- function(gcmDir=NULL, gcm=NULL, shpDir=NULL, stationDir=NULL, country=NULL, variable=NULL, divide=T, months=c(6,7,8), outDir=NULL, verbose=T) {
  if (verbose) {cat("\n");cat("Processing", gcm, "in", country, "for", variable, "\n")}
  #Various stuff
  if (variable == "prec") {
    st.varName <- "rain"
  } else if (variable == "tmean") {
    st.varName <- "tmean"
  } else {
    st.varName <- variable
  }
  
  #Creating output directory
  if (!file.exists(outDir)) {dir.create(outDir)}
  outDir <- paste(outDir, "/", country, sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
  outDir <- paste(outDir, "/", gcm, sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
  if (verbose) cat("Output directory created \n")
  
  #Checking if this has been already done before (via the logfile)
  lgname <- paste("log-", variable, sep="")
  if (!file.exists(paste(outDir, "/", lgname, sep=""))) {
    #Directories and basic data loading
    gcmrsDir <- paste(gcmDir, "/", gcm, sep="")
    sh <- readShapePoly(paste(shpDir, "/", country, "_adm/", country, "0.shp", sep=""))
    mk <- raster(paste(gcmrsDir, "/prec_01.asc", sep="")) #mask (dummy grid, prec_01.asc by default)
    
    #Creating the mask
    mkRes <- (mk@extent@xmax - mk@extent@xmin) / mk@ncols
    mk <- createMask(sh, mkRes)
    if (verbose) {cat("Mask done! \n")}
    
    #Loading stations
    st <- read.csv(paste(stationDir, "/ghcn_", st.varName, "_1961_1990_mean.csv", sep=""))
    if (verbose) cat("Weather stations loaded! \n")
    
    #Selecting stations within the mask's range
    selst <- st[which(st$LONG >= mk@extent@xmin & st$LONG <= mk@extent@xmax & st$LAT >= mk@extent@ymin & st$LAT <= mk@extent@ymax),]
    if (verbose) cat("Stations within study area selected \n")
    if (nrow(selst) <= 2) {
      if (verbose) cat("No or very few stations within the study area \n")
      return(NA)
    } else {
      #Looping months
      mcounter <- 1
      for (m in months) {
        if (m < 10) {mth <- paste(0, m, sep="")} else {mth <- m} #Putting a 0 before the month for GCMs
        mName <- mList[m]; mName.col <- which(names(selst) == mName)
        pn <- paste(variable,"-",m,sep="") #plot name
        
        #Loading GCM rasterLayer
        gcmgrid <- raster(paste(gcmrsDir, "/", variable, "_", mth, ".asc", sep=""))
        
        #Applying comparison function
        cp <- st.compareAndPlot(mk, gcmgrid, selst, mName, plotit=T, plotDir=outDir, plotName=pn, verbose=verbose)
        if (verbose) cat("Comparison over month done \n")
        
        #Writing respective rasters
        for (l in 1:length(cp$RasterLayers@layers)) {
        	ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",variable,"_",m,"-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
        	rm(ro)
        }
        
        #Summarising metrics and calculate total of months (either average or sum)
    		mt <- cbind(MONTH=rep(m, times=nrow(cp$Metrics)), cp$Metrics)
    		pdo <- cbind(MONTH=rep(m, times=nrow(cp$plotData)), cp$plotData)
    		if (length(months) > 1) {
    			if (mcounter == 1) {
            selst$TOTAL <- selst[,mName.col]
    				rsa <- gcmgrid
    				om <- mt
    				pdt <- pdo
    			} else {
            selst$TOTAL <- selst$TOTAL + selst[,mName.col]
    				rsa <- rsa + gcmgrid
    				om <- rbind(om, mt)
    				pdt <- rbind(pdt, pdo)
    			}
    		}
    		mcounter <- mcounter+1
    	}
      #Total of months if average
    	if (divide) {
    		rsa <- rsa / length(months)
        selst$TOTAL <- selst$TOTAL / length(months)
    	}
      
    	#Performing statistical comparison in total
    	cp <- st.compareAndPlot(mk, gcmgrid, selst, mName, plotit=T, plotDir=outDir, plotName=paste(variable,"-total",sep=""), verbose=verbose)
      if (verbose) cat("Comparison over total done \n")
      
      #Writing rasterLayers
    	for (l in 1:length(cp$RasterLayers@layers)) {
    		ro <- writeRaster(raster(cp$RasterLayers,l), paste(outDir,"/",variable,"_total-",cp$RasterLayer@layernames[l],sep=""), overwrite=T)
    		rm(ro)
    	}
      
      #Summarising
    	mt <- cbind(MONTH=rep("total", times=nrow(cp$Metrics)), cp$Metrics); om <- rbind(om, mt)
    	pdo <- cbind(MONTH=rep("total", times=nrow(cp$plotData)), cp$plotData); pdt <- rbind(pdt, pdo)
      
    	#Writing metrics matrix
    	write.csv(om, paste(outDir,"/metrics-", variable, ".csv",sep=""), row.names=F)
    	write.csv(pdt, paste(outDir, "/plotData-", variable, ".csv", sep=""), row.names=F)
    	createLog(outDir, lgname)
    	return(om)
    }
  } else {
    if (verbose) cat("Task completed previously \n")
    return(NA)
  }
}