#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

#Script to compare WorldClim weather station data with GCM cells

require(raster)
require(maptools)
require(foreign)

setwd("D:/_tools/dapa-climate-change/trunk/PhD/0002-BaselineComparison")
source("createMask.R")

#Function to extract average values @gcmCells from the set of stations
extractGCMCell <- function(x, stations, mnth="JAN", gcmRes) {
  xn <- x[1] - (gcmRes/2)
  xx <- x[1] + (gcmRes/2)
	yn <- x[2] - (gcmRes/2)
	yx <- x[2] + (gcmRes/2)
	#Select stations within the cell's range
  comp.st <- stations[which(stations$LONG >= xn & stations$LONG <= xx & stations$LAT >= yn & stations$LAT <= yx),]
  m.pos <- which(names(comp.st) == mnth)
  comp.data <- comp.st[,m.pos]
  if (length(comp.data) == 0) {res <- c(NA,NA,NA)} else {res <- c(mean(comp.data),max(comp.data),min(comp.data))}
  return(res)
}

#Set up
plotit <- T
gcmDir <- "F:/PhD-work/climate-data-assessment/cru-wcl-gcm-comparison/gcm-data/20C3M/1961_1990"
shpDir <- "F:/Administrative_boundaries/SHP_files"
plotDir <- "F:/PhD-work/climate-data-assessment/cru-wcl-gcm-comparison/testing"
plotName <- "prec-01"
country <- "ETH"
gcm <- "bccr_bcm2_0"
variable <- "prec"

#List of months
mList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
m <- 1; if (m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
mName <- mList[m]

function(gcmDir, gcm, shpDir, country, plotDir, plotName, variable, m)#here i am

#Directories and basic data loading
gcmrsDir <- paste(gcmDir, "/", gcm, sep="")
sh <- readShapePoly(paste(shpDir, "/", country, "_adm/", country, "0.shp", sep=""))
mk <- raster(paste(gcmrsDir, "/prec_01.asc", sep=""))

#Creating the mask
mkRes <- (mk@extent@xmax - mk@extent@xmin) / mk@ncols
msk <- createMask(sh, mkRes); cat("Mask done! \n")

#Loading climate rasters and stations
gcmrs <- raster(paste(gcmrsDir, "/prec_01.asc", sep=""))
st <- read.dbf("F:/PhD-work/climate-data-assessment/wcl-uncertainties/input-data/wc_rain_stations.dbf")
cat("Raster and stations loaded! \n")


#Selecting stations within the mask's range
sel.st <- st[which(st$LONG >= msk@extent@xmin & st$LONG <= msk@extent@xmax & st$LAT >= msk@extent@ymin & st$LAT <= msk@extent@ymax),]

#GCM cell size
cszGCM <- (gcmrs@extent@xmax - gcmrs@extent@xmin) / gcmrs@ncols
#Within-country GCM cells coordinates
coords <- xyFromCell(msk, which(!is.na(msk[]))); coords <- as.matrix(cbind(coords[,1], coords[,2]))

st.vals <- apply(coords, 1, extractGCMCell, sel.st, mnth="JAN", cszGCM)

#Now create a raster for each parameter
#cat("...Raster of each par \n")
strs.m <- raster(msk); strs.m[which(!is.na(msk[]))] <- st.vals[1,]; strs.m@title <- "MEAN"
strs.x <- raster(msk); strs.x[which(!is.na(msk[]))] <- st.vals[2,]; strs.x@title <- "MAX"
strs.n <- raster(msk); strs.n[which(!is.na(msk[]))] <- st.vals[3,]; strs.n@title <- "MIN"
gcmrs.mod <- msk; gcmrs.mod[which(!is.na(msk[]))] <- xyValues(gcmrs, coords)
gcmrs.mod[which(is.na(strs.m[]))] <- NA; gcmrs.mod@title <- "GCM"

stk <- stack(gcmrs.mod, strs.m, strs.x, strs.n); stk@layernames <- c("GCM", "MEAN", "MAX", "MIN")
compMatrix <- extract(stk, which(!is.na(strs.m[])))
compMatrix <- as.data.frame(compMatrix); names(compMatrix) <- c("GCM", "CL.M", "CL.X", "CL.N")
lims <- c(min(compMatrix), max(compMatrix))

#Fit and plot mean
#cat("...Fitting linear regs \n")
fit.mf <- lm(compMatrix$CL.M ~ compMatrix$GCM - 1) #Fit forced to origin
pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
pval.mf <- pf(summary(fit.mf)$fstatistic[1],summary(fit.mf)$fstatistic[2],summary(fit.mf)$fstatistic[3],lower.tail=F)
fit.m <- lm(compMatrix$CL.M ~ compMatrix$GCM) #Fit normal (unforced)
pd.m <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd.m <- cbind(lims, pd.m)
pval.m <- pf(summary(fit.m)$fstatistic[1],summary(fit.m)$fstatistic[2],summary(fit.m)$fstatistic[3],lower.tail=F)
#Fit and plot max
fit.xf <- lm(compMatrix$CL.X ~ compMatrix$GCM - 1) #Fit forced to origin
pd.xf <- lims*fit.xf$coefficients; pd.xf <- cbind(lims, pd.xf); #plot(compMatrix$GCM, compMatrix$CL.X,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
pval.xf <- pf(summary(fit.xf)$fstatistic[1],summary(fit.xf)$fstatistic[2],summary(fit.xf)$fstatistic[3],lower.tail=F)
fit.x <- lm(compMatrix$CL.X ~ compMatrix$GCM) #Fit normal (unforced)
pd.x <- lims*fit.x$coefficients[2] + fit.x$coefficients[1]; pd.x <- cbind(lims, pd.x)
pval.x <- pf(summary(fit.x)$fstatistic[1],summary(fit.x)$fstatistic[2],summary(fit.x)$fstatistic[3],lower.tail=F)
#Fit min
fit.nf <- lm(compMatrix$CL.N ~ compMatrix$GCM - 1) #Fit forced to origin
pd.nf <- lims*fit.nf$coefficients; pd.nf <- cbind(lims, pd.nf); #plot(compMatrix$GCM, compMatrix$CL.N,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="WorldClim values")
pval.nf <- pf(summary(fit.nf)$fstatistic[1],summary(fit.nf)$fstatistic[2],summary(fit.nf)$fstatistic[3],lower.tail=F)
fit.n <- lm(compMatrix$CL.N ~ compMatrix$GCM) #Fit normal (unforced)
pd.n <- lims*fit.n$coefficients[2] + fit.n$coefficients[1]; pd.n <- cbind(lims, pd.n)
pval.n <- pf(summary(fit.n)$fstatistic[1],summary(fit.n)$fstatistic[2],summary(fit.n)$fstatistic[3],lower.tail=F)

#cat("...Plotting charts \n")
if (plotit) {
  #Forced to origin
	jpeg(paste(plotDir, "/", plotName, "-forced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
	plot(compMatrix$GCM, compMatrix$CL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
	lines(pd.mf); #lines(pd, lty=2)
	lines(pd.xf, col="red", lty=3); #lines(pd.m, lty=2); #abline(0,1,lty=2)
	lines(pd.nf, col="red", lty=3); #lines(pd.n, lty=2); #abline(0,1,lty=2)
	for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
	abline(0,1,lty=2)
	dev.off()
  #Not forced to origin
  jpeg(paste(plotDir, "/", plotName, "-unforced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
  plot(compMatrix$GCM, compMatrix$CL.M,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
  lines(pd.m); #lines(pd, lty=2)
  lines(pd.x, col="red", lty=3); #lines(pd.m, lty=2); #abline(0,1,lty=2)
  lines(pd.n, col="red", lty=3); #lines(pd.n, lty=2); #abline(0,1,lty=2)
  for (i in 1:nrow(compMatrix)) {lines(c(compMatrix$GCM[i], compMatrix$GCM[i]), c(compMatrix$CL.N[i], compMatrix$CL.X[i]))}
  abline(0,1,lty=2)
  dev.off()
}

#Calculate RMSQError, rsquare (0,0), rsquare (unforced) (y ~ x - 1 is a line through the origin, or y ~ x + 0)
#cat("...Metrics \n")
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
#Final output data-frame
m <- data.frame(VALUE=c("MEAN","MAX","MIN"), N=npts, R2.FORCED=rsq.f, ADJ.R2.FORCED=adj.rsq.f, P.VALUE.FORCED=p.value.f, SLOPE.FORCED=slp.f, INTERCEPT.FORCED=intc.f, F.STAT.FORCED=f.f, R2=rsq, ADJ.R2=adj.rsq, P.VALUE=p.value, SLOPE=slp, INTERCEPT=intc, F.STAT=f, ERROR=rmsqe)
return(list(Metrics=m, RasterLayers=stk, plotData=compMatrix))


