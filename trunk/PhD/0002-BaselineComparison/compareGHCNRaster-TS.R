#Julian Ramirez
#CIAT / University of Leeds / CCAFS

#Compare a monthly weather station timeseries with the corresponding GCM cell centroid monthly timeseries
#using some metrics

#1. Take the station's location and extract the corresponding 1961-1990 timeseries
#2. Compare each month and also the whole year (Rsq, slope, RMSQE). This should produce a data frame where each line is a station-
#   month/year and the columns are R2, slope and RMSQE (i.e. all januaries, februaries, etc, and also all total ppt). This requires
#   a core function to extract the data and then other to measure the similarity
#3. Compare each month/year (all stations within a country). To do this, climate data from each pixel needs to be extracted
#   from a pixel and then matched with the corresponding station(s). The output data-frame should have each year-month as a
#   row and R2, slope and RMSQE as columns

#Functions in this script

#1. Extract GCM timeseries for all months and then calculate total
#2. Measure Similarity (take into account NAs), this should input a xy (measured,modelled) matrix and output the three measures
#   (R2, slope and RMSQE)
#3. Main function to apply over all stations within an area

#Purge objects in memory
rm(list=ls()); gc(); gc(T)

require(raster); require(maptools)
source("createMask.R")
gcm.chars <- read.csv("gcm_chars.csv")

#Initial setup (to be changed somehow)
vn <- "rain"
vo <- "prec"
gcmmd <- "bccr_bcm2_0"
iso <- "ETH"

#Specify data location
wd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/"
modelDir <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
adminDir <- "F:/Administrative_boundaries/SHP_files"

#Reading shapefile of the selected country
shp <- readShapePoly(paste(adminDir, "/", iso, "_adm/", iso, "0.shp", sep=""))
gcm.res <- getGCMRes(gcmmd, gcm.chars)
msk <- createMask(shp, gcm.res); msk[which(is.na(msk[]))] <- 0

#Setting working directory
setwd(wd)

#Setting output directory
od <- paste("./organized-data/", vn, "-extracted", sep=""); if (!file.exists(od)) {dir.create(od)}
odCountry <- paste(od, "/", iso, sep=""); if (!file.exists(odCountry)) {dir.create(odCountry)}
odm <- paste("./organized-data/", vn, "-metrics", sep=""); if (!file.exists(odm)) {dir.create(odm)}

#Reading stations file and selecting stations within the study area
wts.dir <- paste("./organized-data/", vn, "-per-station", sep="")
st.list <- read.csv(paste("./organized-data/ghcn_", vn, "_1961_1990_mean.csv", sep=""))
st.sel <- st.list[which(st.list$LONG <= msk@extent@xmax & st.list$LONG >= msk@extent@xmin & st.list$LAT <= msk@extent@ymax & st.list$LAT >= msk@extent@ymin),]
st.sel$INSIDE <- extract(msk, cbind(st.sel$LONG,st.sel$LAT))
st.sel <- st.sel[,c(1,3:5,ncol(st.sel))]
st.ids <- st.sel$ID
rm(st.list); g=gc()

#Loading station data and defining the site
for (st.id in st.ids) {
  cat("Processing station", paste(st.id), "\n")
  
  #Reading the weather station data
  std <- read.csv(paste("./organized-data/", vn, "-per-station/", st.id, ".csv", sep=""))
  
  #Checking whether the stuff was already done
  outCompared <- paste(odCountry, "/", st.id, "-comparison.csv", sep="")
  if (file.exists(outCompared)) {
    ts.in <- read.csv(outCompared)
    ghcn.comp <- GHCN.GCM.comp(gcmdir=modelDir, mod="bccr_bcm2_0", std, c(1961:1990), vg=vo, extract=F, ts.out=ts.in, compare=T)
  } else {
    #Getting the comparison metrics and extracted values
    ghcn.comp <- GHCN.GCM.comp(gcmdir=modelDir, mod="bccr_bcm2_0", std, c(1961:1990), vg=vo, extract=T, ts.out=NULL, compare=T)
    write.csv(ghcn.comp$VALUES, outCompared, row.names=F, quote=F)
  }
  
  ghcn.comp$METRICS <- cbind(ID=rep(st.id, times=nrow(ghcn.comp$METRICS)),ghcn.comp$METRICS)
  
  if (st.id == st.ids[1]) {
    out.metrics <- ghcn.comp$METRICS
  } else {
    out.metrics <- rbind(out.metrics, ghcn.comp$METRICS)
  }
}
write.csv(out.metrics, paste(odm, "/metrics-", iso, ".csv", sep=""), row.names=F, quote=F)


#Function to harvest data from a particular GCM and retrieve also the accuracy metrics
GHCN.GCM.comp <- function(gcmdir="", mod="bccr_bcm2_0", station.data, yl=c(1950:1960), vg="prec", extract=T, ts.out=NULL, compare=T) {
  #Define folder where yearly files are located
  yd <- paste(gcmdir, "/", mod, "/yearly_files", sep="")
  
  #Applying functions to the data
  if (extract) {cat("Extracting \n"); ts.out <- extractMonth(yl, yd, std, vg="prec", gcm=mod)}
  cat("Metrics calc \n"); metrics <- compareTS(ts.out, plotit=F)
  row.names(metrics) <- c(1:nrow(metrics))
  
  return(list(VALUES=ts.out, METRICS=metrics))
}


#Routine to compare all months in the matrix and return a data frame in which each month and the total are a line
compareTS <- function(x, plotit=F, plotName="dummy") {
  
  mthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC", "TTL")
  
  for (m in mthList) {
    col.names <- names(x)
    selCols <- grep(m, col.names)
    
    compMatrix <- x[,selCols]
    names(compMatrix) <- c("GCM","WST")
    compMatrix <- compMatrix[which(!is.na(compMatrix[,1])),]; compMatrix <- compMatrix[which(!is.na(compMatrix[,2])),]
    lims <- c(min(compMatrix), max(compMatrix))
    
    #Fit mean
    fit.mf <- lm(compMatrix$WST ~ compMatrix$GCM - 1) #Fit forced to origin
    pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
    pval.mf <- pf(summary(fit.mf)$fstatistic[1],summary(fit.mf)$fstatistic[2],summary(fit.mf)$fstatistic[3],lower.tail=F)
    fit.m <- lm(compMatrix$WST ~ compMatrix$GCM) #Fit normal (unforced)
    pd.m <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd.m <- cbind(lims, pd.m)
    pval.m <- pf(summary(fit.m)$fstatistic[1],summary(fit.m)$fstatistic[2],summary(fit.m)$fstatistic[3],lower.tail=F)
    
    if (plotit) {
      #Forced to origin
      jpeg(paste(plotDir, "/", plotName, "-forced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
      plot(compMatrix$GCM, compMatrix$WST,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
    	lines(pd.mf); #lines(pd, lty=2)
    	abline(0,1,lty=2)
    	dev.off()
      #Not forced to origin
      jpeg(paste(plotDir, "/", plotName, "-unforced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
      plot(compMatrix$GCM, compMatrix$WST,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
      lines(pd.m); #lines(pd, lty=2
      abline(0,1,lty=2)
      dev.off()
      cat("Plots done \n")
    }
    
    #Calculate RMSQError, rsquare (0,0), rsquare (unforced) (y ~ x - 1 is a line through the origin, or y ~ x + 0)
    ("Calculating metrics \n")
    #Forced stuff
    p.value.f <- pval.mf
    rsq.f <- summary(fit.mf)$r.squared
    adj.rsq.f <- summary(fit.mf)$adj.r.squared
    slp.f <- fit.mf$coefficients
    intc.f <- 0
    f.f <- summary(fit.mf)$fstatistic[1]
    if (length(f.f) == 0) {f.f <- NA}
    #Unforced stuff
    p.value <- pval.m
    rsq <- summary(fit.m)$r.squared
    adj.rsq <- summary(fit.m)$adj.r.squared
    slp <- fit.m$coefficients[2]
    intc <- fit.m$coefficients[1]
    f <- summary(fit.m)$fstatistic[1]
    if (length(f) == 0) {f <- NA}
    #Error and number of data points
    npts <- nrow(compMatrix)
    rmsqe <- sqrt(sum((compMatrix$WST - compMatrix$GCM) ^ 2) / nrow(compMatrix))
    
    #Final output data-frame
    res.m <- data.frame(MONTH=m, N=npts, R2.FORCED=rsq.f, ADJ.R2.FORCED=adj.rsq.f, P.VALUE.FORCED=p.value.f, SLOPE.FORCED=slp.f, INTERCEPT.FORCED=intc.f, F.STAT.FORCED=f.f, R2=rsq, ADJ.R2=adj.rsq, P.VALUE=p.value, SLOPE=slp, INTERCEPT=intc, F.STAT=f, ERROR=rmsqe)
    
    if (m == mthList) {
      monthly.mx <- res.m
    } else {
      monthly.mx <- rbind(monthly.mx, res.m)
    }
  }
  return(monthly.mx)
}

#Function to extract over all months across years and return a matrix with GCM and Weather Station data for matching
extractMonth <- function(year.list, year.dir, in.st.data, vg="prec", gcm="bccr_bcm2_0") {
  #Looping through months for data extraction
  for (m in 1:12) {
    cat("Processing month", m, "\n")
    
    #Correcting month
    if (m < 10) {month <- paste("0", m, sep="")} else {month <- paste(m)}
    
    #Applying function
    out <- extractTS(in.st.data, year.dir, year.list, month, vg=vg, gcm=gcm)
    
    #Generating output dataset
    if (m == 1) {
      ts.out <- out
    } else {
      ts.out <- merge(ts.out, out)
    }
  }
  #Selecting columns to calculate totals
  ts.gcm <- ts.out[,c(seq(2,ncol(ts.out),by=2))]
  ts.wst <- ts.out[,c(seq(3,ncol(ts.out),by=2))]
  
  #Calculating totals
  if (vg == "prec") {
    gcm.total <- rowSums(ts.gcm,na.rm=F)
    wst.total <- rowSums(ts.wst,na.rm=F)
  } else {
    gcm.total <- rowMeans(ts.gcm,na.rm=F)
    wst.total <- rowMeans(ts.wst,na.rm=F)
  }
  
  #Getting outputs where they should be in the output matrix
  ts.out$TTL.GCM <- gcm.total
  ts.out$TTL.WST <- wst.total
  
  return(ts.out)
}

#Function to extract data from the GCM yearly stuff for a given station and match both into a matrix
extractTS <- function(st.data, yrdir, yr.list, mth, vg="prec", gcm="bccr_bcm2_0") {
  #Pick up the name of the month from a predefined list
  mthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  mth.name <- mthList[as.numeric(mth)]
  
  #Station location
  xy <- t(as.matrix(c(st.data$LONG[1], st.data$LAT[2])))
  
  #Extracting the corresponding month from the station data file
  colm <- which(names(st.data) == mth.name)
  sel.st.data <- st.data[,c(2,colm)]
  sel.st.data[,2] <- sel.st.data[,2] * 0.1
  
  #Listing the rasters and extract the values
  rs.list <- as.list(paste(yrdir, "/", yr.list, "/", vg, "_", mth, ".asc", sep=""))
  rstack <- stack(rs.list)
  ex.vals <- extract(rstack, xy)
  
  gcm.values <- data.frame(yr.list, t(ex.vals))
  row.names(gcm.values) <- c(1:nrow(gcm.values))
  names(gcm.values) <- c("YEAR", "VALUE")
  
  m <- merge(gcm.values, sel.st.data, all.y=F)
  names(m) <- c("YEAR",paste(mth.name, ".GCM", sep=""),paste(mth.name, ".WST", sep=""))
  return(m)
}

#Basic function to get GCM resolution
getGCMRes <- function(gcm, gcm.params) {
  return(gcm.params$dxNW[which(gcm.params$model == gcm)])
}

