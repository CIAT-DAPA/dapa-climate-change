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
rm(list=ls()); g=gc()

####################################################################################
########PACKAGE REQUIREMENTS
####################################################################################
require(raster); require(maptools)
source("createMask.R")
gcm.chars <- read.csv("gcm_chars.csv")
####################################################################################

####################################################################################
########FUNCTION: processCompareWS
########ARGUMENTS: work.dir, out.dir, gcmdir, which, aDir, var.in, var.out, iso.ctry, time.series
####################################################################################
########work.dir: working directory where the weather station data are stored
########out.dir: output folder (gsod-vs-gcm-ts) where [var.out]-extracted, [var.out]-timeseries,
########         and [var.out]-metrics folders will be created
########gcmdir: folder where GCM data is located (i.e. [folder]/[GCM]/yearly_files/[year]/*.*)
########which: either a numeric vector between 1 and 24 indicating the GCM(s) to process, or "ALL" if all GCMs are to
########       be processed (i.e. [ALL | c(1,2,5,10)]).
########aDir: folder where all administrative-type shapefiles are located ([aDir]/[iso.ctry]_adm/*.*)
########var.in: name of variable as referred in weather station data (typically rain, tmean, tmin, tmax)
########var.out: name of variable as referred in GCM data (typically prec, tmean, tmin, tmax)
########iso.ctry: ISO of the country to process. Can be any as long as shapefile is within aDir (i.e. ETH, AFG, COL)
########time.series: a numeric vector with all years to process (i.e. c(1961:1999), or c(1950,1951,1966))
####################################################################################
#Main function to process all GCMs, or a subset of them if required
processCompareWS <- function(work.dir="", out.dir="", gcmdir="", which="ALL", aDir="", var.in="rain", var.out="prec", iso.ctry="ETH", time.series=c(1961:1990)) {
  gcmList <- list.files(gcmdir)
  if (length(which) == 1) {
    if (which != "ALL") {gcmList <- gcmList[which]}
  } else {
    gcmList <- gcmList[which]
  }
  
  for (gcm in gcmList) {
    cat("\n")
    cat("Processing GCM:", gcm, "\n")
    asts <- analyseStations.TS(wd=work.dir, modelDir=gcmdir, adminDir=aDir, outDir=out.dir, vn=var.in, vo=var.out, gcmmd=gcm, iso=iso.ctry, timeseries=time.series)
  }
  return(gcmdir)
}


####################################################################################
########FUNCTION: analyseStations.TS
########ARGUMENTS: wd, modelDir, adminDir, outDir, vn, vo, gcmmd, iso, timeseries
####################################################################################
########wd: passed from processCompareWS, same as work.dir
########modelDir: passed from processCompareWS, same as gcmdir
########aDir: passed from processCompareWS, same as aDir
########outDir: passed from processCompareWS, same as out.dir
########vn: passed from processCompareWS, same as var.in
########vo: passed from processCompareWS, same as var.out
########iso: passed from processCompareWS, same as iso.ctry
########gcmmd: model name either in double or single quotes (i.e. bccr_bcm2_0)
########timeseries: passed from processCompareWS, same as time.series
####################################################################################
#Function to analyse one single model as a whole and store the outputs
analyseStations.TS <- function(wd="", modelDir="", adminDir="", outDir="", vn="rain", vo="prec", 
gcmmd="bccr_bcm2_0", iso="ETH", timeseries=c(1961:1990)) {
  #Reading shapefile of the selected country
  shp <- readShapePoly(paste(adminDir, "/", iso, "_adm/", iso, "0.shp", sep=""))
  gcm.res <- getGCMRes(gcmmd, gcm.chars)
  msk <- createMask(shp, gcm.res); msk[which(is.na(msk[]))] <- 0
  
  #Setting working directory
  setwd(wd)
  
  #Setting output directories
  od <- paste(outDir, "/", vn, "-extracted", sep=""); if (!file.exists(od)) {dir.create(od)}
  odCountry <- paste(od, "/", iso, sep=""); if (!file.exists(odCountry)) {dir.create(odCountry)}
  odCountryGCM <- paste(odCountry, "/", gcmmd, sep=""); if (!file.exists(odCountryGCM)) {dir.create(odCountryGCM)}
  odm <- paste(outDir, "/", vn, "-metrics", sep=""); if (!file.exists(odm)) {dir.create(odm)}
  odmGCM <- paste(odm, "/", gcmmd, sep=""); if (!file.exists(odmGCM)) {dir.create(odmGCM)}
  
  #Output directory for timeseries
  ots <- paste(outDir, "/", vn, "-timeseries", sep=""); if (!file.exists(ots)) {dir.create(ots)}
  otsCountry <- paste(ots, "/", iso, sep=""); if (!file.exists(otsCountry)) {dir.create(otsCountry)}
  otsCountryGCM <- paste(otsCountry, "/", gcmmd, sep=""); if (!file.exists(otsCountryGCM)) {dir.create(otsCountryGCM)}
  
  #Check if metrics file exists
  if (!file.exists(paste(odmGCM, "/metrics-", iso, ".csv", sep=""))) {
    
    #############################################################################
    ###READING STATIONS FILE, SETTING UNIQUE LOCATIONS
    #############################################################################
    #Reading stations file and selecting stations within the study area
    wts.dir <- paste("./organized-data/", vn, "-per-station", sep="")
    st.list <- read.csv(paste("./organized-data/ghcn_", vn, "_", min(timeseries), "_", max(timeseries), "_mean.csv", sep=""))
    st.sel <- st.list[which(st.list$LONG <= msk@extent@xmax & st.list$LONG >= msk@extent@xmin & st.list$LAT <= msk@extent@ymax & st.list$LAT >= msk@extent@ymin),]
    st.sel$INSIDE <- extract(msk, cbind(st.sel$LONG,st.sel$LAT))
    st.sel <- st.sel[which(st.sel$INSIDE == 1),]
    st.sel <- st.sel[,c(1,3:5,ncol(st.sel))]
    st.ids <- st.sel$ID
    rm(st.list); g=gc()
    
    ###Here create a numbered mask and extract, make NAs those outside range
    msk.cellNumbers <- msk
    msk.cellNumbers[] <- 1:ncell(msk.cellNumbers)
    msk.cellNumbers[which(msk[] == 0)] <- NA
    
    #Extract values using station locations and this grid
    st.cellNumbers <- extract(msk.cellNumbers, cbind(st.sel$LONG,st.sel$LAT))
    st.cellNumbers <- st.cellNumbers[which(!is.na(st.cellNumbers))]
    
    #Create a unique value vector
    st.unique <- unique(st.cellNumbers)
    
    #Extract xy at these cell numbers
    st.uniqueXY <- as.data.frame(xyFromCell(msk,st.unique))
    st.uniqueXY <- cbind(cell=st.unique,st.uniqueXY)
    rm(msk.cellNumbers); rm(st.cellNumbers); g=gc()
    
    
    #############################################################################
    ###UNIQUE LOCATION DATA EXTRACTION
    #############################################################################
    if (length(st.ids) > 0) {
      cat("Analysing", length(st.ids), "stations \n")
      cat("Resulted in", nrow(st.uniqueXY), "locations \n")
      
      #Extract the timeseries for these unique locations for each of the months
      un.counter <- 1
      for (un.pt in 1:nrow(st.uniqueXY)) {
        cat("Processing location with ID", paste(st.unique[un.pt]), "\n")
        
        #Now extract by month
        lonlat <- st.uniqueXY[un.pt,c(2,3)]
        id <- st.uniqueXY$cell[un.pt]
        
        #Checking for file to avoid extraction
        outTSFile <- paste(otsCountryGCM, "/TS.", id, ".csv", sep="")
        
        if (file.exists(outTSFile)) {
          assign(paste("TS.",id,sep=""), read.csv(outTSFile))
        } else {
          for (m in 1:12) {
            #Verbosing
            if (m==1) {
              cat("Extraction", m, ".")
            }
            else if (m==12) {
              cat(m,"\n")
            } else {
              cat(m,".")
            }
            
            if (m < 10) {month <- paste("0", m, sep="")} else {month <- paste(m)}
            
            #Actual data extraction
            #extractTS(xy, gcmdir, yr.list, mth, vg="prec", gcm="bccr_bcm2_0")
            tmp <- extractTS(lonlat,modelDir,timeseries,month,vg=vo,gcm=gcmmd)
            
            if (m==1) {
              assign(paste("TS.",id,sep=""), tmp)
            } else {
              assign(paste("TS.",id,sep=""),merge(get(paste("TS.",id,sep="")),tmp))
            }
            rm(tmp); g=gc()
          }
          write.csv(get(paste("TS.",id,sep="")),outTSFile,quote=F,row.names=F)
        }
      }
      
    #############################################################################
    ###STATION MATCHING AND COMPARISON
    #############################################################################
      cat("Analysing", length(st.ids), "stations \n")
      
      #Loop throught stations and match these and extract metrics
      st.counter <- 1
      #Loading station data and defining the site
      for (st.id in st.ids) {
        if (st.counter%%10 == 0 | st.counter == 1) {cat("Processing station", paste(st.id), "\n")}
        
        #Reading the weather station data
        std <- read.csv(paste("./organized-data/", vn, "-per-station/", st.id, ".csv", sep=""))
        #print(str(std))
        
        #Find out where the point is located
        xy <- data.frame(x=std$LONG[1],y=std$LAT[1])
        whereXY <- cellFromXY(msk,xy)
        gcm.in.data <- get(paste("TS.",whereXY,sep=""))
        
        #Checking whether the stuff was already done
        outCompared <- paste(odCountryGCM, "/", st.id, "-comparison.csv", sep="")
        if (file.exists(outCompared)) {
          ts.in <- read.csv(outCompared)
          ghcn.comp <- GHCN.GCM.comp(std, gcm.in.data, vg=vo, match=F, ts.out=ts.in, compare=T)
        } else {
          #Getting the comparison metrics and extracted values
          ghcn.comp <- GHCN.GCM.comp(std, gcm.in.data, vg=vo, match=T, ts.out=NULL, compare=T)
          write.csv(ghcn.comp$VALUES, outCompared, row.names=F, quote=F)
        }
        rm(gcm.in.data); rm(std); g=gc()
        
        ghcn.comp$METRICS <- cbind(ID=rep(st.id, times=nrow(ghcn.comp$METRICS)),ghcn.comp$METRICS)
        
        if (st.id == st.ids[1]) {
          out.metrics <- ghcn.comp$METRICS
        } else {
          out.metrics <- rbind(out.metrics, ghcn.comp$METRICS)
        }
        st.counter <- st.counter+1
      }
      write.csv(out.metrics, paste(odmGCM, "/metrics-", iso, ".csv", sep=""), row.names=F, quote=F)
      return(out.metrics)
    } else {
      cat(length(st.ids),"stations to analyse \n")
    }
  }
}


####################################################################################
########FUNCTION: GHCN.GCM.comp
########ARGUMENTS: station.data, model.data, vg, match, ts.out, compare
####################################################################################
########station.data: Input weather station data of the structure (ID,LAT,LONG,YEAR,JAN,FEB,...)
########model.data: Input GCM associated data (YEAR,JAN.GCM,FEB.GCM,MAR.GCM,...)
########vg: passed from analyseStations.TS, same as vg
########match: logical. FALSE if the matched data file already exists and is thus directly read from disk,
########       otherwise TRUE.
########ts.out: NULL if match=FALSE, otherwise this is a data frame with the all years data 
########        for a particular station and associated GCM values. Structure is YEAR,[MONTH].GCM,
########        [MONTH].WST (e.g. YEAR,JAN.GCM,JAN.WST,FEB.GCM,FEB.WST,...)
########compare: logical. TRUE if the comparison is to be done, otherwise FALSE
####################################################################################
#Function to harvest data from a particular GCM and also retrieve the accuracy metrics
GHCN.GCM.comp <- function(station.data, model.data, vg="prec", match=T, ts.out=NULL, compare=T) {
  
  #Applying functions to the data
  if (match) {ts.out <- matchMonth(station.data, model.data, vg=vg)}
  metrics <- compareTS(ts.out, plotit=F)
  row.names(metrics) <- c(1:nrow(metrics))
  
  return(list(VALUES=ts.out, METRICS=metrics))
}


####################################################################################
########FUNCTION: compareTS
########ARGUMENTS: x, plotit, plotName
####################################################################################
########x: data frame with the all years data for a particular station and associated GCM values.
########   Structure is YEAR,[MONTH].GCM,[MONTH].WST (e.g. YEAR,JAN.GCM,JAN.WST,FEB.GCM,FEB.WST,...)
########plotit: logical. TRUE if you want to plot the results, otherwise F
########plotName: name of the plot, NA if plotit=F, otherwise provide a name
####################################################################################
#Routine to compare all months in the matrix and return a data frame in which each month and the total are a line
compareTS <- function(x, plotit=F, plotName="dummy") {
  
  mthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC", "TTL", "DJF", "JJA")
  
  for (m in mthList) {
    col.names <- names(x)
    selCols <- grep(m, col.names)
    
    compMatrix <- x[,selCols]
    names(compMatrix) <- c("GCM","WST")
    compMatrix <- compMatrix[which(!is.na(compMatrix[,1])),]; compMatrix <- compMatrix[which(!is.na(compMatrix[,2])),]
    if (nrow(compMatrix) > 2) {
      lims <- c(min(compMatrix), max(compMatrix))
      #cat("month",m, "\n")
      
      #Check if compMatrix has any of its columns with full zeros
      nz.GCM <- length(which(compMatrix$GCM == 0))
      nz.WST <- length(which(compMatrix$WST == 0))
      
      #Check unique values in compMatrix columns
      uv.GCM <- length(unique(compMatrix$GCM))
      uv.WST <- length(unique(compMatrix$WST))
      
      if (nz.GCM == nrow(compMatrix) | nz.WST == nrow(compMatrix) | uv.GCM == 1 | uv.WST == 1) {
        fit.mf <- lm(compMatrix$WST ~ compMatrix$GCM - 1) #Fit forced to origin
        pval.mf <- NA
        fit.m <- lm(compMatrix$WST ~ compMatrix$GCM) #Fit normal (unforced)
        pval.m <- NA
        plot.M <- F
      } else {
        #Fit mean
        fit.mf <- lm(compMatrix$WST ~ compMatrix$GCM - 1) #Fit forced to origin
        pd.mf <- lims*fit.mf$coefficients; pd.mf <- cbind(lims, pd.mf)
        pval.mf <- pf(summary(fit.mf)$fstatistic[1],summary(fit.mf)$fstatistic[2],summary(fit.mf)$fstatistic[3],lower.tail=F)
        fit.m <- lm(compMatrix$WST ~ compMatrix$GCM) #Fit normal (unforced)
        pd.m <- lims*fit.m$coefficients[2] + fit.m$coefficients[1]; pd.m <- cbind(lims, pd.m)
        pval.m <- pf(summary(fit.m)$fstatistic[1],summary(fit.m)$fstatistic[2],summary(fit.m)$fstatistic[3],lower.tail=F)
        plot.M <- T
      }
      
      if (plotit) {
        #Forced to origin
        jpeg(paste(plotDir, "/", plotName, "-forced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
        plot(compMatrix$GCM, compMatrix$WST,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
        if (plot.M) {lines(pd.mf)}; #lines(pd, lty=2)
      	abline(0,1,lty=2)
      	dev.off()
        #Not forced to origin
        jpeg(paste(plotDir, "/", plotName, "-unforced.jpg", sep=""), quality=100, width=780, height=780, pointsize=18)
        plot(compMatrix$GCM, compMatrix$WST,xlim=lims, ylim=lims, col="black", pch=20, xlab="GCM values", ylab="Observed values")
        if (plot.M) {lines(pd.m)}; #lines(pd, lty=2
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
    } else {
      npts <- nrow(compMatrix)
      res.m <- data.frame(MONTH=m, N=npts, R2.FORCED=NA, ADJ.R2.FORCED=NA, P.VALUE.FORCED=NA, SLOPE.FORCED=NA, INTERCEPT.FORCED=NA, F.STAT.FORCED=NA, R2=NA, ADJ.R2=NA, P.VALUE=NA, SLOPE=NA, INTERCEPT=NA, F.STAT=NA, ERROR=NA)
    }
    
    if (m == mthList[1]) {
      monthly.mx <- res.m
    } else {
      monthly.mx <- rbind(monthly.mx, res.m)
    }
  }
  return(monthly.mx)
}


####################################################################################
########FUNCTION: matchMonth
########ARGUMENTS: in.st.data, in.gcm.data, vg
####################################################################################
########in.st.data: Input weather station data of the structure (ID,LAT,LONG,YEAR,JAN,FEB,...)
########in.gcm.data: Input GCM associated data (YEAR,JAN.GCM,FEB.GCM,MAR.GCM,...)
########vg: variable for which the calculation is being done
####################################################################################
#Function to extract over all months across years and return a matrix with GCM and Weather Station data for matching
matchMonth <- function(in.st.data, in.gcm.data, vg="prec") {
  #Looping through months for data extraction
  for (m in 1:12) {
#     cat("Processing month", m, "\n")
    
    #Correcting month (i.e. "1"" by "01")
    if (m < 10) {month <- paste("0", m, sep="")} else {month <- paste(m)}
    
    #Applying function
    #matchTS(st.data, gcm.data, mth)
    out <- matchTS(in.st.data, in.gcm.data, month)
    
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
    
    gcm.djf <- ts.out$DEC.GCM + ts.out$JAN.GCM + ts.out$FEB.GCM
    wst.djf <- ts.out$DEC.WST + ts.out$JAN.WST + ts.out$FEB.WST
    
    gcm.jja <- ts.out$JUN.GCM + ts.out$JUL.GCM + ts.out$AUG.GCM
    wst.jja <- ts.out$JUN.WST + ts.out$JUL.WST + ts.out$AUG.WST
    
  } else {
    gcm.total <- rowMeans(ts.gcm,na.rm=F)
    wst.total <- rowMeans(ts.wst,na.rm=F)
    
    gcm.djf <- (ts.out$DEC.GCM + ts.out$JAN.GCM + ts.out$FEB.GCM) / 3
    wst.djf <- (ts.out$DEC.WST + ts.out$JAN.WST + ts.out$FEB.WST) / 3
    
    gcm.jja <- (ts.out$JUN.GCM + ts.out$JUL.GCM + ts.out$AUG.GCM) / 3
    wst.jja <- (ts.out$JUN.WST + ts.out$JUL.WST + ts.out$AUG.WST) / 3
  }
  
  #Getting outputs where they should be in the output matrix
  ts.out$TTL.GCM <- gcm.total
  ts.out$TTL.WST <- wst.total
  ts.out$DJF.GCM <- gcm.djf
  ts.out$DJF.WST <- wst.djf
  ts.out$JJA.GCM <- gcm.jja
  ts.out$JJA.WST <- wst.jja
  
  return(ts.out)
}


####################################################################################
########FUNCTION: matchTS
########ARGUMENTS: in.st.data, in.gcm.data, vg
####################################################################################
########st.data: passed from matchMonth, same as in.st.data
########gcm.data: passed from matchMonth, same as in.gcm.data
########mth: month as passed from matchMonth (i.e. "01","02", ... instead of 1,2,...)
####################################################################################
#Function to match GCM extracted data and Station data into a matrix
#st.data should contain LAT,LON,MONTHLY_VALUES
#GCM data should be YEAR,mth.name.GCM (e.g. YEAR,JAN.GCM,FEB.GCM)
#mth is numeric and indicates the corresponding moth
matchTS <- function(st.data, gcm.data, mth) {
  #Pick up the name of the month from a predefined list
  mthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  mth.name <- mthList[as.numeric(mth)]
  
  #Extracting the corresponding month from the station data file
  colm <- which(names(st.data) == mth.name)
  sel.st.data <- st.data[,c(2,colm)]
  
  #Verifying duplicates and removing them
  st.yrs <- unique(sel.st.data$YEAR)
  if (length(st.yrs) != nrow(sel.st.data)) {
    st.ydup <- sel.st.data$YEAR[duplicated(sel.st.data$YEAR)]
    temp.data <- sel.st.data[!duplicated(sel.st.data$YEAR),]
    for (yk in st.ydup) {
      temp.data[which(temp.data$YEAR == yk),2] <- mean(sel.st.data[which(sel.st.data$YEAR==yk),2],na.rm=T)
    }
    sel.st.data <- temp.data
    rm(temp.data); g=gc()
  }
  
  colm.gcm <- which(names(gcm.data) == paste(mth.name,".GCM",sep=""))
  sel.gcm.data <- gcm.data[,c(1,colm.gcm)]
  
  #Multiply by 0.1 to rescale (data are originally multiplied by 10)
  sel.st.data[,2] <- sel.st.data[,2] * 0.1
  
  names(sel.gcm.data) <- c("YEAR", "VALUE")
  
  m <- merge(sel.gcm.data, sel.st.data, all.y=F)
  names(m) <- c("YEAR",paste(mth.name, ".GCM", sep=""),paste(mth.name, ".WST", sep=""))
  return(m)
}


####################################################################################
########FUNCTION: extractTS
########ARGUMENTS: xy, gcmdir, yr.list, mth, vg, gcm
####################################################################################
########xy: data.frame with two columns (X,Y) and one row referring to the location of the point to extract
########gcmdir: passed from analyseStations.TS, same as modelDir
########yr.list: passed from analyseStations.TS, same as time.series
########mth: month as passed from analyseStations.TS (i.e. "01","02", ... instead of 1,2,...)
########vg: passed from analyseStations.TS, same as vo
########gcm: passed from analyseStations.TS, same as gcmmd
####################################################################################
#Function to extract data from the GCM yearly stuff for a given:
#location in long,lat (xy)
#month in number (mth)
#variable (vg)
#model name (gcm) in
#a hard drive location (gcmdir, where yrdir = gcmDir/yearly_files/$YEAR/[files])
#vector of years (yr.list)
extractTS <- function(xy, gcmdir, yr.list, mth, vg="prec", gcm="bccr_bcm2_0") {
  #Pick up the name of the month from a predefined list
  yrdir <- paste(gcmdir, "/", gcm, "/yearly_files", sep="")
  mthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  mth.name <- mthList[as.numeric(mth)]
  
  #Listing the rasters and extract the values
  rs.list <- as.list(paste(yrdir, "/", yr.list, "/", vg, "_", mth, ".asc", sep=""))
  rstack <- stack(rs.list)
  ex.vals <- extract(rstack, xy); rm(rstack); rm(rs.list); g=gc()
  
  gcm.values <- data.frame(yr.list, t(ex.vals))
  row.names(gcm.values) <- c(1:nrow(gcm.values))
  names(gcm.values) <- c("YEAR", "VALUE")
  
  m <- gcm.values
  names(m) <- c("YEAR",paste(mth.name, ".GCM", sep=""))
  return(m)
}


####################################################################################
########FUNCTION: getGCMRes
########ARGUMENTS: gcm, gcm.params
####################################################################################
########gcm: passed from analyseStations.TS, same as gcmmd
########gcm.params: loaded gcm data (as in a csv that should be present in the scripts folder).
########            this should be named gcm_chars.csv and should contain gcm resolutions.
########            See line 31 of this script.
####################################################################################
#Basic function to get GCM resolution
getGCMRes <- function(gcm, gcm.params) {
  return(gcm.params$dxNW[which(gcm.params$model == gcm)])
}
