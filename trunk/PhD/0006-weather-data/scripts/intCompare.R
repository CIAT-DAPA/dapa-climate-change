#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("Error: do not run whole thing \n")

library(raster)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/interpolate-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#folders and locations
bd <- "F:/PhD-work/crop-modelling/climate-data"

#details
ye <- 1960

#Determine if leap year or not (calculate number of days)
nd <- leap(ye)
ndm <- ts(diff(seq(as.Date("1960-01-01"), as.Date("2010-01-01"), by = "month")), 
               start = c(1960, 01), freq = 12) #get days in each month of time series
ndm <- data.frame(matrix(ndm,ncol=12,byrow=T))
row.names(ndm) <- c(1960:2009)
names(ndm) <- month.abb

#load altitude raster to get the xy coordinates of the points to validate
region <- "eaf"

alt_rs <- raster(paste(bd,"/daily-interpolations/0_files/alt-prj-",region,".asc",sep=""))
xy <- xyFromCell(alt_rs,which(!is.na(alt_rs[])))

for (ye in 1960:2009) {
  for (i in 1:nrow(xy)) {
    rmx <- extractIntCRU(lon=xy[i,1],lat=xy[i,2],bd,ye,region,ndm)
    
  }
}


plot(1:12,rmx$CRU_RAIN,type='l',col="black",ylim=c(min(rmx$CRU_RAIN,rmx$INT_RAIN),max(rmx$CRU_RAIN,rmx$INT_RAIN)))
lines(1:12,rmx$INT_RAIN,type='l',col="red",ylim=c(min(rmx$CRU_RAIN,rmx$INT_RAIN),max(rmx$CRU_RAIN,rmx$INT_RAIN)))


###############################################
#Extract the data
extractIntCRU <- function(lon,lat,bDir,year,re,ndaysMth) {
  #folders and other stuff
  intDir <- paste(bDir,"/daily-interpolations",sep="")
  cruDir <- paste(bDir,"/CRU_TS_v3-1_data/monthly_grids",sep="")
  
  #region details
  if (re=="eaf" | re=="waf") {rg <- "afr"} else {rg <- "sas"}
  rsDir <- paste(intDir,"/",year,"-",rg,sep="")
  
  #julian dates data.frame
  theRow <- which(row.names(ndaysMth)==year)
  jDates <- data.frame(MONTH=rep(names(ndaysMth),times=as.numeric(ndaysMth[theRow,])),JDAY=1:nd,
                     DOM=rep(as.numeric(ndaysMth[theRow,]),times=as.numeric(ndaysMth[theRow,])))
  
  #load dummy raster
  dumm <- raster(paste(intDir,"/0_files/rain_",re,"_dummy.asc",sep="")); dumm[] <- NA
  
  #First verify if each raster exists, else create an all-NA raster there
  cat("Verifying existence of raster data \n")
  mcount <- 0
  for (i in 1:366) {
    rsTest <- paste(rsDir,"/",i,"/rain_",re,".asc",sep="")
    if (!file.exists(rsTest)) { #if file does not exist then create it
      writeRaster(dumm,rsTest,format='ascii',overwrite=F)
      mcount <- mcount+1
    }
  }
  if (mcount > 0) { #text to state the creation
    cat(mcount, "files were missing and created as all-NA rasters \n")
  } else {
    cat("No files were missing \n")
  }
  
  #loop through months
  for (mth in month.abb) {
    #load the cru rasters
    mnth <- which(names(ndaysMth)==mth)
    cruRaster <- raster(paste(cruDir,"/pre/pre_",year,"_",mnth,sep=""))
    
    #load interpolated stack for this month
    theCol <- which(names(ndaysMth)==mth)
    ndays <- ndaysMth[theRow,theCol]
    iday <- min(jDates$JDAY[which(jDates$MONTH==mth)])
    eday <- max(jDates$JDAY[which(jDates$MONTH==mth)])
    
    #Now loading the daily data
    cat("Loading days",iday,"to",eday,"(",mth,")\n")
    intStk <- stack(paste(rsDir,"/",iday:eday,"/rain_",re,".asc",sep=""))
    
    #extract data for given lon,lats and neighbors (corners)
    xyMat <- expand.grid(X=c(lon-0.25,lon,lon+0.25),
                         Y=c(lat-0.25,lat,lat+0.25))
    cruCell <- unique(cellFromXY(cruRaster,xyMat))
    intVals <- extract(intStk,cbind(X=lon,Y=lat))
    intVals[which(intVals<0)] <- 0
    cruVals <- extract(cruRaster,cruCell)
    
    resrow <- data.frame(LON=lon,LAT=lat,CRU_RAIN=mean(cruVals,na.rm=T),INT_RAIN=sum(intVals,na.rm=T))
    
    if (mth == month.abb[1]) {
      resMx <- resrow
    } else {
      resMx <- rbind(resMx,resrow)
    }
  }
  return(resMx)
}



