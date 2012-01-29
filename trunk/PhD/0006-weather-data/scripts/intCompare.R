#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("Error: do not run whole thing \n")

library(raster)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/interpolate-functions.R",sep=""))

#folders and locations
bDir <- "D:/CIAT_work/crop-modelling/climate-data"
cruDir <- paste(bDir,"/CRU_TS_v3-1_data/monthly_grids",sep="")
intDir <- paste(bDir,"/interpolations",sep="")

#details
vr <- "pre"
year <- 1960

#Determine if leap year or not (calculate number of days)
nd <- leap(ye)
ndaysMth <- ts(diff(seq(as.Date("1960-01-01"), as.Date("2011-01-01"), by = "month")), 
               start = c(1960, 01), freq = 12) #get days in each month of time series
ndaysMth <- data.frame(matrix(ndaysMth,ncol=12))
row.names(ndaysMth) <- c(1960:2010)
names(ndaysMth) <- month.abb

#load altitude raster to get the xy coordinates of the points to validate
re <- "eaf"
if (re=="eaf" | re=="waf") {rg <- "afr"} else {rg <- "sas"}

alt_rs <- raster(paste(intDir,"/0_files/alt-prj-",re,".asc",sep=""))
xy <- xyFromCell(alt_rs,which(!is.na(alt_rs[])))

lon <- xy[1,1]; lat <- xy[1,2]

#load the interpolated rasters
rsDir <- paste(intDir,"/",year,"-",rg,sep="")
for (mth in month.abb) {
  #load the cru rasters
  mnth <- which(names(ndaysMth)==mth)
  cruRaster <- raster(paste(bDir,"/CRU_TS_v3-1_data/monthly_grids/",vr,"/",vr,"_",year,"_",mnth,sep=""))
  
  #load interpolated stack for this month
  ndays <- ndaysMth[which(row.names(ndaysMth)==year),which(names(ndaysMth)==mth)]
  intStk <- stack(paste(rsDir,"/",1:ndays,"/rain_",re,".asc",sep=""))
  
  #extract data for given lon,lats
  intVals <- extract(intStk,cbind(X=lon,Y=lat))
  intVals[which(intVals<0)] <- 0
  cruVals <- extract(cruRaster,cbind(X=lon,Y=lat))
  
  resrow <- data.frame(LON=lon,LAT=lat,CRU_RAIN=cruVals,INT_RAIN=sum(intVals,na.rm=T))
  
  if (mth == month.abb[1]) {
    resMx <- resrow
  } else {
    resMx <- rbind(resMx,resrow)
  }
}




