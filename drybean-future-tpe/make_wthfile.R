#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014 --borrows from PhD script called glam-make_wth.R

#make DSSAT weather files

#including CO2 concentration is done as follows:
#@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT   CO2
#  P4NE    15.52     7.03 -99.0  30.1  15.2   2.0   2.0 550.0

#function to write DSSAT wth data (with all years in a single .WTH file)
#inData should have fields named: DATE, SRAD, TMAX, TMIN, RAIN
#site.details should have the AMP and related header-type data
write_wth <- function(in_data,out_file,site_details,append=T) {
  #Open file
  if (append) {
    wthfil <- file(out_file,open="a")
  } else {
    wthfil <- file(out_file,open="w")
  }
  
  #Write header (only for first year)
  if (!append) {
    cat(paste("*WEATHER DATA : ",site_details$NAME,sep=""),"\n",sep="",file=wthfil)
    cat("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT   CO2\n",file=wthfil)
    cat(sprintf("%6s",site_details$INSI),sep="",file=wthfil)
    cat(sprintf("%9.2f",site_details$LAT),file=wthfil)
    cat(sprintf("%9.2f",site_details$LONG),file=wthfil)
    cat(sprintf("%6.1f",site_details$ELEV),file=wthfil)
    cat(sprintf("%6.1f",site_details$TAV),file=wthfil)
    cat(sprintf("%6.1f",site_details$AMP),file=wthfil)
    cat(sprintf("%6.1f",site_details$REFHT),file=wthfil)
    cat(sprintf("%6.1f",site_details$WNDHT),file=wthfil)
    cat(sprintf("%6.1f",site_details$CO2),file=wthfil)
    cat("\n",file=wthfil)
    cat("@DATE  SRAD  TMAX  TMIN  RAIN    \n",file=wthfil)
  }
  
  for (row in 1:nrow(in_data)) {
    cat(in_data$DATE[row],file=wthfil)
    cat(sprintf("%6.1f",in_data$SRAD[row]),file=wthfil)
    cat(sprintf("%6.1f",in_data$TMAX[row]),file=wthfil)
    cat(sprintf("%6.1f",in_data$TMIN[row]),file=wthfil)
    cat(sprintf("%6.1f",in_data$RAIN[row]),file=wthfil)
    cat("\n",file=wthfil)
  }
  
  close(wthfil)
  return(out_file)
}


#################################################################################
#################################################################################
# function to make weather for a number of cells
#################################################################################
#################################################################################
retrieve_header <- function(x,xy_loc,years,basename="XXXX") {
  #xy_loc <- hh_mill_xy[1,]
  #x <- wth_site
  #years <- 2011
  
  #site name and details
  lon <- xy_loc$lon; lat <- xy_loc$lat; elev <- xy_loc$elev
  
  #filename
  ext=".WTH"; yri <- substr(paste(min(years)),3,4); nyrs <- sprintf("%02d",length(years))
  fname <- paste(basename,yri,nyrs,ext,sep="")
  
  #site details
  s_details <- data.frame(NAME=paste("Brazil, GO, ",xy_loc$mun,sep=""),INSI=basename,LAT=lat,LONG=lon,
                          ELEV=elev,TAV=-99,AMP=-99,REFHT=2.00,WNDHT=2.00,CO2=380,FNAME=fname)
  
  #calculate tav and tamp details
  tav <- (x$TMAX + x$TMIN) * 0.5
  s_details$TAV <- mean(tav, na.rm=T); rm(tav)
  
  x$jday <- as.numeric(format(as.Date(x$DATE), "%j"))
  x$year <- as.numeric(format(as.Date(x$DATE), "%Y"))
  months <- data.frame(month=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),jday=1:365)
  tamp <- merge(x, months, by="jday", sort=F)
  tamp$trange <- tamp$TMAX - tamp$TMIN
  tamp$lon <- tamp$lat <- tamp$pr <- tamp$rsds <- NULL
  tamp <- aggregate(tamp[,c("TMIN","TMAX","trange")], by=list(year=tamp$year, month=tamp$month), FUN=function(x) {mean(x,na.rm=T)})
  tamp <- aggregate(tamp[,c("TMIN","TMAX","trange")], by=list(year=tamp$year), FUN=function(x) {mean(x,na.rm=T)})
  s_details$AMP <- mean(tamp$trange, na.rm=T); rm(tamp)
  return(s_details)
}

#function to determine if a year is leap year
#from http://support.microsoft.com/kb/214019
is_leap <- function(yr) {
  if (yr%%4 == 0) {
    if (yr%%100 == 0) {
      if (yr%%400 == 0) {
        outp <- T
      } else {
        outp <- F
      }
    } else {
      outp <- T
    }
  } else {
    outp <- F
  }
  return(outp)
}
