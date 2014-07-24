#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#January 2014 --borrows from PhD script called glam-make_wth.R

#make DSSAT weather files
#wd <- "~/Leeds-work/quest-for-robustness"
#datdir <- paste(wd,"/data/model_data",sep="")
#wthdir <- paste(wd,"/data/meteorology/ascii_extract_raw",sep="")
#load(paste(datdir,"/initial_conditions_major_dssat.RData",sep="")) #load the soil data from initial conditions

#select location
#i <- 1
#fildir <- make_wth(x=data.frame(CELL=xy_main$LOC[i],X=xy_main$x[i],Y=xy_main$y[i],ELEV=xy_main$ELEV[i]),
#                   wthDir_in=paste(wthdir,"/obs_hist_WFD",sep=""),
#                   wthDir_out=paste(wthdir,"/obs_hist_WFD/loc-",xy_main$LOC[i],sep=""),
#                   years=1980:2001,fields=list(CELL="CELL",X="X",Y="Y",ELEV="ELEV"),out_file=NA)

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
    cat("\n",file=wthfil)
    cat("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n",file=wthfil)
    cat(sprintf("%6s",site_details$INSI),sep="",file=wthfil)
    cat(sprintf("%9.3f",site_details$LAT),file=wthfil)
    cat(sprintf("%9.3f",site_details$LONG),file=wthfil)
    cat(sprintf("%6.1f",site_details$ELEV),file=wthfil)
    cat(sprintf("%6.1f",site_details$TAV),file=wthfil)
    cat(sprintf("%6.1f",site_details$AMP),file=wthfil)
    cat(sprintf("%6.1f",site_details$REFHT),file=wthfil)
    cat(sprintf("%6.1f",site_details$WNDHT),file=wthfil)
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
make_wth <- function(x,wthDir_in,wthDir_out=NA,years,fields=list(CELL="CELL",X="X",Y="Y",ELEV="ELEV"),out_file=NA) {
  #checks
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y","ELEV"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  if (is.na(wthDir_out)) {wthDir_out <- wthDir_in} #if not specified then o/ dir is i/ dir
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  names(x)[which(toupper(names(x)) == toupper(fields$ELEV))] <- "ELEV"
  
  #check if wthDir_out does exist. wthDir_in must exist
  if (!file.exists(wthDir_out)) {dir.create(wthDir_out,recursive=T)}
  if (!file.exists(wthDir_in)) {stop("wthDir_in not found, please check")}
  
  #all cells
  cell <- x$CELL
  
  #loop cells
  for (cll in cell) {
    #cll <- cell[1]
    #site name and details
    lon <- x$X[which(x$CELL == cll)]; lat <- x$Y[which(x$CELL == cll)]
    elev <- round(x$ELEV[which(x$CELL == cll)],0)
    
    #filename
    if (is.na(out_file)) {
      basename <- "AFRB"; ext=".WTH"; yri <- substr(paste(min(years)),3,4); nyrs <- length(years)
      wthfile <- paste(wthDir_out,"/",basename,yri,nyrs,"_loc-",cll,ext,sep="")
    } else {
      wthfile <- paste(wthDir_out,"/",out_file,sep="")
    }
    
    #site details
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="AFRB",LAT=lat,LONG=lon,
                            ELEV=elev,TAV=-99,AMP=-99,REFHT=1.00,WNDHT=1.00)
    
    ###read in meteorology
    metdata <- read.table(paste(wthDir_in,"/meteo_cell-",cll,".met",sep=""),header=T,sep="\t")
    
    #calculate tav and tamp details
    tav <- (metdata$tasmax + metdata$tasmin) * 0.5
    s_details$TAV <- mean(tav, na.rm=T); rm(tav)
    months <- data.frame(month=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),jday=1:365)
    
    tamp <- merge(metdata, months, by="jday", sort=F)
    tamp$trange <- tamp$tasmax - tamp$tasmin
    tamp$lon <- tamp$lat <- tamp$pr <- tamp$rsds <- NULL
    tamp <- aggregate(tamp[,c("tasmin","tasmax","trange")], by=list(year=tamp$year, month=tamp$month), FUN=function(x) {mean(x,na.rm=T)})
    tamp <- aggregate(tamp[,c("tasmin","tasmax","trange")], by=list(year=tamp$year), FUN=function(x) {mean(x,na.rm=T)})
    s_details$AMP <- mean(tamp$trange, na.rm=T); rm(tamp)
    
    #loop years, fix leap year, and write / append
    for (yr in years) {
      #yr <- years[1]
      mdat <- metdata[which(metdata$year == yr),]
      mdat$year <- NULL
      isleap <- is_leap(yr)
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=mdat$rsds,TMAX=mdat$tasmax,TMIN=mdat$tasmin,RAIN=mdat$pr)
      if (isleap) {wx <- rbind(wx,wx[365,]); row.names(wx) <- 1:366; wx$JDAY[366] <- 366}
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      if (yr == years[1]) {
        wthfile <- write_wth(in_data=wx,out_file=wthfile,site_details=s_details,append=F)
      } else {
        wthfile <- write_wth(in_data=wx,out_file=wthfile,site_details=s_details,append=T)
      }
    }
  }
  return(list(WTH_DIR=wthDir_out))
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
}
