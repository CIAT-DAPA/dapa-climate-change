#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#function to extract monthly CRU data and convert to daily, single location
extract_CRU_single <- function(in_dir,year,dates,varname,lon,lat) {
  tvar <- varname
  cat("...processing CRU variable=",tvar,"\n")
  cru_files <- list.files(in_dir,pattern=paste(".",tvar,".",sep=""))
  
  alldates <- data.frame()
  for (fi in 1:length(cru_files)) {
    #fi <- 1
    tfile <- cru_files[fi]
    tfile <- gsub("cru_ts3.23.","",tfile)
    tfile <- gsub(paste(".",tvar,".dat.nc.gz",sep=""),"",tfile)
    year_i <- as.numeric(unlist(strsplit(tfile,".",fixed=T))[1])
    year_f <- as.numeric(unlist(strsplit(tfile,".",fixed=T))[2])
    whichfi <- fi; whichyi <- year_i; whichyf <- year_f
    tempfile_gz <- cru_files[whichfi]
    tempfile <- gsub(".gz","",tempfile_gz)
    
    #go to folder, decompress, and open file, then compress again
    datestemp <- format(seq(as.Date(paste0(whichyi,"/1/1")), as.Date(paste0(whichyf,"/12/31")), "months") ,"%Y-%m")
    datestemp <- cbind.data.frame("date"=datestemp, "value"=NA)
    twd <- getwd()
    setwd(in_dir)
    system(paste("gunzip ",tempfile_gz,sep=""))
    trs <- stack(tempfile)
    datestemp$value <- as.numeric(extract(trs, data.frame(x=lon,y=lat)))
    rm(trs); x <- gc(); rm(x)
    system(paste("gzip ",tempfile,sep=""))
    setwd(twd)
    
    #append data
    alldates <- rbind(alldates,datestemp)
  }
  
  #put year and month
  alldates$year <- as.numeric(unlist(lapply(paste(alldates$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]})))
  alldates$month <- unlist(lapply(paste(alldates$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
  if (year == min(alldates$year)) {
    seldates <- alldates[which(alldates$year == year | alldates$year == (year+1)),]
    nday <- 2*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year+1))
    torigin <- paste((year),"-1-1",sep="")
  } else if (year == max(alldates$year)) {
    seldates <- alldates[which(alldates$year == year | alldates$year == (year-1)),]
    nday <- 2*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year-1))
    torigin <- paste((year-1),"-1-1",sep="")
  } else {
    seldates <- alldates[which(alldates$year == year | alldates$year == (year-1) | alldates$year == (year+1)),]
    nday <- 3*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year-1)) + 1*as.numeric(leap.year(year+1))
    torigin <- paste((year-1),"-1-1",sep="")
  }
  #spline interpolate to daily
  ttest <- splineInterpolateMonthlytoDaily(nday=nday,val=matrix(seldates$value),origin=torigin,
                                           first_row=1,last_row=nday,no_spline=F,no_mean=F)
  
  #put into final form
  ttestdates <- format(seq(as.Date(paste0((min(seldates$year)),"/1/1")), as.Date(paste0((max(seldates$year)),"/12/31")), "days") ,"%Y-%m-%d")
  ttestdates <- cbind.data.frame("date"=ttestdates, "value"=ttest)
  ttestdates$year <- unlist(lapply(paste(ttestdates$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  ttestdates <- ttestdates[which(ttestdates$year == year),]
  dates$value <- ttestdates$value
  names(dates)[ncol(dates)] <- tvar
  return(dates)
}


###
#function to extract monthly CRU data and convert to daily, multiple location
extract_CRU_mult <- function(in_dir,year,dates,varname,xy) {
  tvar <- varname
  cat("...processing CRU variable=",tvar,"for n=",nrow(xy),"sites\n")
  cru_files <- list.files(in_dir,pattern=paste(".",tvar,".",sep=""))
  
  alldates <- lapply(1:nrow(xy), FUN=function(x) {y <- data.frame(); return(y)}) #data.frame()
  for (fi in 1:length(cru_files)) {
    #fi <- 1
    tfile <- cru_files[fi]
    tfile <- gsub("cru_ts3.23.","",tfile)
    tfile <- gsub(paste(".",tvar,".dat.nc.gz",sep=""),"",tfile)
    year_i <- as.numeric(unlist(strsplit(tfile,".",fixed=T))[1])
    year_f <- as.numeric(unlist(strsplit(tfile,".",fixed=T))[2])
    whichfi <- fi; whichyi <- year_i; whichyf <- year_f
    tempfile_gz <- cru_files[whichfi]
    tempfile <- gsub(".gz","",tempfile_gz)
    
    #create temporary output object
    datestemp <- lapply(1:nrow(xy),FUN=function(x) {tdates <- format(seq(as.Date(paste0(whichyi,"/1/1")), as.Date(paste0(whichyf,"/12/31")), "months") ,"%Y-%m");return(tdates)})
    datestemp <- lapply(datestemp, FUN=function(x) {tdates <- cbind.data.frame("date"=x, "value"=NA); return(tdates)})
    
    #go to folder, decompress, and open file, then compress again
    twd <- getwd()
    setwd(in_dir)
    system(paste("gunzip ",tempfile_gz,sep=""))
    trs <- stack(tempfile)
    allvalues <- extract(trs, data.frame(x=xy$lon,y=xy$lat))
    rm(trs); x <- gc(); rm(x)
    system(paste("gzip ",tempfile,sep=""))
    setwd(twd)
    
    #put data into datestemp data.frame, and append data onto alldates data.frame
    for (x_i in 1:nrow(xy)) {
      datestemp[[x_i]]$value <- allvalues[x_i,]
      alldates[[x_i]] <- rbind(alldates[[x_i]],datestemp[[x_i]])
    }
  }
  
  #loop locations to spline interpolate monthly to daily
  for (x_i in 1:nrow(xy)) {
    #put year and month
    alldates[[x_i]]$year <- as.numeric(unlist(lapply(paste(alldates[[x_i]]$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]})))
    alldates[[x_i]]$month <- unlist(lapply(paste(alldates[[x_i]]$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
    if (year == min(alldates[[x_i]]$year)) {
      seldates <- alldates[[x_i]][which(alldates[[x_i]]$year == year | alldates[[x_i]]$year == (year+1)),]
      nday <- 2*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year+1))
      torigin <- paste((year),"-1-1",sep="")
    } else if (year == max(alldates[[x_i]]$year)) {
      seldates <- alldates[[x_i]][which(alldates[[x_i]]$year == year | alldates[[x_i]]$year == (year-1)),]
      nday <- 2*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year-1))
      torigin <- paste((year-1),"-1-1",sep="")
    } else {
      seldates <- alldates[[x_i]][which(alldates[[x_i]]$year == year | alldates[[x_i]]$year == (year-1) | alldates[[x_i]]$year == (year+1)),]
      nday <- 3*365 + 1*as.numeric(leap.year(year)) + 1*as.numeric(leap.year(year-1)) + 1*as.numeric(leap.year(year+1))
      torigin <- paste((year-1),"-1-1",sep="")
    }
    #spline interpolate to daily
    ttest <- splineInterpolateMonthlytoDaily(nday=nday,val=matrix(seldates$value),origin=torigin,
                                             first_row=1,last_row=nday,no_spline=F,no_mean=F)
    
    #put into final form
    ttestdates <- format(seq(as.Date(paste0((min(seldates$year)),"/1/1")), as.Date(paste0((max(seldates$year)),"/12/31")), "days") ,"%Y-%m-%d")
    ttestdates <- cbind.data.frame("date"=ttestdates, "value"=ttest)
    ttestdates$year <- unlist(lapply(paste(ttestdates$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
    ttestdates <- ttestdates[which(ttestdates$year == year),]
    dates[[x_i]]$value <- ttestdates$value
    names(dates[[x_i]])[ncol(dates[[x_i]])] <- tvar
  }
  return(dates)
}



