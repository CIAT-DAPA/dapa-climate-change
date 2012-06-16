#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#February 2012


#function to write GLAM wth data (compatible with DSSAT)
#inData should have fields named: DATE, SRAD, TMAX, TMIN, RAIN
#site.details should have the AMP and related header-type data
write_wth <- function(inData,outfile,site.details) {
  #Open file
  wthfil <- file(outfile,open="w")
  
  #Write header
  cat(paste("*WEATHER : ",site.details$NAME,sep=""),"\n",sep="",file=wthfil)
  #cat("\n",file=wthfil)
  cat("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n",file=wthfil)
  cat(sprintf("%6s",site.details$INSI),sep="",file=wthfil)
  cat(sprintf("%9.3f",site.details$LAT),file=wthfil)
  cat(sprintf("%9.3f",site.details$LONG),file=wthfil)
  cat(sprintf("%6.1f",site.details$ELEV),file=wthfil)
  cat(sprintf("%6.1f",site.details$TAV),file=wthfil)
  cat(sprintf("%6.1f",site.details$AMP),file=wthfil)
  cat(sprintf("%6.1f",site.details$REFHT),file=wthfil)
  cat(sprintf("%6.1f",site.details$WNDHT),file=wthfil)
  cat("\n",file=wthfil)
  cat("@DATE  SRAD  TMAX   TMIN    RAIN    \n",file=wthfil)
  
  for (row in 1:nrow(inData)) {
    cat(inData$DATE[row],file=wthfil)
    cat(sprintf("%6.1f",inData$SRAD[row]),file=wthfil)
    cat(sprintf("%7.2f",inData$TMAX[row]),file=wthfil)
    cat(sprintf("%7.2f",inData$TMIN[row]),file=wthfil)
    cat(sprintf("%7.2f",inData$RAIN[row]),file=wthfil)
    cat("\n",file=wthfil)
  }
  
  close(wthfil)
  return(outfile)
}


#################################################################################
#################################################################################
# function to make weather for a number of cells
#################################################################################
#################################################################################
make_wth <- function(x,cell,wthDir,wthDataDir,fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE")) {
  #checks
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y","SOW_DATE"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  names(x)[which(toupper(names(x)) == toupper(fields$SOW_DATE))] <- "SOW_DATE"
  
  #check if wthDir does exist
  if (!file.exists(wthDir)) {dir.create(wthDir)}
  
  #loop cells
  col <- 0; row <- 1
  for (cll in cell) {
    #site name and details
    lon <- x$X[which(cells$CELL == cll)]; lat <- x$Y[which(cells$CELL == cll)]
    
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    
    if (col < 10) {col_t <- paste("00",col,sep="")}
    if (col >= 10 & col < 100) {col_t <- paste("0",col,sep="")}
    if (col >= 100) {col_t <- paste(col)}
    
    if (row < 10) {row_t <- paste("00",row,sep="")}
    if (row >= 10 & row < 100) {row_t <- paste("0",row,sep="")}
    if (row >= 100) {row_t <- paste(row)}
    
    
    ###sowing date
    sdate <- x$SOW_DATE[which(cells$CELL == cll)]
    hdate <- sdate+120
    
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="INGC",LAT=lat,LONG=lon,ELEV=-99,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)
    
    #loop through years and write weather files
    for (yr in 1966:1994) {
      #yr <- 1966
      #the below needs to be changed if you wanna write more than 1 cell
      wthfile <- paste(wthDir,"/ingc",row_t,col_t,yr,".wth",sep="")
      
      #get the weather data for that particular gridcell
      if (hdate > 365) {
        osdate <- 31 #output planting date
        
        #planted in prev. year, get that weather
        pyr <- yr-1
        tmin <- read.csv(paste(wthDataDir,"/cru_tmn/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==pyr | tmin$YEAR==(pyr-1) | tmin$YEAR==(pyr+1)),]
        tmin$YEAR <- NULL
        tmin <- c(tmin$MONTH12[1],as.numeric(tmin[2,]),tmin$MONTH1[3])
        tmin <- linearise(tmin)[16:(365+15)] #interpolate to daily
        
        #shorten planting year series (30 days before planting date) to day 365
        tmin_1 <- tmin[(sdate-30):365]
        
        #get for harvest year
        tmin <- read.csv(paste(wthDataDir,"/cru_tmn/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==yr | tmin$YEAR==(yr-1) | tmin$YEAR==(yr+1)),]
        tmin$YEAR <- NULL
        tmin <- c(tmin$MONTH12[1],as.numeric(tmin[2,]),tmin$MONTH1[3])
        tmin <- linearise(tmin)[16:(365+15)] #interpolate to daily
        
        #shorten this planting series so to complete series (from the first day)
        #to the remainder of days
        tmin_2 <- tmin[1:(365-length(tmin_1))]
        tmin <- c(tmin_1,tmin_2)
        
        #previous year
        tmax <- read.csv(paste(wthDataDir,"/cru_tmx/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==pyr | tmax$YEAR==(pyr-1) | tmax$YEAR==(pyr+1)),]
        tmax$YEAR <- NULL
        tmax <- c(tmax$MONTH12[1],as.numeric(tmax[2,]),tmax$MONTH1[3])
        tmax <- linearise(tmax)[16:(365+15)] #interpolate to daily
        tmax_1 <- tmax[(sdate-30):365]
        
        #harvest year
        tmax <- read.csv(paste(wthDataDir,"/cru_tmx/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==yr | tmax$YEAR==(yr-1) | tmax$YEAR==(yr+1)),]
        tmax$YEAR <- NULL
        tmax <- c(tmax$MONTH12[1],as.numeric(tmax[2,]),tmax$MONTH1[3])
        tmax <- linearise(tmax)[16:(365+15)] #interpolate to daily
        tmax_2 <- tmax[1:(365-length(tmax_1))]
        tmax <- c(tmax_1,tmax_2)
        
        #previous year
        prec <- read.csv(paste(wthDataDir,"/rain/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==pyr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_1 <- prec[(sdate-30):365]
        
        #harv year
        prec <- read.csv(paste(wthDataDir,"/rain/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_2 <- prec[1:(365-length(prec_1))]
        prec <- c(prec_1,prec_2)
        
        srad <- read.csv(paste(wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_1 <- srad[(sdate-30):365]
        
        srad <- read.csv(paste(wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_2 <- srad[1:(365-length(srad_1))]
        srad <- c(srad_1,srad_2)
        
      } else {
        osdate <- sdate #output planting date
        
        tmin <- read.csv(paste(wthDataDir,"/cru_tmn/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==yr | tmin$YEAR==(yr-1) | tmin$YEAR==(yr+1)),]
        tmin$YEAR <- NULL
        tmin <- c(tmin$MONTH12[1],as.numeric(tmin[2,]),tmin$MONTH1[3])
        tmin <- linearise(tmin)[16:(365+15)] #interpolate to daily
        
        tmax <- read.csv(paste(wthDataDir,"/cru_tmx/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==yr | tmax$YEAR==(yr-1) | tmax$YEAR==(yr+1)),]
        tmax$YEAR <- NULL
        tmax <- c(tmax$MONTH12[1],as.numeric(tmax[2,]),tmax$MONTH1[3])
        tmax <- linearise(tmax)[16:(365+15)] #interpolate to daily
        
        prec <- read.csv(paste(wthDataDir,"/rain/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        
        srad <- read.csv(paste(wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
      }
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=srad,TMAX=tmax,TMIN=tmin,RAIN=prec)
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
    }
  }
  return(list(WTH_DIR=wthDir,SOW_DATE=osdate))
}



#################################################################################
#################################################################################
# function to make weather for a number of cells using GCM data
#################################################################################
#################################################################################
make_wth_gcm <- function(x,cell,wthDir,cmip_wthDataDir,base_wthDataDir,fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE")) {
  #checks
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y","SOW_DATE"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  names(x)[which(toupper(names(x)) == toupper(fields$SOW_DATE))] <- "SOW_DATE"
  
  #check if wthDir does exist
  if (!file.exists(wthDir)) {dir.create(wthDir)}
  
  #loop cells
  col <- 0; row <- 1
  for (cll in cell) {
    #site name and details
    lon <- x$X[which(cells$CELL == cll)]; lat <- x$Y[which(cells$CELL == cll)]
    
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    
    if (col < 10) {col_t <- paste("00",col,sep="")}
    if (col >= 10 & col < 100) {col_t <- paste("0",col,sep="")}
    if (col >= 100) {col_t <- paste(col)}
    
    if (row < 10) {row_t <- paste("00",row,sep="")}
    if (row >= 10 & row < 100) {row_t <- paste("0",row,sep="")}
    if (row >= 100) {row_t <- paste(row)}
    
    
    ###sowing date
    sdate <- x$SOW_DATE[which(cells$CELL == cll)]
    hdate <- sdate+120
    
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="INGC",LAT=lat,LONG=lon,ELEV=-99,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)
    
    #loop through years and write weather files
    for (yr in 1966:1993) {
      #yr <- 1966
      #the below needs to be changed if you wanna write more than 1 cell
      wthfile <- paste(wthDir,"/ingc",row_t,col_t,yr,".wth",sep="")
      
      #get the weather data for that particular gridcell
      if (hdate > 365) {
        osdate <- 31 #output planting date
        
        #planted in prev. year, get that weather
        pyr <- yr-1
        #previous year
        tmin <- read.csv(paste(cmip_wthDataDir,"/tasmin/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==pyr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        tmin_1 <- tmin[(sdate-30):365]
        
        #harv year
        tmin <- read.csv(paste(cmip_wthDataDir,"/tasmin/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==yr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        tmin_2 <- tmin[1:(365-length(tmin_1))]
        tmin <- c(tmin_1,tmin_2)
        
        #previous year
        tmax <- read.csv(paste(cmip_wthDataDir,"/tasmax/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==pyr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        tmax_1 <- tmax[(sdate-30):365]
        
        #harv year
        tmax <- read.csv(paste(cmip_wthDataDir,"/tasmax/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==yr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        tmax_2 <- tmax[1:(365-length(tmax_1))]
        tmax <- c(tmax_1,tmax_2)
        
        #previous year
        prec <- read.csv(paste(cmip_wthDataDir,"/pr/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==pyr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_1 <- prec[(sdate-30):365]
        
        #harv year
        prec <- read.csv(paste(cmip_wthDataDir,"/pr/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_2 <- prec[1:(365-length(prec_1))]
        prec <- c(prec_1,prec_2)
        
        #previous year
        srad <- read.csv(paste(base_wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_1 <- srad[(sdate-30):365]
        
        srad <- read.csv(paste(base_wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_2 <- srad[1:(365-length(srad_1))]
        srad <- c(srad_1,srad_2)
        
      } else {
        osdate <- sdate #output planting date
        
        tmin <- read.csv(paste(cmip_wthDataDir,"/tasmin/cell-",cll,".csv",sep=""))
        tmin <- tmin[which(tmin$YEAR==yr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        
        tmax <- read.csv(paste(cmip_wthDataDir,"/tasmax/cell-",cll,".csv",sep=""))
        tmax <- tmax[which(tmax$YEAR==yr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        
        prec <- read.csv(paste(cmip_wthDataDir,"/pr/cell-",cll,".csv",sep=""))
        prec <- prec[which(prec$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        
        srad <- read.csv(paste(base_wthDataDir,"/srad_e40/cell-",cll,".csv",sep=""))
        srad <- srad[which(srad$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
      }
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=srad,TMAX=tmax,TMIN=tmin,RAIN=prec)
      wx$SRAD[which(is.na(wx$SRAD))] <- -99.0
      wx$TMAX[which(is.na(wx$TMAX))] <- -99.0
      wx$TMIN[which(is.na(wx$TMIN))] <- -99.0
      wx$RAIN[which(is.na(wx$RAIN))] <- -99.0
      
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
    }
  }
  return(list(WTH_DIR=wthDir,SOW_DATE=osdate))
}

