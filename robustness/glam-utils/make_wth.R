#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#January 2014 --borrows from PhD script called glam-make_wth.R

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
make_wth <- function(x,wthDir,years,fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE")) {
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
  
  #all cells
  cell <- x$CELL
  
  #loop cells
  col <- 0; row <- 1
  for (cll in cell) {
    #cll <- cell[1]
    #site name and details
    lon <- x$X[which(x$CELL == cll)]; lat <- x$Y[which(x$CELL == cll)]
    
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
    sdate <- x$SOW_DATE[which(x$CELL == cll)]
    hdate <- sdate+120
    
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="AFRB",LAT=lat,LONG=lon,
                            ELEV=-99,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)
    
    ###read in meteorology
    metdata <- read.table(paste(wthDir,"/meteo_cell-",cll,".met",sep=""),header=T,sep="\t")
    
    #loop through years and write weather files
    for (yr in years) {
      #yr <- 1950
      #the below needs to be changed if you wanna write more than 1 cell
      wthfile <- paste(wthDir,"/afrb",row_t,col_t,yr,".wth",sep="")
      
      #get the weather data for that particular gridcell
      if (hdate > 365) {
        osdate <- 31 #output planting date
        
        #planted in prev. year, get that weather
        pyr <- yr-1
        
        #previous year
        mdat <- metdata[which(metdata$year == pyr),]
        mdat$year <- NULL
        mdat_1 <- mdat[(sdate-30):365,]
        
        #harv year
        mdat <- metdata[which(metdata$year == yr),]
        mdat$year <- NULL
        mdat_2 <- mdat[1:(365-nrow(mdat_1)),]
        mdat <- rbind(mdat_1,mdat_2)
      } else {
        osdate <- sdate #output planting date
        
        mdat <- metdata[which(metdata$year == yr),]
        mdat$year <- NULL
      }
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=mdat$rsds,TMAX=mdat$tasmax,TMIN=mdat$tasmin,RAIN=mdat$pr)
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
    }
  }
  return(list(WTH_DIR=wthDir,SOW_DATE=osdate))
}


