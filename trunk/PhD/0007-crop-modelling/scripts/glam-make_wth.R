#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#February 2012

# 
# for (ws in wstList) {
#   xp <- mriData$ID[which(mriData$WSTID==ws)][1]
#   
#   cat(paste(ws),"of experiment",paste(xp),"\n")
#   #get the data formatted
#   siteData <- mriData[which(mriData$WSTID==ws),]
#   siteData$DATE <- siteData$YRDOY
#   siteData$YRDOY <- NULL
#   
#   yr <- siteData$YEAR[1]
#   if (yr<10) {
#     siteData$YEAR <- paste("0",siteData$YEAR,sep="")
#     siteData$DATE <- paste(siteData$YEAR,substring(siteData$DATE,2,nchar(siteData$DATE[1])+1),sep="")
#   }
#   
#   
#   #get the site details and srad from original file
#   wthFile <- paste("./DSSAT/",xp,"/",ws,"01.WTH",sep="")
#   siteDetails <- read.fortran(file=wthFile,skip=3,nrows=1,format=c("A6","2F9","5F6"))
#   names(siteDetails) <- c("SITE","LAT","LONG","ELEV","TAV","AMP","REFHT","WNDHT")
#   
#   #get the radiation data and put it into the other data
#   input <- read.fortran(file=wthFile,skip=5,format=c("A5","4F6"))
#   names(input) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
#   input$TMAX <- NULL; input$TMIN <- NULL; input$RAIN <- NULL
#   siteData <- merge(siteData,input,by="DATE")
#   #siteData$SRAD <- input$SRAD
#   
#   #Reading site name
#   sitetitle <- data.frame(NAME=paste("MRI Data",ws))
#   
#   #output folder and name
#   oName <- paste(ws,"01.WTH",sep="")
#   oFolder <- paste("./DSSAT-MRI/",xp,sep="")
#   if (!file.exists(oFolder)) {dir.create(oFolder)}
#   
#   #write dssat format
#   writeDSSAT(siteData,oName,outFolder=oFolder,site.longName=sitetitle,site.details=siteDetails)
# }


#write dssat format
#writeDSSAT(siteData,oName,outFolder=oFolder,site.longName=sitetitle,site.details=siteDetails)


#function to write dssat data
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

make_wth <- function(x,cell,wthDir,wthDataDir,fields=list(CELL="CELL",X="X",Y="Y")) {
  #checks
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y"))) != 3) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 3) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  
  
  #loop cells
  for (cll in cell) {
    #site name and details
    lon <- cells$X[which(cells$CELL == cll)]; lat <- cells$Y[which(cells$CELL == cll)]
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="INGC",LAT=lat,LONG=lon,ELEV=-99,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)
    
    #loop through years and write weather files
    for (yr in 1966:1994) {
      #yr <- 1966
      #the below needs to be changed if you wanna write more than 1 cell
      wthfile <- paste(wthDir,"/ingc001001",yr,".wth",sep="")
      
      #get the weather data for that particular gridcell
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
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=srad,TMAX=tmax,TMIN=tmin,RAIN=prec)
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
    }
  }
  return(wthDir)
}


