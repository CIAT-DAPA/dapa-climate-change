#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#February 2012

#for MRI paper!

wd <- "D:/Dropbox/MRI_Validation/crop-modelling"
setwd(wd)

#read MRI daily data
mriData <- read.csv("./DSSAT-MRI/MRI_data.csv")

#list unique weather stations
wstList <- unique(mriData$WSTID)

for (ws in wstList) {
  xp <- mriData$ID[which(mriData$WSTID==ws)][1]
  
  cat(paste(ws),"of experiment",paste(xp),"\n")
  #get the data formatted
  siteData <- mriData[which(mriData$WSTID==ws),]
  siteData$DATE <- siteData$YRDOY
  siteData$YRDOY <- NULL
  
  yr <- siteData$YEAR[1]
  if (yr<10) {
    siteData$YEAR <- paste("0",siteData$YEAR,sep="")
    siteData$DATE <- paste(siteData$YEAR,substring(siteData$DATE,2,nchar(siteData$DATE[1])+1),sep="")
  }
  
  
  #get the site details and srad from original file
  wthFile <- paste("./DSSAT/",xp,"/",ws,"01.WTH",sep="")
  siteDetails <- read.fortran(file=wthFile,skip=3,nrows=1,format=c("A6","2F9","5F6"))
  names(siteDetails) <- c("SITE","LAT","LONG","ELEV","TAV","AMP","REFHT","WNDHT")
  
  #get the radiation data and put it into the other data
  input <- read.fortran(file=wthFile,skip=5,format=c("A5","4F6"))
  names(input) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
  input$TMAX <- NULL; input$TMIN <- NULL; input$RAIN <- NULL
  siteData <- merge(siteData,input,by="DATE")
  #siteData$SRAD <- input$SRAD
  
  #Reading site name
  sitetitle <- data.frame(NAME=paste("MRI Data",ws))
  
  #output folder and name
  oName <- paste(ws,"01.WTH",sep="")
  oFolder <- paste("./DSSAT-MRI/",xp,sep="")
  if (!file.exists(oFolder)) {dir.create(oFolder)}
  
  #write dssat format
  writeDSSAT(siteData,oName,outFolder=oFolder,site.longName=sitetitle,site.details=siteDetails)
}


#function to write dssat data
#inData should have fields named: DATE, SRAD, TMAX, TMIN, RAIN
#site.details should have the AMP and related header-type data
writeDSSAT <- function(inData,outName,outFolder,site.longName,site.details) {
  #Open file
  dssat <- file(paste(outFolder,"/",outName,sep=""),open="w")
  
  inData$JDAY <- as.numeric(substr(inData$DATE,3,5))
  inData$DSSATDATE <- inData$DATE
  
  #Write header
  cat(paste("*WEATHER DATA : ",site.longName$NAME,sep=""),"\n",sep="",file=dssat)
  cat("\n",file=dssat)
  cat("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n",file=dssat)
  cat(site.details$SITE,sep="",file=dssat)
  cat(sprintf("%9.3f",site.details$LAT),file=dssat)
  cat(sprintf("%9.3f",site.details$LONG),file=dssat)
  cat(sprintf("%6.1f",site.details$ELEV),file=dssat)
  cat(sprintf("%6.1f",site.details$TAV),file=dssat)
  cat(sprintf("%6.1f",site.details$AMP),file=dssat)
  cat(sprintf("%6.1f",site.details$REFHT),file=dssat)
  cat(sprintf("%6.1f",site.details$WNDHT),file=dssat)
  cat("\n",file=dssat)
  cat("@DATE  SRAD  TMAX  TMIN  RAIN\n",file=dssat)
  
  for (row in 1:nrow(inData)) {
    cat(inData$DSSATDATE[row],file=dssat)
    cat(sprintf("%6.1f",inData$SRAD[row]),file=dssat)
    cat(sprintf("%6.1f",inData$TMAX[row]),file=dssat)
    cat(sprintf("%6.1f",inData$TMIN[row]),file=dssat)
    cat(sprintf("%6.1f",inData$RAIN[row]),file=dssat)
    cat("\n",file=dssat)
  }
  
  close(dssat)
  
}


