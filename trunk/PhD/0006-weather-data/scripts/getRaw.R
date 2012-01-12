#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

inDir <- "D:/CIAT_work/crop-modelling/climate-data"
gdir <- paste(inDir,"/gsod-daily",sep="")
hdir <- paste(inDir,"/ghcn-daily",sep="")
ye <- 1960; re <- "sas"

##########################################
############### FOR GHCN #################
#load stations
stat <- read.fortran(paste(hdir,"/ghcnd-stations.txt",sep=""),
  					format=c("A11","F9","F10","F7","1X","A2","1X","A31","A3","1X","A3","I6"))
names(stat) <- c("ID","LAT","LON","ALT","STATE","NAME","GSN_FLAG","HCN_FLAG","WMO_ID")

#output directory
oDir <- paste(hdir,"/grouped_output-",re,sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#loop through years
for (ye in 1960:2010) {
  cat("Processing year",ye,"\n")
  #naming output file
  oFile <- paste(oDir,"/",ye,".csv",sep="")
  if (!file.exists(oFile)) {
    omx <- getRawGHCN(hdir,ye,re,stat)
    write.csv(omx,oFile,quote=F,row.names=F)
  } else {
    omx <- read.csv(oFile)
  }
  
  #count NAs for each of the days
  allNAs <- apply(omx,2,countNADays); allNAs <- c(ye,allNAs[5:370])
  #plot(1:366,allNAs[2:367]/nrow(omx),type='l',xlab="Day of year",ylab="Fraction of NAs")
  
  #rbind all the stations
  if (ye==1960) {
    allStNAs <- allNAs
  } else {
    allStNAs <- rbind(allStNAs,allNAs)
  }
}
write.csv(allStNAs,paste(oDir,"/NA-count.csv",sep=""),row.names=F,quote=F)


##########################################
############### FOR GSOD #################
#output folder for region
oDir <- paste(gdir,"/grouped_output-",re,sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#Getting and writing data (loop of years)
for (ye in 1960:2010) {
  cat("Processing year",ye,"\n")
  #Process the station
  oFile <- paste(oDir,"/",ye,".csv",sep="")
  if (!file.exists(oFile)) {
    dmx <- getRawGSOD(gdir,ye,re)
    write.csv(dmx,oFile,quote=F,row.names=F)
  } else {
    dmx <- read.csv(oFile)
  }
  
  #count the NAs for each day
  allNAs <- apply(dmx,2,countNADays); allNAs <- c(ye,allNAs[7:372])
  #plot(1:366,allNAs[2:367]/nrow(dmx),type='l',xlab="Day of year",ylab="Fraction of NAs")
  
  #rbind all the stations
  if (ye==1960) {
    allStNAs <- allNAs
  } else {
    allStNAs <- rbind(allStNAs,allNAs)
  }
}
write.csv(allStNAs,paste(oDir,"/NA-count.csv",sep=""),row.names=F,quote=F)

##############################################################################
#Functions
#function to read in and input data for GHCN stuff
getRawGHCN <- function(ghcnDir,yr,rg,stations) {
  #list stations in folder
  reDir <- paste(hdir,"/ghcnd_",rg,sep="")
  stList <- list.files(reDir)
  
  #create output matrix
  dayMx <- as.data.frame(matrix(ncol=370,nrow=length(stList)))
  names(dayMx) <- c("ID","LON","LAT","ALT",1:366)
  dayMx$ID <- stList
  
  for (i in 1:nrow(dayMx)) {
    id <- dayMx$ID[i]
    stFile <- paste(reDir,"/",id,"/",yr,".csv",sep="")
    stData <- read.csv(stFile)
    
    #put everything back onto matrix
    dayMx$LON[i] <- stations$LON[which(stations$ID==dayMx$ID[i])]
    dayMx$LAT[i] <- stations$LAT[which(stations$ID==dayMx$ID[i])]
    dayMx$ALT[i] <- stations$ALT[which(stations$ID==dayMx$ID[i])]
    dd <- stData$PRCP; if (length(dd) == 365) {dd <- c(dd,NA)}
    dayMx[which(dayMx$ID==id),5:370] <- dd
  }
  return(dayMx)
}


#Function to get GSOD data and write it
getRawGSOD <- function(gsodDir,yr,rg) {
  #open list of stations
  stations <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
  
  #list stations and set IDs
  yrDir <- paste(gsodDir,"/",yr,"_out-",rg,sep="")
  stList <- list.files(yrDir)
  #create output matrix
  dayMx <- as.data.frame(matrix(ncol=372,nrow=length(stList)))
  names(dayMx) <- c("ID","USAF","WBAN","LON","LAT","ALT",1:366)
  
  #if no stations exist for that year
  if (length(stList)!=0) {
    stList <- gsub(paste("-",yr,".csv",sep=""),"",stList)
    which.usaf <- seq(1,length(stList)*2-1,by=2)
    usaf <- unlist(strsplit(stList,"-",fixed=T))[which.usaf]
    wban <- unlist(strsplit(stList,"-",fixed=T))[-which.usaf]
    
    #get ids in output matrix
    dayMx$ID <- stList; dayMx$USAF <- usaf; dayMx$WBAN <- wban
    
    #assign IDs
    stations$ID <- paste(stations$USAF,"-",stations$WBAN,sep="")
    
    #assign latitude and longitude
    for (i in 1:length(stList)) {
      dayMx$LON[i] <- stations$LON[which(stations$ID==dayMx$ID[i])] / 1000
      dayMx$LAT[i] <- stations$LAT[which(stations$ID==dayMx$ID[i])] / 1000
      dayMx$ALT[i] <- stations$ELEV..1M.[which(stations$ID==dayMx$ID[i])] / 10
    }
    
    #read in and assign the variable data
    for (i in 1:nrow(dayMx)) {
      id <- dayMx$ID[i]
      stFile <- paste(yrDir,"/",id,"-",yr,".csv",sep="")
      stData <- read.csv(stFile)
      
      #make NA all those data that are untrustable, RAIN.FLAG == I | A | B | C | E
      stData$RAIN[which(stData$RAIN.FLAG=="I")] <- NA
      stData$RAIN[which(stData$RAIN.FLAG=="A")] <- NA
      stData$RAIN[which(stData$RAIN.FLAG=="B")] <- NA
      stData$RAIN[which(stData$RAIN.FLAG=="C")] <- NA
      stData$RAIN[which(stData$RAIN.FLAG=="E")] <- NA
      
      #put everything onto matrix
      dd <- stData$RAIN; if (length(dd) == 365) {dd <- c(dd,NA)}
      dayMx[which(dayMx$ID==id),7:372] <- dd
    }
  }
  return(dayMx)
}

#count number of stations per day
countNADays <- function(dayData) {
  nas <- length(which(is.na(dayData)))
  return(nas)
}

