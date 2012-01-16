#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/getRaw-functions.R",sep=""))

inDir <- "E:/PhD-work/crop-modelling/climate-data"
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

