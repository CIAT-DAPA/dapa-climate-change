#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#plot the variation in the number of data points per day and year
library(raster)

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

wd <- "F:/PhD-work/crop-modelling/climate-data"
setwd(wd)

outEval <- "./daily-interpolations/0_out_eval"
if (!file.exists(outEval)) {dir.create(outEval)}

x <- getAvailableWS("afr",gdir="./gsod-daily",hdir="./ghcn-daily",1960,2009,outEval)
x <- getAvailableWS("sas",gdir="./gsod-daily",hdir="./ghcn-daily",1960,2009,outEval)

afr <- read.csv(paste(outEval,"/all_years-availability-afr.csv",sep=""))
sas <- read.csv(paste(outEval,"/all_years-availability-sas.csv",sep=""))

afr$TIME <- afr$YEAR+afr$DAY/366
sas$TIME <- sas$YEAR+sas$DAY/366

lims_avail <- c(min(afr$WST_TOTAL,sas$WST_TOTAL),max(afr$WST_TOTAL,sas$WST_TOTAL))

#make the plot
tiff(paste(outEval,"/availability_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(afr$TIME,afr$WST_TOTAL,ylim=lims_avail,type="l",
     col="red",lwd=0.25,xlab="Year",ylab="Number of weather stations")
lines(afr$TIME,afr$WST_CELLS,col="orange",lwd=0.25,lty=2)
lines(sas$TIME,sas$WST_TOTAL,col="black",lwd=0.25)
lines(sas$TIME,sas$WST_CELLS,col="grey 35",lwd=0.25,lty=2)
abline(v=1979,lty=2,col="black",lwd=0.75)
abline(v=1996,lty=2,col="black",lwd=0.75)
grid(col="grey 70")
dev.off()




#################################################################################
#Function to process all years and get the values
getAvailableWS <- function(re,gdir,hdir,iniyr,finyr,outEval) {
  for (ye in iniyr:finyr) {
    cat("\nProcessing year",ye,"\n")
    #loading the input data
    goData <- read.csv(paste(gdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
    goData$USAF <- NULL; goData$WBAN <- NULL #remove extra fields
    ghData <- read.csv(paste(hdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
     
    #merge both datasets
    gaData <- rbind(goData,ghData)
    nd <- leap(ye) #check whether leap year so to remove day 366 if needed
    if (nd==365) {gaData$X366 <- NULL}
    
    for (day in 1:nd) {
      cat(day," ")
      intMx <- gaData[,c(1:4,4+day)] #get data for that day
      names(intMx) <- c("ID","LON","LAT","ALT","RAIN") #rename matrix
      intMx <- intMx[which(!is.na(intMx$RAIN)),] #remove all rows with NAs in rainfall
      
      #count of 0.5 degree gridcell station presence
      rs <- raster(ncol=720,nrow=360); rs[] <- 1:ncell(rs)
      intMx$cells <- cellFromXY(rs,cbind(intMx$LON,intMx$LAT))
      uCells <- unique(intMx$cells)
      
      uStat <- nrow(intMx)
      outRow <- data.frame(YEAR=ye,DAY=day,WST_TOTAL=uStat,WST_CELLS=length(uCells))
      
      if (day==1) {
        outAll <- outRow
      } else {
        outAll <- rbind(outAll,outRow)
      }
    }
    cat("\n")
    
    if (ye==1960) {
      outYearAll <- outAll
    } else {
      outYearAll <- rbind(outYearAll,outAll)
    }
  }
  write.csv(outYearAll,paste(outEval,"/all_years-availability-",re,".csv",sep=""),row.names=F,quote=F)
  return(paste(outEval,"/all_years-availability-",re,".csv",sep=""))
}

