#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#plot the variation in the number of data points per day and year
library(raster)

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/interpolate-functions.R",sep=""))

#wd <- "F:/PhD-work/crop-modelling/climate-data"
#wd <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data"
#wd <- "W:/eejarv/PhD-work/crop-modelling/climate-data"
setwd(wd)

outEval <- "./daily-interpolations-v3/0_out_eval"
if (!file.exists(outEval)) {dir.create(outEval)}

#africa
x <- getAvailableWS("afr",gdir="./gsod-daily",hdir="./ghcn-daily",cdir="./CIAT-daily/selection_afr",1960,2009,outEval)
afr <- read.csv(paste(outEval,"/all_years-availability-afr.csv",sep=""))
afr$TIME <- afr$YEAR+afr$DAY/366
lims_avail <- c(min(afr$WST_TOTAL),max(afr$WST_TOTAL))

#south asia. i did not work on this one anymore after i got the IITM dataset
#x <- getAvailableWS("sas",gdir="./gsod-daily",hdir="./ghcn-daily",1960,2009,outEval)
#sas <- read.csv(paste(outEval,"/all_years-availability-sas.csv",sep=""))
#sas$TIME <- sas$YEAR+sas$DAY/366
#lims_avail <- c(min(afr$WST_TOTAL,sas$WST_TOTAL),max(afr$WST_TOTAL,sas$WST_TOTAL))


#make the plot
tiff(paste(outEval,"/availability_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(afr$TIME,afr$WST_TOTAL,ylim=lims_avail,type="l",
     col="red",lwd=0.25,xlab="Year",ylab="Number of weather stations")
lines(afr$TIME,afr$WST_CELLS,col="orange",lwd=0.25,lty=2)
#lines(sas$TIME,sas$WST_TOTAL,col="black",lwd=0.25)
#lines(sas$TIME,sas$WST_CELLS,col="grey 35",lwd=0.25,lty=2)
abline(v=1979,lty=2,col="black",lwd=0.75)
abline(v=1996,lty=2,col="black",lwd=0.75)
grid(col="grey 70")
dev.off()




