#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#plot the variation in the correlation coefficient

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/interpolate-functions.R",sep=""))

#working directory
#wd <- "W:/eejarv/PhD-work/crop-modelling/climate-data/daily-interpolations-v3"
#wd <- "F:/PhD-work/crop-modelling/climate-data/daily-interpolations"
setwd(wd)

outEval <- "./0_out_eval"
if (!file.exists(outEval)) {dir.create(outEval)}

iniyr <- 1960; finyr <- 2009

#x <- getAllMetrics("eaf",1960,2009)
x <- getAllMetrics("waf",1960,2009)
#x <- getAllMetrics("igp",1960,2009)

#eaf <- read.csv(paste(outEval,"/all_years-metrics-eaf.csv",sep=""))
waf <- read.csv(paste(outEval,"/all_years-metrics-waf.csv",sep=""))
#igp <- read.csv(paste(outEval,"/all_years-metrics-igp.csv",sep=""))

#lims_rmse <- c(min(eaf$RMSE,waf$RMSE,igp$RMSE),max(eaf$RMSE,waf$RMSE,igp$RMSE))
lims_rmse <- c(min(waf$RMSE),max(waf$RMSE))

#rmse plot
tiff(paste(outEval,"/rmse_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(iniyr:finyr,waf$RMSE,ylim=lims_rmse,pch=20,type="p",
     col="blue",cex=0.75,xlab="YEAR",ylab="RMSE (mm/month/gridcell)")
lines(iniyr:finyr,waf$RMSE,col="blue",lwd=0.7)
#points(iniyr:finyr,eaf$RMSE,pch=20,col="red",cex=0.75)
#lines(iniyr:finyr,eaf$RMSE,col="red",lwd=0.7)
#points(iniyr:finyr,igp$RMSE,pch=20,col="black",cex=0.75)
#lines(iniyr:finyr,igp$RMSE,col="black",lwd=0.7)
grid(col="grey 70")
abline(v=1979,lwd=0.7,lty=2)
abline(v=1996,lwd=0.7,lty=2)
dev.off()

#rsq plot
tiff(paste(outEval,"/rsqf_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(iniyr:finyr,waf$RSQ.FORCED,ylim=c(0,1),pch=20,type="p",
     col="blue",cex=0.75,xlab="YEAR",ylab="RMSE (mm/month/gridcell)")
lines(iniyr:finyr,waf$RSQ.FORCED,col="blue",lwd=0.7)
#points(iniyr:finyr,eaf$RSQ.FORCED,pch=20,col="red",cex=0.75)
#lines(iniyr:finyr,eaf$RSQ.FORCED,col="red",lwd=0.7)
#points(iniyr:finyr,igp$RSQ.FORCED,pch=20,col="black",cex=0.75)
#lines(iniyr:finyr,igp$RSQ.FORCED,col="black",lwd=0.7)
abline(h=0.5,lwd=0.7,lty=2,col="red")
abline(h=0.75,lwd=0.7,lty=2)
abline(v=1979,lwd=0.7,lty=2)
abline(v=1996,lwd=0.7,lty=2)
grid(col="grey 70")
dev.off()



