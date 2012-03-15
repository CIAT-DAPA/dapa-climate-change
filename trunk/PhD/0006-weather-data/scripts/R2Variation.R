#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#plot the variation in the correlation coefficient

wd <- "F:/PhD-work/crop-modelling/climate-data/daily-interpolations"
setwd(wd)

outEval <- "./0_out_eval"
if (!file.exists(outEval)) {dir.create(outEval)}

x <- getAllMetrics("eaf",1960,2009)
x <- getAllMetrics("waf",1960,2009)
x <- getAllMetrics("igp",1960,2009)

eaf <- read.csv(paste(outEval,"/all_years-metrics-eaf.csv",sep=""))
waf <- read.csv(paste(outEval,"/all_years-metrics-waf.csv",sep=""))
igp <- read.csv(paste(outEval,"/all_years-metrics-igp.csv",sep=""))

lims_rmse <- c(min(eaf$RMSE,waf$RMSE,igp$RMSE),max(eaf$RMSE,waf$RMSE,igp$RMSE))

#rmse plot
tiff(paste(outEval,"/rmse_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(iniyr:finyr,eaf$RMSE,ylim=lims_rmse,pch=20,type="p",
     col="blue",cex=0.75,xlab="YEAR",ylab="RMSE (mm/month/gridcell)")
lines(iniyr:finyr,eaf$RMSE,col="blue",lwd=0.7)
points(iniyr:finyr,waf$RMSE,pch=20,col="red",cex=0.75)
lines(iniyr:finyr,waf$RMSE,col="red",lwd=0.7)
points(iniyr:finyr,igp$RMSE,pch=20,col="black",cex=0.75)
lines(iniyr:finyr,igp$RMSE,col="black",lwd=0.7)
grid(col="grey 70")
dev.off()

#rsq plot
tiff(paste(outEval,"/rsqf_historical.tif",sep=""),res=300,pointsize=10,width=1000,
     height=700,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.6)
plot(iniyr:finyr,eaf$RSQ.FORCED,ylim=c(0,1),pch=20,type="p",
     col="blue",cex=0.75,xlab="YEAR",ylab="RMSE (mm/month/gridcell)")
lines(iniyr:finyr,eaf$RSQ.FORCED,col="blue",lwd=0.7)
points(iniyr:finyr,waf$RSQ.FORCED,pch=20,col="red",cex=0.75)
lines(iniyr:finyr,waf$RSQ.FORCED,col="red",lwd=0.7)
points(iniyr:finyr,igp$RSQ.FORCED,pch=20,col="black",cex=0.75)
lines(iniyr:finyr,igp$RSQ.FORCED,col="black",lwd=0.7)
abline(h=0.5,lwd=0.7,lty=2)
grid(col="grey 70")
dev.off()


##############################################################################
##############################################################################
#function
getAllMetrics <- function(region,iniyr,finyr,ourEval) {
  #set up region and output folder
  if (region=="eaf" | region=="waf") {rgn <- "afr"} else {rgn <- "sas"}
  
  #loop through years
  for (yr in iniyr:finyr) {
    cat("year", yr,"\n")
    yrDir <- paste("./",yr,"-",rgn,"-eval",sep="")
    
    #load metrics
    mets <- read.csv(paste(yrDir,"/metrics-",region,".csv",sep=""))
    rawd <- read.csv(paste(yrDir,"/raw_eval_data-",region,".csv",sep=""))
    
    rawd <- rawd[which(!is.na(rawd$CRU_RAIN)),]
    rawd <- rawd[which(!is.na(rawd$INT_RAIN)),]
    
    #calculate origin-fixed correlation
    fit.f <- lm(rawd$CRU_RAIN ~ rawd$INT_RAIN - 1) #Fit forced to origin
    rsq.f <- summary(fit.f)$r.squared
    pval.f <- pf(summary(fit.f)$fstatistic[1],summary(fit.f)$fstatistic[2],summary(fit.f)$fstatistic[3],lower.tail=F)
    
    #calculate RMSE
    rawd$SQDIFF <- (rawd$CRU_RAIN-rawd$INT_RAIN)^2
    rmse <- sqrt(sum(rawd$SQDIFF)/nrow(rawd))
    
    outRow <- data.frame(YEAR=yr,RMSE=rmse,RSQ.FORCED=rsq.f,PVAL.FORCED=pval.f)
    
    if (yr==iniyr) {
      outAll <- outRow
    } else {
      outAll <- rbind(outAll,outRow)
    }
  }
  
  write.csv(outAll,paste(outEval,"/all_years-metrics-",region,".csv",sep=""),row.names=F,quote=F)
  return(paste(outEval,"/all_years-metrics-",region,".csv",sep=""))
}

