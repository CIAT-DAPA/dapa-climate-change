#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/EcoCrop-gnut_ps-functions.R",sep=""))

#base dir
bd <- "E:/PhD-work/data-quality-study/EcoCrop-GNUT"

#do the summarising
summariseExperiments(bd)
generalSummary(bd)


########################################################
############# SHUFFLED EXPERIMENTS #####################
#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/shuffle-perturb/climate_results/s-performance.csv",sep=""))
perf$LEGEND <- paste(toupper(perf$VAR)," (",toupper(perf$SCALE),")",sep="") #add legend column

#here you need to plot the stuff
tifName <- paste(bd,"/shuffle-perturb_results/s-performance_rmse-test.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$RMSE.TEST~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="Test RMSE",
        horizontal=T,boxwex=0.4)
abline(v=0.295,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()

tifName <- paste(bd,"/shuffle-perturb_results/s-performance_or-test.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$OR.TEST~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="Test RMSE",
        horizontal=T,boxwex=0.4)
abline(v=0.0183,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb_results/s-performance_rmse-or-test.tif",sep="")
tiff(tifName,res=300,pointsize=10,width=1000,height=1000,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.8)
uniqueLeg <- unique(perf$LEGEND)
count<-1
for (leg in uniqueLeg) {
  me.rmse <- mean(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mx.rmse <- max(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mn.rmse <- min(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  sd.rmse <- sd(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  
  me.or <- mean(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mx.or <- max(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mn.or <- min(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  sd.or <- sd(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  
  if (leg == uniqueLeg[1]) {
    plot(me.or,me.rmse,pch=20,xlim=c(0,1),ylim=c(0,1),
         xlab='OR',ylab='RMSE')
    lines(x=c(mn.or,mx.or),y=c(me.rmse,me.rmse),lty=1,lwd=1.2)
    lines(x=c(me.or,me.or),y=c(mn.rmse,mx.rmse),lty=1,lwd=1.2)
    #text(me.or+0.05,me.rmse+0.05,paste(leg))
  } else {
    points(me.or,me.rmse,pch=20+count)
    lines(x=c(mn.or,mx.or),y=c(me.rmse,me.rmse),lty=1,lwd=1.2)
    lines(x=c(me.or,me.or),y=c(mn.rmse,mx.rmse),lty=1,lwd=1.2)
    #text(me.or+0.05,me.rmse+0.05,paste(leg))
  }
  count <- count+1
}
grid()
abline(h=0.5,lty=2); abline(v=0.1,lty=2)
abline(h=0.295,lty=1,col='red'); abline(v=0.0183,lty=1,col='red')
legend(0.6,1,cex=0.9,pch=c(20,21,22),legend=c("Precipitation","Tmean","Tmin"))
dev.off()


#tpr, tnr, fpr
tifName <- paste(bd,"/shuffle-perturb_results/s-performance_tpr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$TPR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="True Positive Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.97,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb_results/s-performance_tnr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$TNR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="True Negative Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.877,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb_results/s-performance_fpr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$FPR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="False Positive Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.0302,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


########################################################
############# PERTURBED EXPERIMENTS ####################
#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/shuffle-perturb/climate_results/p-performance.csv",sep=""))
perf$LEGEND <- paste(toupper(perf$VAR)," (",toupper(perf$SCALE),")",sep="") #add legend column

#here you need to plot the stuff
tifName <- paste(bd,"/shuffle-perturb/climate_results/p-performance_rmse-test.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$RMSE.TEST~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="Test RMSE",
        horizontal=T,boxwex=0.4)
abline(v=0.295,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()

tifName <- paste(bd,"/shuffle-perturb/climate_results/p-performance_or-test.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$OR.TEST~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="Test RMSE",
        horizontal=T,boxwex=0.4)
abline(v=0.0183,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb/climate_results/p-performance_rmse-or-test.tif",sep="")
tiff(tifName,res=300,pointsize=10,width=1000,height=1000,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.6,las=2,lwd=0.8)
uniqueLeg <- unique(perf$LEGEND)
count<-1
for (leg in uniqueLeg) {
  me.rmse <- mean(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mx.rmse <- max(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mn.rmse <- min(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  sd.rmse <- sd(perf$RMSE.TEST[which(perf$LEGEND==leg)],na.rm=T)
  
  me.or <- mean(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mx.or <- max(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  mn.or <- min(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  sd.or <- sd(perf$OR.TEST[which(perf$LEGEND==leg)],na.rm=T)
  
  if (leg == uniqueLeg[1]) {
    plot(me.or,me.rmse,pch=19,xlim=c(0,1),ylim=c(0,1),
         xlab='OR',ylab='RMSE')
    lines(x=c(mn.or,mx.or),y=c(me.rmse,me.rmse),lty=1,lwd=1.2)
    lines(x=c(me.or,me.or),y=c(mn.rmse,mx.rmse),lty=1,lwd=1.2)
    #text(me.or+0.05,me.rmse+0.05,paste(leg))
  } else {
    points(me.or,me.rmse,pch=19+count)
    lines(x=c(mn.or,mx.or),y=c(me.rmse,me.rmse),lty=1,lwd=1.2)
    lines(x=c(me.or,me.or),y=c(mn.rmse,mx.rmse),lty=1,lwd=1.2)
    #text(me.or+0.05,me.rmse+0.05,paste(leg))
  }
  count <- count+1
}
grid()
abline(h=0.5,lty=2); abline(v=0.1,lty=2)
abline(h=0.295,lty=1,col='red'); abline(v=0.0183,lty=1,col='red')
legend(0.6,1,cex=0.8,pch=c(19,20,21,22,23,24,25),
       legend=c("Prec-SEAS","Prec-SPAT","Tmean-SEAS","Tmean-SPAT","Tmin-SEAS","Tmin-SPAT"),ncol=2)
dev.off()


#tpr, tnr, fpr
tifName <- paste(bd,"/shuffle-perturb_results/s-performance_tpr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$TPR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="True Positive Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.97,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb_results/s-performance_tnr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$TNR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="True Negative Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.877,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()


tifName <- paste(bd,"/shuffle-perturb_results/s-performance_fpr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=600,units="px",compression="lzw")
par(mar=c(5,10,1,1),cex=0.6,las=2,lwd=0.5)
boxplot(perf$FPR~perf$LEGEND,
        col="grey",lty=1,ylim=c(0,1),
        pch=20,outwex=0.3,
        xlab="False Positive Rate",
        horizontal=T,boxwex=0.4)
abline(v=0.0302,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.1),lwd=0.6,lty=2,col="grey50")
dev.off()

