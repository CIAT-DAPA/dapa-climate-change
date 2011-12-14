#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/DSSAT-functions.R",sep=""))

#base dir
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"

#initial summarising
summariseExperiments(bd)
shuffleSummary(bd)

#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/s-performance.csv",sep=""))
tims <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/s-timeseries.csv",sep=""))
perf$LEGEND <- paste(toupper(perf$VAR)," (",toupper(perf$SCALE),")",sep="") #add legend column

#plot tif of RMSE
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_rmse2.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,8,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(perf$RMSE~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="Optimal RMSE (kg/ha)",
        horizontal=T,boxwex=0.7)
abline(v=227.5,lwd=0.7,lty=1,col="red")
abline(v=seq(0,700,by=50),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of r
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_corr2.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,8,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(perf$CORR~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="Optimal R (Pearson)",
        horizontal=T,boxwex=0.7)
abline(v=0.8482,lwd=0.7,lty=1,col="red")
abline(v=seq(-1,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of slpf
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_slpf2.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,8,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(perf$SLPF~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,ylim=c(0,1),
        horizontal=T,boxwex=0.7,
        xlab="Optimal SLPF")
abline(v=0.8,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()

#load observed yield
yData <- read.fortran("D:/CIAT_work/GLAM/PNAS-paper/GJ-yield/obsyield.txt",format=c("A12","F13"))
names(yData) <- c("DUMM1","OBYL")
OBYL <- yData$OBYL; rm(yData)

#plot a timeseries with error bars for each year for each experiment
for (i in 1:nrow(experiments)) {
  ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
  tims_sel <- tims[which(tims$TYPE==paste(ty) & tims$VAR==paste(va) & tims$SCALE==paste(sc)),]
  
  for (year in 1966:1989) {
    tims_year <- tims_sel[which(tims_sel$YEAR==year),]
    out_row <- data.frame(YEAR=year,HWAH=mean(tims_year$HWAH),OBYL=mean(tims_year$OBYL))
    if (year==1966) {
      out_mat <- out_row
    } else {
      out_mat <- rbind(out_mat,out_row)
    }
  }
  
  mn <- min(tims_sel$HWAH,tims_sel$OBYL) #min of matrix
  mx <- max(tims_sel$HWAH,tims_sel$OBYL) #max of matrix
  
  tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-timeseries-",ty,"-",va,"-",sc,".tif",sep="")
  tiff(tifName,res=300,pointsize=8,width=1000,height=800,units="px",compression="lzw")
  par(mar=c(3.5,4.5,1,1),cex=0.7,las=2,lwd=0.6)
  plot(out_mat$YEAR,out_mat$OBYL,pch=20,col="black",ylim=c(mn,mx),
       ylab="Yield (kg/ha)",xlab=NA)
  lines(out_mat$YEAR,out_mat$OBYL,lty=1,col="black",lwd=0.9)
  points(out_mat$YEAR,out_mat$OBYL,pch=20,col="black")
  lines(out_mat$YEAR,out_mat$HWAH,lty=1,col="red",lwd=0.9)
  points(out_mat$YEAR,out_mat$HWAH,pch=20,col="red")
  bp <- boxplot(tims_sel$HWAH~tims_sel$YEAR,plot=F)$stats #boxplot statistics
  lines(1966:1989,bp[1,],lty=3,col="red",lwd=0.9)
  lines(1966:1989,bp[2,],lty=2,col="red",lwd=0.9)
  lines(1966:1989,bp[4,],lty=2,col="red",lwd=0.9)
  lines(1966:1989,bp[5,],lty=3,col="red",lwd=0.9)
#   yrs <- 1966:1989
#   for (j in 1:24) {
#     lines(c(yrs[j],yrs[j]),c(bp[1,j],bp[5,j]),lty=2)
#   }
  grid()
  legend(1967,mx*0.98,legend=c("Observed","Average","top/bottom 25%","max/min"),
         col=c("black","red","red","red"),lty=c(1,1,2,3),
         pch=c(20,20,20,20),cex=0.8,ncol=2)
  dev.off()
}

