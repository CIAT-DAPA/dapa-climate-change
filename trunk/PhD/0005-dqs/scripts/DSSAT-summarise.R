#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/DSSAT-functions.R",sep=""))

#base dir
bd <- "E:/PhD-work/data-quality-study/DSSAT-PNUT"

#initial summarising
summariseExperiments(bd)
generalSummary(bd)

###################################################################################
############################## SHUFFLED EXPERIMENTS ###############################
###################################################################################
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


###################################################################################
############################## PERTURBED EXPERIMENTS ##############################
###################################################################################
#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/p-performance.csv",sep=""))
tims <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/p-timeseries.csv",sep=""))
perf$LEGEND <- paste(toupper(perf$VAR)," (",toupper(perf$SCALE),")",sep="") #add legend column

#plot tif of RMSE
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_rmse.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,9,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(perf$RMSE~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="Optimal RMSE (kg/ha)",
        horizontal=T,boxwex=0.7)
abline(v=227.5,lwd=0.7,lty=1,col="red")
abline(v=seq(0,5000,by=500),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of r
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_corr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,9,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(perf$CORR~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,ylim=c(-1,1),
        xlab="Optimal R (Pearson)",
        horizontal=T,boxwex=0.7)
abline(v=0.8482,lwd=0.7,lty=1,col="red")
abline(v=seq(-1,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of slpf
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_slpf.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,9,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(perf$SLPF~perf$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,ylim=c(0,1),
        horizontal=T,boxwex=0.7,
        xlab="Optimal SLPF")
abline(v=0.8,lwd=0.7,lty=1,col="red")
abline(v=seq(0,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()

# special plots for each of the perturbed experiments where x-axis is the value of p (0-299) 
# and y-axis is RMSE, corr and etc, with 100-fold variability expressed as the interquartile range
expe <- unique(perf$LEGEND)
for (xp in expe) {
  dxp <- perf[which(perf$LEGEND==xp),] #sub-select the data
  ty <- dxp$TYPE[1]; va <- dxp$VAR[1]; sc <- dxp$SCALE[1]
  
  cat(paste(ty),"/",paste(va),"/",paste(sc),"\n")
  
  #RMSE for this first experiment
  bpxp <- boxplot(dxp$RMSE~dxp$P,plot=F)$stats
  #ylims <- c(min(bpxp[2:4,]),max(bpxp[2:4,]))
  ylims <- c(100,1000)
  #leglocy <- min(bpxp[2:4,])*1.25
  tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_pval-rmse-",
                   ty,"-",va,"-",sc,"-.tif",sep="")
  tiff(tifName,res=300,pointsize=8,width=1000,height=800,units="px",compression="lzw")
  par(mar=c(4.5,4.5,1,1),cex=0.7,las=2,lwd=0.6)
  plot(c(0:299),bpxp[3,],type="l",col="black",
       ylim=ylims,xlab="Perturbing value",ylab="Optimal RMSE (kg/ha)")
  lines(c(0:299),bpxp[2,],col="red",lty=2)
  lines(c(0:299),bpxp[4,],col="red",lty=2)
  #lines(c(0:299),bpxp[1,],col="red",lty=3)
  #lines(c(0:299),bpxp[5,],col="red",lty=3)
  #abline(h=227.5,lwd=0.7,lty=1,col="red")
  grid()
  legend(0,950,legend=c("Average","top/bottom 25%"),
         col=c("black","red"),lty=c(1,2),
         pch=c(20,20),cex=0.8,ncol=1)
  dev.off()
  
  #correlation for this first experiment
  bpxp <- boxplot(dxp$CORR~dxp$P,plot=F)$stats
  leglocy <- min(bpxp[2:4,])*1.25
  tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_pval-corr-",
                   ty,"-",va,"-",sc,"-.tif",sep="")
  tiff(tifName,res=300,pointsize=8,width=1000,height=800,units="px",compression="lzw")
  par(mar=c(4.5,4.5,1,1),cex=0.7,las=2,lwd=0.6)
  plot(c(0:299),bpxp[3,],type="l",col="black",
       ylim=c(-1,1),xlab="Perturbing value",ylab="Optimal R Pearson")
  lines(c(0:299),bpxp[2,],col="red",lty=2)
  lines(c(0:299),bpxp[4,],col="red",lty=2)
  #lines(c(0:299),bpxp[1,],col="red",lty=3)
  #lines(c(0:299),bpxp[5,],col="red",lty=3)
  abline(h=227.5,lwd=0.7,lty=1,col="red")
  grid()
  legend(230,-0.5,legend=c("Average","top/bottom 25%"),
         col=c("black","red"),lty=c(1,2),
         pch=c(20,20),cex=0.8,ncol=1)
  dev.off()
  
  #slpf for this first experiment
  bpxp <- boxplot(dxp$SLPF~dxp$P,plot=F)$stats
  leglocy <- min(bpxp)*1.25
  tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-performance_pval-slpf-",
                   ty,"-",va,"-",sc,"-.tif",sep="")
  tiff(tifName,res=300,pointsize=8,width=1000,height=800,units="px",compression="lzw")
  par(mar=c(4.5,4.5,1,1),cex=0.7,las=2,lwd=0.6)
  plot(c(0:299),bpxp[3,],type="l",col="black",
       ylim=c(0,1),xlab="Perturbing value",ylab="Optimal SLPF")
  lines(c(0:299),bpxp[2,],col="red",lty=2)
  lines(c(0:299),bpxp[4,],col="red",lty=2)
  #lines(c(0:299),bpxp[1,],col="red",lty=3)
  #lines(c(0:299),bpxp[5,],col="red",lty=3)
  #abline(h=227.5,lwd=0.7,lty=1,col="red")
  grid()
  legend(230,0.2,legend=c("Average","top/bottom 25%"),
         col=c("black","red"),lty=c(1,2),
         pch=c(20,20),cex=0.8,ncol=1)
  dev.off()
}

###########
#yield time series plots
###########

#load observed yield
yData <- read.fortran("E:/PhD-work/data-quality-study/GJ-yield/obsyield.txt",format=c("A12","F13"))
names(yData) <- c("DUMM1","OBYL")
OBYL <- yData$OBYL; rm(yData)

#plot a timeseries with error bars for each year for each experiment
expe <- unique(perf$LEGEND)
for (xp in expe) {
  dxp <- perf[which(perf$LEGEND==xp),] #sub-select the data
  ty <- dxp$TYPE[1]; va <- dxp$VAR[1]; sc <- dxp$SCALE[1]
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
  
  tifName <- paste(bd,"/GJ-weather/shuf-pert_results/p-timeseries-",ty,"-",va,"-",sc,".tif",sep="")
  tiff(tifName,res=300,pointsize=8,width=1000,height=800,units="px",compression="lzw")
  par(mar=c(3.5,4.5,1,1),cex=0.7,las=2,lwd=0.6)
  plot(out_mat$YEAR,out_mat$OBYL,pch=20,col="black",ylim=c(0,3000),
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
  legend(1967,3000*0.98,legend=c("Observed","Average","top/bottom 25%","max/min"),
         col=c("black","red","red","red"),lty=c(1,1,2,3),
         pch=c(20,20,20,20),cex=0.8,ncol=2)
  dev.off()
}


