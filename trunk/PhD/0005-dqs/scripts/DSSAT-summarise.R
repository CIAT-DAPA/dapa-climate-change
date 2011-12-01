#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/DSSAT-functions.R",sep=""))

#base dir
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"


#function to get the summarised data and plot the results in boxplot
shuffleSummary <- function(bDir) {
  experiments <- createExpMat(bDir)[11:20,]
  for (i in 1:nrow(experiments)) {
    cat("processing",paste(ty),"/",paste(va),"/",paste(sc),"\n")
    ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
    sp_folder <- paste(bDir,"/GJ-weather/shuf-pert/",ty,"_",va,"_",sc,sep="")
    performance <- read.csv(paste(sp_folder,"/performance.csv",sep=""))
    timeseries <- read.csv(paste(sp_folder,"/timeseries.csv",sep=""))
    if (i==1) {
      perf <- performance
      tims <- timeseries
    } else {
      perf <- rbind(perf,performance)
      tims <- rbind(tims,timeseries)
    }
  }
  write.csv(perf,paste(bDir,"/GJ-weather/shuf-pert_results/s-performance.csv",sep=""),quote=F,row.names=F)
  write.csv(tims,paste(bDir,"/GJ-weather/shuf-pert_results/s-timeseries.csv",sep=""),quote=F,row.names=F)
}

#read in files again to avoid re-running the above
perf <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/s-performance.csv",sep=""))
tims <- read.csv(paste(bd,"/GJ-weather/shuf-pert_results/s-timeseries.csv",sep=""))
perf$LEGEND <- paste(toupper(perf$VAR)," (",toupper(perf$SCALE),")",sep="") #add legend column

#plot tif of RMSE
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_rmse.tif",sep="")
tiff(tifName,res=300,pointsize=8.5,width=1000,height=800,units="px",compression="lzw")
par(mar=c(8,5,1,1),cex=0.7,las=2,lwd=0.6)
boxplot(perf$RMSE~perf$LEGEND,
        col="grey",boxwex=0.5,lty=1,
        pch=20,outwex=0.3,
        ylab="Optimal RMSE (kg/ha)")
abline(h=227.5,lwd=0.7,lty=1,col="red")
abline(h=seq(0,700,by=50),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of r
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_corr.tif",sep="")
tiff(tifName,res=300,pointsize=8.5,width=1000,height=800,units="px",compression="lzw")
par(mar=c(8,5,1,1),cex=0.7,las=2,lwd=0.6)
boxplot(perf$CORR~perf$LEGEND,
        col="grey",boxwex=0.5,lty=1,
        pch=20,outwex=0.3,
        ylab="Optimal R (Pearson)")
abline(h=0.8482,lwd=0.7,lty=1,col="red")
abline(h=seq(-1,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of slpf
tifName <- paste(bd,"/GJ-weather/shuf-pert_results/s-performance_slpf.tif",sep="")
tiff(tifName,res=300,pointsize=8.5,width=1000,height=800,units="px",compression="lzw")
par(mar=c(8,5,1,1),cex=0.7,las=2,lwd=0.6)
boxplot(perf$SLPF~perf$LEGEND,
        col="grey",boxwex=0.5,lty=1,
        pch=20,outwex=0.3,ylim=c(0,1),
        ylab="Optimal SLPF")
abline(h=0.8,lwd=0.7,lty=1,col="red")
abline(h=seq(0,1,by=0.2),lwd=0.6,lty=2,col="grey50")
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

####################################################################################
####################################################################################
#function to summarise experiments
summariseExperiments <- function(bDir) {
  experiments <- createExpMat(bDir)[11:19,] #change when perturbed runs are ready
  for (i in 1:nrow(experiments)) {
    ty <- experiments$TYPE[i]; va <- experiments$VAR[i]; sc <- experiments$SCALE[i] #get data from matrix
    cat("\n")
    cat("processing",paste(ty),"/",paste(va),"/",paste(sc),"\n")
    #list folders and loop to get what we need
    sp_folder <- paste(bDir,"/GJ-weather/shuf-pert/",ty,"_",va,"_",sc,sep="")
    if (!file.exists(paste(sp_folder,"/timeseries.csv",sep=""))) {
      f_list <- list.files(sp_folder); f_list <- f_list[grep("s-",f_list)]
      
      for (f_name in f_list) {
        cat("reading in experiment",f_name,"\n")
        PSDataFolder <- paste(sp_folder,"/",f_name,sep="") #data folder
        opt <- read.csv(paste(PSDataFolder,"/optimisation.csv",sep="")) #load optimisation curve
        tse <- read.csv(paste(PSDataFolder,"/timeseries.csv",sep="")) #load time series
        best_slpf <- opt$SLPF[which(opt$RMSE == min(opt$RMSE,na.rm=T))] #get best slpf
        if (length(best_slpf)>1) { #get highest best slpf in the case there is more than 1
          best_slpf <- max(best_slpf)
        }
        #get metrics for best slpf
        best_rmse <- opt$RMSE[which(opt$SLPF==best_slpf)]
        best_corr <- opt$CORR[which(opt$SLPF==best_slpf)]
        best_tser <- tse[which(tse$SLPF==best_slpf),]
        if (ty == "s") { #get experimental details
          p <- NA
          s <- as.numeric(gsub("s-","",f_name))
        } else {
          p <- as.numeric(gsub("p-","",strsplit(f_name,"_")[[1]][1]))
          s <- as.numeric(gsub("s-","",strsplit(f_name,"_")[[1]][2]))
        }
        #get the performance row
        per_row <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s,SLPF=best_slpf,RMSE=best_rmse,CORR=best_corr)
        #get the best time series
        best_tse <- data.frame(TYPE=ty,VAR=va,SCALE=sc,P=p,SEED=s,SLPF=best_slpf,
                               YEAR=best_tser$YEAR,HWAH=best_tser$HWAH,OBYL=best_tser$OBYL)
        if (f_name==f_list[1]) { #summarise
          performance <- per_row
          timeseries <- best_tse
        } else {
          performance <- rbind(performance,per_row)
          timeseries <- rbind(timeseries,best_tse)
        }
      }
      
      write.csv(performance,paste(sp_folder,"/performance.csv",sep=""),row.names=F,quote=F)
      write.csv(timeseries,paste(sp_folder,"/timeseries.csv",sep=""),row.names=F,quote=F)
      
      rm(performance);rm(timeseries);g=gc();rm(g)
    }
  }
}
