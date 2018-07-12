# Carlos Navarro 
# CIAT - CCAFS
# November 2012

require("raster")

# Set params
bDir <- "D:/cenavarro/lat-sal/outputs"
oDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/performance"
years <- c("1981_2010")
nfolds <- 25
ntiles <- 1
varList <- c("rain", "tmin", "tmax")

if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}

for (yr in years){

  # # Join all metrics files
  # for (vn in varList){    
  #   iDir <- paste0(bDir, "/", vn, "/", yr)
  #   metrics <- c()    
  #   for (f in 1:nfolds){      
  #     for (t in 1:length(ntiles)){
  #       metric_i <- read.csv(paste0(iDir, "/fold-", f, "/tile-", t, "/", vn, "_metrics.csv"))
  #     }
  #     metrics <- rbind(metrics, metric_i)
  #   }    
  #   write.csv(metrics, paste(oDir, "/", vn, "_metrics_", yr , ".csv", sep=""), row.names=F)
  # }

  setwd(oDir)
  
  id_rain <- read.csv(paste("rain_metrics_", yr , ".csv", sep=""))
  id_tmax <- read.csv(paste("tmax_metrics_", yr , ".csv", sep=""))
  id_tmin <- read.csv(paste("tmin_metrics_", yr , ".csv", sep=""))
  oPlot <- paste0(oDir, '/skill_', yr, '_v2.tif') 
  
  mth_lb = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # windowsFonts(A=windowsFont("Cambria"))
  
  # Combine everything in one single plot
  tiff(oPlot, width=2800, height=1800, pointsize=8, compression='lzw',res=300)
  par(mfrow=c(2,3), oma=c(0,0,1,0))
  
  # rsq
  
  rsq_prec_max <- max(id_rain$R2.TEST)
  # tiff('fig-3c-rsq_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(1,4,2,1))
  boxplot(id_rain$R2.TEST ~ id_rain$MONTH,ylab='R2',pch=20,cex=1.1,ylim=c(0,1), cex.axis=1.2, cex.lab=1.2, col="gray90", xaxt="n",xlab="")
  title("Precipitation", line = 1, cex.main=1.5)
  # text(x=11.9, y=0.98,"C", pos=4, cex=1.1.4)
  grid()
  axis(1:12, side=1, labels=FALSE)
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("topleft", c("A"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.2, inset=c(-0.08,-0.02))
  # dev.off()
  
  
  rsq_tmax_max <- max(id_tmax$R2.TEST)
  
  # tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(1,4,2,1))
  boxplot(id_tmax$R2.TEST ~ id_tmax$MONTH,ylab='R2',pch=20,cex=1.1,ylim=c(0,1), cex.axis=1.2, cex.lab=1.2, col="gray90", xaxt="n",xlab="")
  # text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
  title("Maximum Temperature", line = 1, cex.main=1.5)
  grid()
  axis(1:12, side=1, labels=FALSE)
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("topleft", c("B"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.5, inset=c(-0.08,-0.02))
  # dev.off()
  
  
  rsq_tmin_max <- max(id_tmin$R2.TEST)
  
  # tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(1,4,2,1))
  boxplot(id_tmin$R2.TEST ~ id_tmin$MONTH,ylab='R2',pch=20,cex=1.1,ylim=c(0,1), cex.axis=1.2, cex.lab=1.2, col="gray90", xaxt="n",xlab="")
  # text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
  title("Minimum Temperature", line = 1, cex.main=1.5)
  grid()
  axis(1:12, side=1, labels=FALSE)
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("topleft", c("C"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.5, inset=c(-0.08,-0.02))
  # dev.off()
  
  
  # rmse 
  # In precipitation is better to use ER
  
  rmse_prec_max <- max(id_rain$RMSE.TEST)
  
  # tiff('fig-3e-rmse_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_rain$RMSE.TEST ~ id_rain$MONTH,xlab='Month',xaxt = "n", ylab='RMSE (mm/month)',pch=20,cex=1.1,ylim=c(0,rmse_prec_max), naMonth=(mth_lb), cex.axis=1.2, cex.lab=1.2, col="gray90")
  # text(x=11.9, y=0.98,"E", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_prec_max *.5, rmse_prec_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_prec_max *.75,rmse_prec_max *.75),col='red')
  axis(1, at=1:12, labels=mth_lb)
  legend("topleft", c("D"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.5, inset=c(-0.08,-0.02))
  # dev.off()
  
  # 
  # re_prec_max <- max(id_rain$RE.TEST)
  # 
  # # tiff('fig-3e-re_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  # par(mar=c(4,4,2,1))
  # boxplot(id_rain$RE.TEST ~ id_rain$MONTH,xlab='Month',ylab='ER',pch=20,cex=1.1,ylim=c(0,re_prec_max), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # # text(x=11.9, y=0.98,"E", pos=4, cex=1.1.4)
  # grid()
  # lines(c(0,13),c(re_prec_max *.5, re_prec_max *.5),col='red',lty=2)
  # lines(c(0,13),c(re_prec_max *.75,re_prec_max *.75),col='red')
  # legend("topleft", c("D"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # # dev.off()
  
  
  rmse_tmin_max <- max(id_tmin$RMSE.TEST)/10
  rmse_tmax_max <- max(id_tmax$RMSE.TEST)/10
  
  # tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmax$RMSE.TEST/10 ~ id_tmax$MONTH,xlab='Month',xaxt = "n", ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,1), naMonth=(mth_lb), cex.axis=1.2, cex.lab=1.2, col="gray90")
  # text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_tmax_max *.5, rmse_tmax_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_tmax_max *.75,rmse_tmax_max *.75),col='red')
  axis(1, at=1:12, labels=mth_lb)
  legend("topleft", c("E"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.5, inset=c(-0.08,-0.02))
  # dev.off()
  
  
  # tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmin$RMSE.TEST/10 ~ id_tmin$MONTH,xlab='Month',xaxt = "n", ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,1), naMonth=(mth_lb), cex.axis=1.2, cex.lab=1.2, col="gray90")
  # text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_tmin_max *.5, rmse_tmin_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_tmin_max *.75,rmse_tmin_max *.75),col='red')
  axis(1, at=1:12, labels=mth_lb)
  legend("topleft", c("F"), box.lty=0, bg="transparent", cex = 2.5, yjust= 0.5, inset=c(-0.08,-0.02))
  # dev.off()
  
  dev.off()

}