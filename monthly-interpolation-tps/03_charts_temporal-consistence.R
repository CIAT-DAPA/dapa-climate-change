# Carlos Navarro 
# CIAT - CCAFS
# 15-11-2012

# Boxplots for article
require("raster")
# source("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/00-scripts/02_mth_interpolations/accuracy_mod.R")
periods <- c("1976-1985", "1980-2010", "1986-1995", "1996-2005", "2006-2015")

iDir <- "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/outputs_complemented"
pDir <- "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/performance"

for (period in periods){
  
  bDir <- paste0(iDir, "/", period)
  oDir <- paste0(pDir, "/", period)
  
  varList <- c("rain", "tmin", "tmax")
  ntiles <- 1
  
  for (vn in varList){
    
    metrics <- c()
    
    for (f in 1:25){
      
      for (t in 1:length(ntiles)){
        
        #Crossvalide data metrics
        cat("Accuracy metrics \n")
        
        # # st <- read.csv(paste0(bDir, "/", vn, "/fold-", f, "/", vn, "_ame.csv"))
        # st.sel.10y <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax & st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
        # train.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TRAIN"),]
        # test.data <- st.sel.10y[which(st.sel.10y$TRAIN_TEST == "TEST"),]
        # 
        # setwd(paste0(bDir, "/", vn, "/fold-", f, "/tile-", t))
        # acc <- accuracy(trainMx=train.data, testMx=test.data, variable=vn)
        # write.csv(acc$METRICS, paste(vn, "_metrics_mod.csv", sep=""), quote=F, row.naMonth=F)
        
        metric_i <- read.csv(paste0(bDir, "/", vn, "/fold-", f, "/tile-", t, "/", vn, "_metrics.csv"))
        
      }
      
      metrics <- rbind(metrics, metric_i)
    }
    
    write.csv(metrics, paste(bDir, "/", vn, "_metrics.csv", sep=""), row.naMonth=F)
  }
  
}




for (period in periods){
  
  bDir <- paste0(iDir, "/", period)
  oDir <- paste0(pDir)
  
  id_rain <- read.csv(paste(bDir, "/rain_metrics.csv", sep=""))
  id_tmax <- read.csv(paste(bDir, "/tmax_metrics.csv", sep=""))
  id_tmin <- read.csv(paste(bDir, "/tmin_metrics.csv", sep=""))
  
  setwd(bDir)
  
  mth_lb = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # windowsFonts(A=windowsFont("Cambria"))
  
  # Combine everything in one single plot
  tiff(paste0(oDir, '/skill_', period, '.tif'), width=1300, height=900, pointsize=8, compression='lzw',res=150)
  par(mfrow=c(2,3), oma=c(0,0,1,0))
  
  # rsq
  
  rsq_prec_max <- max(id_rain$R2.TEST)
  # tiff('fig-3c-rsq_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_rain$R2.TEST ~ id_rain$MONTH,xlab='Month',ylab='R2',pch=20,cex=1.1,ylim=c(0,1), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  title("PREC", line = 1)
  # text(x=11.9, y=0.98,"C", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("bottomleft", c("A"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # dev.off()
  
  
  rsq_tmax_max <- max(id_tmax$R2.TEST)
  
  # tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmax$R2.TEST ~ id_tmax$MONTH,xlab='Month',ylab='R2',pch=20,cex=1.1,ylim=c(0,1), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
  title("TMAX", line = 1)
  grid()
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("bottomleft", c("B"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # dev.off()
  
  
  rsq_tmin_max <- max(id_tmin$R2.TEST)
  
  # tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmin$R2.TEST ~ id_tmin$MONTH,xlab='Month',ylab='R2',pch=20,cex=1.1,ylim=c(0,1), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
  title("TMIN", line = 1)
  grid()
  lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
  lines(c(0,13),c(1 *.75,1 *.75),col='red')
  legend("bottomleft", c("C"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # dev.off()
  
  
  # rmse 
  # In precipitation is better to use ER
  
  rmse_prec_max <- max(id_rain$RMSE.TEST)
  
  # tiff('fig-3e-rmse_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_rain$RMSE.TEST ~ id_rain$MONTH,xlab='Month',ylab='RMSE (mm/month)',pch=20,cex=1.1,ylim=c(0,rmse_prec_max), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # text(x=11.9, y=0.98,"E", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_prec_max *.5, rmse_prec_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_prec_max *.75,rmse_prec_max *.75),col='red')
  legend("bottomleft", c("D"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
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
  # legend("bottomleft", c("D"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # # dev.off()
  
  
  rmse_tmin_max <- max(id_tmin$RMSE.TEST)/10
  rmse_tmax_max <- max(id_tmax$RMSE.TEST)/10
  
  # tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmax$RMSE.TEST/10 ~ id_tmax$MONTH,xlab='Month',ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,1.5), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_tmax_max *.5, rmse_tmax_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_tmax_max *.75,rmse_tmax_max *.75),col='red')
  legend("bottomleft", c("E"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # dev.off()
  
  
  
  
  # tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
  par(mar=c(4,4,2,1))
  boxplot(id_tmin$RMSE.TEST/10 ~ id_tmin$MONTH,xlab='Month',ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,1.5), naMonth=(mth_lb), cex.axis=0.9, col="gray90")
  # text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
  grid()
  lines(c(0,13),c(rmse_tmin_max *.5, rmse_tmin_max *.5),col='red',lty=2)
  lines(c(0,13),c(rmse_tmin_max *.75,rmse_tmin_max *.75),col='red')
  legend("bottomleft", c("F"), box.lty=0, bg="transparent", cex = 2, yjust= 0.5, inset=c(-0.03,0))
  # dev.off()
  
  
  dev.off()
}