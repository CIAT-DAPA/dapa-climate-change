# Carlos Navarro 
# CIAT - CCAFS
# 15-11-2012

# Boxplots for article
require("gplots")

bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/outputs"


### Join metrics 

varList <- c("rain", "tmin", "tmax")

for (vn in varList){
  metrics <- c()
  for (f in 1:25){
    met_t <- read.csv(paste(bDir, "/", vn, "/fold-", f, "/tile-1/", vn, "_metrics.csv", sep=""))
    metrics <- rbind(metrics, met_t)    
  }
  write.csv(metrics, paste(bDir, "/",vn, "_metrics.csv", sep=""), row.names=F)
}


id_rain <- read.csv(paste(bDir, "/rain_metrics.csv", sep=""))
id_tmax <- read.csv(paste(bDir, "/tmax_metrics.csv", sep=""))
id_tmin <- read.csv(paste(bDir, "/tmin_metrics.csv", sep=""))


setwd(bDir)

mth_lb = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
# windowsFonts(A=windowsFont("Cambria"))

# Combine everything in one single plot
tiff('rmse-rsq-slo-prec-tmean.tif', width=1200, height=800, pointsize=8, compression='lzw',res=150)
par(mfrow=c(2,3))

# rsq

rsq_prec_max <- max(id_rain$R2.TEST)
# tiff('fig-3c-rsq_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_rain$R2.TEST ~ id_rain$MONTH,xlab='Month',ylab='RSQ',pch=20,cex=1.1,ylim=c(0,1), names=(mth_lb), cex.axis=0.9, col="gray90",main="PREC")
# text(x=11.9, y=0.98,"C", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
lines(c(0,13),c(1 *.75,1 *.75),col='red')
legend("bottomleft", c("A"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


rsq_tmax_max <- max(id_tmax$R2.TEST)

# tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_tmax$R2.TEST ~ id_tmax$MONTH,xlab='Month',ylab='RSQ',pch=20,cex=1.1,ylim=c(0,1), names=(mth_lb), cex.axis=0.9, col="gray90",main="TMAX")
# text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
lines(c(0,13),c(1 *.75,1 *.75),col='red')
legend("bottomleft", c("B"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


rsq_tmin_max <- max(id_tmin$R2.TEST)

# tiff('fig-3d-rsq_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_tmin$R2.TEST ~ id_tmin$MONTH,xlab='Month',ylab='RSQ',pch=20,cex=1.1,ylim=c(0,1), names=(mth_lb), cex.axis=0.9, col="gray90",main="TMIN")
# text(x=11.9, y=0.98,"D", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(1 *.5, 1 *.5),col='red',lty=2)
lines(c(0,13),c(1 *.75,1 *.75),col='red')
legend("bottomleft", c("C"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


# rmse 

rmse_prec_max <- max(id_rain$RMSE.TEST)

# tiff('fig-3e-rmse_prec_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_rain$RMSE.TEST ~ id_rain$MONTH,xlab='Month',ylab='RMSE (mm/month)',pch=20,cex=1.1,ylim=c(0,rmse_prec_max), names=(mth_lb), cex.axis=0.9, col="gray90")
# text(x=11.9, y=0.98,"E", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(rmse_prec_max *.5, rmse_prec_max *.5),col='red',lty=2)
lines(c(0,13),c(rmse_prec_max *.75,rmse_prec_max *.75),col='red')
legend("bottomleft", c("D"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


rmse_tmax_max <- max(id_tmax$RMSE.TEST)

# tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_tmax$RMSE.TEST ~ id_tmax$MONTH,xlab='Month',ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,rmse_tmax_max), names=(mth_lb), cex.axis=0.9, col="gray90")
# text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(rmse_tmax_max *.5, rmse_tmax_max *.5),col='red',lty=2)
lines(c(0,13),c(rmse_tmax_max *.75,rmse_tmax_max *.75),col='red')
legend("bottomleft", c("E"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


rmse_tmin_max <- max(id_tmin$RMSE.TEST)

# tiff('fig-3f-rmse_tmean_monthly.tif',width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,1,1))
boxplot(id_tmin$RMSE.TEST ~ id_tmin$MONTH,xlab='Month',ylab='RMSE (ºC)',pch=20,cex=1.1,ylim=c(0,rmse_tmax_max), names=(mth_lb), cex.axis=0.9, col="gray90")
# text(x=11.9, y=0.98,"F", pos=4, cex=1.1.4)
grid()
lines(c(0,13),c(rmse_tmin_max *.5, rmse_tmin_max *.5),col='red',lty=2)
lines(c(0,13),c(rmse_tmin_max *.75,rmse_tmin_max *.75),col='red')
legend("bottomleft", c("F"), box.lty=0, bg="transparent", cex = 1.5, yjust= 0.5, inset=c(-0.03,0))
# dev.off()


dev.off()
