## Make a funtion to create the error bar

path="D:/CIAT/Articles/Maxent_Nicaragua/others/precipitacion.csv"
path2050="D:/CIAT/Articles/Maxent_Nicaragua/others/precipitacion2050.csv"
pathTem="D:/CIAT/Articles/Maxent_Nicaragua/others/tmedia.csv"
pathTem2050="D:/CIAT/Articles/Maxent_Nicaragua/others/tmedia2050.csv"

tiff(paste("D:/CIAT/Articles/Maxent_Nicaragua/others/fig-2-climate-trends.tif", sep=""),width=1024, height=768,pointsize=8,compression='lzw',res=150)
  
  par(mar=c(4,4,1,4)+ 0.1)

  mth_lb = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    arrows(x, y+upper, x, y-lower, angle=90, code=3, length=0.05, ...)
  }
  
  ## Load the climate information
  ## Current Precipitation  
  rr = read.table(path, header=T,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  r.means=apply(rr,2,mean)
  r.sd=apply(rr,2,sd)
  
  ## Future Precipitation
  rr2050 = read.table(path2050, header=T,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  rr.means2050=apply(rr2050,2,mean)
  rr.sd2050=apply(rr2050,2,sd)
  
  ##Current temperature 
  tem = read.table(pathTem, header=T,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  ytem.means=apply(tem,2,mean)

  ##Future temperature
  tem2050 = read.table(pathTem2050, header=T,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  ytem.means2050=apply(tem2050,2,mean)
  
  ##Precipitation bar Plot
  yy <- matrix(c(r.means,rr.means2050),2,12,byrow=TRUE)
  sdtotal <- matrix(c(r.sd,rr.sd2050),2,12,byrow=TRUE)*1.96/10 

  par(new=TRUE)
  
  barx2 <- barplot(yy, beside=T, axes=F, col=c("royalblue2","skyblue"), ylim=c(0,300), axis.lty=1, xlab="", ylab="", 
                   cex.axis=0.8, names.arg=mth_lb, tcl=-0.3, cex.names=0.8)
  error.bar(barx2,yy,sdtotal)
  axis(2, ylim=c(0,300), col="black", lwd=1, cex.axis=0.8)
  
  ##Plot current and future temperature  
  par(new=TRUE)  
  plot(ytem.means,axes=FALSE,ann=FALSE,type="n",ylim=c(1.11,28.9),ylab="", xlab="", cex.lab=0.8, cex.axis=0.8)
  
  
  axis(4, tcl=-0.3, cex.axis=0.8, ylim=c(1,29))
  
  mtext(2, text="Precipitation (mm/month)", line=2.5) 
  mtext(4,text="Temperature (°C)", line=2.5)
  mtext(1,text="Month",line=2.5)

  legend("topleft", c("Current precipitation","2050 Precipitation","Current temperature","2050 temperature"), 
         fill=c("royalblue2","skyblue", 0, 0), border = c("black","black", 0, 0), bty="n", lty=c(NA,NA,1,2), 
         col=c("red","red"), lwd=c(NA,NA,1.5,1.5), merge = T)

  points(ytem.means,type="l",col="red",lwd=1.5)
  
  par(new=TRUE)
  points(spline(ytem.means2050),lty=2, type="l",col="red",lwd=1.5)
  
  grid(NA, 6)
  box()

dev.off()