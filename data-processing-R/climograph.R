# Climograph
# Carlos Navarro 
# CIAT - CCAFS
# Feb 2020

###############################################
#### 01 Plots annual cycle current, future ####
###############################################
install.packages("matrixStats")
library(matrixStats)

baseDir <- "C:/Users/cenavarro/Dropbox/Training Materials/Week_2/R_examples"
oDir <- baseDir

mthData_prec <- read.csv(paste0(baseDir, "/th_prec.csv"), header=T)
mthData_tavg <- read.csv(paste0(baseDir, "/th_tavg.csv"), header=T)
mthData_tmax <- read.csv(paste0(baseDir, "/th_tmax.csv"), header=T)
mthData_tmin <- read.csv(paste0(baseDir, "/th_tmin.csv"), header=T)
mth_lb = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x, y+upper, x, y-lower, angle=90, code=3, length=0.05, ...)
}

## Load the climate information
## Current Precipitation  
rr = as.vector(colMeans(mthData_prec, na.rm = T))
rr.sd = as.vector(colSds(as.matrix(mthData_prec), na.rm = T))

## Load the climate information
## Tavg   
tavg = as.vector(colMeans(mthData_tavg[2:13], na.rm = T))
tmin = as.vector(colMeans(mthData_tmin[2:13], na.rm = T))
tmax = as.vector(colMeans(mthData_tmax[2:13], na.rm = T))

##Precipitation bar Plot
tiff(paste(oDir, "/plot_trends.tif", sep=""), width=1024, height=600,pointsize=8,compression='lzw',res=150)

par(mar=c(7,4,1,4)+ 0.1)

barx2 <- barplot(rr[2:13], beside=T, axes=F, col=c("lightblue"), ylim=c(0,max(rr[2:13])+40), axis.lty=1, xlab="", ylab="", 
                   cex.axis=0.8, names.arg=mth_lb, tcl=-0.3, cex.names=0.8)
error.bar(barx2,rr[2:13],rr.sd)
axis(2, ylim=c(0,max(yy)+100), col="black", lwd=1, cex.axis=0.8)


##Plot temperature  
par(new=TRUE)  
plot(tmax,axes=FALSE,ann=FALSE,type="n",ylim=c(0,round(max(tmax) + 0.5)),ylab="", xlab="", cex.lab=0.8, cex.axis=0.8)

axis(4, tcl=-0.3, cex.axis=0.8, ylim=c(0,round(max(tavg) + 0.5)))

mtext(2, text="Precipitation (mm/month)", line=2.5) 
mtext(4,text="Temperature (°C)", line=2.5)
mtext(1,text="Month",line=2.5)

par(new=TRUE)
points(spline(tmin), type="l",col="yellow",lwd=1.5)
points(spline(tavg), type="l",col="orange",lwd=1.5)
points(spline(tmax), type="l",col="red",lwd=1.5)

par(xpd=TRUE)
legend_order <- matrix(1:4,ncol=4)
legend("bottom",
       inset=c(0, -0.28),
       c("Precipitation", "Minimum Temperature",
         "Average Temperature", "Maximum temperature")[legend_order],
       fill=c("lightblue", 0, 0, 0),
       border = c("black",0,0, 0),
       bty="n",
       lty=c(NA,1,1,1),
       col=c("yellow", "orange", "red"),
       lwd=c(NA,1.5,1.5,1.5),
       merge = T,
       ncol=4,
       cex = 0.9
)

dev.off()

