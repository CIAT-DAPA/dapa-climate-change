#Julian Ramirez-Villegas
#November 2011

setwd("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate")
library(raster)

for (i in 1:12) {
  rs <- raster(paste("ind_2_5min/prec_",i,".asc",sep=""))
  mn <- mean(rs[],na.rm=T)
  
  if (i==1) {rt <- mn} else {rt <- c(rt,mn)}
}

tiff("./../analyses/img/rainy-season.tiff",
     res=300,pointsize=10,width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=0.8)
plot(c(1:12),rt,type="b",xlab="Month",ylab="Rainfall (mm)")
grid()
abline(v=6,lty=2,col="red"); abline(v=10,lty=2,col="black")
dev.off()
