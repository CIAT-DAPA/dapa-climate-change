#Julian Ramirez-Villegas
#November 2011

library(raster); library(maptools); data(wrld_simpl)
setwd("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT")

msk <- raster("./analysis-mask/countries/ind_0.asc")
mon <- raster("./cropdist/monfreda.asc"); mon <- crop(mon,msk); mon[which(mon[]==0)] <- NA
mir <- raster("./cropdist/mirca2000-rfd.asc"); mir <- crop(mir,msk); mir[which(mir[]==0)] <- NA
spm <- raster("./cropdist/spam.asc"); spm <- crop(spm,msk); spm[which(spm[]==0)] <- NA

suit <- raster("./analyses/runs/6-gnut-tmean_suitability.asc"); suit[which(suit[]==0)] <- NA

z <- extent(suit); aspect <- (z@ymax-z@ymin)*1.15/(z@xmax-z@xmin)

#SPAM
quant <- quantile(spm[],probs=c(0.05,0.1,0.25,0.5,0.75,0.80,0.90,0.95,0.99),na.rm=T)
brks <- c(min(spm[],na.rm=T),quant,max(spm[],na.rm=T))
brks.lab <- round(brks,1)
nb <- length(brks.lab)-1

tiff("analyses/img/gnut-SPAM.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(spm,useRaster=F,
     col=colorRampPalette(c("grey90","light blue","blue"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()

#MIRCA
quant <- quantile(mir[],probs=c(0.05,0.1,0.25,0.5,0.75,0.80,0.90,0.95,0.99),na.rm=T)
brks <- c(min(mir[],na.rm=T),quant,max(mir[],na.rm=T))
brks.lab <- round(brks,1)
nb <- length(brks.lab)-1

tiff("analyses/img/gnut-MIRCA.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(mir,useRaster=F,
     col=colorRampPalette(c("grey90","light blue","blue"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()


#MONFREDA
quant <- quantile(mon[],probs=c(0.05,0.1,0.25,0.5,0.75,0.80,0.90,0.95,0.99),na.rm=T)
brks <- c(min(mon[],na.rm=T),quant,max(mon[],na.rm=T))
brks.lab <- round(brks,1)
nb <- length(brks.lab)-1

tiff("analyses/img/gnut-MONFREDA.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(mon,useRaster=F,
     col=colorRampPalette(c("grey90","light blue","blue"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()


#SUITABILITY
brks <- seq(0,100,by=10)
brks.lab <- brks
nb <- length(brks.lab)-1

tiff("analyses/img/gnut-EcoCrop.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(suit,useRaster=F,
     col=colorRampPalette(c("red","orange","yellow","dark green"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()
