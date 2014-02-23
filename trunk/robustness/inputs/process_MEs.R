#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the CIMMYT ME data and rasterise it. Convert to Iizumi's grid of 1.125 x 1.125

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
meDir <- paste(wd,"/data/maize_MEs",sep="")
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#read ME shp
shp <- readShapePoly(paste(meDir,"/maizeMESglobal.shp",sep=""))
medata <- shp@data
medata <- cbind(ME=1:nrow(medata),medata)
medata$NAME[which(medata$NAME == "Wet lowland")] <- as.factor("Wet Lowland")

#convert ME shp to raster and save
rs <- raster(ncol=7200,nrow=3600)
rs[] <- 1:ncell(rs)
rs <- rasterize(shp,rs)
rs <- shift(rs, x=-0.025, y=0.025)
rs <- writeRaster(rs, paste(meDir,"/maizeMESglobal.tif",sep=""),format="GTiff", overwrite=T)

#aggregate ME raster to 1.125 dataset
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
rs_af <- crop(rs, yrs)
rs_af <- resample(rs, yrs, method="ngb")
rs_af <- writeRaster(rs_af, paste(meDir,"/maizeMESglobal_lowres.tif",sep=""),format="GTiff")

#for each mega-environment extract mean and sigma yield (from 1982 to 2005)
dodgypoints <- cbind(x=c(28.6875,20.8125,21.9375),y=c(-23.0625,-25.3125,-26.4375))

#load stack of crop yields
yi_stk <- stack(paste(yiDir,"/rs_",1982:2005,"/gdhy_2013JAN29_maize_ModelYld500_",1982:2005,".tif",sep=""))
yi_stk[cellFromXY(yi_stk,dodgypoints)] <- NA
yi_mean <- calc(yi_stk, fun=function(x) {mean(x,na.rm=T)})
yi_sd <- calc(yi_stk, fun=function(x) {sd(x,na.rm=T)})
yi_cv <- yi_sd / yi_mean * 100

#loop mega environments to extract mean, sd and c.v.
mes <- unique(rs_af[which(!is.na(rs_af[]))])
allme <- data.frame()
for (me in mes) {
  #me <- mes[1]
  xyme <- as.data.frame(xyFromCell(rs_af, which(rs_af[]==me)))
  xyme$mean <- extract(yi_mean, xyme[,c("x","y")])
  xyme$sd <- extract(yi_sd, xyme[,c("x","y")])
  xyme$cv <- extract(yi_cv, xyme[,c("x","y")])
  xyme <- cbind(ME=me,xyme)
  allme <- rbind(allme, xyme)
}

#figure dir
figDir <- paste(meDir,"/figures",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#produce boxplot with ME as factor
allme <- merge(allme, medata, by="ME")
allme$NAME <- paste(allme$NAME)

#determine limits
yilim <- range(allme$mean, na.rm=T); yilim[1] <- floor(yilim[1]); yilim[2] <- ceiling(yilim[2])
sdlim <- range(allme$sd, na.rm=T); sdlim[1] <- floor(sdlim[1]); sdlim[2] <- ceiling(sdlim[2])
cvlim <- range(allme$cv, na.rm=T); cvlim[1] <- floor(cvlim[1]); cvlim[2] <- ceiling(cvlim[2])

#boxplots
pdf(paste(figDir,"/boxplot_maize_MES_meanyield.pdf",sep=""), height=8,width=10,pointsize=14)
par(mar=c(5,10.5,1,1),las=1)
boxplot(allme$mean ~ allme$NAME, horizontal=T, col="grey 80", pch=20, xlab="Yield (ton/ha)",ylim=yilim)
grid()
dev.off()

pdf(paste(figDir,"/boxplot_maize_MES_sdyield.pdf",sep=""), height=8,width=10,pointsize=14)
par(mar=c(5,10.5,1,1),las=1)
boxplot(allme$sd ~ allme$NAME, horizontal=T, col="grey 80", pch=20, xlab="Yield s.d. (ton/ha)",ylim=sdlim)
grid()
dev.off()

pdf(paste(figDir,"/boxplot_maize_MES_cvyield.pdf",sep=""), height=8,width=10,pointsize=14)
par(mar=c(5,10.5,1,1),las=1)
boxplot(allme$cv ~ allme$NAME, horizontal=T, col="grey 80", pch=20, 
        xlab="Coefficient of variation (%)", ylim=cvlim)
grid()
dev.off()


###
### scatter plot for each ME
for (me in mes) {
  #me <- mes[1]
  mename <- paste(medata$NAME[which(medata$ME == me)])
  xyme <- as.data.frame(xyFromCell(rs_af, which(rs_af[]==me)))
  xyme$mean <- extract(yi_mean, xyme[,c("x","y")])
  xyme$sd <- extract(yi_sd, xyme[,c("x","y")])
  xyme$cv <- extract(yi_cv, xyme[,c("x","y")])
  
  pdf(paste(figDir,"/scatter_maize_MES_mean_sd_ME-",me,".pdf",sep=""), height=8,width=10,pointsize=14)
  par(mar=c(5,5,2,1),las=1)
  plot(xyme$mean,xyme$sd,pch=20,xlim=yilim,ylim=sdlim,xlab="Mean yield (ton/ha)",
       ylab="Yield s.d. (ton/ha)", main=mename)
  grid()
  dev.off()
  
  pdf(paste(figDir,"/scatter_maize_MES_mean_cv_ME-",me,".pdf",sep=""), height=8,width=10,pointsize=14)
  par(mar=c(5,5,2,1),las=1)
  plot(xyme$mean,xyme$cv,pch=20,xlim=yilim,ylim=cvlim,xlab="Mean yield (ton/ha)",
       ylab="Coefficient of variation (%)", main=mename)
  grid()
  dev.off()
}


#all in one
pdf(paste(figDir,"/scatter_maize_MES_mean_cv_MEs.pdf",sep=""), height=8,width=10,pointsize=14)
par(mar=c(5,5,1,1),las=1)
allcols <- c("black","red","blue","orange","dark green","yellow")
allname <- c()
for (me in mes) {
  #me <- mes[1]
  xyme <- as.data.frame(xyFromCell(rs_af, which(rs_af[]==me)))
  xyme$mean <- extract(yi_mean, xyme[,c("x","y")])
  xyme$sd <- extract(yi_sd, xyme[,c("x","y")])
  xyme$cv <- extract(yi_cv, xyme[,c("x","y")])
  allname <- c(allname, paste(medata$NAME[which(medata$ME == me)]))
  
  if (me == mes[1]) {
    plot(xyme$mean,xyme$cv,pch=20,xlim=yilim,ylim=cvlim,xlab="Mean yield (ton/ha)",
         ylab="Coefficient of variation (%)",col=allcols[which(mes == me)], cex=1.25)
  } else {
    points(xyme$mean,xyme$cv,pch=20,col=allcols[which(mes == me)], cex=1.25)
  }
}
grid()
legend(3.5,70,legend=allname, col=allcols, pch=20)
dev.off()




