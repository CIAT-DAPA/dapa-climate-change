#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the CIMMYT ME data and rasterise it. Convert to Iizumi's grid of 1.125 x 1.125

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); data(wrld_simpl)

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
if (!file.exists(paste(meDir,"/maizeMESglobal.tif",sep=""))) {
  rs <- raster(ncol=7200,nrow=3600)
  rs[] <- 1:ncell(rs)
  rs <- rasterize(shp,rs)
  rs <- shift(rs, x=-0.025, y=0.025)
  rs <- writeRaster(rs, paste(meDir,"/maizeMESglobal.tif",sep=""),format="GTiff", overwrite=T)
} else {
  rs <- raster(paste(meDir,"/maizeMESglobal.tif",sep=""))
}

#aggregate ME raster to 1.125 dataset
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
rs_af <- crop(rs, yrs)

#create data.frame with coordinates of big grid cell (all)
xy <- as.data.frame(xyFromCell(yrs,1:ncell(yrs)))
xy$ID <- 1:nrow(xy)

if (!file.exists(paste(meDir,"/maizeMESglobal_lowres_me.tif",sep=""))) {
  #function to aggregate based on frequency
  agg_me <- function(x,target_res,in_rs) {
    #x <- as.numeric(xy[900,])
    lon <- x[1]; lat <- x[2]; id <- x[3]
    cat("...loc ",id," out of ",3618," (",round((id/3618*100),2)," %)\n",sep="")
    
    rs_cut <- crop(in_rs, extent((lon-target_res*.5),(lon+target_res*.5),(lat-target_res*.5),(lat+target_res*.5)))
    rs_cut <- rs_cut[]
    if (length(which(is.na(rs_cut))) == length(rs_cut)) {
      ret_val <- c(NA,1)
    } else {
      rs_cut <- as.data.frame(table(rs_cut))
      rs_cut$RFreq <- rs_cut$Freq / (sum(rs_cut$Freq))
      rs_cut <- rs_cut[which(rs_cut$Freq == max(rs_cut$Freq)),]
      if (nrow(rs_cut) > 1) {rs_cut <- rs_cut[1,]}
      ret_val <- c(as.numeric(paste(rs_cut$rs_cut)),rs_cut$RFreq)
    }
    return(ret_val)
  }
  
  #apply function to high-resolution dataset
  rs_af_ag <- apply(xy,1,agg_me,xres(yrs),rs_af)
  rs_af_me <- yrs; rs_af_me[] <- NA; rs_af_me[] <- rs_af_ag[1,]
  rs_af_fr <- yrs; rs_af_fr[] <- NA; rs_af_fr[] <- rs_af_ag[2,]
  rs_af_fr[which(is.na(rs_af_me[]))] <- NA
  
  rs_af_fr <- writeRaster(rs_af_fr, paste(meDir,"/maizeMESglobal_lowres_fraction.tif",sep=""),format="GTiff")
  rs_af_me <- writeRaster(rs_af_me, paste(meDir,"/maizeMESglobal_lowres_me.tif",sep=""),format="GTiff")
} else {
  rs_af_fr <- raster(paste(meDir,"/maizeMESglobal_lowres_fraction.tif",sep=""))
  rs_af_me <- raster(paste(meDir,"/maizeMESglobal_lowres_me.tif",sep=""))
}

#plot mega environments at low resolution
rsx1 <- rs_af_me; rsx1[which(is.na(yrs[]))] <- NA
rsx2 <- rs_af_fr; rsx2[which(is.na(yrs[]))] <- NA
#rsx1[which(rsx1[] == 24)] <- NA

xy$fraction <- extract(rsx2, xy[,c("x","y")])

#without "X" indicating fraction above a given threshols
pdf(file=paste(meDir,"/maizeMESglobal_lowres_me.pdf",sep=""),height=8,width=8,pointsize=12,family="Helvetica")
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)
plot(wrld_simpl,add=T)
grid()
dev.off()

#with "X" indicating fraction above a given threshols
pdf(file=paste(meDir,"/maizeMESglobal_lowres_me_frac055.pdf",sep=""),height=8,width=8,pointsize=12,family="Helvetica")
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)
plot(wrld_simpl,add=T)
points(xy[which(xy$fraction <= 0.55),c("x","y")],pch=4,cex=0.75)
grid()
dev.off()

#with "X" indicating fraction above a given threshols
pdf(file=paste(meDir,"/maizeMESglobal_lowres_me_frac075.pdf",sep=""),height=8,width=8,pointsize=12,family="Helvetica")
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)
plot(wrld_simpl,add=T)
points(xy[which(xy$fraction <= 0.75),c("x","y")],pch=4,cex=0.75)
grid()
dev.off()


### deleting grid cells
x11()
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)

#first 20
crm <- c(250,317,383,449,516,582,648,581,515,514,448,513,512,577,578,644,643,709,710)
rsx1[crm] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)

#second 20
crm2 <- c(734,874,811,588,579,579,580,645,645,646,647,712,712,713,778,778,779,776,777,711)
rsx1[crm2] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red","light blue"),horizontal=T)

#third 20
crm3 <- c(3077,2947,2948,2274,2207,2141,1107,1173,1239,1238,2751,2884,1106,1683,1548,1680,1746,2541,2476,2075)
rsx1[crm3] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

#fourth 20
crm4 <- c(2142,2143,2208,2209,2408,2409,2294,1769,1768,1243,1308,1373,1307,1241,1240,1306,1372,1438,1504,1504)
rsx1[crm4] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

#fifth 20
crm5 <- c(1439,1569,1570,1636,1976,2041,2042,2106,2107,2108,2170,2171,2172,2173,2174,2236,2237,2238,2239,2302)
rsx1[crm5] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

#sixth 20
crm6 <- c(2303,2304,2305,2371,2370,2369,2368,2434,2435,2436,2502,2501,2500,2499,2565,2566,2567,2568,2633,2632)
rsx1[crm6] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

#seventh 20
crm7 <- c(2630,2698,2699)
rsx1[crm7] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

#last grid cells
crm8 <- c(2631,2015,2082,739,740,674,806,469,535,620)
rsx1[crm8] <- NA
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)

pdf(file=paste(meDir,"/maizeMESglobal_lowres_me_final.pdf",sep=""),height=8,width=8,pointsize=12,family="Helvetica")
plot(rsx1, col=c("dark green","green","yellow","orange","red"),horizontal=T)
plot(wrld_simpl,add=T)
grid()
dev.off()

rsx1 <- writeRaster(rsx1,paste(meDir,"/maizeMESglobal_lowres_me_final.tif",sep=""),format="GTiff")

#temporary for deleting grid cells
# xy_rm <- locator(20)
# xy_rm1 <- data.frame(x=xy_rm$x,y=xy_rm$y)
# xy_rm1$cell <- cellFromXY(rsx1,xy_rm1)
# xy_rm1$cell

#aggregate ME raster to 1.125 dataset (using nearest neighbour interpolation)
#yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
#rs_af <- crop(rs, yrs)
#rs_af <- resample(rs, yrs, method="ngb")
#rs_af <- writeRaster(rs_af, paste(meDir,"/maizeMESglobal_lowres_nn.tif",sep=""),format="GTiff")

#for each mega-environment extract mean and sigma yield (from 1982 to 2005)
dodgypoints <- cbind(x=c(28.125,21.375,20.250),y=c(-23.0625,-26.4375,-25.3125))

#load stack of crop yields
yi_stk <- stack(paste(yiDir,"/rs_",1982:2005,"/gdhy_2013JAN29_maize_ModelYld500_",1982:2005,".tif",sep=""))
yi_stk[cellFromXY(yi_stk,dodgypoints)] <- NA
yi_mean <- calc(yi_stk, fun=function(x) {mean(x,na.rm=T)})
yi_sd <- calc(yi_stk, fun=function(x) {sd(x,na.rm=T)})
yi_cv <- yi_sd / yi_mean * 100

rs_af <- rs_af_me

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




