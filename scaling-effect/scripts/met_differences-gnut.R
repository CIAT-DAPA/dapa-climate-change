#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
library(ggplot2); library(plyr)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_gnut",sep="")

#model run details
trial <- 3
crop_name <- "gnut"

#get 12km mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

p00 <- extent(msk)
p00@ymax <- 15

msk <- crop(msk,p00)

#get 3deg mask
msk2 <- raster(paste(lsmDir,"/3deg_mask.tif",sep=""))
msk2 <- crop(msk2,p00)

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string
ahar <- crop(ahar,p00)

#new points
s1 <- extent(-16.5,-13.5,12,15)
s2 <- extent(7.5,10.5,12,15)

## for each of the 3deg grid cells calculate the mean, std, range of P and T in the 12km one
## also calculate mean, std and range of suit in the 12km run
## add 3deg values to the table

resol <- "12km_exp"
cat("resolution:",resol,"\n")
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
srunDir <- paste(runDir,"/3deg/",resol,"-run_",trial,sep="")

#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))

#load suitability, rain and temp raster ---at low resolution
suit_sc <- raster(paste(srunDir,"/",crop_name,"_suitability.tif",sep="")); suit_sc <- crop(suit_sc,msk2)
prec_sc <- raster(paste(srunDir,"/",crop_name,"_gsrain.tif",sep="")); prec_sc <- crop(prec_sc,msk2)
tmen_sc <- raster(paste(srunDir,"/",crop_name,"_gstmean.tif",sep="")); tmen_sc <- crop(tmen_sc,msk2)

msk2 <- mask(msk2, suit_sc)
msk2 <- crop(msk2, ahar)

#loop grid cells
meteffect <- data.frame()
meteffect010 <- data.frame()
for (i in which(!is.na(msk2[]))) {
  #i <- which(!is.na(msk2[]))[1]
  cxy <- xyFromCell(msk2,i)
  ext <- extent(cxy[1]-1.5,cxy[1]+1.5,cxy[2]-1.5,cxy[2]+1.5)
  #plot(msk2); plot(ext,add=T,col="red")
  #points(cxy)
  
  #crop high-res
  suit_p <- crop(suit,ext); prec_p <- crop(prec,ext); tmen_p <- crop(tmen,ext) * 0.1
  
  #put data of All areas
  outdata <- data.frame(loc=i,x=cxy[1],y=cxy[2],pbar=mean(prec_p[],na.rm=T),psd=sd(prec_p[],na.rm=T),
                        pran=diff(range(prec_p[],na.rm=T)),tbar=mean(tmen_p[],na.rm=T),
                        tsd=sd(tmen_p[],na.rm=T),tran=diff(range(tmen_p[],na.rm=T)),
                        sbar=mean(suit_p[],na.rm=T),ssd=sd(suit_p[],na.rm=T),
                        sran=diff(range(suit_p[],na.rm=T)),suit=extract(suit_sc,cxy),
                        prec=extract(prec_sc,cxy),tmen=extract(tmen_sc,cxy))
  
  #put data of 010 areas
  ahar_p <- crop(ahar,ext)
  
  xy010 <- as.data.frame(xyFromCell(ahar_p,1:ncell(ahar_p)))
  xy010$ahar <- extract(ahar_p,xy010[,c("x","y")])
  xy010 <- xy010[which(!is.na(xy010$ahar)),]
  xy010 <- xy010[which(xy010$ahar >= 0.1),]
  if (nrow(xy010) == 0) {
    outdata010 <- data.frame(loc=i,x=cxy[1],y=cxy[2],pbar=NA,psd=NA,pran=NA,tbar=NA,
                             tsd=NA,tran=NA,sbar=NA,ssd=NA,sran=NA,suit=extract(suit_sc,cxy),
                             prec=extract(prec_sc,cxy),tmen=extract(tmen_sc,cxy))
  } else {
    xy010$suit <- extract(suit_p, xy010[,c("x","y")])
    xy010$prec <- extract(prec_p, xy010[,c("x","y")])
    xy010$tmen <- extract(tmen_p, xy010[,c("x","y")])
    
    outdata010 <- data.frame(loc=i,x=cxy[1],y=cxy[2],pbar=mean(xy010$prec,na.rm=T),psd=sd(xy010$prec,na.rm=T),
                             pran=diff(range(xy010$prec,na.rm=T)),tbar=mean(xy010$tmen,na.rm=T),
                             tsd=sd(xy010$tmen,na.rm=T),tran=diff(range(xy010$tmen,na.rm=T)),
                             sbar=mean(xy010$suit,na.rm=T),ssd=sd(xy010$suit,na.rm=T),
                             sran=diff(range(xy010$suit,na.rm=T)),suit=extract(suit_sc,cxy),
                             prec=extract(prec_sc,cxy),tmen=extract(tmen_sc,cxy))
  }
  meteffect <- rbind(meteffect,outdata)
  meteffect010 <- rbind(meteffect010,outdata010)
}

### all areas
#remove NAs
meteffect <- meteffect[which(!is.na(meteffect$suit)),]
meteffect$precdiff <- (meteffect$pbar - meteffect$prec) / meteffect$pbar * 100
meteffect$tmendiff <- (meteffect$tbar - meteffect$tmen * 0.1)
meteffect$suitdiff <- (meteffect$sbar - meteffect$suit) #/ meteffect$sbar * 100

pdf(paste(figDir,"/correl_differences_prec.pdf",sep=""),width=10,height=7, pointsize=15)
par(las=1, mar=c(4.5,4.5,1,1))
plot(meteffect$suitdiff, meteffect$precdiff, pch=20, ylim=c(-80,50), xlim=c(-30,80),
     xlab="Difference in suitability (%)", ylab="Difference in precipitation (%)")

for (i in 1:2) {
  #i <- 1
  text <- get(paste("s",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  a <- meteffect$suitdiff[which(meteffect$x==xy[1] & meteffect$y==xy[2])]
  b <- meteffect$precdiff[which(meteffect$x==xy[1] & meteffect$y==xy[2])]
  points(a,b,pch=20,col="red",cex=1.05)
  text(a,b,labels=paste("R",i,sep=""),adj=1.25)
}
grid()
dev.off()


pdf(paste(figDir,"/correl_differences_temp.pdf",sep=""),width=10,height=7, pointsize=15)
par(las=1, mar=c(4.5,4.5,1,1))
plot(meteffect$suitdiff, meteffect$tmendiff, pch=20, xlim=c(-30,80), ylim=c(-1,2),
     xlab="Difference in suitability (%)", ylab="Difference in temperature (K)")
for (i in 1:2) {
  #i <- 1
  text <- get(paste("s",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  a <- meteffect$suitdiff[which(meteffect$x==xy[1] & meteffect$y==xy[2])]
  b <- meteffect$tmendiff[which(meteffect$x==xy[1] & meteffect$y==xy[2])]
  points(a,b,pch=20,col="red",cex=1.05)
  text(a,b,labels=paste("R",i,sep=""),adj=-.25)
}
grid()
dev.off()

#cor.test(meteffect$precdiff,meteffect$suitdiff)
#cor.test(meteffect$tmendiff,meteffect$suitdiff)


### 010 areas
#remove NAs
meteffect010 <- meteffect010[which(!is.na(meteffect010$sbar)),]
meteffect010$precdiff <- (meteffect010$pbar - meteffect010$prec) / meteffect010$pbar * 100
meteffect010$tmendiff <- (meteffect010$tbar - meteffect010$tmen * 0.1)
meteffect010$suitdiff <- (meteffect010$sbar - meteffect010$suit) #/ meteffect$sbar * 100

pdf(paste(figDir,"/correl_differences_prec_010.pdf",sep=""),width=10,height=7, pointsize=15)
par(las=1, mar=c(4.5,4.5,1,1))
plot(meteffect010$suitdiff, meteffect010$precdiff, pch=20, ylim=c(-100,20), xlim=c(-60,40),
     xlab="Difference in suitability (%)", ylab="Difference in precipitation (%)")

for (i in 1:5) {
  #i <- 1
  text <- get(paste("s",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  a <- meteffect010$suitdiff[which(meteffect010$x==xy[1] & meteffect010$y==xy[2])]
  b <- meteffect010$precdiff[which(meteffect010$x==xy[1] & meteffect010$y==xy[2])]
  points(a,b,pch=20,col="red",cex=1.05)
  text(a,b,labels=paste("R",i,sep=""),adj=1.25)
}
grid()
dev.off()


pdf(paste(figDir,"/correl_differences_temp_010.pdf",sep=""),width=10,height=7, pointsize=15)
par(las=1, mar=c(4.5,4.5,1,1))
plot(meteffect010$suitdiff, meteffect010$tmendiff, pch=20, xlim=c(-60,40), ylim=c(-3,1.5),
     xlab="Difference in suitability (%)", ylab="Difference in temperature (K)")
for (i in 1:5) {
  #i <- 1
  text <- get(paste("s",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  a <- meteffect010$suitdiff[which(meteffect010$x==xy[1] & meteffect010$y==xy[2])]
  b <- meteffect010$tmendiff[which(meteffect010$x==xy[1] & meteffect010$y==xy[2])]
  points(a,b,pch=20,col="red",cex=1.05)
  text(a,b,labels=paste("R",i,sep=""),adj=-.25)
}
grid()
dev.off()


#cor.test(meteffect010$precdiff,meteffect010$suitdiff)
#cor.test(meteffect010$tmendiff,meteffect010$suitdiff)



