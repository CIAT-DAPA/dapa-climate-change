#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_new",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#model run details
trial <- 6
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#locations that i must analyse
#S4 lat 3, lon 4.5
#S5 lat 3, lon 7.5

#find other interesting points
if (!file.exists(paste(lsmDir,"/3deg_mask.tif",sep=""))) {
  msk2 <- msk
  msk2[which(!is.na(msk2[]))] <- rnorm(length(which(!is.na(msk2[]))),10,2)
  writeRaster(msk2,paste(lsmDir),format="GTiff")
} else {
  msk2 <- raster(paste(lsmDir,"/3deg_mask.tif",sep=""))
}
#then check in arcgis

#new points
s4 <- extent(4.5,7.5,3,6)
s5 <- extent(7.5,10.5,3,6)

p00 <- extent(msk)
p00@ymax <- 15
#plot(p00,add=T)


####
#### plotting functions
#functions to make plot
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
  }
  if (rev) {pal <- rev(pal)}
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal,region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}


rs_print <- function(p,pdfName) {
  pdf(pdfName,height=ht,width=wt,pointsize=14)
  print(p)
  dev.off()
}

#figure details
ht <- 6
rs <- msk
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))


#figure with locations
pdf(paste(figDir,"/sites.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(msk,0,1,nb=1,scale=NA,col_i="grey 80",col_f="grey 80",ncol=3,rev=F,leg=F)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


#figure with harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string

pdf(paste(figDir,"/sites_ahar.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(ahar,0,1,nb=10,scale=NA,col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()



###
#plot growing season rainfall for all resolutions
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
gsrain <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
minval <- min(gsrain[],na.rm=T)
maxval <- max(gsrain[],na.rm=T)
#here i am

resol <- "12km_exp"
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
gsrain <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
rsx <- gsrain

rsx[which(rsx[] > 5000)] <- 5100
rbrks <- c(seq(0,5000,by=250),5100)

pdf(paste(figDir,"/gsrain_",resol,".pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(rbrks),brks=rbrks,scale=NA,
                     col_i="#DEEBF7",col_f="#08306B",ncol=9,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


#plot temperature data
gstemp <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))
rsx <- gstemp*0.1
minval <- min(gstemp[],na.rm=T)
maxval <- max(gstemp[],na.rm=T)

tbrks <- seq(10,45,by=2.5)

pdf(paste(figDir,"/gstemp_",resol,".pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(tbrks),brks=tbrks,scale="YlOrRd",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()



#plot the worldclim ones
gsrain <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
rsx <- gsrain

pdf(paste(figDir,"/gsrain_wcl.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(rbrks),brks=rbrks,scale=NA,
                     col_i="#DEEBF7",col_f="#08306B",ncol=9,rev=F,leg=T)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


gstemp <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))
rsx <- gstemp*0.1

pdf(paste(figDir,"/gstemp_wcl.pdf",sep=""), height=8,width=10,pointsize=14)
tplot <- rs_levplot2(rsx,minval,maxval,nb=length(tbrks),brks=tbrks,scale="YlOrRd",ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.polygons(as(s4,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s4@xmin+s4@xmax)*.5, (s4@ymin+s5@ymax)*.5, "S4",cex=2))
tplot <- tplot + layer(sp.polygons(as(s5,'SpatialPolygons'),lwd=1.5,col="blue"))
tplot <- tplot + layer(panel.text((s5@xmin+s5@xmax)*.5, (s5@ymin+s5@ymax)*.5, "S5",cex=2))
tplot <- tplot + layer(sp.polygons(as(p00,'SpatialPolygons'),lwd=3,col="red",lty=2))
print(tplot)
dev.off()


###############################################################################
###############################################################################
#make the scaling plots
#### 12 km explicit sites S4 and S5
scaleplotDir <- paste(figDir,"/scale_plots_12km_exp",sep="")
if (!file.exists(scaleplotDir)) {dir.create(scaleplotDir)}

pr_seq <- seq(0,4000,by=100)
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)

tm_seq <- seq(0,50,by=0.5)
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)

resol <- "12km_exp"
cat("resolution:",resol,"\n")
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
srunDir <- paste(runDir,"/3deg/",resol,"-run_",trial,sep="")

#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))

#load suitability, rain and temp raster ---at low resolution
suit_sc <- raster(paste(srunDir,"/",crop_name,"_suitability.tif",sep=""))
prec_sc <- raster(paste(srunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen_sc <- raster(paste(srunDir,"/",crop_name,"_gstmean.tif",sep=""))

#produce the scaling plot for each point
for (i in c(4,5)) {
  #i <- 4
  cat("...",i,"\n")
  text <- get(paste("s",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  suit_p <- crop(suit,text); prec_p <- crop(prec,text); tmen_p <- crop(tmen,text) * 0.1
  ahar_p <- crop(ahar,text)
  
  #put all data in a single data frame
  tcells <- data.frame(CELL=1:ncell(prec_p))
  tcells$x <- xFromCell(prec_p,tcells$CELL); tcells$y <- yFromCell(prec_p,tcells$CELL)
  tcells$PREC <- extract(prec_p,tcells[,c("x","y")])
  tcells <- tcells[which(!is.na(tcells$PREC)),]
  
  tcells$TMEN <- extract(tmen_p,tcells[,c("x","y")])
  
  tcells$SUIT <- extract(suit_p,tcells[,c("x","y")])
  tcells <- tcells[which(!is.na(tcells$SUIT)),]
  
  tcells$AHAR <- extract(ahar_p,tcells[,c("x","y")])
  tcells <- tcells[which(!is.na(tcells$AHAR)),]
  
  #calculate precip stuff
  pcurve <- data.frame()
  for (cl in 1:nrow(pr_seq)) {
    #tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
    kcells <- tcells[which(tcells$PREC >= pr_seq$INI[cl] & tcells$PREC < pr_seq$FIN[cl]),]
    
    if (nrow(tcells) == 0) {
      smean <- NA; sstdv <- NA; pmean <- NA
    } else {
      if (cl < nrow(pr_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,PREC=pmean)
    pcurve <- rbind(pcurve,clout)
  }
  
  #remove NAs
  pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/",resol,"_S",i,"_prec.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(pcurve$PREC,pcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal precipitation (mm)",ylab="Suitability (%)",
       xlim=c(1000,3500),ylim=c(0,100),col="blue")
  grid()
  polup <- pcurve$SUIT.ME+pcurve$SUIT.SD
  poldw <- pcurve$SUIT.ME-pcurve$SUIT.SD
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(pcurve$PREC,rev(pcurve$PREC)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
  lines(pcurve$PREC,pcurve$SUIT.ME,col="blue",lwd=1.5)
  abline(v=extract(prec_sc,text),lty=2,col="red",lwd=0.8)
  abline(h=extract(suit_sc,text),lty=2,col="red",lwd=0.8)
  abline(v=mean(tcells$PREC,na.rm=T),lty=2,col="dark green",lwd=0.8)
  abline(h=mean(tcells$SUIT,na.rm=T),lty=2,col="dark green",lwd=0.8)
  abline(v=mean(tcells$PREC[which(tcells$AHAR >= 0.1)],na.rm=T),lty=2,col="black",lwd=0.8)
  abline(h=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),lty=2,col="black",lwd=0.8)
  dev.off()
  
  #calculate temperature stuff
  tcurve <- data.frame()
  for (cl in 1:nrow(tm_seq)) {
    kcells <- tcells[which(tcells$TMEN >= tm_seq$INI[cl] & tcells$TMEN < tm_seq$FIN[cl]),]
    if (length(tcells) == 0) {
      smean <- NA; sstdv <- NA; tmean <- NA
    } else {
      if (cl < nrow(tm_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean)
    tcurve <- rbind(tcurve,clout)
  }
  
  #remove NAs
  tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/",resol,"_S",i,"_tmean.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(tcurve$TMEAN,tcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal mean temperature (C)",
       ylab="Suitability (%)",xlim=c(15,30),ylim=c(0,100),col="red")
  grid()
  polup <- tcurve$SUIT.ME+tcurve$SUIT.SD*.5
  poldw <- tcurve$SUIT.ME-tcurve$SUIT.SD*.5
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(tcurve$TMEAN,rev(tcurve$TMEAN)),y=c(polup,rev(poldw)),col="#FF000040",border=NA)
  lines(tcurve$TMEAN,tcurve$SUIT.ME,col="red",lwd=1.5)
  abline(v=extract(tmen_sc,text)*.1,lty=2,col="red",lwd=0.8)
  abline(h=extract(suit_sc,text),lty=2,col="red",lwd=0.8)
  abline(v=mean(tcells$TMEN,na.rm=T),lty=2,col="dark green",lwd=0.8)
  abline(h=mean(tcells$SUIT,na.rm=T),lty=2,col="dark green",lwd=0.8)
  abline(v=mean(tcells$TMEN[which(tcells$AHAR >= 0.1)],na.rm=T),lty=2,col="black",lwd=0.8)
  abline(h=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),lty=2,col="black",lwd=0.8)
  dev.off()
}

####
#### obs sites S4 and S5



