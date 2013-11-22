#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); data(wrld_simpl)

#i/o directories and details
bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
figDir <- paste(bDir,"/figures",sep="")
cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

#list of resolutions
resList <- c("04km_exp","12km_exp","12km","40km")

#model run details
trial <- 6
crop_name <- "maiz"


#get mask from CASCADE output
msk <- raster(paste(cascadeDir,"/cascade_input/Glam_3deg_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#loc     yll xll   yur   xur   res
#R1_3deg  3	 7.5	   6	10.5 	3deg
#R2_3deg  6	10.5	   9	13.5	3deg
#R3_3deg  9	 7.5	  12	10.5	3deg
#Sahel   -3-22.5    15  22.5  3deg

#find other interesting points
if (!file.exists(paste(bDir,"/lsm/3deg_mask.tif",sep=""))) {
  msk2 <- msk
  msk2[which(!is.na(msk2[]))] <- rnorm(length(which(!is.na(msk2[]))),10,2)
  writeRaster(msk2,paste(bDir,"/lsm/3deg_mask.tif",sep=""),format="GTiff")
} else {
  msk2 <- raster(paste(bDir,"/lsm/3deg_mask.tif",sep=""))
}
#then check in arcgis

#points
p1 <- extent(7.5,10.5,3,6) #BJP
p2 <- extent(10.5,13.5,6,9) #BJP
p3 <- extent(7.5,10.5,9,12) #BJP
p4 <- extent(-13.5,-10.5,12,15) #JRV SEN+GMB
p5 <- extent(-16.5,-13.5,12,15) #JRV SEN+GMB
p6 <- extent(-7.5,-4.5,15,18) #JRV MLI
p7 <- extent(-7.5,-4.5,12,15) #JRV MLI
p8 <- extent(1.5,4.5,15,18) #JRV NER
p9 <- extent(1.5,4.5,12,15) #JRV NER

p00 <- extent(-16.5,22.5,9,18) #extent(-22.5,22.5,-3,15) BJP

#figure with locations
png(paste(figDir,"/sites.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
plot(msk,col="grey 80",legend=F)
plot(wrld_simpl,add=T,border="blue")
for (i in 1:9) {
  if (i > 3) {tcol <- "red"} else {tcol <- "black"}
  text <- get(paste("p",i,sep="")); plot(text,add=T,col=tcol,lwd=2)
  text(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5,labels=paste("R",i,sep=""),cex=2)
}
plot(p00,add=T,col="black",lwd=5,lty=2)
dev.off()


#plot growing season rainfall for all resolutions
for (resol in resList) {
  #resol <- resList[1]
  cat("resolution:",resol,"\n")
  
  trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
  
  gsrain <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
  png(paste(figDir,"/prec_",resol,".png",sep=""), height=1000,width=1500,units="px",pointsize=22)
  par(mar=c(3,3,1,2))
  rsx <- gsrain
  plot(rsx,col=colorRampPalette(c("grey 95","blue","purple","red"))(20))
  plot(wrld_simpl,add=T)
  for (i in 1:9) {
    if (i > 3) {tcol <- "red"} else {tcol <- "black"}
    text <- get(paste("p",i,sep="")); plot(text,add=T,col=tcol,lwd=3)
    text(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5,labels=paste("R",i,sep=""),cex=2)
  }
  grid(lwd=1.5)
  plot(p00,add=T,col="black",lwd=5,lty=2)
  dev.off()
  
  gstemp <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))
  png(paste(figDir,"/tmean_",resol,".png",sep=""), height=1000,width=1500,units="px",pointsize=22)
  par(mar=c(3,3,1,2))
  rsx <- gstemp*0.1
  plot(rsx,col=colorRampPalette(c("grey 95","orange","red","purple"))(20))
  plot(wrld_simpl,add=T)
  for (i in 1:9) {
    if (i > 3) {tcol <- "red"} else {tcol <- "black"}
    text <- get(paste("p",i,sep="")); plot(text,add=T,col=tcol,lwd=3)
    text(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5,labels=paste("R",i,sep=""),cex=2)
  }
  grid(lwd=1.5)
  plot(p00,add=T,col="black",lwd=5,lty=2)
  dev.off()
}


#plot the worldclim ones
gsrain <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
png(paste(figDir,"/prec_calib.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- gsrain
plot(rsx,col=colorRampPalette(c("grey 95","blue","purple","red"))(20))
plot(wrld_simpl,add=T)
for (i in 1:9) {
  if (i > 3) {tcol <- "red"} else {tcol <- "black"}
  text <- get(paste("p",i,sep="")); plot(text,add=T,col=tcol,lwd=3)
  text(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5,labels=paste("P",i,sep=""),cex=2)
}
grid(lwd=1.5)
plot(p00,add=T,col="black",lwd=5,lty=2)
dev.off()

gstemp <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))
png(paste(figDir,"/tmean_calib.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- gstemp*0.1
plot(rsx,col=colorRampPalette(c("grey 95","orange","red","purple"))(20))
plot(wrld_simpl,add=T)
for (i in 1:9) {
  if (i > 3) {tcol <- "red"} else {tcol <- "black"}
  text <- get(paste("p",i,sep="")); plot(text,add=T,col="blue",lwd=3)
  text(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5,labels=paste("P",i,sep=""),cex=2)
}
grid(lwd=1.5)
plot(p00,add=T,col="black",lwd=5,lty=2)
dev.off()


#make the scaling plots
scaleplotDir <- paste(figDir,"/scale_plots",sep="")
if (!file.exists(scaleplotDir)) {dir.create(scaleplotDir)}

pr_seq <- seq(0,4000,by=25)
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)

tm_seq <- seq(0,50,by=0.5)
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)

for (resol in resList) {
  #resol <- resList[1]
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
  for (i in 1:9) {
    #i <- 1
    cat("...",i,"\n")
    text <- get(paste("p",i,sep=""))
    xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
    suit_p <- crop(suit,text); prec_p <- crop(prec,text); tmen_p <- crop(tmen,text) * 0.1
    
    #calculate precip stuff
    pcurve <- data.frame()
    for (cl in 1:nrow(pr_seq)) {
      tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
      if (length(tcells) == 0) {
        smean <- NA; sstdv <- NA; pmean <- NA
      } else {
        if (cl < nrow(pr_seq)) {
          smean <- mean(suit_p[tcells],na.rm=T)
          sstdv <- sd(suit_p[tcells],na.rm=T)
          pmean <- mean(prec_p[tcells],na.rm=T)
        } else {
          smean <- mean(suit_p[tcells],na.rm=T)
          sstdv <- sd(suit_p[tcells],na.rm=T)
          pmean <- mean(prec_p[tcells],na.rm=T)
          
        }
      }
      clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,PREC=pmean)
      pcurve <- rbind(pcurve,clout)
    }
    
    #remove NAs
    pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
    
    #produce plot
    png(paste(scaleplotDir,"/",resol,"_R",i,"_prec.png",sep=""), res=300,
        height=1000,width=1500,units="px",pointsize=7)
    par(mar=c(5,5,1,1))
    plot(pcurve$PREC,pcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal precipitation (mm)",ylab="Suitability (%)",
         xlim=c(0,4000),ylim=c(0,100),col="blue")
    grid()
    polup <- pcurve$SUIT.ME+pcurve$SUIT.SD
    poldw <- pcurve$SUIT.ME-pcurve$SUIT.SD
    poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
    polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
    polygon(x=c(pcurve$PREC,rev(pcurve$PREC)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
    lines(pcurve$PREC,pcurve$SUIT.ME,col="blue",lwd=1.5)
    abline(v=extract(prec_sc,text),lty=2,col="black",lwd=1)
    abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1)
    dev.off()
    
    #calculate temperature stuff
    tcurve <- data.frame()
    for (cl in 1:nrow(tm_seq)) {
      tcells <- which(tmen_p[] >= tm_seq$INI[cl] & tmen_p[] < tm_seq$FIN[cl])
      if (length(tcells) == 0) {
        smean <- NA; sstdv <- NA; tmean <- NA
      } else {
        if (cl < nrow(tm_seq)) {
          smean <- mean(suit_p[tcells],na.rm=T)
          sstdv <- sd(suit_p[tcells],na.rm=T)
          tmean <- mean(tmen_p[tcells],na.rm=T)
        } else {
          smean <- mean(suit_p[tcells],na.rm=T)
          sstdv <- sd(suit_p[tcells],na.rm=T)
          tmean <- mean(tmen_p[tcells],na.rm=T)
          
        }
      }
      clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean)
      tcurve <- rbind(tcurve,clout)
    }
    
    
    #remove NAs
    tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
    
    #produce plot
    png(paste(scaleplotDir,"/",resol,"_R",i,"_tmean.png",sep=""), res=300,
        height=1000,width=1500,units="px",pointsize=7)
    par(mar=c(5,5,1,1))
    plot(tcurve$TMEAN,tcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal mean temperature (C)",
         ylab="Suitability (%)",xlim=c(15,45),ylim=c(0,100),col="blue")
    grid()
    polup <- tcurve$SUIT.ME+tcurve$SUIT.SD*.5
    poldw <- tcurve$SUIT.ME-tcurve$SUIT.SD*.5
    poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
    polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
    polygon(x=c(tcurve$TMEAN,rev(tcurve$TMEAN)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
    lines(tcurve$TMEAN,tcurve$SUIT.ME,col="blue",lwd=1.5)
    abline(v=extract(tmen_sc,text)*.1,lty=2,col="black",lwd=1)
    abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1)
    dev.off()
  }
}


#plot the worldclim ones
#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(runDir,"/calib/run_",trial,"/",crop_name,"_gstmean.tif",sep=""))

#produce the scaling plot for each point
for (i in 1:9) {
  #i <- 1
  cat("...",i,"\n")
  text <- get(paste("p",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  suit_p <- crop(suit,text); prec_p <- crop(prec,text); tmen_p <- crop(tmen,text) * 0.1
  
  #calculate precip stuff
  pcurve <- data.frame()
  for (cl in 1:nrow(pr_seq)) {
    tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
    if (length(tcells) == 0) {
      smean <- NA; sstdv <- NA; pmean <- NA
    } else {
      if (cl < nrow(pr_seq)) {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        pmean <- mean(prec_p[tcells],na.rm=T)
      } else {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        pmean <- mean(prec_p[tcells],na.rm=T)
        
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,PREC=pmean)
    pcurve <- rbind(pcurve,clout)
  }
  
  #remove NAs
  pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/calib_R",i,"_prec.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(pcurve$PREC,pcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal precipitation (mm)",ylab="Suitability (%)",
       xlim=c(0,2500),ylim=c(0,100),col="blue")
  grid()
  polup <- pcurve$SUIT.ME+pcurve$SUIT.SD
  poldw <- pcurve$SUIT.ME-pcurve$SUIT.SD
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(pcurve$PREC,rev(pcurve$PREC)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
  lines(pcurve$PREC,pcurve$SUIT.ME,col="blue",lwd=1.5)
  abline(v=mean(prec_p[],na.rm=T),lty=2,col="black",lwd=1)
  #abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1.5)
  dev.off()
  
  #calculate temperature stuff
  tcurve <- data.frame()
  for (cl in 1:nrow(tm_seq)) {
    tcells <- which(tmen_p[] >= tm_seq$INI[cl] & tmen_p[] < tm_seq$FIN[cl])
    if (length(tcells) == 0) {
      smean <- NA; sstdv <- NA; tmean <- NA
    } else {
      if (cl < nrow(tm_seq)) {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        tmean <- mean(tmen_p[tcells],na.rm=T)
      } else {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        tmean <- mean(tmen_p[tcells],na.rm=T)
        
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean)
    tcurve <- rbind(tcurve,clout)
  }
  
  
  #remove NAs
  tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/calib_R",i,"_tmean.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(tcurve$TMEAN,tcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal mean temperature (C)",
       ylab="Suitability (%)",xlim=c(15,35),ylim=c(0,100),col="blue")
  grid()
  polup <- tcurve$SUIT.ME+tcurve$SUIT.SD
  poldw <- tcurve$SUIT.ME-tcurve$SUIT.SD
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(tcurve$TMEAN,rev(tcurve$TMEAN)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
  lines(tcurve$TMEAN,tcurve$SUIT.ME,col="blue",lwd=1.5)
  abline(v=mean(tmen_p[],na.rm=T),lty=2,col="black",lwd=1)
  #abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1.5)
  dev.off()
}


#### 12 km explicit sites R6 and R8
scaleplotDir <- paste(figDir,"/scale_plots_12km_exp",sep="")
if (!file.exists(scaleplotDir)) {dir.create(scaleplotDir)}

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
for (i in c(6,8)) {
  #i <- 8
  cat("...",i,"\n")
  text <- get(paste("p",i,sep=""))
  xy <- c(x=(text@xmin+text@xmax)*.5,y=(text@ymin+text@ymax)*.5)
  suit_p <- crop(suit,text); prec_p <- crop(prec,text); tmen_p <- crop(tmen,text) * 0.1
  
  #calculate precip stuff
  pcurve <- data.frame()
  for (cl in 1:nrow(pr_seq)) {
    tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
    if (length(tcells) == 0) {
      smean <- NA; sstdv <- NA; pmean <- NA
    } else {
      if (cl < nrow(pr_seq)) {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        pmean <- mean(prec_p[tcells],na.rm=T)
      } else {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        pmean <- mean(prec_p[tcells],na.rm=T)
        
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,PREC=pmean)
    pcurve <- rbind(pcurve,clout)
  }
  
  #remove NAs
  pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/",resol,"_R",i,"_prec.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(pcurve$PREC,pcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal precipitation (mm)",ylab="Suitability (%)",
       xlim=c(0,1000),ylim=c(0,100),col="blue")
  grid()
  polup <- pcurve$SUIT.ME+pcurve$SUIT.SD
  poldw <- pcurve$SUIT.ME-pcurve$SUIT.SD
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(pcurve$PREC,rev(pcurve$PREC)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
  lines(pcurve$PREC,pcurve$SUIT.ME,col="blue",lwd=1.5)
  abline(v=extract(prec_sc,text),lty=2,col="black",lwd=1)
  abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1)
  dev.off()
  
  #calculate temperature stuff
  tcurve <- data.frame()
  for (cl in 1:nrow(tm_seq)) {
    tcells <- which(tmen_p[] >= tm_seq$INI[cl] & tmen_p[] < tm_seq$FIN[cl])
    if (length(tcells) == 0) {
      smean <- NA; sstdv <- NA; tmean <- NA
    } else {
      if (cl < nrow(tm_seq)) {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        tmean <- mean(tmen_p[tcells],na.rm=T)
      } else {
        smean <- mean(suit_p[tcells],na.rm=T)
        sstdv <- sd(suit_p[tcells],na.rm=T)
        tmean <- mean(tmen_p[tcells],na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean)
    tcurve <- rbind(tcurve,clout)
  }
  
  #remove NAs
  tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
  
  #produce plot
  png(paste(scaleplotDir,"/",resol,"_R",i,"_tmean.png",sep=""), res=300,
      height=1000,width=1500,units="px",pointsize=7)
  par(mar=c(5,5,1,1))
  plot(tcurve$TMEAN,tcurve$SUIT.ME,ty="l",main=NA,xlab="Seasonal mean temperature (C)",
       ylab="Suitability (%)",xlim=c(30,40),ylim=c(0,100),col="red")
  grid()
  polup <- tcurve$SUIT.ME+tcurve$SUIT.SD*.5
  poldw <- tcurve$SUIT.ME-tcurve$SUIT.SD*.5
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  polup <- sapply(polup,FUN=function(x) {min(c(100,x))})
  polygon(x=c(tcurve$TMEAN,rev(tcurve$TMEAN)),y=c(polup,rev(poldw)),col="#FF000040",border=NA)
  lines(tcurve$TMEAN,tcurve$SUIT.ME,col="red",lwd=1.5)
  abline(v=extract(tmen_sc,text)*.1,lty=2,col="black",lwd=1)
  abline(h=extract(suit_sc,text),lty=2,col="black",lwd=1)
  dev.off()
}


