#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

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
s1 <- extent(-1.5,1.5,6,9)
s2 <- extent(1.5,4.5,6,9)
s3 <- extent(4.5,7.5,6,9)
s4 <- extent(4.5,7.5,3,6)
s5 <- extent(7.5,10.5,3,6)

p00 <- extent(msk)
p00@ymax <- 15

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string


###############################################################################
###############################################################################
#make the scaling plots
#### 12 km explicit sites S4 and S5
scaleplotDir <- paste(figDir,"/scale_plots_12km_exp",sep="")
if (!file.exists(scaleplotDir)) {dir.create(scaleplotDir)}

pr_seq <- seq(-100,100,by=5)
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)
pr_seq$CENTER <- (pr_seq$INI + pr_seq$FIN) * 0.5

tm_seq <- seq(-6,6,by=0.5)
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)
tm_seq$CENTER <- (tm_seq$INI + tm_seq$FIN) * 0.5

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
  
  tcells$PREC_DIF <- (tcells$PREC - mean(tcells$PREC)) / mean(tcells$PREC) * 100
  tcells$TMEN_DIF <- tcells$TMEN - mean(tcells$TMEN)
  
  #calculate precip stuff
  pcurve <- data.frame()
  for (cl in 1:nrow(pr_seq)) {
    #cl <- 1
    #tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
    kcells <- tcells[which(tcells$PREC_DIF >= pr_seq$INI[cl] & tcells$PREC_DIF < pr_seq$FIN[cl]),]
    
    if (nrow(kcells) == 0) {
      smean <- NA; sstdv <- NA; pmean <- NA; pdmean <- NA
    } else {
      if (cl < nrow(pr_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
        pdmean <- mean(kcells$PREC_DIF,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
        pdmean <- mean(kcells$PREC_DIF,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,MID=pr_seq$CENTER[cl],SUIT.ME=smean,SUIT.SD=sstdv,
                        PREC=pmean,PREC_DIF=pdmean,COUNT=nrow(kcells))
    pcurve <- rbind(pcurve,clout)
  }
  
  #remove NAs
  pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
  pcurve$FREQ <- pcurve$COUNT / sum(pcurve$COUNT) * 100
  
  #ggplot plot
  p <- ggplot(pcurve, aes(x=MID,y=FREQ))
  p <- p + geom_bar(alpha=0.5, stat="identity")
  p <- p + geom_line(data=pcurve, aes(x=MID, y=SUIT.ME), colour="red")
  p <- p + geom_point(x=((extract(prec_sc,text)-mean(tcells$PREC)) / mean(tcells$PREC) * 100),
                      y=extract(suit_sc,text),colour="black",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$PREC_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                      colour="red",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$PREC_DIF[which(tcells$AHAR >= 0.1)],na.rm=T),
                      y=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),
                      colour="dark green",shape="*",size=10)
  p <- p + scale_x_continuous(breaks=seq(-100,100,by=10),limits=c(min(pcurve$PREC_DIF),100))
  p <- p + labs(x="Precipitation difference (%)",y="Suitability (%)")
  p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
                 axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
                 axis.title=element_text(size=13,face="bold"))
  
  pdf(paste(scaleplotDir,"/",resol,"_S",i,"_prec_v2.pdf",sep=""),width=10,height=7)
  print(p)
  dev.off()
  
  #calculate temperature stuff
  tcurve <- data.frame()
  for (cl in 1:nrow(tm_seq)) {
    kcells <- tcells[which(tcells$TMEN_DIF >= tm_seq$INI[cl] & tcells$TMEN_DIF < tm_seq$FIN[cl]),]
    if (length(kcells) == 0) {
      smean <- NA; sstdv <- NA; tmean <- NA; tmeand <- NA
    } else {
      if (cl < nrow(tm_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
        tmeand <- mean(kcells$TMEN_DIF,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
        tmeand <- mean(kcells$TMEN_DIF,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,MID=tm_seq$CENTER[cl],SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean,
                        TMEAN_DIF=tmeand,COUNT=nrow(kcells))
    tcurve <- rbind(tcurve,clout)
  }
  
  #remove NAs
  tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
  tcurve$FREQ <- tcurve$COUNT / sum(tcurve$COUNT) * 100
  
  #produce plot
  p <- ggplot(tcurve, aes(x=MID,y=FREQ))
  p <- p + geom_bar(alpha=0.5, stat="identity")
  p <- p + geom_line(data=tcurve, aes(x=MID, y=SUIT.ME), colour="red")
  p <- p + geom_point(x=(extract(tmen_sc,text)*.1-mean(tcells$TMEN)),
                      y=extract(suit_sc,text),colour="black",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$TMEN_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                      colour="red",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$TMEN_DIF[which(tcells$AHAR >= 0.1)],na.rm=T),
                      y=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),
                      colour="dark green",shape="*",size=10)
  p <- p + scale_x_continuous(breaks=seq(-10,10,by=0.5),limits=c(min(tcurve$TMEAN_DIF),max(tcurve$TMEAN_DIF)))
  p <- p + labs(x="Mean temperature difference (K)",y="Suitability (%)")
  p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
                 axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
                 axis.title=element_text(size=13,face="bold"))
  
  pdf(paste(scaleplotDir,"/",resol,"_S",i,"_tmean_v2.pdf",sep=""),width=10,height=7)
  print(p)
  dev.off()
}



####
#### obs sites S4 and S5

#######################################################################
#produce 3deg data by aggregation from (04km_exp, 12kmexp, 12km, and 40km)
msk_3d <- raster(paste(bDir,"/lsm/Glam_3deg_lsm.nc",sep=""))
msk_3d[which(msk_3d[] > 0)] <- 1
msk_3d[which(msk_3d[] < 0)] <- NA

cat("aggregating",resol,"\n")
odataDir <- paste(clmDir,"/global_5min",sep="")

toutDir <- paste(clmDir,"/cascade_3deg-",resol,sep="")
if (!file.exists(toutDir)) {dir.create(toutDir)}

tmsk <- raster(paste(bDir,"/lsm/Glam_12km_lsm.nc",sep=""))
tmsk[which(tmsk[] > 0)] <- 1
tmsk[which(tmsk[] < 0)] <- NA

for (vn in c("prec","tmin","tmax","tmean")) {
  #vn <- "prec"
  cat("...",vn,"\n")
  for (m in 1:12) {
    #m <- 1
    cat("...",m,"\n")
    if (!file.exists(paste(toutDir,"/",vn,"_",m,".tif",sep=""))) {
      rs <- raster(paste(odataDir,"/",vn,"_",m,sep=""))
      rs <- crop(rs, tmsk)
      fac <- xres(msk_3d) / xres(rs)
      rs <- aggregate(rs, fact=fac, fun=mean, na.rm=T)
      rs <- resample(rs, msk_3d, method="ngb")
      rs <- mask(rs, msk_3d)
      rs <- writeRaster(rs,paste(toutDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
    }
  }
}

###
#perform the 3deg runs
trunDir <- paste(runDir,"/3deg",sep="")
if (!file.exists(trunDir)) {dir.create(trunDir)}

tcalDir <- paste(trunDir,"/calendar",sep="")

#three rasters
tpdate2 <- raster(paste(tcalDir,"/plant_3deg.tif",sep=""))
thdate2 <- raster(paste(tcalDir,"/harvest_3deg.tif",sep=""))

#resol <- resList[1]
odataDir <- paste(clmDir,"/cascade_3deg-",resol,sep="")

trial <- 6
outf <- paste(trunDir,"/",resol,"-run_",trial,sep="")

#model parameters
tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6
rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6

#run the model
eco <- suitCalc(climPath=odataDir, 
                sowDat=tpdate2@file@name,
                harDat=thdate2@file@name,
                Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                Ropmax=ropmax,Rmax=rmax, 
                outfolder=outf,
                cropname=crop_name,ext=".tif",cropClimate=F)

png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()


#### here plot
scaleplotDir <- paste(figDir,"/scale_plots_obs",sep="")
if (!file.exists(scaleplotDir)) {dir.create(scaleplotDir)}

pr_seq <- seq(-100,100,by=5)
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)
pr_seq$CENTER <- (pr_seq$INI + pr_seq$FIN) * 0.5

tm_seq <- seq(-6,6,by=0.5)
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)
tm_seq$CENTER <- (tm_seq$INI + tm_seq$FIN) * 0.5

resol <- "obs"
cat("resolution:",resol,"\n")
trunDir <- paste(runDir,"/calib/run_",trial,sep="")
srunDir <- paste(runDir,"/3deg/",resol,"-run_",trial,sep="")

#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))

#load suitability, rain and temp raster ---at low resolution
suit_sc <- raster(paste(srunDir,"/",crop_name,"_suitability.tif",sep=""))
prec_sc <- raster(paste(srunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen_sc <- raster(paste(srunDir,"/",crop_name,"_gstmean.tif",sep=""))


############################################################################
############################################################################
#produce the scaling plot for each point
for (i in c(4,5)) {
  #i <- 5
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
  
  tcells$PREC_DIF <- (tcells$PREC - mean(tcells$PREC)) / mean(tcells$PREC) * 100
  tcells$TMEN_DIF <- tcells$TMEN - mean(tcells$TMEN)
  
  #calculate precip stuff
  pcurve <- data.frame()
  for (cl in 1:nrow(pr_seq)) {
    #cl <- 1
    #tcells <- which(prec_p[] >= pr_seq$INI[cl] & prec_p[] < pr_seq$FIN[cl])
    kcells <- tcells[which(tcells$PREC_DIF >= pr_seq$INI[cl] & tcells$PREC_DIF < pr_seq$FIN[cl]),]
    
    if (nrow(kcells) == 0) {
      smean <- NA; sstdv <- NA; pmean <- NA; pdmean <- NA
    } else {
      if (cl < nrow(pr_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
        pdmean <- mean(kcells$PREC_DIF,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        pmean <- mean(kcells$PREC,na.rm=T)
        pdmean <- mean(kcells$PREC_DIF,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,MID=pr_seq$CENTER[cl],SUIT.ME=smean,SUIT.SD=sstdv,
                        PREC=pmean,PREC_DIF=pdmean,COUNT=nrow(kcells))
    pcurve <- rbind(pcurve,clout)
  }
  
  #remove NAs
  pcurve <- pcurve[which(!is.na(pcurve$SUIT.SD)),]
  pcurve$FREQ <- pcurve$COUNT / sum(pcurve$COUNT) * 100
  
  #ggplot plot
  p <- ggplot(pcurve, aes(x=MID,y=FREQ))
  p <- p + geom_bar(alpha=0.5, stat="identity")
  p <- p + geom_line(data=pcurve, aes(x=MID, y=SUIT.ME), colour="red")
  p <- p + geom_point(x=((extract(prec_sc,text)-mean(tcells$PREC)) / mean(tcells$PREC) * 100),
                      y=extract(suit_sc,text),colour="black",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$PREC_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                      colour="red",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$PREC_DIF[which(tcells$AHAR >= 0.1)],na.rm=T),
                      y=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),
                      colour="dark green",shape="*",size=10)
  p <- p + scale_x_continuous(breaks=seq(-100,100,by=5),limits=c(min(pcurve$PREC_DIF),max(pcurve$PREC_DIF)))
  p <- p + labs(x="Precipitation difference (%)",y="Suitability (%)")
  p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
                 axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
                 axis.title=element_text(size=13,face="bold"))
  
  pdf(paste(scaleplotDir,"/",resol,"_S",i,"_prec_v2.pdf",sep=""),width=10,height=7)
  print(p)
  dev.off()
  
  #calculate temperature stuff
  tcurve <- data.frame()
  for (cl in 1:nrow(tm_seq)) {
    kcells <- tcells[which(tcells$TMEN_DIF >= tm_seq$INI[cl] & tcells$TMEN_DIF < tm_seq$FIN[cl]),]
    if (nrow(kcells) == 0) {
      smean <- NA; sstdv <- NA; tmean <- NA; tmeand <- NA
    } else {
      if (cl < nrow(tm_seq)) {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
        tmeand <- mean(kcells$TMEN_DIF,na.rm=T)
      } else {
        smean <- mean(kcells$SUIT,na.rm=T)
        sstdv <- sd(kcells$SUIT,na.rm=T)
        tmean <- mean(kcells$TMEN,na.rm=T)
        tmeand <- mean(kcells$TMEN_DIF,na.rm=T)
      }
    }
    clout <- data.frame(CLASS=cl,MID=tm_seq$CENTER[cl],SUIT.ME=smean,SUIT.SD=sstdv,TMEAN=tmean,
                        TMEAN_DIF=tmeand,COUNT=nrow(kcells))
    tcurve <- rbind(tcurve,clout)
  }
  
  #remove NAs
  tcurve <- tcurve[which(!is.na(tcurve$SUIT.SD)),]
  tcurve$FREQ <- tcurve$COUNT / sum(tcurve$COUNT) * 100
  
  #produce plot
  p <- ggplot(tcurve, aes(x=MID,y=FREQ))
  p <- p + geom_bar(alpha=0.5, stat="identity")
  p <- p + geom_line(data=tcurve, aes(x=MID, y=SUIT.ME), colour="red")
  p <- p + geom_point(x=(extract(tmen_sc,text)*.1-mean(tcells$TMEN)),
                      y=extract(suit_sc,text),colour="black",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$TMEN_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                      colour="red",shape="*",size=10)
  p <- p + geom_point(x=mean(tcells$TMEN_DIF[which(tcells$AHAR >= 0.1)],na.rm=T),
                      y=mean(tcells$SUIT[which(tcells$AHAR >= 0.1)],na.rm=T),
                      colour="dark green",shape="*",size=10)
  p <- p + scale_x_continuous(breaks=seq(-10,10,by=0.5),limits=c(min(c(tcurve$TMEAN_DIF,(extract(tmen_sc,text)*.1-mean(tcells$TMEN)))),max(tcurve$TMEAN_DIF)))
  p <- p + labs(x="Mean temperature difference (K)",y="Suitability (%)")
  p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
                 axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
                 axis.title=element_text(size=13,face="bold"))
  
  pdf(paste(scaleplotDir,"/",resol,"_S",i,"_tmean_v2.pdf",sep=""),width=10,height=7)
  print(p)
  dev.off()
 }


