#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014
stop("!")

### line plots for maize in G2 (i.e. Inland 1)

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
library(ggplot2); library(plyr)

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures_v2",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))
msk2 <- raster(paste(lsmDir,"/3deg_mask.tif",sep=""))

#new points
g2 <- extent(7.5,10.5,12,15)
m1 <- extent(1.5,4.5,6,9)

p00 <- extent(msk)
p00@ymax <- 15

#######
####### maize in G2
#######

#model run details
trial <- 6
crop_name <- "maiz"
runDir <- paste(bDir,"/model-runs",sep="")

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string

###############################################################################
###############################################################################
#make the scaling plots
#### 12 km explicit site
scaleplotDir <- figDir

resol <- "12km_exp"
cat("resolution:",resol,"\n")
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
srunDir <- paste(runDir,"/3deg/12km_exp_bil-run_",trial,sep="")

#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))

#load suitability, rain and temp raster ---at low resolution
suit_sc <- raster(paste(srunDir,"/",crop_name,"_suitability.tif",sep=""))
prec_sc <- raster(paste(srunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen_sc <- raster(paste(srunDir,"/",crop_name,"_gstmean.tif",sep=""))

#matrix of sites, intervals and max/min values
plotinfo <- data.frame(SITE=paste("M",1:2,sep=""),P_int=c(25,25),
                       T_int=c(0.5,0.5),P_min=c(-100,-100),
                       P_max=c(250,250),T_min=c(-4,-4),
                       T_max=c(4,4))

#produce the scaling plot for each point
### G2
i <- 2
cat("...",i,"\n")
text <- get(paste("g",i,sep=""))
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

tcells$PREC_DIF <- (tcells$PREC - mean(tcells$PREC)) / mean(tcells$PREC) * 100
tcells$TMEN_DIF <- tcells$TMEN - mean(tcells$TMEN)

#classes
pr_seq <- seq(-250,250,by=plotinfo$P_int[i])
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)
pr_seq$CENTER <- (pr_seq$INI + pr_seq$FIN) * 0.5

tm_seq <- seq(-6,6,by=plotinfo$T_int[i])
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)
tm_seq$CENTER <- (tm_seq$INI + tm_seq$FIN) * 0.5

#calculate temperature stuff for all areas
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
                    y=extract(suit_sc,text),colour="black",shape=8,size=3)
p <- p + geom_point(x=mean(tcells$TMEN_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                    colour="red",shape=8,size=3)
p <- p + scale_x_continuous(breaks=seq(-10,10,by=1),
                            limits=c(plotinfo$T_min[i],plotinfo$T_max[i]))
p <- p + labs(x="Mean temperature difference (K)",y="Suitability (%)")
p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
               axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
               axis.title=element_text(size=13,face="bold"))

pdf(paste(scaleplotDir,"/SI-Fig11_ecocrop_maize_in_g2_temperature.pdf",sep=""),width=6,height=4,family="Helvetica")
print(p)
dev.off()


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
                    y=extract(suit_sc,text),colour="black",shape=8,size=3)
p <- p + geom_point(x=mean(tcells$PREC_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                    colour="red",shape=8,size=3)
p <- p + scale_x_continuous(breaks=seq(-250,250,by=plotinfo$P_int[i]),
                            limits=c(plotinfo$P_min[i],plotinfo$P_max[i]))
p <- p + labs(x="Precipitation difference (%)",y="Suitability (%)")
p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
               axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
               axis.title=element_text(size=13,face="bold"))

pdf(paste(scaleplotDir,"/SI-Fig11_ecocrop_maize_in_g2_precipitation.pdf",sep=""),width=6,height=4,pointsize=12,family="Helvetica")
print(p)
dev.off()


#######
####### gnut in M1
#######

#model run details
trial <- 3
crop_name <- "gnut"
runDir <- paste(bDir,"/model-runs_gnut",sep="")

#load harvested area and locations on top
ahar <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
ahar[which(ahar[]==0)] <- NA; ahar[which(ahar[]>1)] <- 1
ahar@crs <- wrld_simpl@proj4string

##########################################################################################
##########################################################################################
scaleplotDir <- figDir

resol <- "12km_exp"
cat("resolution:",resol,"\n")
trunDir <- paste(runDir,"/",resol,"/run_",trial,sep="")
srunDir <- paste(runDir,"/3deg/12km_exp_bil-run_",trial,sep="")

#load suitability, rain and temp raster ---at high resolution
suit <- raster(paste(trunDir,"/",crop_name,"_suitability.tif",sep=""))
prec <- raster(paste(trunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen <- raster(paste(trunDir,"/",crop_name,"_gstmean.tif",sep=""))

#load suitability, rain and temp raster ---at low resolution
suit_sc <- raster(paste(srunDir,"/",crop_name,"_suitability.tif",sep=""))
prec_sc <- raster(paste(srunDir,"/",crop_name,"_gsrain.tif",sep=""))
tmen_sc <- raster(paste(srunDir,"/",crop_name,"_gstmean.tif",sep=""))

#matrix of sites, intervals and max/min values
plotinfo <- data.frame(SITE=paste("M",1:2,sep=""),P_int=c(25,25),
                       T_int=c(0.5,0.5),P_min=c(-100,-100),
                       P_max=c(250,250),T_min=c(-4,-4),
                       T_max=c(4,4))

#produce the scaling plot for each point
### M1
i <- 1
cat("...",i,"\n")
text <- get(paste("m",i,sep=""))
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

tcells$PREC_DIF <- (tcells$PREC - mean(tcells$PREC)) / mean(tcells$PREC) * 100
tcells$TMEN_DIF <- tcells$TMEN - mean(tcells$TMEN)

#classes
pr_seq <- seq(-250,250,by=plotinfo$P_int[i])
pr_seq <- data.frame(INI=pr_seq[1:(length(pr_seq)-1)],FIN=pr_seq[2:length(pr_seq)])
pr_seq <- cbind(CLASS=1:nrow(pr_seq),pr_seq)
pr_seq$CENTER <- (pr_seq$INI + pr_seq$FIN) * 0.5

tm_seq <- seq(-6,6,by=plotinfo$T_int[i])
tm_seq <- data.frame(INI=tm_seq[1:(length(tm_seq)-1)],FIN=tm_seq[2:length(tm_seq)])
tm_seq <- cbind(CLASS=1:nrow(tm_seq),tm_seq)
tm_seq$CENTER <- (tm_seq$INI + tm_seq$FIN) * 0.5

#calculate temperature stuff for all areas
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
                    y=extract(suit_sc,text),colour="black",shape=8,size=3)
p <- p + geom_point(x=mean(tcells$TMEN_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                    colour="red",shape=8,size=3)
p <- p + scale_x_continuous(breaks=seq(-10,10,by=1),
                            limits=c(plotinfo$T_min[i],plotinfo$T_max[i]))
p <- p + scale_y_continuous(breaks=seq(0,100,by=25),
                            limits=c(0,100))
p <- p + labs(x="Mean temperature difference (K)",y="Suitability (%)")
p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
               axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
               axis.title=element_text(size=13,face="bold"))

pdf(paste(scaleplotDir,"/SI-Fig11_ecocrop_gnut_in_m1_temperature.pdf",sep=""),width=6,height=4,family="Helvetica")
print(p)
dev.off()


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
                    y=extract(suit_sc,text),colour="black",shape=8,size=3)
p <- p + geom_point(x=mean(tcells$PREC_DIF,na.rm=T),y=mean(tcells$SUIT,na.rm=T),
                    colour="red",shape=8,size=3)
p <- p + scale_x_continuous(breaks=seq(-250,250,by=plotinfo$P_int[i]),
                            limits=c(plotinfo$P_min[i],plotinfo$P_max[i]))
p <- p + labs(x="Precipitation difference (%)",y="Suitability (%)")
p <- p + theme(panel.background=element_rect(fill="white",colour="black"),
               axis.ticks=element_line(colour="black"),axis.text=element_text(size=12,colour="black"),
               axis.title=element_text(size=13,face="bold"))

pdf(paste(scaleplotDir,"/SI-Fig11_ecocrop_gnut_in_m1_precipitation.pdf",sep=""),width=6,height=4,pointsize=12,family="Helvetica")
print(p)
dev.off()





