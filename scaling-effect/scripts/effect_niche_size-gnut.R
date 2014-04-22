#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

###
#look at changes in niche size with dT and dP changes
###

#load packages
library(rgdal); library(raster); library(maptools); library(dismo); data(wrld_simpl)

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
figDir <- paste(bDir,"/paper_figures_v2",sep="")
sensDir <- paste(runDir,"/sens",sep="")

#model run details
trial <- 3
crop_name <- "gnut"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#resolution
resol <- "12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")

#mask
extn <- extent(msk)
extn@ymax <- 15
msk2 <- crop(msk, extn)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
aharv <- resample(aharv,msk,method="ngb")

###
#1. open baseline to determine suitability threshold
suit_base <- raster(paste(runDir,"/",resol,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))

#create xy for determining threshold
xy <- as.data.frame(xyFromCell(msk,which(!is.na(msk[]))))
xy <- cbind(cell=cellFromXY(msk,xy[,c("x","y")]),xy)
xy$aharv <- extract(aharv, xy[,c("x","y")])
xy$suit <- extract(suit_base, xy[,c("x","y")])
xy <- xy[which(!is.na(xy$suit)),]
xy <- xy[which(!is.na(xy$aharv)),]

#calculate threshold (bootstrap)
# thresh <- c(); auc <- c()
# for (i in 1:100) {
#   #i <- 1
#   xyt <- xy[sample(1:nrow(xy),size=(0.9*nrow(xy)),replace=F),]
#   eval_model <- evaluate(p=xyt$suit[which(xyt$aharv>=0.05)], a=xyt$suit[which(xyt$aharv<0.05)])
#   tpr_tnr <- eval_model@TPR + eval_model@TNR
#   thresh <- c(thresh, eval_model@t[which(tpr_tnr == max(tpr_tnr))])
#   auc <- c(auc, eval_model@auc)
# }
# thresh_m <- mean(thresh); auc_m <- mean(auc)

# plot(density(thresh)) #double-peaked distribution
th1 <- 29; th2 <- 33
# abline(v=c(th1,th2),col="red")

#set to threshold
suit_bth1 <- suit_base; suit_bth1[which(suit_base[] < th1)] <- 0#; suit_bth1 <- crop(suit_bth1, extn)
suit_bth2 <- suit_base; suit_bth2[which(suit_base[] < th2)] <- 0#; suit_bth2 <- crop(suit_bth2, extn)

#percentage of grid cells that are "niche" in baseline
pg_th1 <- length(which(suit_bth1[] > 0)) / length(which(!is.na(suit_bth1[]))) * 100
pg_th2 <- length(which(suit_bth2[] > 0)) / length(which(!is.na(suit_bth2[]))) * 100

#percentage of grid cells that are "niche" (in EcoCrop) and are also niche from harvested area
pg_th1_n <- length(which(xy$aharv >= 0.05 & xy$suit >= th1)) / length(which(xy$aharv >= 0.05)) * 100
pg_th2_n <- length(which(xy$aharv >= 0.05 & xy$suit >= th2)) / length(which(xy$aharv >= 0.05)) * 100

#calculate change in percent of grid cells that are niche
outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  #tsuit <- crop(tsuit, extn)
  
  #calculate percentage of niched grid cells
  tpg_th1 <- length(which(tsuit[] >= th1)) / length(which(!is.na(tsuit[]))) * 100
  tpg_th2 <- length(which(tsuit[] >= th2)) / length(which(!is.na(tsuit[]))) * 100
  
  #calculate percentage of niched within "niche"
  xy$tsuit <- extract(tsuit, xy[,c("x","y")])
  tpg_th1_n <- length(which(xy$aharv >= 0.05 & xy$tsuit >= th1)) / length(which(xy$aharv >= 0.05)) * 100
  tpg_th2_n <- length(which(xy$aharv >= 0.05 & xy$tsuit >= th2)) / length(which(xy$aharv >= 0.05)) * 100
  
  #change in size
  #chg1 <- (tpg_th1 - pg_th1) / pg_th1 * 100; chg2 <- (tpg_th2 - pg_th2) / pg_th2 * 100
  chg1 <- tpg_th1 / pg_th1 * 100; chg2 <- tpg_th2 / pg_th2 * 100
  chg1_n <- tpg_th1_n / pg_th1_n * 100; chg2_n <- tpg_th2_n / pg_th2_n * 100
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,niche_size1=tpg_th1, niche_size2=tpg_th2,
                      size_chg1=chg1, size_chg2=chg2, niche_size1_n=tpg_th1_n, niche_size2_n=tpg_th2_n,
                      size_chg1_n=chg1_n, size_chg2_n=chg2_n)
  outsens <- rbind(outsens,outdf)
}
xy$tsuit <- NULL

#plot change in niche size
pdf(paste(figDir,"/extra_plots/niche_size_change_temperature_gnut.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
tchg_12km <- outsens[which(outsens$prec == 0 & outsens$temp != -1),]
plot(tchg_12km$temp, tchg_12km$size_chg1,ty="p",pch=1,xlim=c(0,5),col="blue",ylim=c(0,110),
     xlab="Temperature change (K)", ylab="Area (%)",cex=1.5)
points(tchg_12km$temp, tchg_12km$size_chg2,pch=4,col="blue",cex=1.5)
points(tchg_12km$temp, tchg_12km$size_chg1_n,pch=1,col="red",cex=1.5)
points(tchg_12km$temp, tchg_12km$size_chg2_n,pch=4,col="red",cex=1.5)
grid()
legend(0,20,legend=c("Threshold 1","Threshold 2"),col=c("blue","blue"),pch=c(1,4),bg="white",cex=0.9)
legend(1.5,20,legend=c("All","Niche"),col=c("blue","red"),pch=c(1,1),bg="white",cex=0.9)
dev.off()


#plot change in niche size
pdf(paste(figDir,"/extra_plots/niche_size_change_precipitation_gnut.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
tchg_12km <- outsens[which(outsens$temp == 0),]
plot(tchg_12km$prec*100, tchg_12km$size_chg1, ty="p",pch=1,xlim=c(-90,20),col="blue",ylim=c(0,110),
     xlab="Precipitation change (%)", ylab="Area (%)",cex=1.5)
points(tchg_12km$prec*100, tchg_12km$size_chg2,pch=4,col="blue",cex=1.5)
points(tchg_12km$prec*100, tchg_12km$size_chg1_n,pch=1,col="red",cex=1.5)
points(tchg_12km$prec*100, tchg_12km$size_chg2_n,pch=4,col="red",cex=1.5)
grid()
legend(-30,20,legend=c("Threshold 1","Threshold 2"),col=c("blue","blue"),pch=c(1,4),bg="white",cex=0.9)
legend(0,20,legend=c("All","Niche"),col=c("blue","red"),pch=c(1,1),bg="white",cex=0.9)
dev.off()


#### boxplots

#change in niche size vs. temperature
tchg_bp <- outsens[which(outsens$prec == 0 & outsens$temp != -1 & outsens$temp != 6),]
tchg_bp1 <- cbind(thresh="th1",tchg_bp[,c("temp","size_chg1","size_chg1_n")])
names(tchg_bp1) <- c("thresh","temp","size_chg","size_chg_n")
tchg_bp2 <- cbind(thresh="th2",tchg_bp[,c("temp","size_chg2","size_chg2_n")])
names(tchg_bp2) <- c("thresh","temp","size_chg","size_chg_n")
tchg_bp <- rbind(tchg_bp1, tchg_bp2); tchg_bp$temp <- as.factor(tchg_bp$temp)
tchg_bp <- rbind(cbind(type="all",tchg_bp[,c("thresh","temp")],size_chg=tchg_bp$size_chg),
                 cbind(type="niche",tchg_bp[,c("thresh","temp")],size_chg=tchg_bp$size_chg_n))

pdf(paste(figDir,"/extra_plots/niche_size_change_temperature_gnut_boxplot.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
boxplot(size_chg ~ temp, data=tchg_bp, at=0:5-0.25, subset= type== "all",
        outcol="blue",medcol=NA,boxcol="blue",
        pch=NA, pars=list(staplelty=0, whisklty=0),col="white",boxwex=0.25,
        xlim=c(-0.75,5.5), ylim=c(0,100), axes=F,
        xlab="Temperature change (K)", ylab="Area (%)")
boxplot(size_chg ~ temp, data=tchg_bp, at=0:5+0.25, subset= type== "niche", add=T,
        outcol="red",medcol=NA,boxcol="red",
        pch=NA, pars=list(staplelty=0, whisklty=0), col="white", boxwex=0.25,axes=F)
axis(1, 0:5, labels=seq(0,5,by=1), lwd=1.75)
axis(2, seq(0,100,by=10), labels=seq(0,100,by=10), lwd=1.75)
box(lwd=1.75)
grid(lwd=0.5)
legend(-0.75,20,legend=c("All","Niche"),col=c("blue","red"),lty=c(1,1),bg="white",cex=0.9)
dev.off()


#change in niche size vs. precipitation
tchg_bp <- outsens[which(outsens$temp == 0),]
tchg_bp1 <- cbind(thresh="th1",tchg_bp[,c("prec","size_chg1","size_chg1_n")])
names(tchg_bp1) <- c("thresh","prec","size_chg","size_chg_n")
tchg_bp2 <- cbind(thresh="th2",tchg_bp[,c("prec","size_chg2","size_chg2_n")])
names(tchg_bp2) <- c("thresh","prec","size_chg","size_chg_n")
tchg_bp <- rbind(tchg_bp1, tchg_bp2); tchg_bp$prec <- as.factor(tchg_bp$prec*100)
tchg_bp <- rbind(cbind(type="all",tchg_bp[,c("thresh","prec")],size_chg=tchg_bp$size_chg),
                 cbind(type="niche",tchg_bp[,c("thresh","prec")],size_chg=tchg_bp$size_chg_n))

pdf(paste(figDir,"/extra_plots/niche_size_change_precipitation_gnut_boxplot.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
boxplot(size_chg ~ prec, data=tchg_bp, at=1:12-0.25, subset= type== "all",
        outcol="blue",medcol=NA,boxcol="blue",
        pch=NA, pars=list(staplelty=0, whisklty=0),col="white",boxwex=0.25,
        xlim=c(0.5,12.5), ylim=c(0,110), axes=F,
        xlab="Precipitation change (%)", ylab="Area (%)")
boxplot(size_chg ~ prec, data=tchg_bp, at=1:12+0.25, subset= type== "niche", add=T,
        outcol="red",medcol=NA,boxcol="red",
        pch=NA, pars=list(staplelty=0, whisklty=0), col="white", boxwex=0.25,axes=F)
axis(1, 1:12, labels=seq(-90,20,by=10), lwd=1.75)
axis(2, seq(0,110,by=10), labels=seq(0,110,by=10), lwd=1.75)
box(lwd=1.75)
grid(lwd=0.5)
legend(10,20,legend=c("All","Niche"),col=c("blue","red"),lty=c(1,1),bg="white",cex=0.9)
dev.off()


