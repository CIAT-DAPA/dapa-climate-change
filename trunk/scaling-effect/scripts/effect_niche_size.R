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
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
figDir <- paste(bDir,"/paper_figures_v2",sep="")
sensDir <- paste(runDir,"/sens",sep="")

#model run details
trial <- 6
crop_name <- "maiz"

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
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
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
thresh <- c(); auc <- c()
for (i in 1:100) {
  #i <- 1
  xyt <- xy[sample(1:nrow(xy),size=(0.9*nrow(xy)),replace=F),]
  eval_model <- evaluate(p=xyt$suit[which(xyt$aharv>=0.1)], a=xyt$suit[which(xyt$aharv<0.1)])
  tpr_tnr <- eval_model@TPR + eval_model@TNR
  thresh <- c(thresh, eval_model@t[which(tpr_tnr == max(tpr_tnr))])
  auc <- c(auc, eval_model@auc)
}
thresh_m <- mean(thresh); auc_m <- mean(auc)

plot(density(thresh)) #double-peaked distribution
th1 <- 48; th2 <- 71
abline(v=c(th1,th2),col="red")

#set to threshold
suit_bth1 <- suit_base; suit_bth1[which(suit_base[] < th1)] <- 0#; suit_bth1 <- crop(suit_bth1, extn)
suit_bth2 <- suit_base; suit_bth2[which(suit_base[] < th2)] <- 0#; suit_bth2 <- crop(suit_bth2, extn)

#percentage of grid cells that are "niche" in baseline
pg_th1 <- length(which(suit_bth1[] > 0)) / length(which(!is.na(suit_bth1[]))) * 100
pg_th2 <- length(which(suit_bth2[] > 0)) / length(which(!is.na(suit_bth2[]))) * 100

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
  
  #change in size
  chg1 <- (tpg_th1 - pg_th1) / pg_th1 * 100
  chg2 <- (tpg_th2 - pg_th2) / pg_th2 * 100
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,niche_size1=tpg_th1, niche_size2=tpg_th2,
                      size_chg1=chg1, size_chg2=chg2)
  outsens <- rbind(outsens,outdf)
}

#load sensitivity of suitability
sensDir_12km <- paste(runDir,"/sens",sep="")
outsens_12km <- read.csv(paste(sensDir_12km,"/sensitivity_result.csv",sep=""))

#plot change in niche size
pdf(paste(figDir,"/extra_plots/niche_size_change_temperature_maize.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
tchg_12km <- outsens[which(outsens$prec == 0 & outsens$temp != -1),]
sens_12km <- outsens_12km[which(outsens_12km$prec == 0 & outsens_12km$temp != -1),]
plot(tchg_12km$temp, tchg_12km$size_chg1,ty="p",pch=1,xlim=c(0,5),col="blue",ylim=c(-20,0),
     xlab="Temperature change (K)", ylab="Percentage change (%)",cex=1.5)
points(tchg_12km$temp, tchg_12km$size_chg2,pch=4,col="blue",cex=1.5)
points(sens_12km$temp, sens_12km$reldiff_har,pch=22,col="blue",cex=1.5)
grid()
legend(0,-15,legend=c("size at thresh=48 %","size at thresh=71 %","suitability"),col=c("blue","blue","blue"),pch=c(1,4,22),bg="white",cex=0.9)
dev.off()


#plot change in niche size
pdf(paste(figDir,"/extra_plots/niche_size_change_precipitation_maize.pdf",sep=""), height=6,width=8,pointsize=16)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
tchg_12km <- outsens[which(outsens$temp == 0),]
sens_12km <- outsens_12km[which(outsens_12km$temp == 0),]
plot(tchg_12km$prec*100, tchg_12km$size_chg1, ty="p",pch=1,xlim=c(-90,20),col="blue",ylim=c(-100,10),
     xlab="Temperature change (K)", ylab="Percentage change (%)",cex=1.5)
points(tchg_12km$prec*100, tchg_12km$size_chg2,pch=4,col="blue",cex=1.5)
points(sens_12km$prec*100, sens_12km$reldiff_har,pch=22,col="blue",cex=1.5)
grid()
legend(-20,-75,legend=c("size at thresh=48 %","size at thresh=71 %", "suitability"),col=c("blue","blue","blue"),pch=c(1,4,22),bg="white",cex=0.9)
dev.off()




