#Julian Ramirez-Villegas
#Feb 2014
#UoL / CCAFS / CIAT

###
#plot AR5 style for maize and groundnut in site G2 (i.e. Inland 1)
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis)
library(ggplot2); library(reshape2)

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

#1. read raster from sensitivity runs
#2. extract data corresponding to sites
#3. construct sensitivity_result type of data.frame
#4. repeat for 3deg runs

##########################################################################################
##########################################################################################
### maize
### 12km stuff
#model run details
trial <- 6
crop_name <- "maiz"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

############################################################
#maize in M1
extn <- m1
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 6
crop_name <- "maiz"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- m1
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_m1_ecocrop_maize.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)


############################################################
#maize in M2
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

trial <- 6
crop_name <- "maiz"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

extn <- m2
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 6
crop_name <- "maiz"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- m2
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_m2_ecocrop_maize.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)


##########################################################################################
##########################################################################################
### maize (in gnut sites)
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

trial <- 6
crop_name <- "maiz"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

############################################################
#maize in G1
extn <- g1
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 6
crop_name <- "maiz"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- g1
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_g1_ecocrop_maize.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)


############################################################
#maize in G2
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

trial <- 6
crop_name <- "maiz"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

extn <- g2
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 6
crop_name <- "maiz"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- g2
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_g2_ecocrop_maize.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)



##########################################################################################
##########################################################################################
### groundnut
### 12km stuff
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

#model run details
trial <- 3
crop_name <- "gnut"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#groundnut in M1
extn <- m1
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 3
crop_name <- "gnut"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- m1
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_m1_ecocrop_gnut.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()


###############################################
# groundnut in m2
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

#model run details
trial <- 3
crop_name <- "gnut"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#groundnut in M2
extn <- m2
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 3
crop_name <- "gnut"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- m2
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_m2_ecocrop_gnut.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)


##############################################################
# groundnut in g1
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

#model run details
trial <- 3
crop_name <- "gnut"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#groundnut in G1
extn <- g1
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 3
crop_name <- "gnut"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- g1
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_g1_ecocrop_gnut.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)


##############################################################
# groundnut in g2
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs_gnut",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir_12km <- paste(runDir,"/sens",sep="")
sensDir_3d12 <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures",sep="")

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))

#site to extract data from
m1 <- extent(1.5,4.5,6,9)
m2 <- extent(4.5,7.5,6,9)
g1 <- extent(-16.5,-13.5,12,15)
g2 <- extent(7.5,10.5,12,15)

#model run details
trial <- 3
crop_name <- "gnut"
sensDir <- paste(runDir,"/sens",sep="")

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#groundnut in G2
extn <- g2
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_12km <- outsens


################################################################################
### 3deg stuff
#model run details
trial <- 3
crop_name <- "gnut"
resol <- "3deg-12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")
sensDir <- paste(runDir,"/sens_3deg-12km_exp",sep="")

#get mask
extn <- g2
msk2 <- crop(raster(paste(metDir,"/tmax_1.tif",sep="")), extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Groundnuts.crop.calendar/cascade_aharv.tif",sep=""))
aharv_a <- area(aharv)
aharv <- aharv*aharv_a

aharv_3d <- raster(paste(runDir,"/3deg/12km_exp-run_",trial,"/",crop_name,"_suitability.tif",sep=""))
aharv_3d <- crop(aharv_3d, msk2)
aharv_3d[which(!is.na(aharv_3d[]))] <- 0
for (i in which(!is.na(aharv_3d[]))) {
  #i <- 1
  xyval <- xyFromCell(aharv_3d,i)
  aharv_c <- crop(aharv, extent(xyval[1]-1.5,xyval[1]+1.5,xyval[2]-1.5,xyval[2]+1.5))
  aharv_3d[i] <- sum(aharv_c[],na.rm=T)
}
xy$aharv <- extract(aharv, xy[,c("x","y")])

tsuit0 <- raster(paste(sensDir,"/sens_74/","/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
suit_vals0 <- extract(tsuit0, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  tsuit <- crop(tsuit, extn)
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  suitdiff <- suit_vals - suit_vals0 / suit_vals0 * 100
  suitdiff <- suitdiff[which(!is.na(suitdiff))]
  suit_p1 <- length(which(suitdiff > 0)) / length(suitdiff)
  suit_n1 <- length(which(suitdiff < 0)) / length(suitdiff)
  suit_s1 <- sd(suitdiff, na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_all_pos=suit_p1,
                      suit_all_neg=suit_n1,suit_all_sd=suit_s1)
  outsens <- rbind(outsens,outdf)
}

#4. calculate change in suitability with respect to the unperturbed run
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens_3d12 <- outsens


##########################################################################################
### the AR5 style plot

#sub-select only dT between 0 and 5
outsens_12km <- outsens_12km[which(outsens_12km$temp >= 0 & outsens_12km$temp <= 5),]
outsens_3d12 <- outsens_3d12[which(outsens_3d12$temp >= 0 & outsens_3d12$temp <= 5),]

#AR5 style for 12km explicit runs
pdf(paste(figDir,"/SI-Fig12_ar5_g2_ecocrop_gnut.pdf",sep=""), height=6,width=8,pointsize=12)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
pchg <- 0
tchg_12km <- outsens_12km[which(outsens_12km$prec == pchg & outsens_12km$temp != -1),]
tchg_3d12 <- outsens_3d12[which(outsens_3d12$prec == pchg & outsens_3d12$temp != -1),]

tcol <- "blue"
plot(tchg_12km$temp, tchg_12km$reldiff_all,ty="p",pch=4,xlim=c(0,5),ylim=c(-100,50),col=tcol,
     xlab="Temperature change (K)", ylab="Suitability change (%)",cex=1.5)
points(tchg_3d12$temp, tchg_3d12$reldiff_all,pch=1,col=tcol,cex=1.5)

grid()
legend(4,50,legend=c("3 degree","12 km"),col=c("blue","blue"),pch=c(1,4),bg="white")
dev.off()

rm(list=ls()); g=gc(); rm(g)



