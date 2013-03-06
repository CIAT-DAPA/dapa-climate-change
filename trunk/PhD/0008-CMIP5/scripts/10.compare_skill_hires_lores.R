#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2013

stop("do not run thing")

library(ggplot2)

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"


src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a102/eejarv/CMIP5"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#directories
cmip5Dir <- paste(mdDir,"/assessment/output-data/_summary_revised2",sep="") #ERL revision
cmip3Dir <- paste(mdDir,"/assessment/output-data-cmip3/_summary",sep="") #ERL revision

figDir <- paste(cmip5Dir,"/hires_vs_lores",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#loading data
load(file=paste(cmip5Dir,"/cl-summary_final.RData",sep=""))
c5_mets <- all_mets
c5_mets <- c5_mets[which(c5_mets$OBS != "cl_rev2-E40"),]
c5_mets$GCM <- sapply(c5_mets$GCM,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[1]})
c5_mets <- cbind(CMIP="CMIP5",c5_mets)
c5_mets <- c5_mets[which(c5_mets$SEASON != "ANN"),]
c5_mets$PRMSE1 <- abs(c5_mets$PRMSE1)
rm(all_mets)

load(file=paste(cmip3Dir,"/cl-summary_final.RData",sep=""))
c3_mets <- all_mets
c3_mets <- c3_mets[which(c3_mets$OBS != "cl-E40"),]
c3_mets$GCM <- paste(c3_mets$GCM)
c3_mets <- cbind(CMIP="CMIP3",c3_mets)
c3_mets <- c3_mets[which(c3_mets$SEASON != "ANN"),]
c3_mets$PRMSE1 <- abs(c3_mets$PRMSE1)
rm(all_mets)


#unique(c5_mets$GCM)
#unique(c3_mets$GCM)

#######
#high resolution models
#miroc_miroc4h (c5)
#ichec_ec_earth (c5)
#mri_mri_cgcm3 (c5)
#ncar_ccsm4 (c5)
#ingv_echam4 (c3)
#miroc3_2_hires (c3)
#ncar_ccsm3_0 (c3)
#others C5: cnrm_cerfacs_cnrm_cm5, miroc_miroc5, csiro_access1_0, mohc_hadgem2_cc, mohc_hadgem2_es

#low resolution models
#mohc_hadcm3 (c5)
#ipsl_ipsl_cm5a_lr (c5)
#ipsl_ipsl_cm5b_lr (c5)
#bnu_esm (c5)
#inm_cm3_0 (c3)
#giss_model_er (c3)
#giss_model_eh (c3)
#others C5: bcc_csm1_1, cccma_cancm4, cccma_canesm2, miroc_miroc_esm, miroc_miroc_esm_chem


vn <- "pr"
vn2 <- vn
if (vn == "pr") {vn2 <- "prec"}
if (vn == "tas") {vn2 <- "tmean"}

this_c5m <- c5_mets[which(c5_mets$VAR == vn),]
this_c3m <- c3_mets[which(c3_mets$VAR == vn2),]

#list of cases where models have changed
cGCM <- data.frame(CMIP="CMIP5",RES="high",GCM="miroc_miroc4h",ABR="miroc4h")
#cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="ichec_ec_earth",ABR="ichec_ec_earth"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="mri_mri_cgcm3",ABR="mri_cgcm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="ncar_ccsm4",ABR="ncar_ccsm4"))
#cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="mohc_hadcm3",ABR="hadcm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="ipsl_ipsl_cm5a_lr",ABR="ipsl_cm5a_lr"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="ipsl_ipsl_cm5b_lr",ABR="ipsl_cm5b_lr"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="bnu_esm",ABR="bnu_esm"))

if (vn != "rd") {
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="ingv_echam4",ABR="ingv_echam4"))
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="miroc3_2_hires",ABR="miroc32_hires"))
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="ncar_ccsm3_0",ABR="ncar_ccsm3"))
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="inm_cm3_0",ABR="inm_cm3"))
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="giss_model_er",ABR="giss_model_er"))
  cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="giss_model_eh",ABR="giss_model_eh"))
}

#loop through cases to build data for plot
sub_rawMets <- data.frame()
sub_sumMets <- data.frame()

cGCM <- cbind(ID=1:nrow(cGCM),cGCM)
for (i in cGCM$ID) {
  #i <- cGCM$ID[1]
  tgcm <- paste(cGCM$GCM[i])
  cmip <- paste(cGCM$CMIP[i])
  tabr <- paste(cGCM$ABR[i])
  tres <- paste(cGCM$RES[i])
  
  if (cmip == "CMIP3") {
    rawm <- this_c3m[which(this_c3m$GCM == tgcm),]
  } else {
    rawm <- this_c5m[which(this_c5m$GCM == tgcm),]
  }
  rawm <- cbind(rawm,ABR=tabr,RES=tres)
  rawm <- rawm[which(!is.na(rawm$PRMSE1)),]
  
  nthresh <- length(which(rawm$PRMSE1 > 40))/nrow(rawm)*100
  thrdf <- data.frame(ID=i,CMIP=cmip,RES=tres,GCM=tgcm,ABR=tabr,PRMSE1_A40=nthresh)
  
  #append result
  sub_rawMets <- rbind(sub_rawMets,rawm)
  sub_sumMets <- rbind(sub_sumMets,thrdf)
}


ggplotdf <- sub_rawMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
ggplotdf$RES <- factor(ggplotdf$RES)
ggplotdf$CMIP_RES <- interaction(ggplotdf$RES, ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = CMIP_RES, y = RMSE, fill = RES)) + 
  geom_boxplot(outlier.shape=20,outlier.size=0.5,size=0.5) + 
  scale_fill_discrete(name="Resolution") +
  scale_x_discrete("Ensemble.Resolution") + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.key = element_rect(color="white")) 

if (vn == "pr") {
  p <- p + scale_y_continuous("RMSE (mm/season)", limits = c(0, 600), breaks=seq(0, 1000, by = 200))
  p <- p + geom_vline(xintercept=c(2.5), colour="grey 50",size=1.5)
  p <- p + annotate("text",x=1.5,y=550,label="CMIP5",size=8)
  p <- p + annotate("text",x=3.5,y=550,label="CMIP3",size=8)
} else if (vn == "tas" | vn == "dtr") {
  p <- p + scale_y_continuous("RMSE (Celsius)", limits = c(0, 9.5), breaks=seq(0, 20, by = 2))
  p <- p + geom_vline(xintercept=c(2.5), colour="grey 50",size=1.5)
  p <- p + annotate("text",x=1.5,y=9,label="CMIP5",size=8)
  p <- p + annotate("text",x=3.5,y=9,label="CMIP3",size=8)
} else {
  p <- p + scale_y_continuous("RMSE (days/season)", limits = c(0, 80), breaks=seq(0, 100, by = 20))
}

ratio <- 300/72
### make plot
tiff(paste(figDir,"/",vn,"_high_vs_low.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=800*ratio,res=300)
print(p)
dev.off()



#####################
#x11()
ggplotdf <- sub_sumMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
ggplotdf$RES <- factor(ggplotdf$RES)
ggplotdf$CMIP_RES <- interaction(ggplotdf$RES, ggplotdf$CMIP)

#summary
ggplotdf_m <- aggregate(ggplotdf$PRMSE1_A40,by=list(ggplotdf$CMIP_RES),FUN=mean,na.rm=T)
names(ggplotdf_m)[1:2] <- c("CMIP_RES","PRMSE1_A40.MEAN")

ggplotdf_x <- aggregate(ggplotdf$PRMSE1_A40,by=list(ggplotdf$CMIP_RES),FUN=max,na.rm=T)
ggplotdf_n <- aggregate(ggplotdf$PRMSE1_A40,by=list(ggplotdf$CMIP_RES),FUN=min,na.rm=T)
ggplotdf_m$PRMSE1_A40.MIN <- ggplotdf_x[,2]
ggplotdf_m$PRMSE1_A40.MAX <- ggplotdf_n[,2]

#plot
ggplotdf <- ggplotdf_m
ggplotdf$CMIP_RES <- factor(ggplotdf$CMIP_RES)
if (vn == "rd") {
  ggplotdf$CMIP <- c("CMIP5","CMIP5")
  ggplotdf$RES <- c("high","low")
} else {
  ggplotdf$CMIP <- c("CMIP5","CMIP5","CMIP3","CMIP3")
  ggplotdf$RES <- c("high","low","high","low")
}
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
ggplotdf$RES <- factor(ggplotdf$RES)
p <- ggplot(ggplotdf, aes(x = CMIP_RES, y = PRMSE1_A40.MEAN, fill = RES)) + 
  geom_bar(width=0.75,stat="identity",size=0.5) + 
  scale_fill_discrete(name="Resolution") +
  geom_errorbar(aes(x=CMIP_RES, ymin = PRMSE1_A40.MIN, ymax = PRMSE1_A40.MAX), width=0.1,size=0.5) +
  scale_x_discrete("Ensemble.Resolution") + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=15),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.key = element_rect(color="white")) 

if (vn != "rd") {
  p <- p + geom_vline(xintercept=c(2.5), colour="grey 50",size=1.5)
  p <- p + annotate("text",x=1.5,y=100,label="CMIP5",size=8)
  p <- p + annotate("text",x=3.5,y=100,label="CMIP3",size=8)
}

ratio <- 300/72
tiff(paste(figDir,"/",vn,"_high_vs_low_cases.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=800*ratio,res=300)
print(p)
dev.off()



#########################################################################
#########################################################################
#########################################################################
#curve of number of cases above threshold

cGCM <- cbind(ID=1:nrow(cGCM),cGCM)
for (i in cGCM$ID) {
  #i <- cGCM$ID[1]
  tgcm <- paste(cGCM$GCM[i])
  cmip <- paste(cGCM$CMIP[i])
  tabr <- paste(cGCM$ABR[i])
  tres <- paste(cGCM$RES[i])
  
  if (cmip == "CMIP3") {
    rawm <- this_c3m[which(this_c3m$GCM == tgcm),]
  } else {
    rawm <- this_c5m[which(this_c5m$GCM == tgcm),]
  }
  rawm <- cbind(rawm,ABR=tabr,RES=tres)
  rawm <- rawm[which(!is.na(rawm$PRMSE1)),]
  
  this_thdf <- data.frame()
  for (thresh in seq(0,200,by=5)) {
    #thresh <- seq(0,100,by=5)[1]
    nthresh <- length(which(rawm$PRMSE1 > thresh))/nrow(rawm)*100
    thrdf <- data.frame(THRESH=thresh,VALUE=nthresh)
    this_thdf <- rbind(this_thdf,thrdf)
  }
  names(this_thdf)[2] <- tabr
  
  #append result
  if (i == 1) {
    threshCurv <- this_thdf
  } else {
    threshCurv <- merge(threshCurv,this_thdf,by="THRESH",sort=F)
  }
}


ratio <- 300/72
tiff(paste(figDir,"/",vn,"_high_vs_low_curve.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=800*ratio,res=300,pointsize=18)
par(mar=c(5,5,1,1),lwd=1.5)

if (vn == "tas") {
  plot(threshCurv$THRESH,threshCurv$miroc4h,ty="l",xlim=c(0,80),ylim=c(0,100),lwd=1.5,
       xlab="Threshold of RMSE_M used to discriminate cases (%)",
       ylab="Cases with RMSE_M above threshold (%)")
} else if (vn == "dtr") {
  plot(threshCurv$THRESH,threshCurv$miroc4h,ty="l",xlim=c(0,100),ylim=c(0,100),lwd=2.5,
       xlab="Threshold of RMSE_M used to discriminate cases (%)",
       ylab="Cases with RMSE_M above threshold (%)")
} else if (vn == "pr" | vn == "rd") {
  plot(threshCurv$THRESH,threshCurv$miroc4h,ty="l",xlim=c(0,200),ylim=c(0,100),lwd=2.5,
       xlab="Threshold of RMSE_M used to discriminate cases (%)",
       ylab="Cases with RMSE_M above threshold (%)")
}
lines(threshCurv$THRESH,threshCurv$mri_cgcm3,col="black",lwd=2.5)
lines(threshCurv$THRESH,threshCurv$ncar_ccsm4,col="black",lwd=2.5)
lines(threshCurv$THRESH,threshCurv$ipsl_cm5a_lr,col="red",lwd=2.5)
lines(threshCurv$THRESH,threshCurv$ipsl_cm5b_lr,col="red",lwd=2.5)
lines(threshCurv$THRESH,threshCurv$bnu_esm,col="red",lwd=2.5)

if (vn != "rd") {
  lines(threshCurv$THRESH,threshCurv$ingv_echam4,col="black",lty=2,lwd=2.5)
  lines(threshCurv$THRESH,threshCurv$miroc32_hires,col="black",lty=2,lwd=2.5)
  lines(threshCurv$THRESH,threshCurv$ncar_ccsm3,col="black",lty=2,lwd=2.5)
  lines(threshCurv$THRESH,threshCurv$inm_cm3,col="red",lty=2,lwd=2.5)
  lines(threshCurv$THRESH,threshCurv$giss_model_er,col="red",lty=2,lwd=2.5)
  lines(threshCurv$THRESH,threshCurv$giss_model_eh,col="red",lty=2,lwd=2.5)
}
grid()
dev.off()




