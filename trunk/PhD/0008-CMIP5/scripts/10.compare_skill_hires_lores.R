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


vn <- "rd"
vn2 <- vn
if (vn == "pr") {vn2 <- "prec"}
if (vn == "tas") {vn2 <- "tmean"}

this_c5m <- c5_mets[which(c5_mets$VAR == vn),]
this_c3m <- c3_mets[which(c3_mets$VAR == vn2),]

#list of cases where models have changed
cGCM <- data.frame(CMIP="CMIP5",RES="high",GCM="miroc_miroc4h",ABR="miroc4h")
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="ichec_ec_earth",ABR="ichec_ec_earth"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="mri_mri_cgcm3",ABR="mri_cgcm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="high",GCM="ncar_ccsm4",ABR="ncar_ccsm4"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="mohc_hadcm3",ABR="hadcm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="ipsl_ipsl_cm5a_lr",ABR="ipsl_cm5a_lr"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="ipsl_ipsl_cm5b_lr",ABR="ipsl_cm5b_lr"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP5",RES="low",GCM="bnu_esm",ABR="bnu_esm"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="ingv_echam4",ABR="ingv_echam4"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="miroc3_2_hires",ABR="miroc32_hires"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="high",GCM="ncar_ccsm3_0",ABR="ncar_ccsm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="inm_cm3_0",ABR="inm_cm3"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="giss_model_er",ABR="giss_model_er"))
cGCM <- rbind(cGCM,data.frame(CMIP="CMIP3",RES="low",GCM="giss_model_eh",ABR="giss_model_eh"))

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
p <- ggplot(ggplotdf, aes(x = CMIP_RES, y = RMSE, fill = CMIP)) + 
  geom_boxplot(outlier.shape=20,outlier.size=0.5,size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  scale_x_discrete("Ensemble.Resolution") + 
  #scale_y_continuous("RMSE (Celsius)", limits = c(0, 9.5), breaks=seq(0, 20, by = 2)) + #tas, dtr
  #scale_y_continuous("RMSE (mm/season)", limits = c(0, 600), breaks=seq(0, 1000, by = 200)) + #pr
  #scale_y_continuous("RMSE (days/season)", limits = c(0, 80), breaks=seq(0, 100, by = 20)) + #rd
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust = 1,size=18),
        axis.text.y=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.key = element_rect(color="white")) 

p <- p + geom_vline(xintercept=c(2.5), colour="grey 50",size=1.5)

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
} else {
  ggplotdf$CMIP <- c("CMIP5","CMIP5","CMIP3","CMIP3")
}
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = CMIP_RES, y = PRMSE1_A40.MEAN, fill = CMIP)) + 
  geom_bar(width=0.75,stat="identity",size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  geom_errorbar(aes(x=CMIP_RES, ymin = PRMSE1_A40.MIN, ymax = PRMSE1_A40.MAX), width=0.1,size=0.5) +
  scale_x_discrete("Region") + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust = 1,size=18),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.key = element_rect(color="white")) 

p <- p + geom_vline(xintercept=c(2.5), colour="grey 50",size=1.5)

ratio <- 300/72
tiff(paste(figDir,"/",vn,"_high_vs_low_cases.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=800*ratio,res=300)
print(p)
dev.off()








