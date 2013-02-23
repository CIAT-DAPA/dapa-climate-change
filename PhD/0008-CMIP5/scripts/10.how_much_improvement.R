#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2013

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

figDir <- paste(cmip5Dir,"/improvements",sep="")
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
#which models
#cnrm_cm3                               :  cnrm_cerfacs_cnrm_cm5
#csiro_mk3_0 & csiro_mk3_5              :  csiro_mk3_6_0
#ncar_ccsm3_0                           :  ncar_ccsm4
#ukmo_hadgem1                           :  mohc_hadgem2_cc & mohc_hadgem2_es
#miroc3_2_hires & miroc3_2_medres       :  miroc_miroc4h & miroc_miroc5
#ipsl_cm4                               :  ipsl_ipsl_cm5a_lr & ipsl_ipsl_cm5a_mr & ipsl_ipsl_cm5b_lr
#inm_cm3_0                              :  inm_inmcm4
#mri_cgcm2_3_2a                         :  mri_mri_cgcm3
#cccma_cgcm3_1_t47 & cccma_cgcm3_1_t63  :  cccma_cancm4

vn <- "dtr"
vn2 <- vn
if (vn == "pr") {vn2 <- "prec"}
if (vn == "tas") {vn2 <- "tmean"}

this_c5m <- c5_mets[which(c5_mets$VAR == vn),]
this_c3m <- c3_mets[which(c3_mets$VAR == vn2),]

#list of cases where models have changed
cGCM <- data.frame(ID=1,CMIP="CMIP3",GCM="cnrm_cm3",ABR="cnrm_cm3")
cGCM <- rbind(cGCM,data.frame(ID=1,CMIP="CMIP5",GCM="cnrm_cerfacs_cnrm_cm5",ABR="cnrm_cm5"))
cGCM <- rbind(cGCM,data.frame(ID=2,CMIP="CMIP3",GCM="csiro_mk3_0",ABR="csiro_mk30"))
cGCM <- rbind(cGCM,data.frame(ID=2,CMIP="CMIP3",GCM="csiro_mk3_5",ABR="csiro_mk35"))
cGCM <- rbind(cGCM,data.frame(ID=2,CMIP="CMIP5",GCM="csiro_mk3_6_0",ABR="csiro_mk36"))
cGCM <- rbind(cGCM,data.frame(ID=3,CMIP="CMIP3",GCM="ncar_ccsm3_0",ABR="ncar_ccsm3_0"))
cGCM <- rbind(cGCM,data.frame(ID=3,CMIP="CMIP5",GCM="ncar_ccsm4",ABR="ncar_ccsm4"))
cGCM <- rbind(cGCM,data.frame(ID=4,CMIP="CMIP3",GCM="ukmo_hadgem1",ABR="hadgem1"))
cGCM <- rbind(cGCM,data.frame(ID=4,CMIP="CMIP5",GCM="mohc_hadgem2_cc",ABR="hadgem2_cc"))
cGCM <- rbind(cGCM,data.frame(ID=4,CMIP="CMIP5",GCM="mohc_hadgem2_es",ABR="hadgem2_es"))
cGCM <- rbind(cGCM,data.frame(ID=5,CMIP="CMIP3",GCM="miroc3_2_hires",ABR="miroc32_h"))
cGCM <- rbind(cGCM,data.frame(ID=5,CMIP="CMIP3",GCM="miroc3_2_medres",ABR="miroc32_m"))
cGCM <- rbind(cGCM,data.frame(ID=5,CMIP="CMIP5",GCM="miroc_miroc4h",ABR="miroc4h"))
cGCM <- rbind(cGCM,data.frame(ID=5,CMIP="CMIP5",GCM="miroc_miroc5",ABR="miroc5"))
cGCM <- rbind(cGCM,data.frame(ID=6,CMIP="CMIP3",GCM="ipsl_cm4",ABR="ipsl_cm4"))
cGCM <- rbind(cGCM,data.frame(ID=6,CMIP="CMIP5",GCM="ipsl_ipsl_cm5a_lr",ABR="ipsl_cm5a_lr"))
cGCM <- rbind(cGCM,data.frame(ID=6,CMIP="CMIP5",GCM="ipsl_ipsl_cm5a_mr",ABR="ipsl_cm5a_mr"))
cGCM <- rbind(cGCM,data.frame(ID=6,CMIP="CMIP5",GCM="ipsl_ipsl_cm5b_lr",ABR="ipsl_cm5b_lr"))
cGCM <- rbind(cGCM,data.frame(ID=7,CMIP="CMIP3",GCM="inm_cm3_0",ABR="inmcm3"))
cGCM <- rbind(cGCM,data.frame(ID=7,CMIP="CMIP5",GCM="inm_inmcm4",ABR="inmcm4"))
cGCM <- rbind(cGCM,data.frame(ID=8,CMIP="CMIP3",GCM="mri_cgcm2_3_2a",ABR="mri_cgcm232a"))
cGCM <- rbind(cGCM,data.frame(ID=8,CMIP="CMIP5",GCM="mri_mri_cgcm3",ABR="mri_cgcm3"))
cGCM <- rbind(cGCM,data.frame(ID=9,CMIP="CMIP3",GCM="cccma_cgcm3_1_t47",ABR="cccma_cgcm31_t47"))
cGCM <- rbind(cGCM,data.frame(ID=9,CMIP="CMIP3",GCM="cccma_cgcm3_1_t63",ABR="cccma_cgcm31_t63"))
cGCM <- rbind(cGCM,data.frame(ID=9,CMIP="CMIP5",GCM="cccma_cancm4",ABR="cccma_cancm4"))

#loop through cases to build data for plot
sub_rawMets <- data.frame()
sub_sumMets <- data.frame()

for (i in unique(cGCM$ID)) {
  #i <- unique(cGCM$ID)[1]
  gcms <- cGCM[which(cGCM$ID == i),]
  for (j in 1:nrow(gcms)) {
    #j <- 1
    tgcm <- paste(gcms$GCM[j])
    cmip <- paste(gcms$CMIP[j])
    tabr <- paste(gcms$ABR[j])
    
    if (cmip == "CMIP3") {
      rawm <- this_c3m[which(this_c3m$GCM == tgcm),]
    } else {
      rawm <- this_c5m[which(this_c5m$GCM == tgcm),]
    }
    rawm <- cbind(rawm,ABR=tabr)
    rawm <- rawm[which(!is.na(rawm$PRMSE1)),]
    
    nthresh <- length(which(rawm$PRMSE1 > 40))/nrow(rawm)*100
    thrdf <- data.frame(ID=i,CMIP=cmip,GCM=tgcm,ABR=tabr,PRMSE1_A40=nthresh)
    
    #append result
    sub_rawMets <- rbind(sub_rawMets,rawm)
    sub_sumMets <- rbind(sub_sumMets,thrdf)
  }
}


# x11()
# par(las=2,mar=c(6,10,1,1))
# boxplot(sub_rawMets$RMSE ~ sub_rawMets$ABR,pch=20,ylim=c(0,600),
#         horizontal=T,xlab="RMSE (mm/season)",col="grey 80")
# grid()


ggplotdf <- sub_rawMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = ABR, y = RMSE, fill = CMIP)) + 
  geom_boxplot(outlier.shape=20,outlier.size=0.5,size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  scale_x_discrete("Climate model") + 
  scale_y_continuous("RMSE (Celsius)", limits = c(0, 9.5), breaks=seq(0, 20, by = 2)) + 
  #scale_y_continuous("RMSE (Celsius)", limits = c(0, 600), breaks=seq(0, 1000, by = 200)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust = 1,size=18),
        axis.text.y=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.key = element_rect(color="white")) 

p <- p + geom_vline(xintercept=c(2.5,5.5,7.5,10.5,14.5,18.5,20.5,22.5), 
                                      colour="grey 50",size=1.5)

ratio <- 300/72
### make plot
tiff(paste(figDir,"/",vn,"_gcm_improvement.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=600*ratio,res=300)
print(p)
dev.off()



#####################
#x11()
ggplotdf <- sub_sumMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = ABR, y = PRMSE1_A40, fill = CMIP)) + 
  geom_bar(width=0.75,stat="identity",size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  scale_x_discrete("Climate model") + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust = 1,size=18),
        axis.text.y=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        legend.key = element_rect(color="white")) 

p <- p + geom_vline(xintercept=c(2.5,5.5,7.5,10.5,14.5,18.5,20.5), 
                    colour="grey 50",size=1.5)

tiff(paste(figDir,"/",vn,"_gcm_improvement_cases.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=600*ratio,res=300)
print(p)
dev.off()



#############################################################
#############################################################
#############################################################
#############################################################

regs <- paste(unique(c5_mets$REG))
vn <- "pr"
vn2 <- vn
if (vn == "pr") {vn2 <- "prec"}
if (vn == "tas") {vn2 <- "tmean"}

this_c5m <- c5_mets[which(c5_mets$VAR == vn),]
this_c3m <- c3_mets[which(c3_mets$VAR == vn2),]

#loop regions and gcms
summ_c5 <- data.frame()
summ_c3 <- data.frame()

for (rg in regs) {
  #rg <- regs[1]
  rg5_data <- this_c5m[which(this_c5m$REG == rg),]
  for (tgcm in unique(rg5_data$GCM)) {
    #tgcm <- unique(rg5_data$GCM)[1]
    gcm_data <- rg5_data[which(rg5_data$GCM == tgcm),]
    gcm_data <- gcm_data[which(!is.na(gcm_data$PRMSE1)),]
    nthresh <- length(which(gcm_data$PRMSE1 > 40))/nrow(gcm_data)*100
    thrdf <- data.frame(REG=rg,CMIP="CMIP5",GCM=tgcm,PRMSE1_A40=nthresh)
    summ_c5 <- rbind(summ_c5,thrdf)
  }
  
  rg3_data <- this_c3m[which(this_c3m$REG == rg),]
  for (tgcm in unique(rg3_data$GCM)) {
    #tgcm <- unique(rg3_data$GCM)[1]
    gcm_data <- rg3_data[which(rg3_data$GCM == tgcm),]
    gcm_data <- gcm_data[which(!is.na(gcm_data$PRMSE1)),]
    nthresh <- length(which(gcm_data$PRMSE1 > 40))/nrow(gcm_data)*100
    thrdf <- data.frame(REG=rg,CMIP="CMIP3",GCM=tgcm,PRMSE1_A40=nthresh)
    summ_c3 <- rbind(summ_c3,thrdf)
  }
}


#summary CMIP5
allsum_c5 <- aggregate(summ_c5$PRMSE1_A40,by=list(summ_c5$REG),FUN=mean,na.rm=T)
names(allsum_c5)[1:2] <- c("REG","PRMSE1_A40.MEAN")
allsum_c5 <- cbind(CMIP="CMIP5",allsum_c5)

allsum_c5_x <- aggregate(summ_c5$PRMSE1_A40,by=list(summ_c5$REG),FUN=max,na.rm=T)
allsum_c5_n <- aggregate(summ_c5$PRMSE1_A40,by=list(summ_c5$REG),FUN=min,na.rm=T)
allsum_c5$PRMSE1_A40.MIN <- allsum_c5_n[,2]
allsum_c5$PRMSE1_A40.MAX <- allsum_c5_x[,2]

#summary CMIP3
allsum_c3 <- aggregate(summ_c3$PRMSE1_A40,by=list(summ_c3$REG),FUN=mean,na.rm=T)
names(allsum_c3)[1:2] <- c("REG","PRMSE1_A40.MEAN")
allsum_c3 <- cbind(CMIP="CMIP3",allsum_c3)

allsum_c3_x <- aggregate(summ_c3$PRMSE1_A40,by=list(summ_c3$REG),FUN=max,na.rm=T)
allsum_c3_n <- aggregate(summ_c3$PRMSE1_A40,by=list(summ_c3$REG),FUN=min,na.rm=T)
allsum_c3$PRMSE1_A40.MIN <- allsum_c3_n[,2]
allsum_c3$PRMSE1_A40.MAX <- allsum_c3_x[,2]


ggplotdf <- rbind(allsum_c3,allsum_c5)
ggplotdf <- cbind(REG.CMIP=paste(ggplotdf$REG,ggplotdf$CMIP,sep="."),ggplotdf)
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = REG.CMIP, y = PRMSE1_A40.MEAN, fill = CMIP)) + 
  geom_bar(width=0.75,stat="identity",size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  geom_errorbar(aes(x=REG.CMIP, ymin = PRMSE1_A40.MIN, ymax = PRMSE1_A40.MAX), width=0.1,size=0.5) +
  scale_x_discrete("Region") + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.position="right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.key = element_rect(color="white")) 

p <- p + geom_vline(xintercept=seq(2.5,8.5,length.out=4), 
                    colour="grey 50",size=1.5)
p2 <- p + annotate("text",x=1.5,y=100,label="AND",size=8)
p2 <- p2 + annotate("text",x=3.5,y=100,label="EAF",size=8)
p2 <- p2 + annotate("text",x=5.5,y=100,label="SAF",size=8)
p2 <- p2 + annotate("text",x=7.5,y=100,label="SAS",size=8)
p2 <- p2 + annotate("text",x=9.5,y=100,label="WAF",size=8)

ratio <- 300/72
tiff(paste(figDir,"/",vn,"_gcm_regional_improvement.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=600*ratio,res=300)
print(p2)
dev.off()









