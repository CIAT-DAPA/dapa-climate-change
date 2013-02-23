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
source(paste(src.dir2,"/scripts/10.how_much_improvement-functions.R",sep=""))

#directories
cmip5Dir <- paste(mdDir,"/assessment/output-data/_summary_revised2",sep="") #ERL revision
cmip3Dir <- paste(mdDir,"/assessment/output-data-cmip3/_summary",sep="") #ERL revision

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
vnList <- c("pr","tas","dtr","rd")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#list regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#output directory
outputDir_c5 <- paste(mdDir,"/assessment/output-data",sep="")
outputDir_c3 <- paste(mdDir,"/assessment/output-data-cmip3",sep="")


#get the data
if (!file.exists(paste(cmip5Dir,"/vi-summary_final.RData",sep=""))) {
  all_mets <- lapply(1:nrow(procList),summarise_proc_c5)
  all_mets <- do.call("rbind", all_mets)
  save(list=c("all_mets"),file=paste(cmip5Dir,"/vi-summary_final.RData",sep=""))
} else {
  load(file=paste(cmip5Dir,"/vi-summary_final.RData",sep=""))
}


########################################################################
########################################################################
#CMIP3
#list of gcms
gcmList <- list.files("/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data")
vnList <- c("prec","tmean","dtr")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#get the data
if (!file.exists(paste(cmip3Dir,"/vi-summary_final.RData",sep=""))) {
  all_mets <- lapply(1:nrow(procList),summarise_proc_c3)
  all_mets <- do.call("rbind", all_mets)
  save(list=c("all_mets"),file=paste(cmip3Dir,"/vi-summary_final.RData",sep=""))
} else {
  load(file=paste(cmip3Dir,"/vi-summary_final.RData",sep=""))
}


############################################################################
############################################################################
############################################################################
figDir <- paste(cmip5Dir,"/improvements",sep="")

#loading data
load(file=paste(cmip5Dir,"/vi-summary_final.RData",sep=""))
c5_mets <- all_mets
c5_mets$GCM <- paste(c5_mets$GCM)
c5_mets <- cbind(CMIP="CMIP5",c5_mets)
c5_mets <- c5_mets[which(c5_mets$SEASON != "ANN"),]
c5_mets$ENS <- NULL
rm(all_mets)

load(file=paste(cmip3Dir,"/vi-summary_final.RData",sep=""))
c3_mets <- all_mets
c3_mets$GCM <- paste(c3_mets$GCM)
c3_mets <- cbind(CMIP="CMIP3",c3_mets)
c3_mets <- c3_mets[which(c3_mets$SEASON != "ANN"),]
rm(all_mets)

#####
#####
vn <- "pr"
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
    rawm <- rawm[which(!is.na(rawm$VI)),]
    rawm <- rawm[which(rawm$VI < 10000),]
    
    nthresh <- length(which(rawm$VI > 0.5))/nrow(rawm)*100
    thrdf <- data.frame(ID=i,CMIP=cmip,GCM=tgcm,ABR=tabr,VI_A05=nthresh)
    
    #append result
    sub_rawMets <- rbind(sub_rawMets,rawm)
    sub_sumMets <- rbind(sub_sumMets,thrdf)
  }
}


#### plot of model comparison
ggplotdf <- sub_rawMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = ABR, y = VI, fill = CMIP)) + 
  geom_boxplot(outlier.shape=20,outlier.size=0.5,size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  scale_x_discrete("Climate model") + 
  scale_y_continuous("Variability Index", limits = c(0, 5), breaks=seq(0, 20, by = 2)) + 
  #scale_y_continuous("RMSE (Celsius)", limits = c(0, 600), breaks=seq(0, 10, by = 200)) + 
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
tiff(paste(figDir,"/vi_",vn,"_gcm_improvement.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=600*ratio,res=300)
print(p)
dev.off()



#####################
#x11()
ggplotdf <- sub_sumMets
ggplotdf$CMIP <- factor(ggplotdf$CMIP)
p <- ggplot(ggplotdf, aes(x = ABR, y = VI_A05, fill = CMIP)) + 
  geom_bar(width=0.75,stat="identity",size=0.5) + 
  scale_fill_discrete(name="CMIP\nEnsemble",
                      breaks=c("CMIP3", "CMIP5"),
                      labels=c("CMIP3", "CMIP5")) +
  scale_x_discrete("Climate model") + 
  scale_y_continuous("Cases with VI > 0.5 (%)", limits = c(0, 100), breaks=seq(0, 100, by = 20)) + 
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

tiff(paste(figDir,"/vi_",vn,"_gcm_improvement_cases.tif",sep=""),compression="lzw",
     units="px",width=1000*ratio,height=600*ratio,res=300)
print(p)
dev.off()







