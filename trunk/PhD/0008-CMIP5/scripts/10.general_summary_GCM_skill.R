#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#cmipDir <- "V:/eejarv/CMIP5"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
cmipDir <- "/nfs/a102/eejarv/CMIP5"


####Calculations that i need for summaries
####
#mean climate: calculate the percent of countries-by-seasons with CCOEF < 0.5, >0.8
#mean climate: calculate the percent of countries-by-seasons with PRMSE1 < 25%, >50%
#interannual: calculate the percent of pixels with VI > 0.5
#interannual: calculate ratio blue(wet/warm) to red(dry/cold) in slope maps

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
vnList <- c("pr","tas","dtr")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#list of gcms and regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#data directory
outputDD <- paste(cmipDir,"/assessment/output-data",sep="")
#out_sum <- paste(outputDD,"/_summary",sep="")
out_sum <- paste(outputDD,"/_summary_revised",sep="")

#this_proc <- 1

#function to do lapply
summarise_proc <- function(this_proc) {
  #details of what I need
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  vn <- procList$VAR[this_proc]
  
  #load all datasets, countries and seasons mean climate skill
  all_iso_cl <- data.frame()
  all_iso_vi <- data.frame()
  all_iso_ia <- data.frame()
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    #read file, mean climate skill (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    
    if (!"PRMSE1" %in% names(indat)) {indat$PRMSE1 <- NA}
    if (!"PRMSE2" %in% names(indat)) {indat$PRMSE2 <- NA}
    if (!"mean_OBS" %in% names(indat)) {indat$mean_OBS <- NA}
    if (!"mean_GCM" %in% names(indat)) {indat$mean_GCM <- NA}
    indat <- data.frame(SEASON=indat$SEASON,CCOEF=indat$CCOEF,
                          PVAL=indat$PVAL,RSQ=indat$RSQ,MBR=indat$MBR,
                          RMSE=indat$RMSE,PRMSE1=indat$PRMSE1,PRMSE2=indat$PRMSE2,
                          mean_OBS=indat$mean_OBS,mean_GCM=indat$mean_GCM,N=indat$N)
    
    indat <- cbind(DSET="CRU",indat)
    
    #read file, mean climate skill (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    tmp_dat <- read.csv(infil)
    
    if (!"PRMSE1" %in% names(tmp_dat)) {tmp_dat$PRMSE1 <- NA}
    if (!"PRMSE2" %in% names(tmp_dat)) {tmp_dat$PRMSE2 <- NA}
    if (!"mean_OBS" %in% names(tmp_dat)) {tmp_dat$mean_OBS <- NA}
    if (!"mean_GCM" %in% names(tmp_dat)) {tmp_dat$mean_GCM <- NA}
    tmp_dat <- data.frame(SEASON=tmp_dat$SEASON,CCOEF=tmp_dat$CCOEF,
                          PVAL=tmp_dat$PVAL,RSQ=tmp_dat$RSQ,MBR=tmp_dat$MBR,
                          RMSE=tmp_dat$RMSE,PRMSE1=tmp_dat$PRMSE1,PRMSE2=tmp_dat$PRMSE2,
                          mean_OBS=tmp_dat$mean_OBS,mean_GCM=tmp_dat$mean_GCM,N=tmp_dat$N)
    
    indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    
    #read file, mean climate skill (WCL)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev-WCL/",vn,"_",gcm,"_",ens,".csv",sep="")
    tmp_dat <- read.csv(infil)
    
    if (!"PRMSE1" %in% names(tmp_dat)) {tmp_dat$PRMSE1 <- NA}
    if (!"PRMSE2" %in% names(tmp_dat)) {tmp_dat$PRMSE2 <- NA}
    if (!"mean_OBS" %in% names(tmp_dat)) {tmp_dat$mean_OBS <- NA}
    if (!"mean_GCM" %in% names(tmp_dat)) {tmp_dat$mean_GCM <- NA}
    tmp_dat <- data.frame(SEASON=tmp_dat$SEASON,CCOEF=tmp_dat$CCOEF,
                          PVAL=tmp_dat$PVAL,RSQ=tmp_dat$RSQ,MBR=tmp_dat$MBR,
                          RMSE=tmp_dat$RMSE,PRMSE1=tmp_dat$PRMSE1,PRMSE2=tmp_dat$PRMSE2,
                          mean_OBS=tmp_dat$mean_OBS,mean_GCM=tmp_dat$mean_GCM,N=tmp_dat$N)
    
    indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    
    #final dataset to calculate specific values, mean climate skill
    indat_av <- aggregate(indat[,3:12],by=list(indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av)[1] <- c("SEASON")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    indat_av <- cbind(ISO=iso,indat_av)
    indat_av$N <- NULL
    
    #binding data, mean climate skill
    all_iso_cl <- rbind(all_iso_cl,indat_av)
    
    ###
    #read file, interannual vi (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, interannual vi (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
    
    #final dataset to calculate specific values, interannual vi
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av) <- c("CELL","SEASON","VI")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat_av)
    
    ###
    #read file, interannual rmse (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/ts-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, interannual rmse (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/ts-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
    
    #final dataset to calculate specific values, interannual rmse
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,4:8],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av)[1:2] <- c("CELL","SEASON")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual rmse
    all_iso_ia <- rbind(all_iso_ia,indat_av)
  }
  
  all_iso_cl <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_cl)
  all_iso_vi <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_vi)
  all_iso_ia <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_ia)
  
  ccoef <- all_iso_cl$CCOEF[which(!is.na(all_iso_cl$CCOEF))]
  p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
  p_ccoef08 <- length(which(ccoef >= 0.8)) / length(ccoef)*100
  
  prmse1 <- all_iso_cl$PRMSE1[which(!is.na(all_iso_cl$PRMSE1))]
  p_prmse125 <- length(which(prmse1 <= 25)) / length(ccoef)*100
  p_prmse150 <- length(which(prmse1 >= 50)) / length(ccoef)*100
  
  via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
  p_via05 <- length(which(via > 0.5)) / length(via)*100
  
  mbr <- all_iso_ia$MBR[which(!is.na(all_iso_ia$MBR))]
  r_blue_red <- length(which(mbr < 1)) / length(which(mbr > 1))
  
  res_met <- data.frame(GCM=gcm,ENS=ens,VAR=vn,P_CCOEF_B05=p_ccoef05,
                        P_CCOEF_A08=p_ccoef08,P_VIA_A05=p_via05,BLUE_TO_RED=r_blue_red,
                        PRMSE1_B25=p_prmse125,PRMSE1_A50=p_prmse150)
  
  return(res_met)
}


if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
  out_all <- lapply(1:nrow(procList),summarise_proc)
  out_all2 <- do.call("rbind",out_all)
  write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
} else {
  out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
}

out_all3 <- aggregate(out_all2[,4:9],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
names(out_all3)[1:2] <- c("GCM","VAR")

out_all_x <- aggregate(out_all2[,4:9],by=list(out_all2$GCM,out_all2$VAR),FUN=max,na.rm=T)
out_all_n <- aggregate(out_all2[,4:9],by=list(out_all2$GCM,out_all2$VAR),FUN=min,na.rm=T)

out_all3$P_CCOEF_B05_X <- out_all_x$P_CCOEF_B05
out_all3$P_CCOEF_B05_N <- out_all_n$P_CCOEF_B05
out_all3$P_CCOEF_A08_X <- out_all_x$P_CCOEF_A08
out_all3$P_CCOEF_A08_N <- out_all_n$P_CCOEF_A08
out_all3$P_PRMSE1_B25_X <- out_all_x$PRMSE1_B25
out_all3$P_PRMSE1_B25_N <- out_all_n$PRMSE1_B25
out_all3$P_PRMSE1_A50_X <- out_all_x$PRMSE1_A50
out_all3$P_PRMSE1_A50_N <- out_all_n$PRMSE1_A50
out_all3$P_VIA_A05_X <- out_all_x$P_VIA_A05
out_all3$P_VIA_A05_N <- out_all_n$P_VIA_A05
out_all3$BLUE_TO_RED_X <- out_all_x$BLUE_TO_RED
out_all3$BLUE_TO_RED_N <- out_all_n$BLUE_TO_RED

if (!file.exists(paste(out_sum,"/all-plot_data.csv",sep=""))) {
  write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)
} else {
  out_all3 <- read.csv(paste(out_sum,"/all-plot_data.csv",sep=""))
}


pr_plot <- out_all3[which(out_all3$VAR == "pr"),]
tas_plot <- out_all3[which(out_all3$VAR == "tas"),]
dtr_plot <- out_all3[which(out_all3$VAR == "dtr"),]

gcms <- c("bcc_csm1_1","bnu_esm","cancm4","canesm2","cnrm_cm5","csiro_access10","csiro_mk360",
          "ichec_ec_earth","inmcm4","ipsl_cm5a_lr","ipsl_cm5a_mr","ipsl_cm5b_lr","miroc_esm",
          "miroc_esm_chem","miroc4h","miroc5","hadcm3","hadgem2_cc","hadgem2_es",
          "mpi_esm_lr","mpi_esm_mr","mri_cgcm3","MMM","ncar_ccsm4","noresm1_m",
          "gfdl_esm2g","gfdl_esm2m")
pr_plot$GCM <- gcms

ratio <- 300/72

#bubble plot for CCOEF
library(ggplot2)
tiff(paste(out_sum,"/figures/Fig1.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
ggplot(pr_plot,aes(P_CCOEF_B05, P_CCOEF_A08)) + 
  geom_point(aes(x=P_CCOEF_B05, y=P_CCOEF_A08, size =  P_VIA_A05),shape=20,colour="grey 50") +
  #geom_point(aes(x=P_CCOEF_B05, y=P_CCOEF_A08, size =  P_VIA_A05, colour = PRMSE1_B25),shape=16) +
  #scale_colour_gradient(limits = c(0, 7), low="orange", high="red", breaks= seq(0, 7, by = 1)) +
  scale_area(range=c(2,15),name="VI>0.5 (%)") +
  geom_errorbar(aes(ymin = P_CCOEF_A08_N, ymax = P_CCOEF_A08_X), width=0.07) +
  geom_errorbarh(aes(xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.5) +
  scale_x_continuous("Low correlations ratio (%)", limits = c(-0.1, 7), breaks=seq(0, 7, by = 1)) + 
  scale_y_continuous("High correlations ratio (%)", limits = c(60, 100), breaks=seq(60, 100, by = 5)) +
  theme_bw() +
  opts(axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       legend.text = theme_text(size=12),
       legend.key = theme_blank()) + # switch off the rectangle around symbols in the legend
  geom_text(x=pr_plot$P_CCOEF_B05[1]-0.5,y=pr_plot$P_CCOEF_A08[1],label=pr_plot$GCM[1],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[2]-0.35,y=pr_plot$P_CCOEF_A08[2]-0.5,label=pr_plot$GCM[2],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[3]+0.4,y=pr_plot$P_CCOEF_A08[3]+1,label=pr_plot$GCM[3],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[4]+0.3,y=pr_plot$P_CCOEF_A08[4]-1,label=pr_plot$GCM[4],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[5]+0.4,y=pr_plot$P_CCOEF_A08[5],label=pr_plot$GCM[5],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[6]+0.5,y=pr_plot$P_CCOEF_A08[6]+.9,label=pr_plot$GCM[6],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[7]-0.5,y=pr_plot$P_CCOEF_A08[7]+.3,label=pr_plot$GCM[7],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[8]+0.5,y=pr_plot$P_CCOEF_A08[8]+1.1,label=pr_plot$GCM[8],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[9]+0.2,y=pr_plot$P_CCOEF_A08[9]+1.2,label=pr_plot$GCM[9],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[10]-0.5,y=pr_plot$P_CCOEF_A08[10]+0.7,label=pr_plot$GCM[10],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[11]+0.5,y=pr_plot$P_CCOEF_A08[11],label=pr_plot$GCM[11],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[12]-0.5,y=pr_plot$P_CCOEF_A08[12]-0.4,label=pr_plot$GCM[12],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[13]-0.4,y=pr_plot$P_CCOEF_A08[13],label=pr_plot$GCM[13],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[14]+0.3,y=pr_plot$P_CCOEF_A08[14]-1,label=pr_plot$GCM[14],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[15]-0.25,y=pr_plot$P_CCOEF_A08[15]-0.7,label=pr_plot$GCM[15],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[16]-0.3,y=pr_plot$P_CCOEF_A08[16],label=pr_plot$GCM[16],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[17]+0.3,y=pr_plot$P_CCOEF_A08[17]+.8,label=pr_plot$GCM[17],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[18]-0.5,y=pr_plot$P_CCOEF_A08[18],label=pr_plot$GCM[18],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[19]+0.1,y=pr_plot$P_CCOEF_A08[19]-1.1,label=pr_plot$GCM[19],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[20]+0.4,y=pr_plot$P_CCOEF_A08[20]+0.8,label=pr_plot$GCM[20],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[21]+0.4,y=pr_plot$P_CCOEF_A08[21]+.6,label=pr_plot$GCM[21],size=3.5)+
  geom_text(x=pr_plot$P_CCOEF_B05[22],y=pr_plot$P_CCOEF_A08[22]-1.2,label=pr_plot$GCM[22],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[23]+0.35,y=pr_plot$P_CCOEF_A08[23],label=pr_plot$GCM[23],size=3.5,colour="red") +
  geom_text(x=pr_plot$P_CCOEF_B05[24]+0.4,y=pr_plot$P_CCOEF_A08[24]-0.8,label=pr_plot$GCM[24],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[25],y=pr_plot$P_CCOEF_A08[25]-1.3,label=pr_plot$GCM[25],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[26]+0.5,y=pr_plot$P_CCOEF_A08[26]+0.7,label=pr_plot$GCM[26],size=3.5) +
  geom_text(x=pr_plot$P_CCOEF_B05[27]-0.5,y=pr_plot$P_CCOEF_A08[27],label=pr_plot$GCM[27],size=3.5) 
dev.off()


#bubble plot for PRMSE
library(ggplot2)
tiff(paste(out_sum,"/figures/Fig1_rev.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
ggplot(pr_plot,aes(PRMSE1_B25, PRMSE1_A50)) + 
  geom_point(aes(x=PRMSE1_B25, y=PRMSE1_A50, size =  P_VIA_A05),shape=20,colour="grey 50") +
  scale_area(range=c(2,15),name="VI>0.5 (%)") +
  geom_errorbar(aes(ymin = P_PRMSE1_A50_N, ymax = P_PRMSE1_A50_X), width=0.07) +
  geom_errorbarh(aes(xmin = P_PRMSE1_B25_N, xmax = P_PRMSE1_B25_X), height=0.5) +
  scale_x_continuous("Cases with percent RMSE <25% (%)", limits = c(-0.8, 7), breaks=seq(0, 7, by = 1)) + 
  scale_y_continuous("Cases with percent RMSE >50% (%)", limits = c(60, 100), breaks=seq(60, 100, by = 5)) +
  theme_bw() +
  opts(axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       legend.text = theme_text(size=12),
       legend.key = theme_blank()) + # switch off the rectangle around symbols in the legend
         geom_text(x=pr_plot$PRMSE1_B25[1]-0.5,y=pr_plot$PRMSE1_A50[1],label=pr_plot$GCM[1],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[2]-0.35,y=pr_plot$PRMSE1_A50[2]-0.5,label=pr_plot$GCM[2],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[3]+0.4,y=pr_plot$PRMSE1_A50[3]+1,label=pr_plot$GCM[3],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[4]+0.3,y=pr_plot$PRMSE1_A50[4]-1,label=pr_plot$GCM[4],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[5]+0.4,y=pr_plot$PRMSE1_A50[5],label=pr_plot$GCM[5],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[6]+0.5,y=pr_plot$PRMSE1_A50[6]+.9,label=pr_plot$GCM[6],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[7]-0.5,y=pr_plot$PRMSE1_A50[7]+.3,label=pr_plot$GCM[7],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[8]+0.5,y=pr_plot$PRMSE1_A50[8]+1.1,label=pr_plot$GCM[8],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[9]+0.2,y=pr_plot$PRMSE1_A50[9]+1.2,label=pr_plot$GCM[9],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[10]-0.5,y=pr_plot$PRMSE1_A50[10]+0.7,label=pr_plot$GCM[10],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[11]-0.5,y=pr_plot$PRMSE1_A50[11],label=pr_plot$GCM[11],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[12]-0.5,y=pr_plot$PRMSE1_A50[12]-0.4,label=pr_plot$GCM[12],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[13]-0.4,y=pr_plot$PRMSE1_A50[13],label=pr_plot$GCM[13],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[14]+0.3,y=pr_plot$PRMSE1_A50[14]-1,label=pr_plot$GCM[14],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[15]-0.25,y=pr_plot$PRMSE1_A50[15]-0.7,label=pr_plot$GCM[15],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[16]-0.3,y=pr_plot$PRMSE1_A50[16],label=pr_plot$GCM[16],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[17]+0.3,y=pr_plot$PRMSE1_A50[17]+.8,label=pr_plot$GCM[17],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[18]-0.5,y=pr_plot$PRMSE1_A50[18],label=pr_plot$GCM[18],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[19]+0.1,y=pr_plot$PRMSE1_A50[19]-1.1,label=pr_plot$GCM[19],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[20]+0.4,y=pr_plot$PRMSE1_A50[20]+0.8,label=pr_plot$GCM[20],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[21]+0.4,y=pr_plot$PRMSE1_A50[21]+.6,label=pr_plot$GCM[21],size=3.5)+
         geom_text(x=pr_plot$PRMSE1_B25[22]+0.5,y=pr_plot$PRMSE1_A50[22],label=pr_plot$GCM[22],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[23]+0.35,y=pr_plot$PRMSE1_A50[23],label=pr_plot$GCM[23],size=3.5,colour="red") +
         geom_text(x=pr_plot$PRMSE1_B25[24]+0.4,y=pr_plot$PRMSE1_A50[24]-0.8,label=pr_plot$GCM[24],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[25]+0.5,y=pr_plot$PRMSE1_A50[25],label=pr_plot$GCM[25],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[26]+0.5,y=pr_plot$PRMSE1_A50[26]+0.7,label=pr_plot$GCM[26],size=3.5) +
         geom_text(x=pr_plot$PRMSE1_B25[27]-0.5,y=pr_plot$PRMSE1_A50[27],label=pr_plot$GCM[27],size=3.5) 
dev.off()



tas_plot$GCM <- gcms
tiff(paste(out_sum,"/figures/Fig1_rev_tas.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
ggplot(tas_plot,aes(PRMSE1_B25, PRMSE1_A50)) + 
  geom_point(aes(x=PRMSE1_B25, y=PRMSE1_A50, size =  P_VIA_A05),shape=20,colour="grey 50") +
  scale_area(range=c(2,15),name="VI>0.5 (%)") +
  geom_errorbar(aes(ymin = P_PRMSE1_A50_N, ymax = P_PRMSE1_A50_X), width=0.07) +
  geom_errorbarh(aes(xmin = P_PRMSE1_B25_N, xmax = P_PRMSE1_B25_X), height=0.5) +
  scale_x_continuous("Cases with percent RMSE <25% (%)", limits = c(70, 100), breaks=seq(70, 100, by = 5)) + 
  scale_y_continuous("Cases with percent RMSE >50% (%)", limits = c(-0.5, 5), breaks=seq(0, 5, by = 1)) +
  theme_bw() +
  opts(axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       legend.text = theme_text(size=12),
       legend.key = theme_blank()) + # switch off the rectangle around symbols in the legend
         geom_text(x=tas_plot$PRMSE1_B25[1],y=tas_plot$PRMSE1_A50[1],label=tas_plot$GCM[1],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[2],y=tas_plot$PRMSE1_A50[2],label=tas_plot$GCM[2],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[4],y=tas_plot$PRMSE1_A50[4],label=tas_plot$GCM[4],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[5],y=tas_plot$PRMSE1_A50[5],label=tas_plot$GCM[5],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[6],y=tas_plot$PRMSE1_A50[6],label=tas_plot$GCM[6],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[7],y=tas_plot$PRMSE1_A50[7],label=tas_plot$GCM[7],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[8],y=tas_plot$PRMSE1_A50[8],label=tas_plot$GCM[8],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[9],y=tas_plot$PRMSE1_A50[9],label=tas_plot$GCM[9],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[10],y=tas_plot$PRMSE1_A50[10],label=tas_plot$GCM[10],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[11],y=tas_plot$PRMSE1_A50[11],label=tas_plot$GCM[11],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[12],y=tas_plot$PRMSE1_A50[12],label=tas_plot$GCM[12],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[13],y=tas_plot$PRMSE1_A50[13],label=tas_plot$GCM[13],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[14],y=tas_plot$PRMSE1_A50[14],label=tas_plot$GCM[14],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[15],y=tas_plot$PRMSE1_A50[15],label=tas_plot$GCM[15],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[16],y=tas_plot$PRMSE1_A50[16],label=tas_plot$GCM[16],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[17],y=tas_plot$PRMSE1_A50[17],label=tas_plot$GCM[17],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[18],y=tas_plot$PRMSE1_A50[18],label=tas_plot$GCM[18],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[19],y=tas_plot$PRMSE1_A50[19],label=tas_plot$GCM[19],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[20],y=tas_plot$PRMSE1_A50[20],label=tas_plot$GCM[20],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[21],y=tas_plot$PRMSE1_A50[21],label=tas_plot$GCM[21],size=3.5)+
         geom_text(x=tas_plot$PRMSE1_B25[22],y=tas_plot$PRMSE1_A50[22],label=tas_plot$GCM[22],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[23],y=tas_plot$PRMSE1_A50[23],label=tas_plot$GCM[23],size=3.5,colour="red") +
         geom_text(x=tas_plot$PRMSE1_B25[24],y=tas_plot$PRMSE1_A50[24],label=tas_plot$GCM[24],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[25],y=tas_plot$PRMSE1_A50[25],label=tas_plot$GCM[25],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[26],y=tas_plot$PRMSE1_A50[26],label=tas_plot$GCM[26],size=3.5) +
         geom_text(x=tas_plot$PRMSE1_B25[27],y=tas_plot$PRMSE1_A50[27],label=tas_plot$GCM[27],size=3.5) 
dev.off()



dtr_plot$GCM <- gcms
tiff(paste(out_sum,"/figures/Fig1_rev_dtr.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
ggplot(dtr_plot,aes(PRMSE1_B25, PRMSE1_A50)) + 
  geom_point(aes(x=PRMSE1_B25, y=PRMSE1_A50, size =  P_VIA_A05),shape=20,colour="grey 50") +
  scale_area(range=c(2,15),name="VI>0.5 (%)") +
  geom_errorbar(aes(ymin = P_PRMSE1_A50_N, ymax = P_PRMSE1_A50_X), width=0.07) +
  geom_errorbarh(aes(xmin = P_PRMSE1_B25_N, xmax = P_PRMSE1_B25_X), height=0.5) +
  scale_x_continuous("Cases with percent RMSE <25% (%)", limits = c(0, 80), breaks=seq(0, 80, by = 5)) + 
  scale_y_continuous("Cases with percent RMSE >50% (%)", limits = c(0, 50), breaks=seq(0, 50, by = 5)) +
  theme_bw() +
  opts(axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       legend.text = theme_text(size=12),
       legend.key = theme_blank()) + # switch off the rectangle around symbols in the legend
         geom_text(x=dtr_plot$PRMSE1_B25[1]-0.5,y=dtr_plot$PRMSE1_A50[1],label=dtr_plot$GCM[1],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[2]-0.35,y=dtr_plot$PRMSE1_A50[2]-0.5,label=dtr_plot$GCM[2],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[3]+0.4,y=dtr_plot$PRMSE1_A50[3]+1,label=dtr_plot$GCM[3],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[4]+0.3,y=dtr_plot$PRMSE1_A50[4]-1,label=dtr_plot$GCM[4],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[5]+0.4,y=dtr_plot$PRMSE1_A50[5],label=dtr_plot$GCM[5],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[6]+0.5,y=dtr_plot$PRMSE1_A50[6]+.9,label=dtr_plot$GCM[6],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[7]-0.5,y=dtr_plot$PRMSE1_A50[7]+.3,label=dtr_plot$GCM[7],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[8]+0.5,y=dtr_plot$PRMSE1_A50[8]+1.1,label=dtr_plot$GCM[8],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[9]+0.2,y=dtr_plot$PRMSE1_A50[9]+1.2,label=dtr_plot$GCM[9],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[10]-0.5,y=dtr_plot$PRMSE1_A50[10]+0.7,label=dtr_plot$GCM[10],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[11]-0.5,y=dtr_plot$PRMSE1_A50[11],label=dtr_plot$GCM[11],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[12]-0.5,y=dtr_plot$PRMSE1_A50[12]-0.4,label=dtr_plot$GCM[12],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[13]-0.4,y=dtr_plot$PRMSE1_A50[13],label=dtr_plot$GCM[13],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[14]+0.3,y=dtr_plot$PRMSE1_A50[14]-1,label=dtr_plot$GCM[14],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[15]-0.25,y=dtr_plot$PRMSE1_A50[15]-0.7,label=dtr_plot$GCM[15],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[16]-0.3,y=dtr_plot$PRMSE1_A50[16],label=dtr_plot$GCM[16],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[17]+0.3,y=dtr_plot$PRMSE1_A50[17]+.8,label=dtr_plot$GCM[17],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[18]-0.5,y=dtr_plot$PRMSE1_A50[18],label=dtr_plot$GCM[18],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[19]+0.1,y=dtr_plot$PRMSE1_A50[19]-1.1,label=dtr_plot$GCM[19],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[20]+0.4,y=dtr_plot$PRMSE1_A50[20]+0.8,label=dtr_plot$GCM[20],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[21]+0.4,y=dtr_plot$PRMSE1_A50[21]+.6,label=dtr_plot$GCM[21],size=3.5)+
         geom_text(x=dtr_plot$PRMSE1_B25[22]+0.5,y=dtr_plot$PRMSE1_A50[22],label=dtr_plot$GCM[22],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[23]+0.35,y=dtr_plot$PRMSE1_A50[23],label=dtr_plot$GCM[23],size=3.5,colour="red") +
         geom_text(x=dtr_plot$PRMSE1_B25[24]+0.4,y=dtr_plot$PRMSE1_A50[24]-0.8,label=dtr_plot$GCM[24],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[25]+0.5,y=dtr_plot$PRMSE1_A50[25],label=dtr_plot$GCM[25],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[26]+0.5,y=dtr_plot$PRMSE1_A50[26]+0.7,label=dtr_plot$GCM[26],size=3.5) +
         geom_text(x=dtr_plot$PRMSE1_B25[27]-0.5,y=dtr_plot$PRMSE1_A50[27],label=dtr_plot$GCM[27],size=3.5) 
dev.off()


#PRMSE B25
df_plot <- data.frame(PR=pr_plot$PRMSE1_B25,TAS=tas_plot$PRMSE1_B25,DTR=dtr_plot$PRMSE1_B25)
row.names(df_plot) <- gcms
df_plot <- as.matrix(df_plot)
df_plot.m <- melt(df_plot)
df_plot.m <- rename(df_plot.m,c(X1="GCM",X2="Variable"))

a <- ggplot(df_plot.m, aes(x = GCM, y = value,fill = Variable)) + 
  #opts(title = "Percent of country-region combinations with percent RMSE < 25%") + 
  labs(x = NULL, y = "No. cases with percent RMSE < 25%",fill = NULL)
b <- a + geom_bar(stat = "identity", position = "stack")
b <- b + scale_fill_grey(start=0.5,end=0.5) #scale_fill_brewer(palette = "Grey")
skill_theme <- theme_update(axis.text.x = theme_text(angle = 90,hjust = 1),
                            panel.grid.major = theme_line(colour = "grey90"),
                            panel.grid.minor = theme_blank(), panel.background = theme_blank(),
                            axis.ticks = theme_blank(), legend.position = "none")
c <- b + facet_grid(Variable ~ .) + opts(legend.position = "none")
#the plot with different scales
c1 <- c + facet_grid(Variable ~ ., scale="free_y")


tiff(paste(out_sum,"/figures/FigS7a_1.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
c
dev.off()

tiff(paste(out_sum,"/figures/FigS7a_2.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
c1
dev.off()


#PRMSE A50
df_plot <- data.frame(PR=pr_plot$PRMSE1_A50,TAS=tas_plot$PRMSE1_A50,DTR=dtr_plot$PRMSE1_A50)
row.names(df_plot) <- gcms
df_plot <- as.matrix(df_plot)
df_plot.m <- melt(df_plot)
df_plot.m <- rename(df_plot.m,c(X1="GCM",X2="Variable"))

a <- ggplot(df_plot.m, aes(x = GCM, y = value,fill = Variable)) + 
  #opts(title = "Percent of country-region combinations with percent RMSE < 25%") + 
  labs(x = NULL, y = "No. cases with percent RMSE > 50%",fill = NULL)
b <- a + geom_bar(stat = "identity", position = "stack")
b <- b + scale_fill_grey(start=0.5,end=0.5) #scale_fill_brewer(palette = "Grey")
skill_theme <- theme_update(axis.text.x = theme_text(angle = 90,hjust = 1),
                            panel.grid.major = theme_line(colour = "grey90"),
                            panel.grid.minor = theme_blank(), panel.background = theme_blank(),
                            axis.ticks = theme_blank(), legend.position = "none")
c <- b + facet_grid(Variable ~ .) + opts(legend.position = "none")
#the plot with different scales
c1 <- c + facet_grid(Variable ~ ., scale="free_y")


tiff(paste(out_sum,"/figures/FigS7b_1.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
c
dev.off()

tiff(paste(out_sum,"/figures/FigS7b_2.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
c1
dev.off()






