# #Julian Ramirez-Villegas
# #UoL / CCAFS / CIAT
# #June 2012
# 
# library(raster)
# 
# #local
# #src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# #src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# #cmipDir <- "V:/eejarv/CMIP5"
# 
# src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
# cmipDir <- "/mnt/a102/eejarv/CMIP5"
# cmip3Dir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
# 
# # #eljefe
# # src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# # src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# # cmipDir <- "/nfs/a102/eejarv/CMIP5"
# # cmip3Dir <- "/nfs/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
# 
# ####Calculations that i need for summaries
# ####
# #mean climate: calculate the percent of countries-by-seasons with CCOEF2 < 0.5
# #mean climate: calculate the percent of countries-by-seasons with PRMSE1 > 40%
# #mean climate: calculate the percent of countries-by-seasons with PRMSE2 > 40%
# #interannual: calculate the percent of pixels with VI > 0.5
# 
# 
# ######################################################################3
# ############ CMIP5
# 
# #list of gcms
# gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
# gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
# gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
# vnList <- c("pr","tas","dtr","rd")
# procList <- expand.grid(GCM=gcmList,VAR=vnList)
# 
# #list of gcms and regions
# regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
# isoList <- regions$ISO
# 
# #data directory
# outputDD <- paste(cmipDir,"/assessment/output-data",sep="")
# out_sum <- paste(outputDD,"/_summary_revised2",sep="")
# 
# #this_proc <- 1
# 
# #function to do lapply
# summarise_proc <- function(this_proc) {
#   #details of what I need
#   gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
#   ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
#   vn <- procList$VAR[this_proc]
#   
#   #load all datasets, countries and seasons mean climate skill
#   all_iso_cl <- data.frame()
#   all_iso_vi <- data.frame()
#   for (iso in regions$ISO) {
#     #iso <- regions$ISO[1]
#     reg <- regions$REGION[which(regions$ISO == iso)]
#     
#     #read file, mean climate skill (CRU)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
#     indat <- read.csv(infil)
#     indat <- cbind(DSET="CRU",indat)
#     
#     #read file, mean climate skill (WST)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
#     if (file.exists(infil)) {
#       tmp_dat <- read.csv(infil)
#       indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
#     }
#     
#     #read file, mean climate skill (WCL)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-WCL/",vn,"_",gcm,"_",ens,".csv",sep="")
#     if (file.exists(infil)) {
#       tmp_dat <- read.csv(infil)
#       indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
#     }
#     
#     #final dataset to calculate specific values, mean climate skill
#     indat_av <- aggregate(indat[,3:ncol(indat)],by=list(indat$SEASON),FUN=mean,na.rm=T)
#     names(indat_av)[1] <- c("SEASON")
#     indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
#     indat_av <- cbind(ISO=iso,indat_av)
#     indat_av$N <- NULL
#     
#     #binding data, mean climate skill
#     all_iso_cl <- rbind(all_iso_cl,indat_av)
#     
#     ###
#     #read file, interannual vi (CRU)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/vi_rev-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
#     indat <- read.csv(infil)
#     indat <- cbind(DSET="CRU",indat)
#     
#     #read file, interannual vi (WST)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/vi_rev-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
#     if (file.exists(infil)) {indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))}
#     
#     #final dataset to calculate specific values, interannual vi
#     indat$LON <- NULL; indat$LAT <- NULL
#     indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
#     names(indat_av) <- c("CELL","SEASON","VI")
#     indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
#     
#     #binding data, interannual vi
#     all_iso_vi <- rbind(all_iso_vi,indat_av)
#   }
#   
#   all_iso_cl <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_cl)
#   all_iso_vi <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_vi)
#   
#   ccoef <- all_iso_cl$CCOEF2[which(!is.na(all_iso_cl$CCOEF2))]
#   p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
#   
#   prmse1 <- all_iso_cl$PRMSE1[which(!is.na(all_iso_cl$PRMSE1))]
#   p_prmse140 <- length(which(prmse1 >= 40)) / length(prmse1)*100
#   p_prmse190 <- length(which(prmse1 >= 90)) / length(prmse1)*100
#   
#   prmse3 <- all_iso_cl$PRMSE3[which(!is.na(all_iso_cl$PRMSE3))]
#   p_prmse340 <- length(which(prmse3 >= 40)) / length(prmse3)*100
#   p_prmse390 <- length(which(prmse3 >= 90)) / length(prmse3)*100
#   
#   via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
#   p_via05 <- length(which(via > 0.5)) / length(via)*100
#   
#   res_met <- data.frame(GCM=gcm,ENS=ens,VAR=vn,P_CCOEF_B05=p_ccoef05,
#                         PRMSE1_A40=p_prmse140,PRMSE1_A90=p_prmse190,
#                         PRMSE3_A40=p_prmse340,PRMSE3_A90=p_prmse390,VIA_A05=p_via05)
#   
#   return(res_met)
# }
# 
# 
# if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
#   out_all <- lapply(1:nrow(procList),summarise_proc)
#   out_all2 <- do.call("rbind",out_all)
#   write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
# } else {
#   out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
# }
# 
# out_all3 <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
# names(out_all3)[1:2] <- c("GCM","VAR")
# 
# out_all_x <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=max,na.rm=T)
# out_all_n <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=min,na.rm=T)
# 
# out_all3$P_CCOEF_B05_X <- out_all_x$P_CCOEF_B05
# out_all3$P_CCOEF_B05_N <- out_all_n$P_CCOEF_B05
# out_all3$P_PRMSE1_A40_X <- out_all_x$PRMSE1_A40
# out_all3$P_PRMSE1_A40_N <- out_all_n$PRMSE1_A40
# out_all3$P_PRMSE1_A90_X <- out_all_x$PRMSE1_A50
# out_all3$P_PRMSE1_A90_N <- out_all_n$PRMSE1_A50
# out_all3$P_PRMSE3_A40_X <- out_all_x$PRMSE3_A40
# out_all3$P_PRMSE3_A40_N <- out_all_n$PRMSE3_A40
# out_all3$P_PRMSE3_A90_X <- out_all_x$PRMSE3_A50
# out_all3$P_PRMSE3_A90_N <- out_all_n$PRMSE3_A50
# out_all3$VIA_A05_X <- out_all_x$VIA_A05
# out_all3$VIA_A05_N <- out_all_n$VIA_A05
# 
# if (!file.exists(paste(out_sum,"/all-plot_data.csv",sep=""))) {
#   write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)
# } else {
#   out_all3 <- read.csv(paste(out_sum,"/all-plot_data.csv",sep=""))
# }
# 
# #####################################################################
# #### with out_all3 of CMIP5 and that of CMIP3, scattergrams need 
# #    to be constructed that compare them
# 
# 
# 
# ######################################################################3
# ############ CMIP3
# 
# #list of gcms
# gcmList <- list.files(cmip3Dir)
# vnList <- c("prec","tmean","dtr")
# procList <- expand.grid(GCM=gcmList,VAR=vnList)
# 
# #list of gcms and regions
# regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
# isoList <- regions$ISO
# 
# #data directory
# outputDD <- paste(cmipDir,"/assessment/output-data-cmip3",sep="")
# out_sum <- paste(outputDD,"/_summary",sep="")
# 
# #this_proc <- 1
# 
# #function to do lapply
# summarise_proc <- function(this_proc) {
#   #details of what I need
#   gcm <- paste(procList$GCM[this_proc])
#   vn <- procList$VAR[this_proc]
#   
#   #load all datasets, countries and seasons mean climate skill
#   all_iso_cl <- data.frame()
#   all_iso_vi <- data.frame()
#   for (iso in regions$ISO) {
#     #iso <- regions$ISO[1]
#     reg <- regions$REGION[which(regions$ISO == iso)]
#     
#     #read file, mean climate skill (CRU)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl-CRU/",vn,"_",gcm,".csv",sep="")
#     indat <- read.csv(infil)
#     indat <- cbind(DSET="CRU",indat)
#     
#     #read file, mean climate skill (WST)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WST/",vn,"_",gcm,".csv",sep="")
#     tmp_dat <- read.csv(infil)
#     indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
#     
#     #read file, mean climate skill (WCL)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WCL/",vn,"_",gcm,".csv",sep="")
#     tmp_dat <- read.csv(infil)
#     indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
#     
#     #final dataset to calculate specific values, mean climate skill
#     indat_av <- aggregate(indat[,3:ncol(indat)],by=list(indat$SEASON),FUN=mean,na.rm=T)
#     names(indat_av)[1] <- c("SEASON")
#     indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
#     indat_av <- cbind(ISO=iso,indat_av)
#     indat_av$N <- NULL
#     
#     #binding data, mean climate skill
#     all_iso_cl <- rbind(all_iso_cl,indat_av)
#     
#     ###
#     #read file, interannual vi (CRU)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/vi-CRU/",vn,"_",gcm,".csv",sep="")
#     indat <- read.csv(infil)
#     indat <- cbind(DSET="CRU",indat)
#     
#     #read file, interannual vi (WST)
#     infil <- paste(outputDD,"/",reg,"/",iso,"/vi-WST/",vn,"_",gcm,".csv",sep="")
#     indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
#     
#     #final dataset to calculate specific values, interannual vi
#     indat$LON <- NULL; indat$LAT <- NULL
#     indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
#     names(indat_av) <- c("CELL","SEASON","VI")
#     indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
#     
#     #binding data, interannual vi
#     all_iso_vi <- rbind(all_iso_vi,indat_av)
#   }
#   
#   all_iso_cl <- cbind(GCM=gcm,VAR=vn,all_iso_cl)
#   all_iso_vi <- cbind(GCM=gcm,VAR=vn,all_iso_vi)
#   
#   ccoef <- all_iso_cl$CCOEF2[which(!is.na(all_iso_cl$CCOEF2))]
#   p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
#   
#   prmse1 <- all_iso_cl$PRMSE1[which(!is.na(all_iso_cl$PRMSE1))]
#   p_prmse140 <- length(which(prmse1 >= 40)) / length(prmse1)*100
#   p_prmse190 <- length(which(prmse1 >= 90)) / length(prmse1)*100
#   
#   prmse3 <- all_iso_cl$PRMSE3[which(!is.na(all_iso_cl$PRMSE3))]
#   p_prmse340 <- length(which(prmse3 >= 40)) / length(prmse3)*100
#   p_prmse390 <- length(which(prmse3 >= 90)) / length(prmse3)*100
#   
#   via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
#   p_via05 <- length(which(via > 0.5)) / length(via)*100
#   
#   res_met <- data.frame(GCM=gcm,VAR=vn,P_CCOEF_B05=p_ccoef05,
#                         PRMSE1_A40=p_prmse140,PRMSE1_A90=p_prmse190,
#                         PRMSE3_A40=p_prmse340,PRMSE3_A90=p_prmse390,VIA_A05=p_via05)
#   
#   return(res_met)
# }
# 
# 
# if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
#   out_all <- lapply(1:nrow(procList),summarise_proc)
#   out_all2 <- do.call("rbind",out_all)
#   write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
# } else {
#   out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
# }
# 
# out_all3 <- aggregate(out_all2[,3:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
# names(out_all3)[1:2] <- c("GCM","VAR")
# 
# if (!file.exists(paste(out_sum,"/all-plot_data.csv",sep=""))) {
#   write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)
# } else {
#   out_all3 <- read.csv(paste(out_sum,"/all-plot_data.csv",sep=""))
# }
# 
# 
# ##################################################################################
# ##################################################################################
# #produce a plot where three things are shown: PRMSEM > 40, CCOEF2 > 0.5, VI > 0.5
# library(ggplot2)
# 
# #data directory
# cmip5Dir <- paste(cmipDir,"/assessment/output-data/_summary_revised2",sep="")
# cmip3Dir <- paste(cmipDir,"/assessment/output-data-cmip3/_summary",sep="")
# 
# figDir <- paste(cmip5Dir,"/summ_figs",sep="")
# if (!file.exists(figDir)) {dir.create(figDir)}
# 
# #load skill metrics
# c5_mets <- read.csv(paste(cmip5Dir,"/all-plot_data.csv",sep=""))
# c3_mets <- read.csv(paste(cmip3Dir,"/all-plot_data.csv",sep=""))
# 
# #sub-selecting variables
# pr_c5 <- c5_mets[which(c5_mets$VAR == "pr"),]
# tas_c5 <- c5_mets[which(c5_mets$VAR == "tas"),]
# dtr_c5 <- c5_mets[which(c5_mets$VAR == "dtr"),]
# rd_c5 <- c5_mets[which(c5_mets$VAR == "rd"),]
# 
# pr_c3 <- c3_mets[which(c3_mets$VAR == "prec"),]
# tas_c3 <- c3_mets[which(c3_mets$VAR == "tmean"),]
# dtr_c3 <- c3_mets[which(c3_mets$VAR == "dtr"),]
# 
# #gcm names
# c5_gcms <- c("bcc_csm1_1","bnu_esm","cancm4","canesm2","cnrm_cm5","csiro_access10","csiro_mk360",
#              "ichec_ec_earth","inmcm4","ipsl_cm5a_lr","ipsl_cm5a_mr","ipsl_cm5b_lr","miroc_esm",
#              "miroc_esm_chem","miroc4h","miroc5","hadcm3","hadgem2_cc","hadgem2_es",
#              "mpi_esm_lr","mpi_esm_mr","mri_cgcm3","MMM","ncar_ccsm4","noresm1_m",
#              "gfdl_esm2g","gfdl_esm2m")
# 
# c3_gcms <- c("bccr_bcm2_0","cgcm31_t47","cgcm31_t63","cnrm_cm3","csiro_mk30","csiro_mk35",
#              "gfdl_cm20","gfdl_cm21","giss_aom","giss_modeleh","giss_modeler","fgoals10_g",
#              "echam4","inm_cm30","ipsl_cm4","miroc32_hires","miroc32_medres","miub_echog",
#              "echam5","mri_cgcm232a","MMM","ncar_ccsm30","ncar_pcm1","hadcm3",
#              "hadgem1")
# 
# pr_c5$GCM <- c5_gcms
# pr_c5$LABEL <- c(LETTERS[1:22],"MMM",LETTERS[23:26])
# 
# pr_c3$GCM <- c3_gcms
# pr_c3$LABEL <- c(letters[1:20],"mmm",letters[21:24])

ratio <- 300/72

#x11()

#make the basic plot
p <- ggplot(pr_c5,aes(P_CCOEF_B05, PRMSE1_A40)) + 
  geom_point(aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#FF000040") +
  geom_point(data=pr_c3,aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#0000FF32") +
  scale_size_area(max_size=20,limits=c(1,100),breaks=seq(0, 100, by = 10), name="VI > 0.5 (%)") +
  geom_errorbar(aes(x=P_CCOEF_B05, ymin = P_PRMSE1_A40_N, ymax = P_PRMSE1_A40_X), width=0.25,size=0.3,alpha=0.5) +
  geom_errorbarh(aes(x=P_CCOEF_B05, xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.25,size=0.3,alpha=0.5) +
  scale_x_continuous("Cases with R < 0.5 (%)", limits = c(15, 60), breaks=seq(0, 100, by = 10)) + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(70, 105), breaks=seq(0, 100, by = 10)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.key = element_rect(color="white")) # switch off the rectangle around symbols in the legend

#add the labels
for (i in 1:nrow(pr_c5)) {
  p <- p + geom_text(x=pr_c5$P_CCOEF_B05[i]+0.35,y=pr_c5$PRMSE1_A40[i]-0.3,label=pr_c5$LABEL[i],size=3.5,colour="red")
}

for (i in 1:nrow(pr_c3)) {
  if (pr_c3$LABEL[i] == "n" | pr_c3$LABEL[i] == "e") {
    p <- p + geom_text(x=pr_c3$P_CCOEF_B05[i]+0.3,y=pr_c3$PRMSE1_A40[i]+0.3,label=pr_c3$LABEL[i],size=3,colour="blue")
  } else {
    p <- p + geom_text(x=pr_c3$P_CCOEF_B05[i],y=pr_c3$PRMSE1_A40[i],label=pr_c3$LABEL[i],size=3,colour="blue")
  }
}

### make plot
tiff(paste(figDir,"/pr_summary.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
print(p)
dev.off()


#############################################################################
### temperature
# tas_c5$GCM <- c5_gcms
# tas_c5[which(tas_c5$GCM == "cancm4"),3:ncol(tas_c5)] <- NA
# tas_c5$LABEL <- c(LETTERS[1:22],"MMM",LETTERS[23:26])
# 
# tas_c3$GCM <- c3_gcms
# tas_c3$LABEL <- c(letters[1:20],"mmm",letters[21:24])

ratio <- 300/72

#x11()

#make the basic plot
p <- ggplot(tas_c5,aes(P_CCOEF_B05, PRMSE1_A40)) + 
  geom_point(aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#FF000040") +
  geom_point(data=tas_c3,aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#0000FF32") +
  scale_size_area(max_size=20,limits=c(1,100),breaks=seq(0, 100, by = 10), name="VI > 0.5 (%)") +
  geom_errorbar(aes(x=P_CCOEF_B05, ymin = P_PRMSE1_A40_N, ymax = P_PRMSE1_A40_X), width=0.25,size=0.3,alpha=0.5) +
  geom_errorbarh(aes(x=P_CCOEF_B05, xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.25,size=0.3,alpha=0.5) +
  scale_x_continuous("Cases with R < 0.5 (%)", limits = c(0, 35), breaks=seq(0, 100, by = 10)) + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 15), breaks=seq(0, 100, by = 5)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.key = element_rect(color="white")) # switch off the rectangle around symbols in the legend

#add the labels
for (i in 1:nrow(tas_c5)) {
  if (!is.na(tas_c5$P_CCOEF_B05[i])) {
    if (tas_c5$LABEL[i] == "F") {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]-0.25,y=tas_c5$PRMSE1_A40[i]-0.2,label=tas_c5$LABEL[i],size=3.5,colour="red")
    } else if (tas_c5$LABEL[i] == "U") {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]+0.25,y=tas_c5$PRMSE1_A40[i]+0.3,label=tas_c5$LABEL[i],size=3.5,colour="red")
    } else if (tas_c5$LABEL[i] == "MMM") {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]-0.25,y=tas_c5$PRMSE1_A40[i]-0.4,label=tas_c5$LABEL[i],size=3.5,colour="red")
    } else if (tas_c5$LABEL[i] == "K") {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]+0.35,y=tas_c5$PRMSE1_A40[i]+0.4,label=tas_c5$LABEL[i],size=3.5,colour="red")
    } else if (tas_c5$LABEL[i] == "V") {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]-0.35,y=tas_c5$PRMSE1_A40[i]+0.4,label=tas_c5$LABEL[i],size=3.5,colour="red")
    } else {
      p <- p + geom_text(x=tas_c5$P_CCOEF_B05[i]+0.25,y=tas_c5$PRMSE1_A40[i]-0.2,label=tas_c5$LABEL[i],size=3.5,colour="red")
    }
  }
}

for (i in 1:nrow(tas_c3)) {
  if (tas_c3$LABEL[i] == "d") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]-0.2,y=tas_c3$PRMSE1_A40[i],label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "e") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i],y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "h") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]-0.25,y=tas_c3$PRMSE1_A40[i],label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "w") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i],y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "i") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]-0.2,y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "m") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]+0.2,y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "p") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]+0.2,y=tas_c3$PRMSE1_A40[i]-0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "s") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]-0.2,y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "x") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]-0.2,y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else if (tas_c3$LABEL[i] == "f") {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i]+0.2,y=tas_c3$PRMSE1_A40[i]+0.2,label=tas_c3$LABEL[i],size=3,colour="blue")
  } else {
    p <- p + geom_text(x=tas_c3$P_CCOEF_B05[i],y=tas_c3$PRMSE1_A40[i],label=tas_c3$LABEL[i],size=3,colour="blue")
  }
}

### make plot
tiff(paste(figDir,"/tas_summary.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
print(p)
dev.off()


#############################################################################
### diurnal temperature range
dtr_c5$GCM <- c5_gcms
dtr_c5$LABEL <- c(LETTERS[1:22],"MMM",LETTERS[23:26])

dtr_c3$GCM <- c3_gcms
dtr_c3$LABEL <- c(letters[1:20],"mmm",letters[21:24])

ratio <- 300/72

#x11()
#make the basic plot
p <- ggplot(dtr_c5,aes(P_CCOEF_B05, PRMSE1_A40)) + 
  geom_point(aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#FF000040") +
  geom_point(data=dtr_c3,aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#0000FF32") +
  scale_size_area(max_size=20,limits=c(1,100),breaks=seq(0, 100, by = 10), name="VI > 0.5 (%)") +
  geom_errorbar(aes(x=P_CCOEF_B05, ymin = P_PRMSE1_A40_N, ymax = P_PRMSE1_A40_X), width=0.25,size=0.3,alpha=0.5) +
  geom_errorbarh(aes(x=P_CCOEF_B05, xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.25,size=0.3,alpha=0.5) +
  scale_x_continuous("Cases with R < 0.5 (%)", limits = c(25, 65), breaks=seq(0, 100, by = 10)) + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(0, 85), breaks=seq(0, 100, by = 20)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.key = element_rect(color="white")) # switch off the rectangle around symbols in the legend

#add the labels
for (i in 1:nrow(dtr_c5)) {
  p <- p + geom_text(x=dtr_c5$P_CCOEF_B05[i]+0.35,y=dtr_c5$PRMSE1_A40[i]-0.8,label=dtr_c5$LABEL[i],size=3.5,colour="red")
}

for (i in 1:nrow(dtr_c3)) {
  if (!is.na(dtr_c3$P_CCOEF_B05[i])) {
    p <- p + geom_text(x=dtr_c3$P_CCOEF_B05[i],y=dtr_c3$PRMSE1_A40[i],label=dtr_c3$LABEL[i],size=3,colour="blue")
  }
}



### make plot
tiff(paste(figDir,"/dtr_summary.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
print(p)
dev.off()



#############################################################################
### rain days
rd_c5$GCM <- c5_gcms
rd_c5$LABEL <- c(LETTERS[1:22],"MMM",LETTERS[23:26])

ratio <- 300/72

#x11()
#make the basic plot
p <- ggplot(rd_c5,aes(P_CCOEF_B05, PRMSE1_A40)) + 
  geom_point(aes(x=P_CCOEF_B05, y=PRMSE1_A40, size =  VIA_A05),shape=20,colour="#FF000040") +
  scale_size_area(max_size=20,limits=c(1,100),breaks=seq(0, 100, by = 10), name="VI > 0.5 (%)") +
  geom_errorbar(aes(x=P_CCOEF_B05, ymin = P_PRMSE1_A40_N, ymax = P_PRMSE1_A40_X), width=0.25,size=0.3,alpha=0.5) +
  geom_errorbarh(aes(x=P_CCOEF_B05, xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.25,size=0.3,alpha=0.5) +
  scale_x_continuous("Cases with R < 0.5 (%)", limits = c(20, 55), breaks=seq(0, 100, by = 10)) + 
  scale_y_continuous("Cases with RMSE_M > 40 % (%)", limits = c(85, 105), breaks=seq(0, 100, by = 5)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.key = element_rect(color="white")) # switch off the rectangle around symbols in the legend

#add the labels
for (i in 1:nrow(rd_c5)) {
  if (rd_c5$LABEL[i] == "P") {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i],y=rd_c5$PRMSE1_A40[i]-0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  } else if (rd_c5$LABEL[i] == "O") {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i]+0.1,y=rd_c5$PRMSE1_A40[i]-0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  } else if (rd_c5$LABEL[i] == "M") {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i],y=rd_c5$PRMSE1_A40[i]-0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  } else if (rd_c5$LABEL[i] == "C") {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i]+0.2,y=rd_c5$PRMSE1_A40[i]-0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  } else if (rd_c5$LABEL[i] == "F") {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i],y=rd_c5$PRMSE1_A40[i]+0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  }
  else {
    p <- p + geom_text(x=rd_c5$P_CCOEF_B05[i]+0.35,y=rd_c5$PRMSE1_A40[i]-0.25,label=rd_c5$LABEL[i],size=3.5,colour="red")
  }
}


### make plot
tiff(paste(figDir,"/rd_summary.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
print(p)
dev.off()

