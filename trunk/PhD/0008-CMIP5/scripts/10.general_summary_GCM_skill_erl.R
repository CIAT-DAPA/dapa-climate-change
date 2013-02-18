#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

library(raster)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#cmipDir <- "V:/eejarv/CMIP5"

src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
cmipDir <- "/mnt/a102/eejarv/CMIP5"
cmip3Dir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"

# #eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# cmipDir <- "/nfs/a102/eejarv/CMIP5"
# cmip3Dir <- "/nfs/a17/eejarv/IPCC_CMIP3/20C3M/original-data"

####Calculations that i need for summaries
####
#mean climate: calculate the percent of countries-by-seasons with CCOEF2 < 0.5
#mean climate: calculate the percent of countries-by-seasons with PRMSE1 > 40%
#mean climate: calculate the percent of countries-by-seasons with PRMSE2 > 40%
#interannual: calculate the percent of pixels with VI > 0.5


######################################################################3
############ CMIP5

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
vnList <- c("pr","tas","dtr","rd")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#list of gcms and regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#data directory
outputDD <- paste(cmipDir,"/assessment/output-data",sep="")
out_sum <- paste(outputDD,"/_summary_revised2",sep="")

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
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    #read file, mean climate skill (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, mean climate skill (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    if (file.exists(infil)) {
      tmp_dat <- read.csv(infil)
      indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    }
    
    #read file, mean climate skill (WCL)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl_rev2-WCL/",vn,"_",gcm,"_",ens,".csv",sep="")
    if (file.exists(infil)) {
      tmp_dat <- read.csv(infil)
      indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    }
    
    #final dataset to calculate specific values, mean climate skill
    indat_av <- aggregate(indat[,3:ncol(indat)],by=list(indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av)[1] <- c("SEASON")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    indat_av <- cbind(ISO=iso,indat_av)
    indat_av$N <- NULL
    
    #binding data, mean climate skill
    all_iso_cl <- rbind(all_iso_cl,indat_av)
    
    ###
    #read file, interannual vi (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi_rev-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, interannual vi (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi_rev-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    if (file.exists(infil)) {indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))}
    
    #final dataset to calculate specific values, interannual vi
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av) <- c("CELL","SEASON","VI")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat_av)
  }
  
  all_iso_cl <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_cl)
  all_iso_vi <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_vi)
  
  ccoef <- all_iso_cl$CCOEF2[which(!is.na(all_iso_cl$CCOEF2))]
  p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
  
  prmse1 <- all_iso_cl$PRMSE1[which(!is.na(all_iso_cl$PRMSE1))]
  p_prmse140 <- length(which(prmse1 >= 40)) / length(prmse1)*100
  p_prmse190 <- length(which(prmse1 >= 90)) / length(prmse1)*100
  
  prmse3 <- all_iso_cl$PRMSE3[which(!is.na(all_iso_cl$PRMSE3))]
  p_prmse340 <- length(which(prmse3 >= 40)) / length(prmse3)*100
  p_prmse390 <- length(which(prmse3 >= 90)) / length(prmse3)*100
  
  via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
  p_via05 <- length(which(via > 0.5)) / length(via)*100
  
  res_met <- data.frame(GCM=gcm,ENS=ens,VAR=vn,P_CCOEF_B05=p_ccoef05,
                        PRMSE1_A40=p_prmse140,PRMSE1_A90=p_prmse190,
                        PRMSE3_A40=p_prmse340,PRMSE3_A90=p_prmse390,VIA_A05=p_via05)
  
  return(res_met)
}


if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
  out_all <- lapply(1:nrow(procList),summarise_proc)
  out_all2 <- do.call("rbind",out_all)
  write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
} else {
  out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
}

out_all3 <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
names(out_all3)[1:2] <- c("GCM","VAR")

out_all_x <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=max,na.rm=T)
out_all_n <- aggregate(out_all2[,4:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=min,na.rm=T)

out_all3$P_CCOEF_B05_X <- out_all_x$P_CCOEF_B05
out_all3$P_CCOEF_B05_N <- out_all_n$P_CCOEF_B05
out_all3$P_PRMSE1_A40_X <- out_all_x$PRMSE1_A40
out_all3$P_PRMSE1_A40_N <- out_all_n$PRMSE1_A40
out_all3$P_PRMSE1_A90_X <- out_all_x$PRMSE1_A50
out_all3$P_PRMSE1_A90_N <- out_all_n$PRMSE1_A50
out_all3$P_PRMSE3_A40_X <- out_all_x$PRMSE3_A40
out_all3$P_PRMSE3_A40_N <- out_all_n$PRMSE3_A40
out_all3$P_PRMSE3_A90_X <- out_all_x$PRMSE3_A50
out_all3$P_PRMSE3_A90_N <- out_all_n$PRMSE3_A50
out_all3$VIA_A05_X <- out_all_x$VIA_A05
out_all3$VIA_A05_N <- out_all_n$VIA_A05

if (!file.exists(paste(out_sum,"/all-plot_data.csv",sep=""))) {
  write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)
} else {
  out_all3 <- read.csv(paste(out_sum,"/all-plot_data.csv",sep=""))
}

#####################################################################
#### with out_all3 of CMIP5 and that of CMIP3, scattergrams need 
#    to be constructed that compare them



######################################################################3
############ CMIP3

#list of gcms
gcmList <- list.files(cmip3Dir)
vnList <- c("prec","tmean","dtr")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#list of gcms and regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#data directory
outputDD <- paste(cmipDir,"/assessment/output-data-cmip3",sep="")
out_sum <- paste(outputDD,"/_summary",sep="")

#this_proc <- 1

#function to do lapply
summarise_proc <- function(this_proc) {
  #details of what I need
  gcm <- paste(procList$GCM[this_proc])
  vn <- procList$VAR[this_proc]
  
  #load all datasets, countries and seasons mean climate skill
  all_iso_cl <- data.frame()
  all_iso_vi <- data.frame()
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    #read file, mean climate skill (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-CRU/",vn,"_",gcm,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, mean climate skill (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WST/",vn,"_",gcm,".csv",sep="")
    tmp_dat <- read.csv(infil)
    indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    
    #read file, mean climate skill (WCL)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WCL/",vn,"_",gcm,".csv",sep="")
    tmp_dat <- read.csv(infil)
    indat <- rbind(indat,cbind(DSET="WST",tmp_dat))
    
    #final dataset to calculate specific values, mean climate skill
    indat_av <- aggregate(indat[,3:ncol(indat)],by=list(indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av)[1] <- c("SEASON")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    indat_av <- cbind(ISO=iso,indat_av)
    indat_av$N <- NULL
    
    #binding data, mean climate skill
    all_iso_cl <- rbind(all_iso_cl,indat_av)
    
    ###
    #read file, interannual vi (CRU)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi-CRU/",vn,"_",gcm,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, interannual vi (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/vi-WST/",vn,"_",gcm,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
    
    #final dataset to calculate specific values, interannual vi
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av) <- c("CELL","SEASON","VI")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat_av)
  }
  
  all_iso_cl <- cbind(GCM=gcm,VAR=vn,all_iso_cl)
  all_iso_vi <- cbind(GCM=gcm,VAR=vn,all_iso_vi)
  
  ccoef <- all_iso_cl$CCOEF2[which(!is.na(all_iso_cl$CCOEF2))]
  p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
  
  prmse1 <- all_iso_cl$PRMSE1[which(!is.na(all_iso_cl$PRMSE1))]
  p_prmse140 <- length(which(prmse1 >= 40)) / length(prmse1)*100
  p_prmse190 <- length(which(prmse1 >= 90)) / length(prmse1)*100
  
  prmse3 <- all_iso_cl$PRMSE3[which(!is.na(all_iso_cl$PRMSE3))]
  p_prmse340 <- length(which(prmse3 >= 40)) / length(prmse3)*100
  p_prmse390 <- length(which(prmse3 >= 90)) / length(prmse3)*100
  
  via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
  p_via05 <- length(which(via > 0.5)) / length(via)*100
  
  res_met <- data.frame(GCM=gcm,VAR=vn,P_CCOEF_B05=p_ccoef05,
                        PRMSE1_A40=p_prmse140,PRMSE1_A90=p_prmse190,
                        PRMSE3_A40=p_prmse340,PRMSE3_A90=p_prmse390,VIA_A05=p_via05)
  
  return(res_met)
}


if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
  out_all <- lapply(1:nrow(procList),summarise_proc)
  out_all2 <- do.call("rbind",out_all)
  write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
} else {
  out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
}

out_all3 <- aggregate(out_all2[,3:ncol(out_all2)],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
names(out_all3)[1:2] <- c("GCM","VAR")

if (!file.exists(paste(out_sum,"/all-plot_data.csv",sep=""))) {
  write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)
} else {
  out_all3 <- read.csv(paste(out_sum,"/all-plot_data.csv",sep=""))
}


##################################################################################
##################################################################################
#produce a plot where three things are shown: PRMSEM > 40, CCOEF2 > 0.5, VI > 0.5
library(ggplot2)

#data directory
cmip5Dir <- paste(cmipDir,"/assessment/output-data/_summary_revised2",sep="")
cmip3Dir <- paste(cmipDir,"/assessment/output-data-cmip3/_summary",sep="")

#load skill metrics
c5_mets <- read.csv(paste(cmip5Dir,"/all-plot_data.csv",sep=""))
c3_mets <- read.csv(paste(cmip3Dir,"/all-plot_data.csv",sep=""))

#sub-selecting variables
pr_c5 <- c5_mets[which(c5_mets$VAR == "pr"),]
tas_c5 <- c5_mets[which(c5_mets$VAR == "tas"),]
dtr_c5 <- c5_mets[which(c5_mets$VAR == "dtr"),]
rd_c5 <- c5_mets[which(c5_mets$VAR == "rd"),]

pr_c3 <- c3_mets[which(c3_mets$VAR == "prec"),]
tas_c3 <- c3_mets[which(c3_mets$VAR == "tmean"),]
dtr_c3 <- c3_mets[which(c3_mets$VAR == "dtr"),]

#gcm names
c5_gcms <- c("bcc_csm1_1","bnu_esm","cancm4","canesm2","cnrm_cm5","csiro_access10","csiro_mk360",
             "ichec_ec_earth","inmcm4","ipsl_cm5a_lr","ipsl_cm5a_mr","ipsl_cm5b_lr","miroc_esm",
             "miroc_esm_chem","miroc4h","miroc5","hadcm3","hadgem2_cc","hadgem2_es",
             "mpi_esm_lr","mpi_esm_mr","mri_cgcm3","MMM","ncar_ccsm4","noresm1_m",
             "gfdl_esm2g","gfdl_esm2m")

c3_gcms <- c("bccr_bcm2_0","cgcm31_t47","cgcm31_t63","cnrm_cm3","csiro_mk30","csiro_mk35",
             "gfdl_cm20","gfdl_cm21","giss_aom","giss_modeleh","giss_modeler","fgoals10_g",
             "echam4","inm_cm30","ipsl_cm4","miroc32_hires","miroc32_medres","miub_echog",
             "echam5","mri_cgcm232a","MMM","ncar_ccsm30","ncar_pcm1","hadcm3",
             "hadgem1")

pr_c5$GCM <- c5_gcms
pr_c3$GCM <- c3_gcms

x11()
ggplot(pr_c5,aes(P_CCOEF_B05, P_CCOEF_A08)) + 
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
dev.off()

