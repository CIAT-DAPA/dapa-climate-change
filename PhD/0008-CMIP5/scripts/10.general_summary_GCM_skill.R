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
out_sum <- paste(outputDD,"/_summary",sep="")

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
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, mean climate skill (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
    
    #read file, mean climate skill (WCL)
    infil <- paste(outputDD,"/",reg,"/",iso,"/cl-WCL/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WCL",read.csv(infil)))
    
    #final dataset to calculate specific values, mean climate skill
    indat_av <- aggregate(indat[,3:8],by=list(indat$SEASON),FUN=mean,na.rm=T)
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
    
    #final dataset to calculate specific values, mean climate skill
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,"VI"],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av) <- c("CELL","SEASON","VI")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat_av)
    
    ###
    #read file, interannual
    infil <- paste(outputDD,"/",reg,"/",iso,"/ts-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",indat)
    
    #read file, interannual (WST)
    infil <- paste(outputDD,"/",reg,"/",iso,"/ts-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- rbind(indat,cbind(DSET="WST",read.csv(infil)))
    
    #final dataset to calculate specific values, mean climate skill
    indat$LON <- NULL; indat$LAT <- NULL
    indat_av <- aggregate(indat[,4:8],by=list(indat$CELL,indat$SEASON),FUN=mean,na.rm=T)
    names(indat_av)[1:2] <- c("CELL","SEASON")
    indat_av <- indat_av[which(indat_av$SEASON != "ANN"),]
    
    #binding data, interannual vi
    all_iso_ia <- rbind(all_iso_ia,indat_av)
  }
  
  all_iso_cl <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_cl)
  all_iso_vi <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_vi)
  all_iso_ia <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_ia)
  
  ccoef <- all_iso_cl$CCOEF[which(!is.na(all_iso_cl$CCOEF))]
  p_ccoef05 <- length(which(ccoef <= 0.5)) / length(ccoef)*100
  p_ccoef08 <- length(which(ccoef >= 0.8)) / length(ccoef)*100
  
  via <- all_iso_vi$VI[which(!is.na(all_iso_vi$VI))]
  p_via05 <- length(which(via > 0.5)) / length(via)*100
  
  mbr <- all_iso_ia$MBR[which(!is.na(all_iso_ia$MBR))]
  r_blue_red <- length(which(mbr < 1)) / length(which(mbr > 1))
  
  res_met <- data.frame(GCM=gcm,ENS=ens,VAR=vn,P_CCOEF_B05=p_ccoef05,
                        P_CCOEF_A08=p_ccoef08,P_VIA_A05=p_via05,BLUE_TO_RED=r_blue_red)
  
  return(res_met)
}


if (!file.exists(paste(out_sum,"/all-final_summary.csv",sep=""))) {
  out_all <- lapply(1:nrow(procList),summarise_proc)
  out_all2 <- do.call("rbind",out_all)
  write.csv(out_all2,paste(out_sum,"/all-final_summary.csv",sep=""),quote=F,row.names=F)
} else {
  out_all2 <- read.csv(paste(out_sum,"/all-final_summary.csv",sep=""))
}

out_all3 <- aggregate(out_all2[,4:7],by=list(out_all2$GCM,out_all2$VAR),FUN=mean,na.rm=T)
names(out_all3)[1:2] <- c("GCM","VAR")

out_all_x <- aggregate(out_all2[,4:7],by=list(out_all2$GCM,out_all2$VAR),FUN=max,na.rm=T)
out_all_n <- aggregate(out_all2[,4:7],by=list(out_all2$GCM,out_all2$VAR),FUN=min,na.rm=T)

out_all3$P_CCOEF_B05_X <- out_all_x$P_CCOEF_B05
out_all3$P_CCOEF_B05_N <- out_all_n$P_CCOEF_B05
out_all3$P_CCOEF_A08_X <- out_all_x$P_CCOEF_A08
out_all3$P_CCOEF_A08_N <- out_all_n$P_CCOEF_A08
out_all3$P_VIA_A05_X <- out_all_x$P_VIA_A05
out_all3$P_VIA_A05_N <- out_all_n$P_VIA_A05
out_all3$BLUE_TO_RED_X <- out_all_x$BLUE_TO_RED
out_all3$BLUE_TO_RED_N <- out_all_n$BLUE_TO_RED


write.csv(out_all3,paste(out_sum,"/all-plot_data.csv",sep=""),quote=F,row.names=F)

pr_plot <- out_all3[which(out_all3$VAR == "pr"),]
tas_plot <- out_all3[which(out_all3$VAR == "tas"),]
dtr_plot <- out_all3[which(out_all3$VAR == "dtr"),]

ratio <- 300/72

#bubble plot
library(ggplot2)
tiff(paste(out_sum,"/figures/pr_summary_model_skill.tif",sep=""),compression="lzw",
     units="px",width=800*ratio,height=600*ratio,res=300)
ggplot(pr_plot, aes(P_CCOEF_B05, P_CCOEF_A08)) + 
  geom_point(aes(size =  P_VIA_A05),colour="white", fill="grey 40", shape=21) + 
  scale_area(range=c(2,15),name="VI>0.5 (%)") +
  geom_errorbar(aes(ymin = P_CCOEF_A08_N, ymax = P_CCOEF_A08_X), width=0.07) +
  geom_errorbarh(aes(xmin = P_CCOEF_B05_N, xmax = P_CCOEF_B05_X), height=0.5) +
  scale_x_continuous("Low correlations ratio (%)", limits = c(0, 6.5), breaks=seq(0, 6.5, by = 1)) + 
  scale_y_continuous("High correlations ratio (%)", limits = c(60, 100), breaks=seq(60, 100, by = 5)) +
  theme_bw() +
  opts(axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       legend.text = theme_text(size=12),
       legend.key = theme_blank()) + # switch off the rectangle around symbols in the legend
  geom_text(x=pr_plot$P_CCOEF_B05[1]-0.3,y=pr_plot$P_CCOEF_A08[1],label=pr_plot$GCM[1],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[2]-0.4,y=pr_plot$P_CCOEF_A08[2],label=pr_plot$GCM[2],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[3]+0.4,y=pr_plot$P_CCOEF_A08[3]+1.2,label=pr_plot$GCM[3],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[4]+0.5,y=pr_plot$P_CCOEF_A08[4]+1.2,label=pr_plot$GCM[4],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[5]-0.4,y=pr_plot$P_CCOEF_A08[5],label=pr_plot$GCM[5],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[6]+0.35,y=pr_plot$P_CCOEF_A08[6]+.9,label=pr_plot$GCM[6],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[7]-0.35,y=pr_plot$P_CCOEF_A08[7]+.3,label=pr_plot$GCM[7],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[8]+0.4,y=pr_plot$P_CCOEF_A08[8]+1.1,label=pr_plot$GCM[8],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[9]+0.4,y=pr_plot$P_CCOEF_A08[9]+1.1,label=pr_plot$GCM[9],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[10]-0.45,y=pr_plot$P_CCOEF_A08[10],label=pr_plot$GCM[10],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[11]-0.55,y=pr_plot$P_CCOEF_A08[11],label=pr_plot$GCM[11],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[12]-0.3,y=pr_plot$P_CCOEF_A08[12]-0.3,label=pr_plot$GCM[12],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[13]-0.35,y=pr_plot$P_CCOEF_A08[13],label=pr_plot$GCM[13],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[14],y=pr_plot$P_CCOEF_A08[14]-1,label=pr_plot$GCM[14],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[15]-0.5,y=pr_plot$P_CCOEF_A08[15],label=pr_plot$GCM[15],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[16]-0.5,y=pr_plot$P_CCOEF_A08[16],label=pr_plot$GCM[16],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[17]+0.4,y=pr_plot$P_CCOEF_A08[17]+.8,label=pr_plot$GCM[17],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[18],y=pr_plot$P_CCOEF_A08[18]-1,label=pr_plot$GCM[18],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[19]+0.2,y=pr_plot$P_CCOEF_A08[19]-0.5,label=pr_plot$GCM[19],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[20],y=pr_plot$P_CCOEF_A08[20]-0.9,label=pr_plot$GCM[20],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[21]+0.4,y=pr_plot$P_CCOEF_A08[21]+.9,label=pr_plot$GCM[21],size=3)+
  geom_text(x=pr_plot$P_CCOEF_B05[22],y=pr_plot$P_CCOEF_A08[22]-1.2,label=pr_plot$GCM[22],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[23]+0.3,y=pr_plot$P_CCOEF_A08[23],label=pr_plot$GCM[23],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[24]-0.4,y=pr_plot$P_CCOEF_A08[24],label=pr_plot$GCM[24],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[25],y=pr_plot$P_CCOEF_A08[25]+1.3,label=pr_plot$GCM[25],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[26]+0.5,y=pr_plot$P_CCOEF_A08[26],label=pr_plot$GCM[26],size=3) +
  geom_text(x=pr_plot$P_CCOEF_B05[27]+0.5,y=pr_plot$P_CCOEF_A08[27],label=pr_plot$GCM[27],size=3,colour="red") 
dev.off()









