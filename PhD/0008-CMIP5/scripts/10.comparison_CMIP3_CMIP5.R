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
cmip5Dir <- "/mnt/a102/eejarv/CMIP5"
cmip3Dir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"

# #eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
# cmipDir <- "/nfs/a102/eejarv/CMIP5"
# cmip3Dir <- "/nfs/a17/eejarv/IPCC_CMIP3/20C3M/original-data"


######################################################################3
############ CMIP5

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))

#list of variables and datasets
vnList <- c("pr","tas","dtr","rd")
dsetList <- c("cl_rev2-CRU","cl_rev2-WST","cl_rev2-WCL")

#list of gcms and regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#process list
vn <- "pr"
seas <- "ANN"
metric <- "CCOEF2"
procList <- expand.grid(GCM=gcmList)

#data directory
outputDD <- paste(cmip5Dir,"/assessment/output-data",sep="")
out_sum <- paste(outputDD,"/_summary_revised2",sep="")

#this_proc <- 1

#function to do lapply
#summarise_proc <- function(this_proc) {
for (this_proc in 1:nrow(procList)) {
  #details of what I need
  #this_proc <- 2
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  
  cat(gcm,"/",ens,"\n")
  
  #load all datasets, countries and seasons mean climate skill
  all_iso <- data.frame()
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    for (dset in dsetList) {
      #dset <- dsetList[1]
      #read file, mean climate skill (CRU)
      infil <- paste(outputDD,"/",reg,"/",iso,"/",dset,"/",vn,"_",gcm,"_",ens,".csv",sep="")
      if (file.exists(infil)) {
        indat <- read.csv(infil)
        indat <- indat[which(indat$SEASON == seas),metric]
        indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=indat)
        #names(indat)[ncol(indat)] <- paste(gcm,"_ENS_",ens,sep="")
      } else {
        indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=NA)
      }
      all_iso <- rbind(all_iso,indat)
    }
  }
  
  #binding data as columns
  if (this_proc == 1) {
    gcm_vals <- all_iso
  } else {
    gcm_vals <- cbind(gcm_vals,VALUE=all_iso[,ncol(all_iso)])
  }
  names(gcm_vals)[ncol(gcm_vals)] <- paste(gcm,"_ENS_",ens,sep="")
}

alldata <- as.numeric(unlist(c(as.vector(gcm_vals[,4:ncol(gcm_vals)]))))
brks <- seq(-1,1,by=0.05)

#compute histograms for all parameter sets
for (this_proc in 1:nrow(procList)) {
  #this_proc <- 1
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  
  his_gcm <- hist(gcm_vals[,paste(gcm,"_ENS_",ens,sep="")],breaks=brks,plot=F)
  hgcm <- his_gcm$counts/sum(his_gcm$counts)*100
  hgcm <- data.frame(XVAL=his_gcm$mids,VAL=hgcm)
  names(hgcm)[2] <- paste(gcm,"_ENS_",ens,sep="")
  
  
  if (this_proc == 1) {
    hc5_all <- hgcm
  } else {
    hc5_all <- merge(hc5_all,hgcm,by="XVAL",sort=F)
  }
}

xval <- hc5_all$XVAL
hc5_all$XVAL <- NULL; hc3_all$XVAL <- NULL

hc5_m <- apply(hc5_all[,1:(ncol(hc5_all)-1)],1,mean,na.rm=T)
hc5_sd <- apply(hc5_all[,1:(ncol(hc5_all)-1)],1,sd,na.rm=T)

hc3_m <- apply(hc3_all[,1:(ncol(hc3_all)-1)],1,mean,na.rm=T)
hc3_sd <- apply(hc3_all[,1:(ncol(hc3_all)-1)],1,sd,na.rm=T)

hsum <- data.frame(XVAL=xval,CTR.MEAN=hc5_m,CTR.SD=hc5_m)#,CON.MEAN=hcon_m,CON.SD=hcon_sd)

#produce the plot
tiff(paste(fig_dir,"/effect_plot_",const,".tif",sep=""),res=300,height=1500,width=2048,
     compression="lzw",pointsize=8.5)
par(mar=c(5,5,1,1))

plot(hsum$XVAL,hsum$CTR.MEAN,ty="l",main=NA,xlab="Yield (kg/ha)",ylab="pdf (%)",
     xlim=c(0,5000),ylim=c(0,15))
polup <- hsum$CTR.MEAN+hsum$CTR.SD
poldw <- hsum$CTR.MEAN-hsum$CTR.SD
poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})

polygon(x=c(hsum$XVAL,rev(hsum$XVAL)),y=c(polup,rev(poldw)),col="#FF000040",border=NA)
lines(hsum$XVAL,hsum$CTR.MEAN,col="red")

polup <- hsum$CON.MEAN+hsum$CON.SD
poldw <- hsum$CON.MEAN-hsum$CON.SD
poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})

polygon(x=c(hsum$XVAL,rev(hsum$XVAL)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
lines(hsum$XVAL,hsum$CON.MEAN,col="blue")
grid()
legend(x=3800,y=14.4,legend=c("Control","Not constrained"),
       col=c("red","blue"),lty=c(1,1),cex=1.2,
       box.col="black",box.lwd=1,box.lty=1,bg="white")
dev.off()


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
                        PRMSE3_A40=p_prmse340,PRMSE3_A90=p_prmse390)
  
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



