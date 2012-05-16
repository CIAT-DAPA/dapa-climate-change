#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Open the CMIP3 model data
wd <- "Z:/PhD-work/data-quality-study"
ccDir <- paste(wd,"/climate-comparison",sep="")
oDir <- paste(ccDir,"/results/DSSAT",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#load cmip3 outputs
if (!file.exists(paste(oDir,"/cmip3.csv",sep=""))) {
  cmip3Dir <- paste(ccDir,"/CMIP3_GJ",sep="")
  gcmList <- list.files(cmip3Dir,pattern="\\.csv")
  for (gcm in gcmList) {
    gcmData <- read.csv(paste(cmip3Dir,"/",gcm,sep=""))
    gcmID <- gsub("\\.csv","",gcm)
    if (gcm == gcmList[1]) {
      c3_all <- gcmData
    } else {
      c3_all <- rbind(c3_all,gcmData)
    }
  }
  write.csv(c3_all,paste(oDir,"/cmip3.csv",sep=""),quote=T,row.names=F)
} else {
  c3_all <- read.csv(paste(oDir,"/cmip3.csv",sep=""))
}


#load the cmip5 data
cmip5Dir <- paste(ccDir,"/CMIP5_GJ",sep="")
gcmList <- list.files(cmip5Dir)

if (!file.exists(paste(oDir,"/cmip5.csv",sep=""))) {
  for (gcm in gcmList) {
    cat("processing",gcm,"\n")
    gcmDir <- paste(cmip5Dir,"/",gcm,sep="")
    for (yr in 1966:1989) {
      wList <- list.files(gcmDir,pattern=paste("_",yr,".csv",sep=""))
      for (fn in wList) {
        gcmData <- read.csv(paste(gcmDir,"/",fn,sep=""))
        vn <- names(gcmData)[7]
        if (vn == "pr") {
          ovn <- "prec"
          m6 <- sum(gcmData$pr[which(gcmData$MONTH == "6")])
          m7 <- sum(gcmData$pr[which(gcmData$MONTH == "7")])
          m8 <- sum(gcmData$pr[which(gcmData$MONTH == "8")])
          m9 <- sum(gcmData$pr[which(gcmData$MONTH == "9")])
        } else if (vn == "tasmin") {
          ovn <- "tmin"
          m6 <- mean(gcmData$tasmin[which(gcmData$MONTH == "6")])
          m7 <- mean(gcmData$tasmin[which(gcmData$MONTH == "7")])
          m8 <- mean(gcmData$tasmin[which(gcmData$MONTH == "8")])
          m9 <- mean(gcmData$tasmin[which(gcmData$MONTH == "9")])
        } else if (vn == "tasmax") {
          ovn <- "tmax"
          m6 <- mean(gcmData$tasmax[which(gcmData$MONTH == "6")])
          m7 <- mean(gcmData$tasmax[which(gcmData$MONTH == "7")])
          m8 <- mean(gcmData$tasmax[which(gcmData$MONTH == "8")])
          m9 <- mean(gcmData$tasmax[which(gcmData$MONTH == "9")])
        }
        
        out_row <- data.frame(GCM=gcm,YEAR=yr,VAR=ovn,M6=m6,M7=m7,M8=m8,M9=m9)
        
        if (fn == wList[1]) {
          out_all <- out_row
        } else {
          out_all <- rbind(out_all,out_row)
        }
        
      }
      
      if (yr == 1966) {
        yr_all <- out_all
      } else {
        yr_all <- rbind(yr_all,out_all)
      }
    }
    
    if (gcm == gcmList[1]) {
      c5_all <- yr_all
    } else {
      c5_all <- rbind(c5_all,yr_all)
    }
  }
  write.csv(c5_all,paste(oDir,"/cmip5.csv",sep=""),quote=T,row.names=F)
} else {
  c5_all <- read.csv(paste(oDir,"/cmip5.csv",sep=""))
}


#load observed data
if (!file.exists(paste(oDir,"/obs.csv",sep=""))) {
  obs_prec <- read.csv(paste(ccDir,"/experiments/EcoCrop/prec_p_seasonal/climate_prec_p_seasonal_0.csv",sep=""))
  obs_prec <- obs_prec[which(obs_prec$EXP == "prec_p-0_s-997"),]
  obs_prec$EXP <- NULL; obs_prec$SC <- NULL
  
  obs_tmin <- read.csv(paste(ccDir,"/experiments/EcoCrop/tmin_p_seasonal/climate_tmin_p_seasonal_0.csv",sep=""))
  obs_tmin <- obs_tmin[which(obs_tmin$EXP == "tmin_p-0_s-997"),]
  obs_tmin$EXP <- NULL; obs_tmin$SC <- NULL
  
  obs_tmean <- read.csv(paste(ccDir,"/experiments/EcoCrop/tmean_p_seasonal/climate_tmean_p_seasonal_0.csv",sep=""))
  obs_tmean <- obs_tmean[which(obs_tmean$EXP == "tmean_p-0_s-997"),]
  obs_tmean$EXP <- NULL; obs_tmean$SC <- NULL
  
  obs_all <- rbind(obs_prec,obs_tmean,obs_tmin)
  obs_all <- cbind(DATASET="OBS",obs_all)
  names(obs_all)[2] <- c("VAR")
  
  write.csv(obs_all,paste(oDir,"/obs.csv",sep=""),quote=T,row.names=F)
} else {
  obs_all <- read.csv(paste(oDir,"/obs.csv",sep=""))
}


######################################################################
#shuffled experiments data
xpDir <- paste(ccDir,"/experiments/EcoCrop",sep="")

if (!file.exists(paste(oDir,"/shuffled.csv",sep=""))) {
  expList <- list.files(xpDir,pattern="_s_")
  for (xp in expList) {
    cat(xp,"\n")
    xpData <- read.csv(paste(xpDir,"/",xp,"/climate_",xp,".csv",sep=""))
    
    if (xp == expList[1]) {
      sh_all <- xpData
    } else {
      sh_all <- rbind(sh_all,xpData)
    }
  }
  names(sh_all)[2] <- "VAR"
  write.csv(sh_all,paste(oDir,"/shuffled.csv",sep=""),quote=T,row.names=F)
} else {
  sh_all <- read.csv(paste(oDir,"/shuffled.csv",sep=""))
}


######################################################################
#perturbed experiments data
#shuffled experiments data
xpDir <- paste(ccDir,"/experiments/EcoCrop",sep="")

if (!file.exists(paste(oDir,"/perturbed.csv",sep=""))) {
  expList <- list.files(xpDir,pattern="_p_")
  for (xp in expList) {
    cat(xp,"\n")
    #if (file.exists(paste(xpDir,"/",xp,"/climate_",xp,"_all.csv",sep=""))) {
    xpData <- read.csv(paste(xpDir,"/",xp,"/climate_",xp,"_all.csv",sep=""))
    #} else {
    #  cat("didnt exist\n")
    #}
    names(xpData) <- c("EXP","VAR","SCALE","M6","M7","M8","M9")
    
    #selrw <- sample(1:nrow(xpData),size=(nrow(xpData)*.15))
    #xpData <- xpData[selrw,]
    
    if (xp == expList[1]) {
      pt_all <- xpData
    } else {
      pt_all <- rbind(pt_all,xpData)
    }
  }
  write.csv(pt_all,paste(oDir,"/perturbed.csv",sep=""),quote=T,row.names=F)
} else {
  pt_all <- read.csv(paste(oDir,"/perturbed.csv",sep=""))
}


#which experiment is closest to the max/min in GCMs
maxc3 <- max(c3_all$M6,subset= c3_all$VAR == "prec")
minc3 <- min(c3_all$M6,subset= c3_all$VAR == "prec")

maxc5 <- max(c5_all$M6,subset= c5_all$VN == "prec")
minc5 <- min(c5_all$M6,subset= c5_all$VN == "prec")




#merge everything (remove exp fields)
sh_all <- cbind(DATASET="SH",sh_all)
sh_all$EXP <- NULL; sh_all$SCALE <- NULL

pvals <- unlist(strsplit(paste(pt_all$EXP),"_",fixed=T))[seq(2,nrow(pt_all)*3,by=3)]
pt_all$PVAL <- pvals
pt_all$PVAL <- as.numeric(gsub("p-","",pt_all$PVAL))
pt_all <- pt_all[which(pt_all$PVAL<120),]
pt_all <- cbind(DATASET="PT",pt_all)
pt_all$EXP <- NULL; pt_all$SCALE <- NULL; pt_all$PVAL <- NULL

c3_all <- cbind(DATASET="C3",c3_all)
c3_all$GCM <- NULL; c3_all$YEAR <- NULL

#c5_all <- cbind(DATASET="C5",c5_all)
#c5_all$GCM <- NULL; c5_all$YEAR <- NULL

obs_all$YEAR <- NULL

all_data <- rbind(sh_all,pt_all,c3_all,obs_all)


#PRECIPITATION
prec_data <- all_data[which(all_data$VAR == "prec"),]
m6 <- prec_data[,1:2]
m6$MTH <- 6
m6$PREC <- prec_data$M6

m7 <- prec_data[,1:2]
m7$MTH <- 7
m7$PREC <- prec_data$M7

m8 <- prec_data[,1:2]
m8$MTH <- 8
m8$PREC <- prec_data$M8

m9 <- prec_data[,1:2]
m9$MTH <- 9
m9$PREC <- prec_data$M9

prec_data <- rbind(m6,m7,m8,m9)
#windows(); boxplot(prec_data$PREC ~ prec_data$DATASET*prec_data$MTH,pch=20,pch=0.15,ylim=c(0,600))
nms <- expand.grid(c("SH","PT","C3","OB"),c("JUN","JUL","AUG","SEP"))
nms <- paste(nms$Var1) #,"\n(",nms$Var2,")",sep="")

#output figure (temporal)
tifName <- paste(oDir,"/prec_v2.tif",sep="")
tiff(tifName,res=300,pointsize=7.5,width=1200,height=800,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=0.6,lwd=0.65,font.lab=20,font.axis=20)
boxplot(prec_data$PREC ~ prec_data$DATASET*prec_data$MTH,pch=20,pch=0.15,
        col="grey80",names=nms,font.lab=20,font.axis=20,
        xlab=NA,ylab="Precipitation (mm/month)",ylim=c(0,320))
abline(v=4.5,lty=1,col="black",lwd=1.5); 
abline(v=8.5,lty=1,col="black",lwd=1.5);
abline(v=12.5,lty=1,col="black",lwd=1.5);
text(x=1,y=310,labels="JUN",cex=2,font=20,vfont=NULL)
text(x=5.25,y=310,labels="JUL",cex=2,font=20,vfont=NULL)
text(x=9.25,y=310,labels="AUG",cex=2,font=20,vfont=NULL)
text(x=13.25,y=310,labels="SEP",cex=2,font=20,vfont=NULL)
grid()
dev.off()



#######################################
#MIN. TEMPERATURE
tmin_data <- all_data[which(all_data$VAR == "tmin"),]
m6 <- tmin_data[,1:2]
m6$MTH <- 6
m6$TMIN <- tmin_data$M6

m7 <- tmin_data[,1:2]
m7$MTH <- 7
m7$TMIN <- tmin_data$M7

m8 <- tmin_data[,1:2]
m8$MTH <- 8
m8$TMIN <- tmin_data$M8

m9 <- tmin_data[,1:2]
m9$MTH <- 9
m9$TMIN <- tmin_data$M9

tmin_data <- rbind(m6,m7,m8,m9)
tmin_data$TMIN[which(tmin_data$DATASET != "C3")] <- tmin_data$TMIN[which(tmin_data$DATASET != "C3")] * 0.1

nms <- expand.grid(c("SH","PT","C3","OB"),c("JUN","JUL","AUG","SEP"))
nms <- paste(nms$Var1) #,"\n(",nms$Var2,")",sep="")

#output figure (temporal)
tifName <- paste(oDir,"/tmin_v2.tif",sep="")
tiff(tifName,res=300,pointsize=7.5,width=1200,height=800,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=0.6,lwd=0.65,font.lab=20,font.axis=20)
boxplot(tmin_data$TMIN ~ tmin_data$DATASET*tmin_data$MTH,pch=20,
        col="grey80",names=nms,font.lab=20,font.axis=20,
        xlab=NA,ylab="Temperature (Celsius)",ylim=c(18,32))
abline(v=4.5,lty=1,col="black",lwd=1.5); 
abline(v=8.5,lty=1,col="black",lwd=1.5);
abline(v=12.5,lty=1,col="black",lwd=1.5);
text(x=1,y=31,labels="JUN",cex=2,font=20,vfont=NULL)
text(x=5.25,y=31,labels="JUL",cex=2,font=20,vfont=NULL)
text(x=9.25,y=31,labels="AUG",cex=2,font=20,vfont=NULL)
text(x=13.25,y=31,labels="SEP",cex=2,font=20,vfont=NULL)
grid()
dev.off()





#######################################
#MEAN. TEMPERATURE
tmean_data <- all_data[which(all_data$VAR == "tmean"),]
m6 <- tmean_data[,1:2]
m6$MTH <- 6
m6$TMEAN <- tmean_data$M6

m7 <- tmean_data[,1:2]
m7$MTH <- 7
m7$TMEAN <- tmean_data$M7

m8 <- tmean_data[,1:2]
m8$MTH <- 8
m8$TMEAN <- tmean_data$M8

m9 <- tmean_data[,1:2]
m9$MTH <- 9
m9$TMEAN <- tmean_data$M9

tmean_data <- rbind(m6,m7,m8,m9)
tmean_data$TMEAN[which(tmean_data$DATASET != "C3")] <- tmean_data$TMEAN[which(tmean_data$DATASET != "C3")] * 0.1

nms <- expand.grid(c("SH","PT","C3","OB"),c("JUN","JUL","AUG","SEP"))
nms <- paste(nms$Var1) #,"\n(",nms$Var2,")",sep="")

#output figure (temporal)
tifName <- paste(oDir,"/tmean_v2.tif",sep="")
tiff(tifName,res=300,pointsize=7.5,width=1200,height=800,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=0.6,lwd=0.65,font.lab=20,font.axis=20)
boxplot(tmean_data$TMEAN ~ tmean_data$DATASET*tmean_data$MTH,pch=20,
        col="grey80",names=nms,font.lab=20,font.axis=20,
        xlab=NA,ylab="Temperature (Celsius)",ylim=c(18,32))
abline(v=4.5,lty=1,col="black",lwd=1.5); 
abline(v=8.5,lty=1,col="black",lwd=1.5);
abline(v=12.5,lty=1,col="black",lwd=1.5);
text(x=1,y=31,labels="JUN",cex=2,font=20,vfont=NULL)
text(x=5.25,y=31,labels="JUL",cex=2,font=20,vfont=NULL)
text(x=9.25,y=31,labels="AUG",cex=2,font=20,vfont=NULL)
text(x=13.25,y=31,labels="SEP",cex=2,font=20,vfont=NULL)
grid()
dev.off()

