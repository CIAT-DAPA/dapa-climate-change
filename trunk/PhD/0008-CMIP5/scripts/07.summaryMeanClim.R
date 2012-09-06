#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))


#list of gcms and countries/regions
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:3,GCM=c("pr","tas","dtr"),WCL=c("prec","tmean","dtr"),
                     CL_CRU=c("prec","tmean","dtr"),TS_CRU=c("pre","tmp","dtr"),
                     E40=c("prec","tasm",NA),CL_WST=c("rain","tean","dtr"),
                     TS_WST=c("pr","tas","dtr"))

#scaling factors to datasets per variable
scList <- data.frame(VID=1:3,GCM=c(1,1,1),WCL=c(1,1,1),
                     CL_CRU=c(1,1,1),TS_CRU=c(0.1,0.1,0.1),
                     E40=c(1,1,NA),CL_WST=c(1,1,1),
                     TS_WST=c(1,0.1,0.1))


#processes to complete
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
isoList <- regions$ISO

#### the below line was commented for NCC revised version
#dsetList <- c("cl-CRU","cl-E40","cl-WCL","cl-WST")

#### the below line was introduced for NCC revised version
dsetList <- c("cl_rev-CRU","cl_rev-E40","cl_rev-WCL","cl_rev-WST")

vnList <- c("pr","tas","dtr")
procList <- expand.grid(GCM=gcmList,ISO=isoList,OBS=dsetList,VAR=vnList)
procList$GCM <- paste(procList$GCM); procList$ISO <- paste(procList$ISO)
procList$OBS <- paste(procList$OBS); procList$VAR <- paste(procList$VAR)

#create output folder
#oDir <- paste(mdDir,"/assessment/output-data/_summary",sep="") #original
oDir <- paste(mdDir,"/assessment/output-data/_summary_revised",sep="") #NCC revision
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

#get all metrics
if (!file.exists(paste(oDir,"/cl-summary.csv",sep=""))) {
  #all_mets <- apply(procList,1,get_mean_climate_metrics,mdDir,regions)
  all_mets <- apply(procList,1,get_mean_climate_metrics_revised,mdDir,regions)
  #write outputs
  save(list=c("all_mets"),file=paste(oDir,"/cl-summary_raw.RData",sep=""))
  all_mets <- do.call("rbind", all_mets)
  save(list=c("all_mets"),file=paste(oDir,"/cl-summary_final.RData",sep=""))
  write.csv(all_mets,paste(oDir,"/cl-summary.csv",sep=""),quote=T,row.names=F)
} else {
  all_mets <- read.csv(paste(oDir,"/cl-summary.csv",sep=""))
}

## make some nice plots
figDir <- paste(oDir,"/figures",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#list of things to plot
all_plots <- expand.grid(VAR=vnList,REGION=unique(regions$REGION),PAR=c("RMSE","CCOEF"))

#get limits of x axis for plots
pr_lims <- all_mets[which(all_mets$VAR == "pr"),]
pr_lims <- c(min(pr_lims$RMSE,na.rm=T),max(pr_lims$RMSE,na.rm=T))
tas_lims <- all_mets[which(all_mets$VAR == "tas"),]
tas_lims <- c(min(tas_lims$RMSE,na.rm=T),max(tas_lims$RMSE,na.rm=T))
dtr_lims <- all_mets[which(all_mets$VAR == "dtr"),]
dtr_lims <- c(min(dtr_lims$RMSE,na.rm=T),max(dtr_lims$RMSE,na.rm=T))

all_plots$LL <- NA; all_plots$UL <- NA
all_plots$LL[which(all_plots$PAR == "CCOEF")] <- 0
all_plots$UL[which(all_plots$PAR == "CCOEF")] <- 1
all_plots$LL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "pr")] <- pr_lims[1]
all_plots$UL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "pr")] <- pr_lims[2]
all_plots$LL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "tas")] <- tas_lims[1]
all_plots$UL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "tas")] <- tas_lims[2]
all_plots$LL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "dtr")] <- dtr_lims[1]
all_plots$UL[which(all_plots$PAR == "RMSE" & all_plots$VAR == "dtr")] <- dtr_lims[2]

all_plots$XLAB <- NA
all_plots$XLAB[which(all_plots$PAR == "CCOEF")] <- "Correlation coefficient"
all_plots$XLAB[which(all_plots$PAR == "RMSE" & all_plots$VAR == "pr")] <- "RMSE (mm/season or mm/year)"
all_plots$XLAB[which(all_plots$PAR == "RMSE" & all_plots$VAR != "pr")] <- "RMSE (Celsius/month)"

####
#loop and plot the boxplots
for (i in 1:nrow(all_plots)) {
  reg <- paste(all_plots$REGION[i])
  vn <- paste(all_plots$VAR[i])
  param <- paste(all_plots$PAR[i])
  xlb <- paste(all_plots$XLAB[i])
  ylims <- c(all_plots$LL[i],all_plots$UL[i])
  
  cat("plotting...",reg,vn,param,"\n")
  
  #select data
  pdata <- all_mets[which(all_mets$REG == reg & all_mets$VAR == vn),]
  pdata$ISO <- paste(pdata$ISO)
  pdata <- pdata[which(pdata$OBS != paste("cl-E40")),]
  
  #fix levels in ISO to avoid problems in ordering stuff vs. automatic order of boxplot
  pdata$ISO <- as.factor(pdata$ISO)
  #oi <- as.numeric(order(unique(pdata$ISO)))
  #pdata$ISO <- factor(pdata$ISO, levels=levels(pdata$ISO)[oi])
  
  #plot figure
  figName <- paste(figDir,"/",i,"_",vn,"_cl_",reg,"_",param,".tif",sep="")
  tiff(figName,res=300,pointsize=14,width=1000,height=1200,units="px",compression="lzw")
  par(mar=c(5,3.5,1,1),xpd=F,cex=0.6,las=2,lwd=0.65,font.lab=21,font.axis=21)
  boxplot(pdata[,param]~pdata$ISO,notch=T,horizontal=T,boxwex=0.7,outwex=0.3,
          ylim=ylims,pch=NA,col="grey 75",font.lab=21,font.axis=21,
          xlab=xlb,ylab=NA,main=NA)
  plotpoints(pdata,param=param)
  abline(v=seq(ylims[1],ylims[2],by=(ylims[2]-ylims[1])/10),lty=2,col="gray 80",lwd=0.5)
  text(y=length(unique(pdata$ISO))+0.5,x=((ylims[2]-ylims[1])*0.075),labels=c(reg),cex=1.25)
  dev.off()
  
}


###now make some heat maps of the correlation coefficient, split into seasons but take max of all
###observational datasets and separate ERA-40

x  <- as.matrix(mtcars)
heatmap(x, Rowv = NA, Colv = NA, scale="column",main = "heatmap")

#first need to average ensembles
obs_data <- all_mets[which(all_mets$OBS != "cl-E40"),]
obs_data$GCM <- paste(obs_data$GCM)
gcm <- as.character(sapply(as.character(obs_data$GCM),FUN= function(x) {unlist(strsplit(x,"_ENS_",fixed=T))[1]}))
obs_data$GCM <- gcm
obs_data <- obs_data[which(obs_data$SEASON != "ANN"),]
obs_data$PVAL <- NULL; obs_data$RSQ <- NULL; obs_data$MBR <- NULL
obs_data$RMSE <- NULL; obs_data$N <- NULL


#loop through variables
for (vn in unique(obs_data$VAR)) {
  #vn <- unique(obs_data$VAR)[1]
  vn_data <- obs_data[which(obs_data$VAR == paste(vn)),]
  for (reg in unique(vn_data$REG)) {
    #reg <- unique(vn_data$REG)[1]
    reg_data <- vn_data[which(vn_data$REG == paste(reg)),]
    for (ssn in unique(reg_data$SEASON)) {
      #ssn <- unique(reg_data$SEASON)[1]
      ssn_data <- reg_data[which(reg_data$SEASON == paste(ssn)),]
      odf <- data.frame()
      for (gcm in unique(ssn_data$GCM)) {
        #gcm <- unique(ssn_data$GCM)[1]
        gcm_data <- ssn_data[which(ssn_data$GCM == paste(gcm)),]
        gcm_data$VAR <- NULL; gcm_data$ISO <- NULL; gcm_data$REG <- NULL
        gcm_data$SEASON <- NULL; gcm_data$OBS <- NULL
        
        gcm_odf <- data.frame(GCM=paste(gcm),VAL=mean(gcm_data$CCOEF))
        names(gcm_odf) <- c("GCM",paste(reg,".",ssn,sep=""))
        odf <- rbind(odf,gcm_odf)
      }
      if (ssn == unique(reg_data$SEASON)[1]) {
        ssn_odf <- odf
      } else {
        ssn_odf <- merge(ssn_odf,odf,all=T,by="GCM")
      }
    }
    if (reg == unique(vn_data$REG)[1]) {
      reg_odf <- ssn_odf
    } else {
      reg_odf <- merge(reg_odf,ssn_odf,all=T,by="GCM")
    }
  }
  write.csv(reg_odf,paste(oDir,"/heatmap_cl-obs_",vn,"_CCOEF.csv",sep=""),row.names=F,quote=T)
}


#first need to average ensembles (for ERA40 data)
obs_data <- all_mets[which(all_mets$OBS == "cl-E40"),]
obs_data$GCM <- paste(obs_data$GCM)
gcm <- as.character(sapply(as.character(obs_data$GCM),FUN= function(x) {unlist(strsplit(x,"_ENS_",fixed=T))[1]}))
obs_data$GCM <- gcm
obs_data <- obs_data[which(obs_data$SEASON != "ANN"),]
obs_data$PVAL <- NULL; obs_data$RSQ <- NULL; obs_data$MBR <- NULL
obs_data$RMSE <- NULL; obs_data$N <- NULL


#loop through variables
for (vn in unique(obs_data$VAR)) {
  #vn <- unique(obs_data$VAR)[1]
  vn_data <- obs_data[which(obs_data$VAR == paste(vn)),]
  for (reg in unique(vn_data$REG)) {
    #reg <- unique(vn_data$REG)[1]
    reg_data <- vn_data[which(vn_data$REG == paste(reg)),]
    for (ssn in unique(reg_data$SEASON)) {
      #ssn <- unique(reg_data$SEASON)[1]
      ssn_data <- reg_data[which(reg_data$SEASON == paste(ssn)),]
      odf <- data.frame()
      for (gcm in unique(ssn_data$GCM)) {
        #gcm <- unique(ssn_data$GCM)[1]
        gcm_data <- ssn_data[which(ssn_data$GCM == paste(gcm)),]
        gcm_data$VAR <- NULL; gcm_data$ISO <- NULL; gcm_data$REG <- NULL
        gcm_data$SEASON <- NULL; gcm_data$OBS <- NULL
        
        gcm_odf <- data.frame(GCM=paste(gcm),VAL=mean(gcm_data$CCOEF))
        names(gcm_odf) <- c("GCM",paste(reg,".",ssn,sep=""))
        odf <- rbind(odf,gcm_odf)
      }
      if (ssn == unique(reg_data$SEASON)[1]) {
        ssn_odf <- odf
      } else {
        ssn_odf <- merge(ssn_odf,odf,all=T,by="GCM")
      }
    }
    if (reg == unique(vn_data$REG)[1]) {
      reg_odf <- ssn_odf
    } else {
      reg_odf <- merge(reg_odf,ssn_odf,all=T,by="GCM")
    }
  }
  write.csv(reg_odf,paste(oDir,"/heatmap_cl-e40_",vn,"_CCOEF.csv",sep=""),row.names=F,quote=T)
}



# x <- reg_odf
# row.names(x) <- paste(x$GCM)
# x$GCM <- NULL
# x <- as.matrix(x)
# heatmap(x, Rowv = NA, Colv = NA, scale="column",main = "heatmap")








