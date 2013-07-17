#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT
stop("!")

library(rgdal); library(raster); library(reshape2)

#glam: i/o directories and details
cropName <- "gnut"
ver <- "v6"
glam_bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#glam_bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(glam_bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
ecgDir <- paste(cropDir,"/ecg_analyses",sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))
rs <- raster(paste(cropDir,"/calib/exp-33_outputs/general/calib_results_spat/y_pred.asc",sep=""))
rs[cells$CELL] <- cells$CELL

#ecocrop: i/o directories
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
modDir <- paste(nbDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")
syDir <- paste(ecoDir,"/suit_yield",sep="")
if (!file.exists(syDir)) {dir.create(syDir)}

###
#initial loading of data
#load list of glam runs
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#b. load pot_yield_rfd.csv from ./ecg_analyses/glam_output/exp-$/tables
for (expID in expSel) {
  #expID <- expSel[1]
  ydata <- read.csv(paste(ecgDir,"/glam_output/exp-",expID,"/tables/pot_yield_rfd.csv",sep=""))
  
  #c. calculate time-ybar, normalise using y-min/(max-min)
  ybar <- rowMeans(ydata[,paste("Y",1966:1993,sep="")],na.rm=T)
  ynorm <- (ybar-min(ybar))/(max(ybar)-min(ybar))
  ybar <- cbind(ydata[,c("CELL","X","Y")],VALUE=ybar)
  names(ybar)[ncol(ybar)] <- paste("EXP.",expID,sep="")
  ynorm <- cbind(ydata[,c("CELL","X","Y")],VALUE=ynorm)
  names(ynorm)[ncol(ynorm)] <- paste("EXP.",expID,sep="")
  
  if (expID == expSel[1]) {
    ybar_all <- ybar
    ynor_all <- ynorm
  } else {
    ybar_all <- merge(ybar_all,ybar,by=c("CELL","X","Y"))
    ynor_all <- merge(ynor_all,ynorm,by=c("CELL","X","Y"))
  }
}
ynor_all$EXP.MEAN <- rowMeans(ynor_all[,paste("EXP.",expSel,sep="")],na.rm=T)

# #load obs yield
# obsy <- raster(paste(cropDir,"/calib/exp-33_outputs/general/calib_results_spat/y_obs.asc",sep=""))
# ynor_all$EXP.OBS <- extract(obsy,ynor_all[,c("X","Y")])
# ynor_all$EXP.OBS <- (ynor_all$EXP.OBS-min(ynor_all$EXP.OBS))/(max(ynor_all$EXP.OBS)-min(ynor_all$EXP.OBS))

###
#load the ecocrop runs and construct a data.frame with each of the selected ecocrop runs
#selected runs
skill <- read.csv(paste(ecoDir,"/data/runs_discard.csv",sep=""))
skill$SEL <- T
skill$SEL[which(skill$TEST.OMISSION.RATE > 0.1)] <- F; skill$SEL[which(skill$TEST.ERROR > 0.5)] <- F
ecoRuns <- skill$RUN[which(skill$SEL)]

#load ecocrop data
for (runID in ecoRuns) {
  #runID <- ecoRuns[1]
  ecors <- raster(paste(ecoDir,"/proj/baseline/clm_1966_1993/run_",runID,"/",cropName,"_tsuitability.tif",sep=""))
  sbar <- extract(ecors,cells[,c("X","Y")])
  sbar <- cbind(cells[,c("CELL","X","Y")],VALUE=sbar)
  names(sbar)[ncol(sbar)] <- paste("RUN.",runID,sep="")
  if (runID == ecoRuns[1]) {
    sbar_all <- sbar
  } else {
    sbar_all <- merge(sbar_all,sbar,by=c("CELL","X","Y"))
  }
}
sbar_all$RUN.MEAN <- rowMeans(sbar_all[,paste("RUN.",ecoRuns,sep="")],na.rm=T)
sbar_all$RUN.MEAN <- round(sbar_all$RUN.MEAN,0)

# rsx <- raster(rs)
# rsx[ynor_all$CELL] <- ynor_all$EXP.33
# plot(rsx)
# 
# rsy <- raster(rs)
# rsy[sbar_all$CELL] <- sbar_all$RUN.21
# plot(rsy)


###
#(1) The suitability scale was divided into 11 classes: values equal to 
#zero and 10 equally-spaced classes (of 10 % range each class). 
#A scatter plot with mean suitability and yield values across classes was produced. 

#for each ecocrop run, reclassify and calculate each GLAM em's mean of each class
ecoclass <- seq(0,100,by=5)
ecoclass <- data.frame(CLASS=1:length(ecoclass),INI=c(0,ecoclass[1:(length(ecoclass)-1)]),
                       END=c(0,ecoclass[2:length(ecoclass)]))
eclass <- data.frame()
for (cl in ecoclass$CLASS) {
  #cl <- ecoclass$CLASS[2]
  if (cl == 1) vals <- seq(ecoclass$INI[cl],ecoclass$END[cl],by=1)
  if (cl != 1) vals <- seq(ecoclass$INI[cl]+1,ecoclass$END[cl],by=1)
  tclass <- data.frame(CLASS=cl,SUIT=vals)
  eclass <- rbind(eclass,tclass)
}

#calculate means of ynorm for suit classes
for (runID in c(ecoRuns,"MEAN")) {
  #runID <- ecoRuns[1]
  sbar <- data.frame(CELL=sbar_all$CELL,SUIT=sbar_all[,paste("RUN.",runID,sep="")])
  sbar <- merge(sbar,eclass,by="SUIT",sort=F,all.x=T)
  for (expID in c(expSel,"MEAN")) {
    #expID <- expSel[1]
    ynorm <- data.frame(CELL=ynor_all$CELL,YNORM=ynor_all[,paste("EXP.",expID,sep="")])
    sydata <- merge(sbar,ynorm,by="CELL")
    sydata <- aggregate(sydata$YNORM,by=list(sydata$CLASS),FUN=function(x) {mean(x,na.rm=T)})
    names(sydata) <- c("CLASS",paste("RUN.",runID,"_EXP.",expID,sep=""))
    if (runID == ecoRuns[1] & expID == expSel[1]) {
      all_eg <- sydata
    } else {
      all_eg <- merge(all_eg,sydata,by="CLASS",all.x=T)
    }
  }
}


#make a boxplot
all_egm <- melt(all_eg,measure.vars=names(all_eg)[2:length(names(all_eg))],id=c("CLASS"))
all_egm <- all_egm[which(all_egm$CLASS > 1),]
#all_egm2 <- melt(all_eg,measure.vars=names(all_eg)[grep("EXP.OBS",names(all_eg))],id=c("CLASS"))
#all_egm2 <- all_egm2[which(all_egm2$CLASS > 1),]
tiff(paste(syDir,"/boxplot_suit_vs_yieldmean.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
bp <- boxplot(all_egm$value ~ all_egm$CLASS,pch=NA,ylim=c(0,1),las=2,col="grey",
              ylab="Mean normalised GLAM yield",#xlab="Suitability class",
              names=paste(ecoclass$INI[2:21]," - ",ecoclass$END[2:21],sep=""))
grid()
medvals <- data.frame(class=1:ncol(bp$stats),value=bp$stats[3,])
require(splines)
cubic.lm <- lm(value ~ poly(class, 3),data=medvals)
lines(medvals$class, cubic.lm$fitted.values, type="l", col="red")
dev.off()


#(2) The normalised yield scale was divided into 11 equally-spaced classes. 
#A scatter plot with mean yield and suitability values was produced.
for (expID in c(expSel,"MEAN")) {
  #expID <- expSel[1]
  ynorm <- data.frame(CELL=ynor_all$CELL,YNORM=ynor_all[,paste("EXP.",expID,sep="")])
  ynorm$CLASS <- 0
  
  #classes from quantiles
  glamclass <- as.numeric(quantile(ynorm$YNORM,probs=seq(0,1,by=0.025))) #seq(0,1,by=0.05)
  glamclass <- data.frame(CLASS=1:(length(glamclass)-1),INI=glamclass[1:(length(glamclass)-1)],
                          END=glamclass[2:length(glamclass)])
  for (cl in 1:nrow(glamclass)) {
    #cl <- 1
    if (cl == 1) ynorm$CLASS[which(ynorm$YNORM >= glamclass$INI[cl] & ynorm$YNORM <= glamclass$END[cl])] <- glamclass$CLASS[cl]
    if (cl != 1) ynorm$CLASS[which(ynorm$YNORM > glamclass$INI[cl] & ynorm$YNORM <= glamclass$END[cl])] <- glamclass$CLASS[cl]
  }
  for (runID in c(ecoRuns,"MEAN")) {
    #runID <- ecoRuns[1]
    sbar <- data.frame(CELL=sbar_all$CELL,SUIT=sbar_all[,paste("RUN.",runID,sep="")])
    ysdata <- merge(ynorm,sbar,by="CELL")
    ysdata <- aggregate(ysdata$SUIT,by=list(ysdata$CLASS),FUN=function(x) {mean(x,na.rm=T)})
    names(ysdata) <- c("CLASS",paste("EXP.",expID,"_RUN.",runID,sep=""))
    if (runID == ecoRuns[1] & expID == expSel[1]) {
      all_ge <- ysdata
    } else {
      all_ge <- merge(all_ge,ysdata,by="CLASS",all.x=T)
    }
  }
}


#make a boxplot
all_gem <- melt(all_ge,measure.vars=names(all_ge)[2:length(names(all_ge))],id=c("CLASS"))
#all_gem <- melt(all_ge,measure.vars=names(all_ge)[grep("EXP.MEAN_RUN.MEAN",names(all_ge))],id=c("CLASS"))
all_gem <- all_gem[which(!is.na(all_gem$value)),]
tiff(paste(syDir,"/boxplot_yield_vs_suitmean.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
bp <- boxplot(all_gem$value ~ all_gem$CLASS,pch=NA,ylim=c(0,100),las=2,col="grey",
              ylab="Mean EcoCrop suitability",xlab="GLAM yield quantile")
grid()
medvals <- data.frame(class=1:ncol(bp$stats),value=bp$stats[3,])
cubic.lm <- lm(value ~ poly(class, 4),data=medvals)
lines(medvals$class, cubic.lm$fitted.values, type="l", col="red")
dev.off()


###
### using yield and suitability calculate the I, and RR
if (!file.exists(paste(syDir,"/model_similarity.csv",sep=""))) {
  out_simil <- data.frame()
  for (runID in c(ecoRuns,"MEAN")) {
    #runID <- ecoRuns[1]
    sbar <- data.frame(CELL=sbar_all$CELL,SUIT=sbar_all[,paste("RUN.",runID,sep="")])
    for (expID in c(expSel,"MEAN")) {
      #expID <- expSel[1]
      cat(runID,"and",expID,"\n")
      ynorm <- data.frame(CELL=ynor_all$CELL,YNORM=ynor_all[,paste("EXP.",expID,sep="")])
      sydata <- merge(sbar,ynorm,by="CELL")
      sydata <- sydata[which(!is.na(sydata$SUIT)),]; sydata <- sydata[which(!is.na(sydata$YNORM)),]
      
      #normalise data
      sydata$SUIT <- sydata$SUIT / sum(sydata$SUIT)
      sydata$YNORM <- sydata$YNORM / sum(sydata$YNORM)
      
      #calculate I
      ival <- 1-.5*sqrt(sum((sqrt(sydata$YNORM)-sqrt(sydata$SUIT))^2))
      
      #calculate RR
      if (nrow(sydata)%%2 != 0) {sydata <- sydata[-sample(1:nrow(sydata),size=1,replace=F),]; rownames(sydata) <- 1:nrow(sydata)}
      rr <- c()
      for (i in 1:5) {
        sydata$SAMPLED <- F
        c_match <- 0; c_all <- 0
        while (length(which(sydata$SAMPLED)) != nrow(sydata)) {
          ssel <- sample(1:nrow(sydata),2,replace=F)
          sydata$SAMPLED[ssel] <- T
          sdif_e <- diff(c(sydata$SUIT[ssel[1]],sydata$SUIT[ssel[2]]))
          sdif_g <- diff(c(sydata$YNORM[ssel[1]],sydata$YNORM[ssel[2]]))
          if (sign(sdif_e) == sign(sdif_g)) {c_match <- c_match + 1}
          c_all <- c_all + 1
        }
        rr <- c(rr,c_match/c_all)
      }
      rr <- mean(rr,na.rm=T)
      odf <- data.frame(RUN=runID,EXP=expID,I=ival,RR=rr)
      out_simil <- rbind(out_simil,odf)
    }
  }
  write.csv(out_simil,paste(syDir,"/model_similarity.csv",sep=""),row.names=F,quote=T)
} else {
  out_simil <- read.csv(paste(syDir,"/model_similarity.csv",sep=""))
}

sim_mm <- out_simil[which(out_simil$RUN == "MEAN" & out_simil$EXP == "MEAN"),]

#plotting niche overlap
hd <- hist(out_simil$I,breaks=seq(0,1,by=0.05),plot=F)
tiff(paste(syDir,"/pdf_niche_overlap.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0.5,1),ylim=c(0,50),
     xlab="Niche overlap (I)", ylab="pdf (%)")
abline(v=sim_mm$I,col="red",lty=2)
grid()
dev.off()

#plotting relative rank score
hd <- hist(out_simil$RR,breaks=seq(0,1,by=0.05),plot=F)
tiff(paste(syDir,"/pdf_rank_score.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0,0.6),ylim=c(0,50),
     xlab="Relative rank score (RR)", ylab="pdf (%)")
abline(v=sim_mm$RR,col="red",lty=2)
grid()
dev.off()


#(3) GLAM’s simulated potential yield was then regressed against suitability
#using a three types of regressions: (a) linear, (b) log-linear, where log(Y)
#is regressed against suitability, and (c) robust regression (Maronna et al., 2006).
#Robust regression was used in order to assess the influence of outliers that may
#arise from errors in the structure of either suitability models or GLAM so as to 
#be able to detect the yield-suitability signal more clearly (Serra-Diaz et al., 2013). 
#Residuals of these regressions were then regressed against climatological means 
#and variances of 17 intra-seasonal agro-meteorological indicators (AMIs, Table 7.4) 
#derived from GLAM’s simulated daily output.



