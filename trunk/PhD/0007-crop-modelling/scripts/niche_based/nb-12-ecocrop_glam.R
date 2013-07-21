#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT
stop("!")

library(rgdal); library(raster); library(reshape2)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"
source(paste(src.dir,"/scripts/niche_based/nb-12-ecocrop_glam-fun.R",sep=""))

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
if (!file.exists(paste(syDir,"/input_GLAM_normalised_production.csv",sep=""))) {
  for (expID in expSel) {
    #expID <- expSel[1]
    ydata <- read.csv(paste(ecgDir,"/glam_output/exp-",expID,"/tables/pot_yield_rfd.csv",sep=""))
    
    #c. calculate time-ybar, normalise using y-min/(max-min)
    ybar <- rowMeans(ydata[,paste("Y",1966:1993,sep="")],na.rm=T) #in kg/ha
    ahar <- cells$AHARV #in kha (i.e. ha * 1000)
    tpro <- ybar*ahar/1000 #in kton (i.e. ton * 1000)
    
    #with yield (no relationship)
    #ynorm <- (ybar-min(ybar))/(max(ybar)-min(ybar)) #linear normalisation
    #ynorm <- ybar/(10+ybar) #darango
    #ynorm <- ybar / max(ybar) #linear with min at 0
    #ynorm <- (ybar^0.5-1)/0.5 #box-cox
    #ynorm <- (ynorm - min(ynorm)) / (max(ynorm) - min(ynorm)) #linear to box-cox
    #ybar <- cbind(ydata[,c("CELL","X","Y")],VALUE=ybar)
    #names(ybar)[ncol(ybar)] <- paste("EXP.",expID,sep="")
    #ynorm <- cbind(ydata[,c("CELL","X","Y")],VALUE=ynorm)
    #names(ynorm)[ncol(ynorm)] <- paste("EXP.",expID,sep="")
    
    #with total production (clear relationship)
    ynorm <- (tpro-min(tpro))/(max(tpro)-min(tpro)) #linear normalisation
    ynorm <- (ynorm^0.5-1)/0.5 #box-cox
    ynorm <- (ynorm - min(ynorm)) / (max(ynorm) - min(ynorm)) #linear to box-cox
    ybar <- cbind(ydata[,c("CELL","X","Y")],VALUE=tpro)
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
  write.csv(ynor_all,paste(syDir,"/input_GLAM_normalised_production.csv",sep=""),quote=T,row.names=F)
} else {
  ynor_all <- read.csv(paste(syDir,"/input_GLAM_normalised_production.csv",sep=""))
}

###
#load the ecocrop runs and construct a data.frame with each of the selected ecocrop runs
#selected runs
skill <- read.csv(paste(ecoDir,"/data/runs_discard.csv",sep=""))
ecoRuns <- skill$RUN[which(skill$SEL)]

#load ecocrop data
if (!file.exists(paste(syDir,"/input_EcoCrop_suitability.csv",sep=""))) {
  for (runID in ecoRuns) {
    #runID <- ecoRuns[1]
    ecors <- raster(paste(ecoDir,"/proj/baseline/clm_1966_1993/run_",runID,"/",cropName,"_suitability.tif",sep=""))
    sbar <- extract(ecors,cells[,c("X","Y")])
    #sbar <- log(sbar/100+1) #; plot(sbar,sbar2)
    #sbar <- (sbar-log(1))/(log(2)-log(1))
    sbar <- (sbar^0.5-1)/0.5 #boxcox
    sbar <- round((sbar - min(sbar,na.rm=T)) / (max(sbar,na.rm=T) - min(sbar,na.rm=T)) * 100,0) #linear on box cox
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
  write.csv(sbar_all,paste(syDir,"/input_EcoCrop_suitability.csv",sep=""),quote=T,row.names=F)
} else {
  sbar_all <- read.csv(paste(syDir,"/input_EcoCrop_suitability.csv",sep=""))
}

###qqplot
tiff(paste(syDir,"/qqplot_suit_vs_yield.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
qqplot(sbar_all$RUN.MEAN,ynor_all$EXP.MEAN,xlim=c(0,100),ylim=c(0,1),
       xlab="Suitability quantile (%)",ylab="GLAM production quantile (normalised)")
grid()
abline(a=0,b=0.01)
dev.off()

###scattergram
tiff(paste(syDir,"/xyplot_suit_vs_yield.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
plot(ynor_all$EXP.MEAN,sbar_all$RUN.MEAN,xlim=c(0,1),ylim=c(0,100),pch=20,
       xlab="GLAM yield * area (normalised)",ylab="Suitability (%)")
grid()
abline(a=0,b=100)
dev.off()

###
#(1) The suitability scale was divided into 11 classes: values equal to 
#zero and 10 equally-spaced classes (of 10 % range each class). 
#A scatter plot with mean suitability and yield values across classes was produced. 

#for each ecocrop run, reclassify and calculate each GLAM em's mean of each class
ecoclass <- seq(0,100,by=5)
ecoclass <- data.frame(CLASS=1:length(ecoclass),INI=c(0,ecoclass[1:(length(ecoclass)-1)]),
                       END=c(0,ecoclass[2:length(ecoclass)]))

#calculate means of ynorm for suit classes
for (runID in c(ecoRuns,"MEAN")) {
  #runID <- ecoRuns[1]
  sbar <- data.frame(CELL=sbar_all$CELL,SUIT=sbar_all[,paste("RUN.",runID,sep="")])
  sbar$CLASS <- 0
  for (cl in 1:nrow(ecoclass)) {
    if (cl == 1) sbar$CLASS[which(ynorm$SUIT >= ecoclass$INI[cl] & sbar$SUIT <= ecoclass$END[cl])] <- ecoclass$CLASS[cl]
    if (cl != 1) sbar$CLASS[which(sbar$SUIT > ecoclass$INI[cl] & sbar$SUIT <= ecoclass$END[cl])] <- ecoclass$CLASS[cl]
  }
  
  for (expID in c(expSel,"MEAN")) {
    #expID <- expSel[1]
    ynorm <- data.frame(CELL=ynor_all$CELL,YNORM=ynor_all[,paste("EXP.",expID,sep="")])
    sydata <- merge(sbar,ynorm,by="CELL")
    sydata_m <- aggregate(sydata$YNORM,by=list(sydata$CLASS),FUN=function(x) {mean(x,na.rm=T)})
    sydata_e <- aggregate(sydata$YNORM,by=list(sydata$CLASS),FUN=function(x) {median(x,na.rm=T)})
    sydata_x <- aggregate(sydata$YNORM,by=list(sydata$CLASS),FUN=function(x) {max(x,na.rm=T)})
    names(sydata_m) <- c("CLASS",paste("RUN.",runID,"_EXP.",expID,sep=""))
    names(sydata_e) <- c("CLASS",paste("RUN.",runID,"_EXP.",expID,sep=""))
    names(sydata_x) <- c("CLASS",paste("RUN.",runID,"_EXP.",expID,sep=""))
    if (runID == ecoRuns[1] & expID == expSel[1]) {
      all_eg <- list(MEAN=sydata_m, MEDIAN=sydata_e, MAX=sydata_x)
    } else {
      all_eg$MEAN <- merge(all_eg$MEAN,sydata_m,by="CLASS",all.x=T)
      all_eg$MEDIAN <- merge(all_eg$MEDIAN,sydata_e,by="CLASS",all.x=T)
      all_eg$MAX <- merge(all_eg$MAX,sydata_x,by="CLASS",all.x=T)
    }
  }
}


#make a boxplot
stat <- "MAX"
all_egm <- all_eg[[stat]]
all_egm <- melt(all_egm,measure.vars=names(all_egm)[2:length(names(all_egm))],id=c("CLASS"),na.rm=F)
all_egm <- all_egm[which(!is.na(all_egm$value)),]
all_egm <- all_egm[which(all_egm$CLASS > 1),]
#all_egm2 <- melt(all_eg,measure.vars=names(all_eg)[grep("RUN.MEAN",names(all_eg))],id=c("CLASS"),na.rm=F)
#all_egm2 <- all_egm2[which(all_egm2$CLASS > 1),]

nming <- paste(ecoclass$INI[which(ecoclass$CLASS %in% unique(all_egm$CLASS))]," - ",
               ecoclass$END[which(ecoclass$CLASS %in% unique(all_egm$CLASS))],sep="")
tiff(paste(syDir,"/boxplot_suit_vs_prod",tolower(stat),".tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
bp <- boxplot(all_egm$value ~ all_egm$CLASS,pch=NA,ylim=c(0,1),las=2,col="grey",
              ylab="Mean normalised GLAM production",
              names=nming)
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
  glamclass <- as.numeric(quantile(ynorm$YNORM,probs=seq(0,1,by=0.05))) #seq(0,1,by=0.05)
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
    ysdata <- ysdata[which(ysdata$SUIT > 0),]
    ysdata_m <- aggregate(ysdata$SUIT,by=list(ysdata$CLASS),FUN=function(x) {mean(x,na.rm=T)})
    ysdata_e <- aggregate(ysdata$SUIT,by=list(ysdata$CLASS),FUN=function(x) {median(x,na.rm=T)})
    ysdata_x <- aggregate(ysdata$SUIT,by=list(ysdata$CLASS),FUN=function(x) {max(x,na.rm=T)})
    names(ysdata_m) <- c("CLASS",paste("EXP.",expID,"_RUN.",runID,sep=""))
    names(ysdata_e) <- c("CLASS",paste("EXP.",expID,"_RUN.",runID,sep=""))
    names(ysdata_x) <- c("CLASS",paste("EXP.",expID,"_RUN.",runID,sep=""))
    
    if (runID == ecoRuns[1] & expID == expSel[1]) {
      all_ge <- list(MEAN=ysdata_m, MEDIAN=ysdata_e, MAX=ysdata_x)
    } else {
      all_ge$MEAN <- merge(all_ge$MEAN,ysdata_m,by="CLASS",all.x=T)
      all_ge$MEDIAN <- merge(all_ge$MEDIAN,ysdata_e,by="CLASS",all.x=T)
      all_ge$MAX <- merge(all_ge$MAX,ysdata_x,by="CLASS",all.x=T)
    }
  }
}


#make a boxplot
stat <- "MAX"
all_gem <- all_ge[[stat]]
all_gem <- melt(all_gem,measure.vars=names(all_gem)[2:length(names(all_gem))],id=c("CLASS"))
#all_gem <- melt(all_ge,measure.vars=names(all_ge)[grep("EXP.MEAN_RUN.MEAN",names(all_ge))],id=c("CLASS"))
all_gem <- all_gem[which(!is.na(all_gem$value)),]

tiff(paste(syDir,"/boxplot_prod_vs_suit",tolower(stat),".tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
bp <- boxplot(all_gem$value ~ all_gem$CLASS,pch=NA,ylim=c(0,100),las=2,col="grey",
              ylab="Mean EcoCrop suitability",xlab="GLAM production quantile (normalised)")
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
      sydata <- sydata[which(sydata$SUIT > 0),]
      
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
hd <- hist(out_simil$I,breaks=seq(0,1,by=0.02),plot=F)
tiff(paste(syDir,"/pdf_niche_overlap.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0.5,1),ylim=c(0,60),
     xlab="Niche overlap (I)", ylab="pdf (%)")
abline(v=sim_mm$I,col="red",lty=2)
grid()
dev.off()

#plotting relative rank score
hd <- hist(out_simil$RR,breaks=seq(0,1,by=0.05),plot=F)
tiff(paste(syDir,"/pdf_rank_score.tiff",sep=""),res=300,pointsize=10,
     width=1900,height=1700,units="px",compression="lzw")
par(mar=c(5,4.5,1,1),cex=1)
plot(hd$mids,(hd$counts/sum(hd$counts)*100),ty="l",xlim=c(0,0.6),ylim=c(0,60),
     xlab="Relative rank score (RR)", ylab="pdf (%)")
abline(v=sim_mm$RR,col="red",lty=2)
grid()
dev.off()


#(3) GLAM’s simulated potential yield was then regressed against suitability
#using a three types of regressions: (a) linear, (b) log-linear, where log(Yn+1)
#is regressed against suitability, and (c) robust regression (Maronna et al., 2006).
#Robust regression was used in order to assess the influence of outliers that may
#arise from errors in the structure of either suitability models or GLAM so as to 
#be able to detect the yield-suitability signal more clearly (Serra-Diaz et al., 2013). 
#Residuals of these regressions were then regressed against climatological means 
#and variances of 17 intra-seasonal agro-meteorological indicators (AMIs, Table 7.4) 
#derived from GLAM’s simulated daily output.
require(robustbase); require(MASS)

#a. do the three fits for all possible ensemble members
#potential list of combinations
all_calc <- expand.grid(GLAMEXP=c(expSel,"MEAN"),ECORUN=c(ecoRuns,"MEAN"))
all_calc <- cbind(ID=1:nrow(all_calc),all_calc)

#loop combinations
if (!file.exists(paste(syDir,"/regression_output.csv",sep=""))) {
  model_obj <- list()
  reg_data <- data.frame()
  for (i in 1:nrow(all_calc)) {
    #i <- 380
    cat("working through GLAMEXP=",paste(all_calc$GLAMEXP[i]),"and ECORUN=",paste(all_calc$ECORUN[i]),"\n")
    
    #preparing output object
    model_obj[[i]] <- list()
    model_obj[[i]]$ECORUN <- all_calc$ECORUN[i]; model_obj[[i]]$GLAMEXP <- all_calc$GLAMEXP[i]
    
    #getting input data
    glamexp <- paste("EXP.",all_calc$GLAMEXP[i],sep="")
    ecorun <- paste("RUN.",all_calc$ECORUN[i],sep="")
    x <- data.frame(CELL=ynor_all$CELL,GLAM=ynor_all[,glamexp])
    y <- data.frame(CELL=sbar_all$CELL,SUIT=sbar_all[,ecorun])
    xy <- merge(x,y,by="CELL")
    #xy <- xy[which(xy$SUIT > 0),]
    #xy <- xy[which(xy$SUIT < 100),]
    xy <- xy[which(!is.na(xy$SUIT)),]; xy <- xy[which(!is.na(xy$GLAM)),]
    row.names(xy) <- 1:nrow(xy)
    xy$LGLAM <- log(xy$GLAM+1)
    
    #doing regressions
    #a. linear regression
    lin.m <- glm(GLAM ~ SUIT, data=xy)
    lin.r <- cor.test(xy$GLAM,predict(lin.m,xy))
    model_obj[[i]]$LINEAR <- lin.m
    
    #b. log-linear regression (not needed i think)
    loglin.m <- glm(LGLAM ~ SUIT, data=xy)
    loglin.r <- cor.test(xy$GLAM,exp(predict(loglin.m,xy))-1)
    model_obj[[i]]$LOGLINEAR <- loglin.m
    
    #c. robust regression
    roblin.m <- glmrob(GLAM ~ SUIT, data=xy, method="Mqle", family=gaussian)
    roblin.r <- cor.test(xy$GLAM,predict(roblin.m,xy))
    model_obj[[i]]$ROBUST1 <- roblin.m
    
    robw <- data.frame(PT=names(roblin.m$residuals),ROB.W=roblin.m$w.r)
    xy <- cbind(PT=1:nrow(xy),xy)
    xy <- merge(xy,robw,by="PT")
    xy$PT <- NULL
    xy2 <- xy[which(xy$ROB.W >= 0.5),]
    xy3 <- xy[which(xy$ROB.W < 0.5),]
    roblin.m2 <- glm(GLAM ~ SUIT, data=xy2)
    roblin.r2 <- cor.test(xy2$GLAM,predict(roblin.m2,xy2))
    model_obj[[i]]$ROBUST2 <- roblin.m2
    model_obj[[i]]$XY <- xy
    
    #result data.frame
    out_res <- data.frame(ID=i,GLAMEXP=glamexp,ECORUN=ecorun,LIN.SLOPE=lin.m$coefficients[2],
                          LIN.CCOEF=lin.r$estimate,LIN.PVAL=lin.r$p.value,
                          LOGLIN.SLOPE=loglin.m$coefficients[2],LOGLIN.CCOEF=loglin.r$estimate,
                          LOGLIN.PVAL=loglin.r$p.value,ROBLIN.SLOPE=roblin.m$coefficients[2],
                          ROBLIN.CCOEF=roblin.r$estimate,ROBLIN.PVAL=roblin.r$p.value,
                          ROBLIN2.SLOPE=roblin.m2$coefficients[2],ROBLIN2.CCOEF=roblin.r2$estimate,
                          ROBLIN2.PVAL=roblin.r2$p.value)
    
    #append result
    reg_data <- rbind(reg_data,out_res)
    
    #plot(xy2$SUIT,xy2$GLAM,pch=20,col="black",xlim=c(0,100),ylim=c(0,1),
    #  xlab="Suitability (%)",ylab="Normalised GLAM yield")
    #points(xy3$SUIT,xy3$GLAM,pch=21,col="red")
    #grid()
    #lines(1:100, predict(roblin.m,data.frame(SUIT=1:100)), col="red", lty=1)
    #lines(1:100, predict(lin.m,data.frame(SUIT=1:100)), col="blue", lty=1)
    #lines(1:100, predict(roblin.m2,data.frame(SUIT=1:100)), col="red", lty=2)
    #lines(1:100, exp(predict(loglin.m,data.frame(SUIT=1:100)))-1, col="blue", lty=2)
  }
  save(list=c("model_obj"),file=paste(syDir,"/regression_output.RData",sep=""))
  write.csv(reg_data,paste(syDir,"/regression_output.csv",sep=""),quote=T,row.names=F)
}


#b. load the residuals of the ensemble one and do the multiple regressions
load(file=paste(syDir,"/regression_output.RData",sep=""))

#note: all these models passed the AIC stepwise test
ensmean <- model_obj[[nrow(all_calc)]]
#step(ensmean$LINEAR)
#step(ensmean$LOGLINEAR)
#step(ensmean$ROBUST2)

#list of 100 seeds for crossval
set.seed(1234); seedList <- round(rnorm(100,5000,1500),0)

#xydata
xydata <- cbind(PT=1:nrow(ensmean$XY),ensmean$XY)

#load seasonal metrics and their standard deviations
if (!file.exists(paste(syDir,"/input_agro_meteorological_indicators.csv",sep=""))) {
  out_ami <- list()
  for (expID in expSel) {
    #expID <- expSel[1]
    load(file=paste(ecgDir,"/results/exp-",expID,"/cell_data_wth.RData",sep=""))
    cellid <- unlist(lapply(cell_wth_data,FUN=function(x) {x$CELL[1]}))
    cellid <- as.data.frame(cbind(NUM=1:length(cell_wth_data),CELL=cellid))
    
    #grab information for particular grid cell
    for (gi in xydata$CELL) {
      #gi <- xydata$CELL[1]
      cid <- cellid$NUM[which(cellid$CELL == gi)]
      if (is.null(out_ami[[paste("CELL.",gi,sep="")]])) {out_ami[[paste("CELL.",gi,sep="")]] <- data.frame()}
      gsdata <- cell_wth_data[[cid]]
      gsdata <- gsdata[,c("RCOV","RD.0","RD.2","RD.5","RD.10","RD.15","RD.20","ERATIO.25","ERATIO.50","ERATIO.75","TCOV","EFF.SRAD","EFF.GD")]
      gsd_me <- apply(gsdata,2,FUN=function(x) {mean(x,na.rm=T)})
      gsd_sd <- apply(gsdata,2,FUN=function(x) {sd(x,na.rm=T)})
      names(gsd_me) <- paste(names(gsd_me),".ME",sep="")
      names(gsd_sd) <- paste(names(gsd_sd),".SD",sep="")
      gsd <- data.frame(EXP=expID,t(gsd_me),t(gsd_sd))
      if (gi == xydata$CELL[1] & expID == expSel[1]) {
        out_ami[[paste("CELL.",gi,sep="")]] <- gsd
      } else {
        out_ami[[paste("CELL.",gi,sep="")]] <- rbind(out_ami[[paste("CELL.",gi,sep="")]],gsd)
      }
    }
    rm(cell_wth_data); g=gc(); rm(g)
  }
  
  #calculate average ami values for each gridcell
  ami_all <- lapply(out_ami,FUN=function(x) {apply(x[,2:ncol(x)],2,FUN=function(y) {mean(y,na.rm=T)})})
  ami_all <- do.call("rbind",ami_all)
  rnames <- rownames(ami_all)
  rnames <- as.numeric(sapply(rnames,function(x) {as.numeric(gsub("CELL.","",x))}))
  ami_all <- cbind(CELL=rnames,ami_all)
  rownames(ami_all) <- 1:nrow(ami_all)
  
  #write ami data
  write.csv(ami_all,paste(syDir,"/input_agro_meteorological_indicators.csv",sep=""),quote=T,row.names=F)
} else {
  ami_all <- read.csv(paste(syDir,"/input_agro_meteorological_indicators.csv",sep=""))
}


#perform all regressions
for (regtype in c("LINEAR","LOGLINEAR","ROBUST2")) {
  #regtype <- "LINEAR" #LINEAR LOGLINEAR ROBUST2
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("... working out",regtype,"regression...")
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  if (!file.exists(paste(syDir,"/bootstrapped_regs_01_",tolower(regtype),".RData",sep=""))) {
    resid <- data.frame(PT=names(ensmean[[regtype]]$residuals),RESIDUALS=ensmean[[regtype]]$residuals)
    
    #merge residuals with xy data
    xydata_n <- merge(xydata,resid,by="PT",all.x=T)
    
    #merge ami values with xy data
    regdata <- merge(xydata_n,ami_all,by="CELL")
    regdata$PT <- NULL
    regdata$GLAM <- NULL; regdata$SUIT <- NULL; regdata$LGLAM <- NULL; regdata$ROB.W <- NULL
    
    out_models <- list()
    out_eval <- data.frame()
    for (seed in seedList) {
      #seed <- seedList[1]
      cat("seed=",seed,"\n")
      set.seed(seed); selp <- sample(1:nrow(regdata),size=round(nrow(regdata)*.75,0))
      
      #prepare data
      fitdata <- regdata[selp,]; evadata <- regdata[-selp,]
      fitdata$CELL <- NULL; evadata$CELL <- NULL
      
      #fit model
      frml <- makeFormula(respName="RESIDUALS",explVar=fitdata[,2:ncol(fitdata)],type="quadratic",interaction.level=0)
      m.fit <- glm(frml,data=fitdata)
      #m.fit <- glm(RESIDUALS ~ .,data=fitdata)
      m.fit <- step(m.fit,trace=F)
      
      #predict over train & test data
      fitpred <- predict(m.fit,fitdata)
      evapred <- predict(m.fit,evadata)
      
      #calculat correlation
      r.fit <- cor.test(fitdata$RESIDUALS,fitpred)
      r.eva <- cor.test(evadata$RESIDUALS,evapred)
      
      #output stuff
      if (is.null(out_models[[paste("SD_",seed,sep="")]])) {out_models[[paste("SD_",seed,sep="")]] <- list()}
      out_models[[paste("SD_",seed,sep="")]]$MODEL <- m.fit
      out_models[[paste("SD_",seed,sep="")]]$FITDATA <- fitdata
      out_models[[paste("SD_",seed,sep="")]]$EVADATA <- evadata
      out_models[[paste("SD_",seed,sep="")]]$FITPRED <- fitpred
      out_models[[paste("SD_",seed,sep="")]]$EVAPRED <- evapred
      out_models[[paste("SD_",seed,sep="")]]$CCOEF.FIT <- r.fit
      out_models[[paste("SD_",seed,sep="")]]$CCOEF.EVA <- r.eva
      
      odf_eval <- data.frame(SEED=seed,CCOEF.FIT=r.fit$estimate,PVAL.FIT=r.fit$p.value,
                             CCOEF.EVA=r.eva$estimate,PVAL.EVA=r.eva$p.value)
      out_eval <- rbind(out_eval,odf_eval)
    }
    save(list=c("out_models","ami_all","regdata","out_eval"),file=paste(syDir,"/bootstrapped_regs_01_",tolower(regtype),".RData",sep=""))
    write.csv(out_eval,paste(syDir,"/bootstrapped_regs_01_eval_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
  } else {
    load(file=paste(syDir,"/bootstrapped_regs_01_",tolower(regtype),".RData",sep=""))
  }
  
  if (!file.exists(paste(syDir,"/bootstrapped_regs_02_",tolower(regtype),".RData",sep=""))) {
    #count predictors
    predList <- names(regdata)[3:ncol(regdata)]
    frml <- makeFormula(respName="RESIDUALS",explVar=regdata[,3:ncol(regdata)],type="quadratic",interaction.level=0)
    frml_t <- terms(frml); frml_t <- attr(frml_t,"term.labels")
    predVals <- as.data.frame(matrix(0,ncol=(length(frml_t)+2),nrow=length(seedList)))
    names(predVals) <- c("SEED","Intercept",frml_t)
    
    for (seed in seedList) {
      #seed <- seedList[1]
      tmod <- out_models[[paste("SD_",seed,sep="")]]$MODEL
      tmodcoef <- as.data.frame(t(tmod$coefficients)); rownames(tmodcoef) <- 1
      names(tmodcoef)[1] <- "Intercept"
      
      #put data into predVals data.frame
      predVals$SEED[which(seedList %in% seed)] <- seed
      for (prd in c("Intercept",frml_t)) {
        #prd <- frml_t[1]
        if (prd %in% names(tmodcoef)) {predVals[which(seedList %in% seed),prd] <- tmodcoef[,prd]}
      }
    }
    
    #count number of times not zero
    count_nz <- apply(predVals[,3:ncol(predVals)],2,function(x) {length(which(x!=0))})
    count_nz <- count_nz[which(count_nz > 0)]
    count_nz <- data.frame(PREDICTOR=names(count_nz),USE=as.numeric(count_nz))
    count_nz <- count_nz[order(count_nz$USE),]
    rownames(count_nz) <- 1:nrow(count_nz) #dont forget to produce a plot of this
    write.csv(count_nz,paste(syDir,"/variable_use_frequency_01_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    write.csv(predVals,paste(syDir,"/bootstrapped_coefficients_01_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    
    selpred <- count_nz[which(count_nz$USE >= 50),]
    final_frm <- paste("RESIDUALS ~ ",selpred$PREDICTOR[1],sep="")
    for (pdi in 2:nrow(selpred)) {final_frm <- paste(final_frm," + ",selpred$PREDICTOR[pdi],sep="")}
    final_frm <- as.formula(final_frm)
    
    #re-fit models with reduced predictors
    out_models2 <- list()
    out_eval2 <- data.frame()
    for (seed in seedList) {
      #seed <- seedList[1]
      cat("seed=",seed,"\n")
      set.seed(seed); selp <- sample(1:nrow(regdata),size=round(nrow(regdata)*.75,0))
      
      #prepare data
      fitdata <- regdata[selp,]; evadata <- regdata[-selp,]
      fitdata$CELL <- NULL; evadata$CELL <- NULL
      
      #fit model
      m.fit <- glm(final_frm,data=fitdata)
      m.fit <- step(m.fit,trace=F,direction="both")
      
      #predict over train & test data
      fitpred <- predict(m.fit,fitdata)
      evapred <- predict(m.fit,evadata)
      
      #calculat correlation
      r.fit <- cor.test(fitdata$RESIDUALS,fitpred)
      r.eva <- cor.test(evadata$RESIDUALS,evapred)
      
      #output stuff
      if (is.null(out_models2[[paste("SD_",seed,sep="")]])) {out_models2[[paste("SD_",seed,sep="")]] <- list()}
      out_models2[[paste("SD_",seed,sep="")]]$MODEL <- m.fit
      out_models2[[paste("SD_",seed,sep="")]]$FITDATA <- fitdata
      out_models2[[paste("SD_",seed,sep="")]]$EVADATA <- evadata
      out_models2[[paste("SD_",seed,sep="")]]$FITPRED <- fitpred
      out_models2[[paste("SD_",seed,sep="")]]$EVAPRED <- evapred
      out_models2[[paste("SD_",seed,sep="")]]$CCOEF.FIT <- r.fit
      out_models2[[paste("SD_",seed,sep="")]]$CCOEF.EVA <- r.eva
      
      odf_eval <- data.frame(SEED=seed,CCOEF.FIT=r.fit$estimate,PVAL.FIT=r.fit$p.value,
                             CCOEF.EVA=r.eva$estimate,PVAL.EVA=r.eva$p.value)
      out_eval2 <- rbind(out_eval2,odf_eval)
    }
    save(list=c("out_models2","ami_all","regdata","out_eval2"),file=paste(syDir,"/bootstrapped_regs_02_",tolower(regtype),".RData",sep=""))
    write.csv(out_eval2,paste(syDir,"/bootstrapped_regs_02_eval_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    rm(list=c("out_models2","ami_all","regdata","out_eval2"))
  }
}


#### make a plot of count_nz
for (regtype in c("LINEAR","LOGLINEAR","ROBUST2")) {
  #regtype <- "LINEAR"
  cat("plotting variable importance",regtype,"\n")
  count_nz <- read.csv(paste(syDir,"/variable_use_frequency_01_",tolower(regtype),".csv",sep=""))
  count_nz <- count_nz[order(count_nz$USE,decreasing=T),]
  
  if (!file.exists(paste(syDir,"/parameter_importance_01_",tolower(regtype),".tiff",sep=""))) {
    tiff(paste(syDir,"/parameter_importance_01_",tolower(regtype),".tiff",sep=""),res=300,pointsize=7,
         width=1800,height=1100,units="px",compression="lzw",type="cairo")
    par(mar=c(10,4.5,1,1),cex=1)
    barplot(height=count_nz$USE,names.arg=count_nz$PREDICTOR,las=2,
            ylab="Percent regressions where selected (%)",
            xlab=NA,ylim=c(0,100))
    grid()
    abline(h=50,col="red",lty=2)
    dev.off()
  }
}


### calculate nz_count for bootstrap_02
for (regtype in c("LINEAR","LOGLINEAR","ROBUST2")) {
  #regtype <- "LINEAR"
  cat("plotting variable importance",regtype,"\n")
  
  if (!file.exists(paste(syDir,"/variable_use_frequency_02_",tolower(regtype),".csv",sep=""))) {
    #loading regression data
    load(file=paste(syDir,"/bootstrapped_regs_02_",tolower(regtype),".RData",sep=""))
    count_nz1 <- read.csv(paste(syDir,"/variable_use_frequency_01_",tolower(regtype),".csv",sep=""))
    
    #select predictors and create output data.frame
    selpred <- count_nz1[which(count_nz1$USE >= 50),]
    predVals <- as.data.frame(matrix(0,ncol=(nrow(selpred)+2),nrow=length(seedList)))
    names(predVals) <- c("SEED","Intercept",paste(selpred$PREDICTOR))
    
    for (seed in seedList) {
      #seed <- seedList[1]
      tmod <- out_models2[[paste("SD_",seed,sep="")]]$MODEL
      tmodcoef <- as.data.frame(t(tmod$coefficients)); rownames(tmodcoef) <- 1
      names(tmodcoef)[1] <- "Intercept"
      
      #put data into predVals data.frame
      predVals$SEED[which(seedList %in% seed)] <- seed
      for (prd in c("Intercept",paste(selpred$PREDICTOR))) {
        #prd <- paste(selpred$PREDICTOR)[1]
        if (prd %in% names(tmodcoef)) {predVals[which(seedList %in% seed),prd] <- tmodcoef[,prd]}
      }
    }
    
    #calculate number of predictors
    count_nz2 <- apply(predVals[,3:ncol(predVals)],2,function(x) {length(which(x!=0))})
    count_nz2 <- count_nz2[which(count_nz2 > 0)]
    count_nz2 <- data.frame(PREDICTOR=names(count_nz2),USE=as.numeric(count_nz2))
    count_nz2 <- count_nz2[order(count_nz2$USE, decreasing=T),]
    rownames(count_nz2) <- 1:nrow(count_nz2) #dont forget to produce a plot of this
    write.csv(count_nz2,paste(syDir,"/variable_use_frequency_02_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    write.csv(predVals,paste(syDir,"/bootstrapped_coefficients_02_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    
    rm(list=c("out_models2","ami_all","regdata","out_eval2"))
  } else {
    count_nz2 <- read.csv(paste(syDir,"/variable_use_frequency_02_",tolower(regtype),".csv",sep=""))
  }
  
  #plot the variable importance
  if (!file.exists(paste(syDir,"/parameter_importance_02_",tolower(regtype),".tiff",sep=""))) {
    tiff(paste(syDir,"/parameter_importance_02_",tolower(regtype),".tiff",sep=""),res=300,pointsize=6,
         width=1200,height=800,units="px",compression="lzw",type="cairo")
    par(mar=c(10,4.5,1,1),cex=1)
    barplot(height=count_nz2$USE,names.arg=count_nz2$PREDICTOR,las=2,
            ylab="Percent regressions where selected (%)",
            xlab=NA,ylim=c(0,100))
    grid()
    abline(h=50,col="red",lty=2)
    dev.off()
  }
}


#### plot correlation coefficient of eval data
# for the three types of regressions (different colour)
# for the first and second regressions (different line type)
if (!file.exists(paste(syDir,"/bootstrapped_ccoef.tiff",sep=""))) {
  tiff(paste(syDir,"/bootstrapped_ccoef.tiff",sep=""),res=300,pointsize=10,
       width=1900,height=1500,units="px",compression="lzw",type="cairo")
  par(mar=c(5,4.5,1,1),cex=1)
  for (regtype in c("LINEAR","LOGLINEAR","ROBUST2")) {
    #regtype <- "LINEAR"
    #load eval part 1
    eval1 <- read.csv(paste(syDir,"/bootstrapped_regs_01_eval_",tolower(regtype),".csv",sep=""))
    eval2 <- read.csv(paste(syDir,"/bootstrapped_regs_02_eval_",tolower(regtype),".csv",sep=""))
    
    #calculate histograms
    hd1 <- hist(eval1$CCOEF.EVA,breaks=seq(0,1,by=0.05),plot=F)
    hd2 <- hist(eval2$CCOEF.EVA,breaks=seq(0,1,by=0.05),plot=F)
    
    if (regtype == "LINEAR") {
      plot(hd1$mids,(hd1$counts/sum(hd1$counts)*100),ty="l",xlim=c(0,1),ylim=c(0,50),
           xlab="Correlation coefficient", ylab="pdf (%)",col="red")
      grid()
      lines(hd2$mids,(hd2$counts/sum(hd2$counts)*100),ty="l",col="red",lty=2)
    } else if (regtype == "LOGLINEAR") {
      lines(hd1$mids,(hd1$counts/sum(hd1$counts)*100),ty="l",col="blue",lty=1)
      lines(hd2$mids,(hd2$counts/sum(hd2$counts)*100),ty="l",col="blue",lty=2)
    } else {
      lines(hd1$mids,(hd1$counts/sum(hd1$counts)*100),ty="l",col="dark green",lty=1)
      lines(hd2$mids,(hd2$counts/sum(hd2$counts)*100),ty="l",col="dark green",lty=2)
    }
  }
  abline(v=0.5,col="black",lty=1)
  dev.off()
}


#### make a plot permutation importance for each model 
#(using regdata, only for final models)
for (regtype in c("LINEAR","LOGLINEAR","ROBUST2")) {
  #regtype <- "LINEAR"
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("Regression",regtype,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #load the regressions
  if (!file.exists(paste(syDir,"/bootstrap_permutation_importance_",tolower(regtype),".csv",sep=""))) {
    load(file=paste(syDir,"/bootstrapped_regs_02_",tolower(regtype),".RData",sep=""))
    
    #use regdata
    predList <- read.csv(paste(syDir,"/bootstrapped_coefficients_02_",tolower(regtype),".csv",sep=""))
    predList <- apply(predList,2,FUN=function(x) {length(which(x!=0))})
    predList <- predList[which(predList != 0)]
    predList <- names(predList)[3:length(predList)]
    predList <- gsub("I.","",predList)
    predList <- gsub(".2.","",predList)
    predList <- unique(predList)
    
    predList <- names(regdata)[3:ncol(regdata)]
    all_pimp <- data.frame()
    for (mi in 1:length(out_models2)) {
      #mi <- 1
      if (mi %% 10 == 0) cat("processing",names(out_models2)[mi],"\n")
      orig_pred <- predict(out_models2[[mi]]$MODEL,regdata)
      pred_imp <- c()
      for (prd in predList) {
        #prd <- predList[1]
        regdata_r <- regdata
        avimp <- c()
        for (ri in 1:10) {
          nor <- sample(1:nrow(regdata_r),size=nrow(regdata_r),replace=F)
          regdata_r[,prd] <- regdata[nor,prd]
          new_pred <- predict(out_models2[[mi]]$MODEL,regdata_r)
          vimp <- 1 - as.numeric(cor.test(orig_pred,new_pred,method="pearson")$estimate)
          avimp <- c(avimp,vimp)
        }
        avimp <- mean(avimp,na.rm=T)
        pred_imp <- c(pred_imp,avimp)
        names(pred_imp)[length(pred_imp)] <- prd
      }
      pred_imp <- as.data.frame(t(pred_imp))
      pred_imp <- cbind(ITER=mi, pred_imp)
      all_pimp <- rbind(all_pimp, pred_imp)
    }
    names(all_pimp)[1] <- "SEED"
    all_pimp$SEED <- names(out_models2)
    write.csv(all_pimp,paste(syDir,"/bootstrap_permutation_importance_",tolower(regtype),".csv",sep=""),quote=T,row.names=F)
    
    #remove stuff
    rm(list=c("out_models2","ami_all","regdata","out_eval2"))
    
    #make a boxplot with all these values
    nz_pi <- apply(all_pimp[,2:ncol(all_pimp)],2,FUN=function(x) {length(which(round(x*100)==0))})
    nz_pi <- nz_pi[which(nz_pi != 100)]
    
    pimp_m <- all_pimp[,c("SEED",names(nz_pi))]
    pimp_m <- melt(pimp_m,varnames=names(pimp_m),id=c("SEED"))
    pimp_o <- order(as.numeric(by(pimp_m$value, pimp_m$variable, median)),decreasing=T)    
    pimp_m$variable <- ordered(pimp_m$variable, levels=levels(pimp_m$variable)[pimp_o]) 
    
    tiff(paste(syDir,"/bootstrap_permutation_importance_",tolower(regtype),".tiff",sep=""),res=300,pointsize=10,
         width=1900,height=1700,units="px",compression="lzw",type="cairo")
    par(mar=c(8,4.5,1,1),cex=1)
    boxplot(pimp_m$value ~ pimp_m$variable,pch=NA,ylim=c(0,1.2),las=2,col="grey",
                  ylab="Permutation importance")
    grid()
    abline(h=0.5,col="red")
    dev.off()
  }
}


#### make a map of predicted production





