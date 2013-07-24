#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT
stop("!")

#load packages
library(rgdal); library(raster)

#i/o directories
cropName <- "gnut"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
modDir <- paste(nbDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")
prjDir <- paste(ecoDir,"/proj",sep="")

#selected model runs
skill <- read.csv(paste(ecoDir,"/data/runs_discard.csv",sep=""))
ecoRuns <- skill$RUN[which(skill$SEL)]

#details for calculations
inList <- c("del","loci","raw")
gcmList <- list.files(paste(prjDir,"/rcp_del",sep=""))
all_runs <- expand.grid(RUN=ecoRuns,GCM=gcmList)


#calculate future change in suitability for each future-baseline combination
if (!file.exists(paste(prjDir,"/change_rasters.RData",sep=""))) {
  chgList <- list()
  for (inty in inList) {
    #inty <- inList[1]
    chgList[[inty]] <- list()
    for (i in 1:nrow(all_runs)) {
      #i <- 1
      gcm <- paste(all_runs$GCM[i])
      ecorun <- all_runs$RUN[i]
      cat("...processing",inty," / ",gcm," / run=",ecorun,"\n")
      
      #configure output list
      if (is.null(chgList[[inty]][[gcm]])) {chgList[[inty]][[gcm]] <- list()}
      
      #load baseline
      if (inty == "del") {
        rs_pd <- raster(paste(prjDir,"/baseline/clm_1966_1993/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
      } else {
        rs_pd <- raster(paste(prjDir,"/baseline_",inty,"/",gcm,"/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
      }
      
      #load future
      rs_fc <- raster(paste(prjDir,"/rcp_",inty,"/",gcm,"/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
      
      #calculate relative change
      rs_chg <- calc(stack(rs_pd,rs_fc),fun=function(x) {a <- x[1]; b <- x[2]; if (is.na(a) | is.na(b)) return(NA); if (a == 0) return(NA); chg <- b-a; return(chg)})
      
      #put into list
      chgList[[inty]][[gcm]][[paste("RUN.",ecorun,sep="")]] <- rs_chg
    }
  }
  save(list=c("chgList"),file=paste(prjDir,"/change_rasters.RData",sep=""))
} else {
  load(file=paste(prjDir,"/change_rasters.RData",sep=""))
}


#modelList
modList <- data.frame(GCM_ENS=gcmList)
modList$GCM <- sapply(modList$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[1]})
modList$ENS <- sapply(modList$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[2]})

#calculate means and quantiles
if (!file.exists(paste(prjDir,"/change_summary_rasters.RData",sep=""))) {
  out_summ <- list()
  for (inty in inList) {
    #inty <- inList[1]
    cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
    cat("XXXX processing",inty,"\n")
    cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
    
    #a. first average individual ensemble members
    chg_stk <- list()
    #loop GCMs in modList
    for (i in 1:length(unique(modList$GCM))) {
      #i <- 1
      gcm <- paste(unique(modList$GCM)[i])
      cat("\n...averaging",gcm,"\n")
      
      #create list
      if (is.null(chg_stk[[gcm]])) {chg_stk[[gcm]] <- list()}
      
      #list of ensemble members inside this
      wList <- grep(gcm,names(chgList[[inty]]))
      if (length(wList) == 1) {
        #if only one ensemble member then put directly
        cat("...ens=1, so no need for averaging\n")
        chg_stk[[gcm]] <- chgList[[inty]][[wList]]
      } else {
        #if more than 1 ensemble member then needs to average
        cat("...ens=",wList,", so averaging\n")
        for (rj in ecoRuns) {
          #rj <- ecoRuns[1]
          tstk <- c()
          for (wj in wList) {tstk <- c(tstk,chgList[[inty]][[wj]][[paste("RUN.",rj,sep="")]])}
          tstk <- stack(tstk)
          tstk <- calc(tstk,fun=function(x) {mean(x,na.rm=T)})
          chg_stk[[gcm]][[paste("RUN.",rj,sep="")]] <- tstk
        }
      }
    }
    tchg_stk <- stack(unlist(chg_stk))
    
    #b. then average remaining ensemble members
    chg_m <- calc(tchg_stk,fun=function(x) {mean(x,na.rm=T)})
    
    #c. calculate sd
    chg_sd <- calc(tchg_stk,fun=function(x) {sd(x,na.rm=T)})
    
    #d. calculate q1 and q4
    chg_q1 <- calc(tchg_stk,fun=function(x) {qval <- stats:::quantile(x,probs=0.25,na.rm=T); qm <- mean(x[which(x<=qval)],na.rm=T); return(qm)})
    chg_q4 <- calc(tchg_stk,fun=function(x) {qval <- stats:::quantile(x,probs=0.75,na.rm=T); qm <- mean(x[which(x>=qval)],na.rm=T); return(qm)})
    
    #e. calculate probability of suit change above and below certain thresholds
    #   thresholds are: 5 %, 10 %, 25 %, 50 %
    prob_pos05 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x > 5))/length(x); return(prob)})
    prob_neg05 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x < -5))/length(x); return(prob)})
    prob_pos10 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x > 10))/length(x); return(prob)})
    prob_neg10 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x < -10))/length(x); return(prob)})
    prob_pos25 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x > 25))/length(x); return(prob)})
    prob_neg25 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x < -25))/length(x); return(prob)})
    prob_pos50 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x > 50))/length(x); return(prob)})
    prob_neg50 <- calc(tchg_stk,fun=function(x) {x <- x[which(!is.na(x))]; prob <- length(which(x < -50))/length(x); return(prob)})
    
    #put into final object
    out_summ[[inty]] <- list()
    out_summ[[inty]]$CHG_MEAN <- chg_m
    out_summ[[inty]]$CHG_SD <- chg_sd
    out_summ[[inty]]$CHG_Q1 <- chg_q1
    out_summ[[inty]]$CHG_Q4 <- chg_q4
    out_summ[[inty]]$PROB_POS05 <- prob_pos05
    out_summ[[inty]]$PROB_NEG05 <- prob_neg05
    out_summ[[inty]]$PROB_POS10 <- prob_pos10
    out_summ[[inty]]$PROB_NEG10 <- prob_neg10
    out_summ[[inty]]$PROB_POS25 <- prob_pos25
    out_summ[[inty]]$PROB_NEG25 <- prob_neg25
    out_summ[[inty]]$PROB_POS50 <- prob_pos50
    out_summ[[inty]]$PROB_NEG50 <- prob_neg50
  }
  
  #save object
  save(list=c("out_summ"),file=paste(prjDir,"/change_summary_rasters.RData",sep=""))
}

