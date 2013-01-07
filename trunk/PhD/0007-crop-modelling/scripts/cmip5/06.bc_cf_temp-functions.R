#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

#make CF and BC weather files for GLAM runs
make_cf_bc_wth_wrapper <- function(i) {
  library(raster)
  
  #source functions of interest
  source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_rain-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/01.make_wth-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_cf_temp-functions.R",sep=""))
  
  sce <- paste(all_proc$GCM[i])
  gcm <- unlist(strsplit(sce,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(sce,"_ENS_",fixed=T))[2]
  loc <- all_proc$LOC[i] #635
  thisLeap <- paste(gcmChars$has_leap[which(gcmChars$GCM == gcm)][1])
  
  #directories where the bias corrected data is
  oDir_cf <- paste(outDir_cf,"/",gcm,"/",ens,sep="") #folder with cf gridded data
  oDir_bc <- paste(outDir_bc,"/",gcm,"/",ens,sep="") #folder with bc gridded data
  oDir_hbc <- paste(outDir_hbc,"/",gcm,"/",ens,sep="") #folder with bc gridded data
  
  #_del (this is: all are mean CF)
  #directories where check files are
  checkDir <- paste(wthDir_del,"/_process",sep="")
  if (!file.exists(checkDir)) {dir.create(checkDir)}
  checkFil_del <- paste(checkDir,"/",sce,"_loc-",loc,".proc",sep="")
  
  if (!file.exists(checkFil_del)) {
    outfol_rcp <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                                  year_i=2022,year_f=2049,wleap=thisLeap,
                                  out_wth_dir=wthDir_del,fut_wth_dir=oDir_cf,
                                  sow_date_dir=sowDir)
    ff <- file(checkFil_del,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  
  #_sh for hist (this is: all are mean BC)
  #directories where check files are
  checkDir <- paste(wthDir_hsh,"/_process",sep="")
  if (!file.exists(checkDir)) {dir.create(checkDir)}
  checkFil_hsh <- paste(checkDir,"/",sce,"_loc-",loc,".proc",sep="")
  
  if (!file.exists(checkFil_hsh)) {
    outfol_his <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                                  year_i=1966,year_f=1993,wleap=thisLeap,
                                  out_wth_dir=wthDir_hsh,fut_wth_dir=oDir_hbc,
                                  sow_date_dir=sowDir)
    ff <- file(checkFil_hsh,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  
  #_sh for rcp (this is: all are mean BC)
  #directories where check files are
  checkDir <- paste(wthDir_sh,"/_process",sep="")
  if (!file.exists(checkDir)) {dir.create(checkDir)}
  checkFil_sh <- paste(checkDir,"/",sce,"_loc-",loc,".proc",sep="")
  
  if (!file.exists(checkFil_sh)) {
    outfol_rcp <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                                  year_i=2022,year_f=2049,wleap=thisLeap,
                                  out_wth_dir=wthDir_sh,fut_wth_dir=oDir_bc,
                                  sow_date_dir=sowDir)
    ff <- file(checkFil_sh,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  
  return(list(HIS=checkFil_del,RCP=checkFil_sh))
}


#total wrapper
wrapper_gcm_bc_cf <- function(gcm_id) {
  ##########
  #gcm and ensemble member
  gcm_ens <- gcmList[gcm_id]
  gcm <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[2]
  wlp <- paste(gcmChars$has_leap[gcmChars$GCM == gcm & gcmChars$Ensemble == ens])[1]
  
  #loop variables
  for (vid in 1:nrow(varmx)) {
    vn_obs <- paste(varmx$OBS[vid])
    vn_gcm <- paste(varmx$MOD[vid])
    vn_pbc <- varmx$BC[vid]
    vn_pcf <- varmx$CF[vid]
    vn_fun <- paste(varmx$FUN[vid])
    vn_cht <- paste(varmx$CHG[vid])
    
    cat(gcm_ens,"var",vn_gcm,"\n")
    
    #gcm outdir
    gcm_oDir_cf <- paste(outDir_cf,"/",gcm,"/",ens,"/",vn_gcm,sep="")
    gcm_oDir_bc <- paste(outDir_bc,"/",gcm,"/",ens,"/",vn_gcm,sep="")
    gcm_oDir_hbc <- paste(outDir_hbc,"/",gcm,"/",ens,"/",vn_gcm,sep="")
    
    if (!file.exists(gcm_oDir_cf)) {dir.create(gcm_oDir_cf,recursive=T)}
    if (!file.exists(gcm_oDir_bc)) {dir.create(gcm_oDir_bc,recursive=T)}
    if (!file.exists(gcm_oDir_hbc)) {dir.create(gcm_oDir_hbc,recursive=T)}
    
    nfil_cf <- length(list.files(gcm_oDir_cf,pattern="\\.csv"))
    nfil_bc <- length(list.files(gcm_oDir_bc,pattern="\\.csv"))
    nfil_hbc <- length(list.files(gcm_oDir_hbc,pattern="\\.csv"))
    nfil <- min(c(nfil_cf,nfil_bc,nfil_hbc))
    
    if (nfil < 195) {
      #loop through the gridcells
      for (i in 1:nrow(cells)) {
        #i<-1
        loc <- cells$CELL[i]
        #cat(loc,"\n")
        
        #load observed monthly data and scale to daily if need be
        obsMth <- read.csv(paste(obsDir,"/",vn_obs,"/cell-",loc,".csv",sep=""))
        if (ncol(obsMth) == 13) {
          obsMth <- cbind(YEAR=obsMth$YEAR,MONTH0=c(NA,obsMth$MONTH12[1:(nrow(obsMth)-1)]),obsMth[,paste("MONTH",1:12,sep="")])
          obsMth$MONTH13 <- c(obsMth$MONTH1[2:nrow(obsMth)],NA)
          obsMth <- obsMth[obsMth$YEAR >= yi_h & obsMth$YEAR <= yf_h,]
          obsMth$YEAR <- NULL
          obsDat <- as.data.frame(t(apply(obsMth,1,linearise)))
          obsDat <- cbind(YEAR=yi_h:yf_h,obsDat)
          obsDat <- apply(obsDat,1,FUN=function(x) {nd <- leap(x[1]); x <- x[2:length(x)]; y <- x[16:(nd+15)]; if (nd==365) {y <- c(y,NA)}; return(y)})
          obsDat <- as.data.frame(t(obsDat))
          names(obsDat) <- paste("X",1:nrow(obsDat),sep="")
          obsDat <- cbind(YEAR=yi_h:yf_h,obsDat)
        } else {
          obsDat <- obsMth
          obsDat <- obsDat[obsDat$YEAR >= yi_h & obsDat$YEAR <= yf_h,]
        }
        
        #load gcm data
        hisDat <- read.csv(paste(hisDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
        rcpDat <- read.csv(paste(rcpDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
        
        #scale to daily if needed
        if (ncol(hisDat) == 13) {
          hisMth <- hisDat
          hisMth <- cbind(YEAR=hisMth$YEAR,MONTH0=c(NA,hisMth$MONTH12[1:(nrow(hisMth)-1)]),hisMth[,paste("MONTH",1:12,sep="")])
          hisMth$MONTH13 <- c(hisMth$MONTH1[2:nrow(hisMth)],NA)
          hisMth <- hisMth[hisMth$YEAR >= yi_h & hisMth$YEAR <= yf_h,]
          hisMth$YEAR <- NULL
          hisDat <- as.data.frame(t(apply(hisMth,1,linearise)))
          hisDat <- cbind(YEAR=yi_h:yf_h,hisDat)
          hisDat <- apply(hisDat,1,FUN=function(x) {nd <- leap(x[1]); x <- x[2:length(x)]; y <- x[16:(nd+15)]; if (nd==365) {y <- c(y,NA)}; return(y)})
          hisDat <- as.data.frame(t(hisDat))
          names(hisDat) <- paste("X",1:nrow(hisDat),sep="")
          hisDat <- cbind(YEAR=yi_h:yf_h,hisDat)
        } else {
          #take only years of interest
          hisDat <- hisDat[hisDat$YEAR >= yi_h & hisDat$YEAR <= yf_h,]
        }
        
        if (ncol(rcpDat) == 13) {
          rcpMth <- rcpDat
          rcpMth <- cbind(YEAR=rcpMth$YEAR,MONTH0=c(NA,rcpMth$MONTH12[1:(nrow(rcpMth)-1)]),rcpMth[,paste("MONTH",1:12,sep="")])
          rcpMth$MONTH13 <- c(rcpMth$MONTH1[2:nrow(rcpMth)],NA)
          rcpMth <- rcpMth[rcpMth$YEAR >= yi_f & rcpMth$YEAR <= yf_f,]
          rcpMth$YEAR <- NULL
          rcpDat <- as.data.frame(t(apply(rcpMth,1,linearise)))
          rcpDat <- cbind(YEAR=yi_h:yf_h,rcpDat)
          rcpDat <- apply(rcpDat,1,FUN=function(x) {nd <- leap(x[1]); x <- x[2:length(x)]; y <- x[16:(nd+15)]; if (nd==365) {y <- c(y,NA)}; return(y)})
          rcpDat <- as.data.frame(t(rcpDat))
          names(rcpDat) <- paste("X",1:nrow(rcpDat),sep="")
          rcpDat <- cbind(YEAR=yi_f:yf_f,rcpDat)
        } else {  
          rcpDat <- rcpDat[rcpDat$YEAR >= yi_f & rcpDat$YEAR <= yf_f,]
        }
        
        #calculate monthly totals
        obsMth <- as.data.frame(t(apply(obsDat,1,FUN=mth_totals,"yes",vn_fun)))
        names(obsMth) <- paste("MONTH",1:12,sep="")
        hisMth <- as.data.frame(t(apply(hisDat,1,FUN=mth_totals,wlp,vn_fun)))
        names(hisMth) <- paste("MONTH",1:12,sep="")
        rcpMth <- as.data.frame(t(apply(rcpDat,1,FUN=mth_totals,wlp,vn_fun)))
        names(rcpMth) <- paste("MONTH",1:12,sep="")
        
        #calculate correction factors
        corr_df <- data.frame(MONTH=1:12,OBS=as.numeric(colMeans(obsMth,na.rm=T)),
                              HIS=as.numeric(colMeans(hisMth,na.rm=T)),
                              RCP=as.numeric(colMeans(rcpMth,na.rm=T)))
        
        if (vn_cht == "rel") {
          corr_df$HIS <- sapply(corr_df$HIS,FUN=function(x) {round(x,3)})
          corr_df$RCP <- sapply(corr_df$RCP,FUN=function(x) {round(x,3)})
          corr_df$OBS <- sapply(corr_df$OBS,FUN=function(x) {round(x,3)})
          corr_df$HIS <- sapply(corr_df$HIS,FUN=function(x) {if (x == 0) {x <- 1}; return(x)})
          corr_df$CF <- (corr_df$RCP-corr_df$HIS)/(corr_df$HIS)
          corr_df$BC <- (corr_df$OBS-corr_df$HIS)/(corr_df$HIS)
        } else {
          corr_df$CF <- corr_df$RCP-corr_df$HIS
          corr_df$BC <- corr_df$OBS-corr_df$HIS
        }
        
        #apply delta method
        if (vn_pcf) {
          if (vn_cht == "rel") {
            cfDat <- as.data.frame(t(apply(obsDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$CF),wlp,vn_cht)))
          } else {
            cfDat <- as.data.frame(t(apply(obsDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$CF),wlp,vn_cht)))
          }
          
          names(cfDat) <- paste("X",1:ncol(cfDat),sep="")
          cfDat <- cbind(YEAR=yi_f:yf_f,cfDat)
          if (wlp == "no") {cfDat$X366 <- NA}
          if (wlp == "all30") {cfDat$X361 <- NA; cfDat$X362 <- NA; cfDat$X363 <- NA; cfDat$X364 <- NA; cfDat$X365 <- NA; cfDat$X366 <- NA}
          write.csv(cfDat,paste(gcm_oDir_cf,"/cell-",loc,".csv",sep=""),row.names=F,quote=T)
        }
        
        #apply bias correction
        if (vn_pbc) {
          if (vn_cht == "rel") {
            bcDat <- as.data.frame(t(apply(rcpDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$BC),wlp,vn_cht)))
            hbcDat <- as.data.frame(t(apply(hisDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$BC),wlp,vn_cht)))
          } else {
            bcDat <- as.data.frame(t(apply(rcpDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$BC),wlp,vn_cht)))
            hbcDat <- as.data.frame(t(apply(hisDat,1,bc_cf_apply,data.frame(MONTH=corr_df$MONTH,FACT=corr_df$BC),wlp,vn_cht)))
          }
          
          names(bcDat) <- paste("X",1:ncol(bcDat),sep="")
          bcDat <- cbind(YEAR=yi_f:yf_f,bcDat)
          if (wlp == "no") {bcDat$X366 <- NA}
          if (wlp == "all30") {bcDat$X361 <- NA; bcDat$X362 <- NA; bcDat$X363 <- NA; bcDat$X364 <- NA; bcDat$X365 <- NA; bcDat$X366 <- NA}
          write.csv(bcDat,paste(gcm_oDir_bc,"/cell-",loc,".csv",sep=""),row.names=F,quote=T)
          
          names(hbcDat) <- paste("X",1:ncol(hbcDat),sep="")
          hbcDat <- cbind(YEAR=yi_h:yf_h,hbcDat)
          if (wlp == "no") {hbcDat$X366 <- NA}
          if (wlp == "all30") {hbcDat$X361 <- NA; hbcDat$X362 <- NA; hbcDat$X363 <- NA; hbcDat$X364 <- NA; hbcDat$X365 <- NA; hbcDat$X366 <- NA}
          write.csv(hbcDat,paste(gcm_oDir_hbc,"/cell-",loc,".csv",sep=""),row.names=F,quote=T)
        }
      }
    }
  }
  
  
#   #quick check on annual cycle
#   ob_ac <- as.numeric(colMeans(obsDat[,2:ncol(obsDat)],na.rm=T))
#   hi_ac <- as.numeric(colMeans(hisDat[,2:ncol(hisDat)],na.rm=T))
#   rc_ac <- as.numeric(colMeans(rcpDat[,2:ncol(rcpDat)],na.rm=T))
#   cf_ac <- as.numeric(colMeans(cfDat[,2:ncol(cfDat)],na.rm=T))
#   bc_ac <- as.numeric(colMeans(bcDat[,2:ncol(bcDat)],na.rm=T))
#   hbc_ac <- as.numeric(colMeans(hbcDat[,2:ncol(bcDat)],na.rm=T))
#   
#   windows()
#   plot(1:366,cf_ac,ty="l",col="blue",ylim=c(-10,30),lty=2)
#   lines(1:366,bc_ac,ty="l",col="red",lty=2)
#   lines(1:366,ob_ac,ty="l",col="black")
#   lines(1:366,hi_ac,ty="l",col="blue")
#   lines(1:366,rc_ac,ty="l",col="red")
#   lines(1:366,hbc_ac,ty="l",col="purple")
  
  return("done!")
}





#### functions
#calculate monthly totals
mth_totals <- function(x,wleap,fun="mean") {
  yr <- x[1]
  dg <- createDateGridCMIP5(year=yr,whatLeap=wleap)
  daily <- x[2:(nrow(dg)+1)]
  dg$VALUE <- daily
  dg$DAY <- NULL; dg$MTH.STR <- NULL; dg$DAY.STR <- NULL; dg$MTH.DAY <- NULL; dg$JD <- NULL
  mthVals <- as.numeric(tapply(dg$VALUE,dg$MONTH,FUN=fun,na.rm=T))
  return(mthVals)
}

#bias correct or delta to daily series of GCM
bc_cf_apply <- function(x,fact,wleap,ch_type="abs") {
  #x <- as.numeric(obsDat[1,])
  yr <- x[1]
  dg <- createDateGridCMIP5(year=yr,whatLeap=wleap)
  daily <- x[2:(nrow(dg)+1)]
  dg$VALUE <- daily
  dg$DAY <- NULL; dg$MTH.STR <- NULL; dg$DAY.STR <- NULL; dg$MTH.DAY <- NULL; dg$JD <- NULL
  dg <- merge(dg,fact,by="MONTH",sort=F)
  
  if (ch_type == "abs") {
    newval <- dg$VALUE+dg$FACT
  } else if (ch_type == "rel") {
    newval <- dg$VALUE * (1 + dg$FACT)
    newval <- sapply(newval,FUN=function(x) max(c(0,x),na.rm=T))
  } else {
    stop("Error in ch_type. Inappropriate selection")
  }
  
  if (wleap=="yes" & length(newval)==365) {newval <- c(newval,NA)}
  
  return(newval)
}



