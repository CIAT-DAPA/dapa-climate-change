#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012


#final wrap
wrap_bc_wthmkr <- function(k) {
  ######
  # bias correct the data
  lmts <- bc_rain_wrapper(k)
  
  ######
  # generate the wth files
  ctrf <- make_bc_wth_wrapper(k)
}


#make bias corrected weather files for GLAM runs
make_bc_wth_wrapper <- function(i) {
  library(raster)
  
  #source functions of interest
  source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_rain-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/01.make_wth-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/glam/glam-make_wth.R",sep=""))
  
  sce <- paste(all_proc$GCM[i])
  gcm <- unlist(strsplit(sce,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(sce,"_ENS_",fixed=T))[2]
  loc <- all_proc$LOC[i] #635
  thisLeap <- paste(gcmChars$has_leap[which(gcmChars$GCM == gcm)][1])
  
  #directories where the uncorrected data is
  inDir_his <- paste(hisDir,"/",gcm,"/",ens,sep="") #folder with raw gridded data
  inDir_rcp <- paste(rcpDir,"/",gcm,"/",ens,sep="") #folder with raw gridded data
  
  #directories where the bias corrected data is
  oDir_his_bc <- paste(bcDir_his,"/",gcm,"/",ens,sep="") #folder with bc gridded data
  oDir_rcp_bc <- paste(bcDir_rcp,"/",gcm,"/",ens,sep="") #folder with bc gridded data
  
  #directories where check files are
  checkDir <- paste(wthDirBc_his,"/_process",sep="")
  if (!file.exists(checkDir)) {dir.create(checkDir)}
  checkFil_his <- paste(checkDir,"/",sce,"_loc-",loc,".proc",sep="")
  
  #historical period
  if (!file.exists(checkFil_his)) {
    #copy all other data from the uncorrected output, and then remove it
    for (cvn in c("rsds","tasmax","tasmin")) {
      codir <- paste(oDir_his_bc,"/",cvn,sep="")
      if (!file.exists(codir)) {dir.create(codir)}
      ff <- file.copy(from=paste(inDir_his,"/tasmax/cell-",loc,".csv",sep=""),to=codir)
    }
    
    #create the daily data files for historical
    outfol_his <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                                  year_i=1966,year_f=1993,wleap=thisLeap,
                                  out_wth_dir=wthDirBc_his,fut_wth_dir=oDir_his_bc,
                                  sow_date_dir=sowDir)
    
    #remove extra files
    for (cvn in c("rsds","tasmax","tasmin")) {
      codir <- paste(oDir_his_bc,"/",cvn,sep="")
      system(paste("rm -rf ",codir,sep=""))
    }
    
    ff <- file(checkFil_his,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  
  #directories where check files are
  checkDir <- paste(wthDirBc_rcp,"/_process",sep="")
  if (!file.exists(checkDir)) {dir.create(checkDir)}
  checkFil_rcp <- paste(checkDir,"/",sce,"_loc-",loc,".proc",sep="")
  
  if (!file.exists(checkFil_rcp)) {
    #copy all other data from the uncorrected output, and then remove it
    for (cvn in c("rsds","tasmax","tasmin")) {
      codir <- paste(oDir_rcp_bc,"/",cvn,sep="")
      if (!file.exists(codir)) {dir.create(codir)}
      ff <- file.copy(from=paste(inDir_rcp,"/tasmax/cell-",loc,".csv",sep=""),to=codir)
    }
    
    outfol_rcp <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                                  year_i=2021,year_f=2049,wleap=thisLeap,
                                  out_wth_dir=wthDirBc_rcp,fut_wth_dir=oDir_rcp_bc,
                                  sow_date_dir=sowDir)
    
    #remove extra files
    for (cvn in c("rsds","tasmax","tasmin")) {
      codir <- paste(oDir_rcp_bc,"/",cvn,sep="")
      system(paste("rm -rf ",codir,sep=""))
    }
    
    ff <- file(checkFil_rcp,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  
  return(list(HIS=checkFil_his,RCP=checkFil_rcp))
}


#bias correction wrapper function
bc_rain_wrapper <- function(i) {
  library(raster)
  
  #source functions of interest
  source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/06.bc_rain-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/01.make_wth-functions.R",sep=""))
  source(paste(src.dir,"/0007-crop-modelling/scripts/glam/glam-make_wth.R",sep=""))
  
  sce <- paste(all_proc$GCM[i])
  gcm <- unlist(strsplit(sce,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(sce,"_ENS_",fixed=T))[2]
  loc <- all_proc$LOC[i] #635
  thisLeap <- paste(gcmChars$has_leap[which(gcmChars$GCM == gcm)][1])
  
  cat("processing",sce," / and loc = ",loc,"\n")
  
  #specific gcm directories
  oDir_his <- paste(bcDir_his,"/",gcm,"/",ens,"/",vn_gcm,sep="")
  oDir_rcp <- paste(bcDir_rcp,"/",gcm,"/",ens,"/",vn_gcm,sep="")
  if (!file.exists(oDir_his)) {dir.create(oDir_his,recursive=T)}
  if (!file.exists(oDir_rcp)) {dir.create(oDir_rcp,recursive=T)}
  
  #############################################################################
  # bias correcting the GCM output precipitation
  #############################################################################
  if (!file.exists(paste(oDir_his,"/fit_cell-",loc,".csv",sep=""))) {
    #read in observed and gcm data
    obs_data <- read.csv(paste(obsDir,"/",vn,"/cell-",loc,".csv",sep=""))
    his_data <- read.csv(paste(hisDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
    rcp_data <- read.csv(paste(rcpDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
    
    #separate months into individual series
    obs_list <- mk_mth_list(obs_data,"createDateGrid",dg_args=NA,yi_h,yf_h)
    his_list <- mk_mth_list(his_data,"createDateGridCMIP5",dg_args=thisLeap,yi_h,yf_h)
    rcp_list <- mk_mth_list(rcp_data,"createDateGridCMIP5",dg_args=thisLeap,yi_f,yf_f)
    
    #calculate loci metrics
    loci_mets <- loci_cal(obs_list,his_list,wdt_obs=1,iter_step=1e-4)
    
    #calculate bias corrected data based on historical data
    his_list <- loci_correct(his_list,loci_mets)
    
    #putting all the series together again into a matrix
    his_data_bc <- remake_daily(his_list,his_data,thisLeap,yi_h,yf_h)
    
    #here bias correct the future climates and plot the pdfs together
    rcp_list <- loci_correct(rcp_list,loci_mets)
    rcp_data_bc <- remake_daily(rcp_list,rcp_data,thisLeap,yi_f,yf_f)
    
    #here write the data
    write.csv(his_data_bc,paste(oDir_his,"/cell-",loc,".csv",sep=""),quote=T,row.names=F)
    write.csv(rcp_data_bc,paste(oDir_rcp,"/cell-",loc,".csv",sep=""),quote=T,row.names=F)
    write.csv(loci_mets,paste(oDir_his,"/fit_cell-",loc,".csv",sep=""),quote=T,row.names=F)
  } else {
    loci_mets <- read.csv(paste(oDir_his,"/fit_cell-",loc,".csv",sep=""))
  }
  return(loci_mets)
}


#calculate total rainfall and number of rainy days for the whole time series
calc_metrics <- function(all_data,dg_fun="createDateGrid",dg_args=NA,yi,yf) {
  cat("calculating rainfall and rain days\n")
  odat_all <- data.frame()
  for (yr in yi:yf) {
    yr_data <- as.numeric(all_data[which(all_data$YEAR == yr),2:ncol(all_data)])
    
    if (!is.na(dg_args)) {
      dg <- do.call(dg_fun,list(yr,dg_args))
    } else {
      dg <- do.call(dg_fun,list(yr))
    }
    dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
    dg$VALUE <- yr_data[1:nrow(dg)]
    
    pr <- as.numeric(by(dg$VALUE,dg$MTH,FUN=sum))
    rd <- as.numeric(by(dg$VALUE,dg$MTH,FUN=function(x) {length(which(x>=1))}))
    
    pr_jja <- sum(pr[6:8])
    rd_jja <- sum(rd[6:8])
    
    pr_ann <- sum(pr)
    rd_ann <- sum(rd)
    
    odat <- data.frame(YEAR=yr,MTH=c(1:12,"JJA","ANN"),PR=c(pr,pr_jja,pr_ann),RD=c(rd,rd_jja,rd_ann))
    odat_all <- rbind(odat_all,odat)
  }
  return(odat_all)
}



#re construct a daily data.frame using a dummy one and the corrected data
remake_daily <- function(his_list,bc_data,wleap,yi,yf) {
  cat("remaking daily data.frame\n")
  bc_data[,2:ncol(bc_data)] <- NA
  for (mth in 1:12) {
    #cat("calculating month",mth,"\n")
    #looping through years
    out_df <- data.frame()
    for (yr in yi:yf) {
      dg <- createDateGridCMIP5(yr,wleap)
      dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
      jdi <- min(dg$JD[which(dg$MTH == mth)])+1 #+1 to account that 1st column is year
      jdf <- max(dg$JD[which(dg$MTH == mth)])+1
      
      data_yr <- his_list[[paste("MTH.",mth,sep="")]]
      data_yr <- data_yr$VALUE_BC[which(data_yr$YEAR==yr)]
      bc_data[which(bc_data$YEAR==yr),jdi:jdf] <- data_yr
    }
  }
  return(bc_data)
}


#correct a given time series based on pre-fitted parameters
loci_correct <- function(his_data,loci_mets) {
  cat("correcting the data\n")
  #now apply the correction to the time series
  for (mth in 1:12) {
    s <- loci_mets$S[which(loci_mets$MTH==mth)]
    wdt_obs <- loci_mets$WDT_OBS[which(loci_mets$MTH==mth)]
    wdt_mod <- loci_mets$WDT_MOD[which(loci_mets$MTH==mth)]
    
    his_data[[paste("MTH.",mth,sep="")]]$VALUE_BC <- sapply(his_data[[paste("MTH.",mth,sep="")]]$VALUE,FUN=function(x) {y<-max(c(0,s*(x-wdt_mod)+wdt_obs));return(y)})
  }
  return(his_data)
}


#calibrate loci for all momths
loci_cal <- function(obs_list,his_list,wdt_obs=1,iter_step=0.0001) {
  cat("calculating loci metrics\n")
  out_mets <- data.frame()
  for (mth in 1:12) {
    ts_obs <- obs_list[[paste("MTH.",mth,sep="")]]
    ts_mod <- his_list[[paste("MTH.",mth,sep="")]]
    mth_mx <- loci_cal_mth(ts_obs,ts_mod,wdt_obs=wdt_obs,iter_step=iter_step)
    mth_mx <- cbind(MTH=mth,mth_mx)
    out_mets <- rbind(out_mets,mth_mx)
  }
  return(out_mets)
}

#calibrate loci for a month
loci_cal_mth <- function(ts_obs,ts_mod,wdt_obs=1,iter_step=0.0001) {
  #calculate number of wet days and average rain in wet days
  wdays_obs <- which(ts_obs$VALUE>=wdt_obs)
  nwd_obs <- length(wdays_obs)
  if (nwd_obs==0) {
    wet_obs <- 0
  } else {
    wet_obs <- mean(ts_obs$VALUE[wdays_obs],na.rm=T)
  }
  
  #find wet-day threshold for GCM
  wdt_mod <- find_wdt(ts_mod$VALUE,nwd_obs,iter_step=iter_step)
  wdays_mod <- which(ts_mod$VALUE>=wdt_mod)
  nwd_mod <- length(wdays_mod)
  if (nwd_mod==0) {
    wet_mod <- 0
  } else {
    wet_mod <- mean(ts_mod$VALUE[wdays_mod],na.rm=T)
  }
  
  #calculate s correction factor
  s <- (wet_obs-wdt_obs)/(wet_mod-wdt_mod)
  
  #put everything into a matrix
  out_mx <- data.frame(WDT_OBS=wdt_obs,NWD_OBS=nwd_obs,WET_OBS=wet_obs,
                       WDT_MOD=wdt_mod,NWD_MOD=nwd_mod,WET_MOD=wet_mod,S=s)
  return(out_mx)
}

#find the GCM wet-day threshold that matches the observed ones
find_wdt <- function(values,nwd_obs,iter_step=0.0001) {
  nwd_mod <- length(values)+1 #initialise
  wdt_mod <- iter_step*-1 #initialise
  
  if (length(which(values>=0)) < nwd_obs) {
    wdt_mod <- 0
  } else {
    while (nwd_mod > nwd_obs) {
      wdt_mod <- wdt_mod+iter_step
      nwd_mod <- length(which(values>=wdt_mod))
      nxt_nwd <- length(which(values>=(wdt_mod+iter_step)))
      
      #if the next value exceeds the value i'm looking for
      if (nxt_nwd < nwd_obs) {
        niter <- 0
        
        #until a value of iter_step is found so that it does not exceed
        #the value i'm looking for
        while (nxt_nwd < nwd_obs) {
          iter_step <- iter_step*0.1
          nxt_nwd <- length(which(values>=(wdt_mod+iter_step)))
          niter <- niter+1
        }
      }
    }
  }
  return(wdt_mod)
}

#### make a monthly list from the yearly matrices
mk_mth_list <- function(all_data,dg_fun="createDateGrid",dg_args=NA,yi,yf) {
  cat("making monthly list\n")
  out_all <- list()
  for (mth in 1:12) {
    #cat("calculating month",mth,"\n")
    
    #looping through years
    out_df <- data.frame()
    for (yr in yi:yf) {
      if (!is.na(dg_args)) {
        dg <- do.call(dg_fun,list(yr,dg_args))
      } else {
        dg <- do.call(dg_fun,list(yr))
      }
      #get days of interest (julian days of month mth)
      dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
      jdi <- min(dg$JD[which(dg$MTH == mth)])+1 #+1 to account that 1st column is year
      jdf <- max(dg$JD[which(dg$MTH == mth)])+1
      
      #get the data from the matrix
      data_yr <- all_data[which(all_data$YEAR==yr),]
      data_yr <- as.numeric(data_yr[,jdi:jdf])
      tmp_df <- data.frame(YEAR=yr,VALUE=data_yr)
      out_df <- rbind(out_df,tmp_df)
    }
    out_all[[paste("MTH.",mth,sep="")]] <- out_df
  }
  return(out_all)
}
