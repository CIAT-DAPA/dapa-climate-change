#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

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
  nwd_mod <- length(values)+1
  wdt_mod <- 0
  while (nwd_mod > nwd_obs) {
    nwd_mod <- length(which(values>=wdt_mod))
    wdt_mod <- wdt_mod+iter_step
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
