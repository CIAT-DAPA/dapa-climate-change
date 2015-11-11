#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

require(RMAWGEN)

#function to generate daily SRAD data for a new year with precip data, using historical data
wgen_srad <- function(daily_hist,dates,year,lon,lat) {
  ### input matrices and vectors
  #mids depending on leap or not leap year
  if (leap.year(year)) {
    mth_mids <- c(16,46,75,106,136,167,197,228,259,289,320,350) + 1
    mth_ends <- c(31,59,90,120,151,181,212,243,273,304,334,365) + 1
  } else {
    mth_mids <- c(16,46,75,106,136,167,197,228,259,289,320,350)
    mth_ends <- c(31,59,90,120,151,181,212,243,273,304,334,365)
  }
  
  #A and B matrices for generator (not used in my implementation)
  a_vals <- matrix(data=c(0.567,0.253,-0.006,0.086,0.504,-0.039,-0.002,-0.050,0.244),ncol=3,nrow=3)
  b_vals <- matrix(data=c(0.781,0.328,0.238,0.0,0.637,-0.341,0.0,0.0,0.873),ncol=3,nrow=3)
  
  ### compute climatological mean and s.d. of srad for all, wet and dry days, per month
  for (jday in 1:nrow(daily_hist)) {
    if (jday == 1) {
      if (daily_hist$prate[jday] < 0.001) {
        srdry <- daily_hist$srad[jday]; srwet <- NA
      } else {
        sdry <- NA; srwet <- daily_hist$srad[jday]
      }
    } else {
      if (daily_hist$prate[jday] < 0.001) {
        srdry <- c(srdry,daily_hist$srad[jday]); srwet <- c(srwet,NA)
      } else {
        srdry <- c(srdry,NA); srwet <- c(srwet,daily_hist$srad[jday])
      }
    }
  }
  daily_hist$srdry <- srdry; daily_hist$srwet <- srwet
  
  ### all days
  #sradmn <- aggregate(daily_hist[,c("srad")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  #sradmn <- aggregate(sradmn[,"x"],by=list(month=sradmn$month),FUN=function(x) {mean(x,na.rm=T)})
  #sradsd <- aggregate(daily_hist[,c("srad")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  #sradsd <- aggregate(sradsd[,"x"],by=list(month=sradsd$month),FUN=function(x) {mean(x,na.rm=T)})
  sradmn <- aggregate(daily_hist[,c("srad")],by=list(month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  sradsd <- aggregate(daily_hist[,c("srad")],by=list(month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  
  ### dry days
  #srdrymn <- aggregate(daily_hist[,c("srdry")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  #srdrymn <- aggregate(srdrymn[,"x"],by=list(month=srdrymn$month),FUN=function(x) {mean(x,na.rm=T)})
  #srdrysd <- aggregate(daily_hist[,c("srdry")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  #srdrysd <- aggregate(srdrysd[,"x"],by=list(month=srdrysd$month),FUN=function(x) {mean(x,na.rm=T)})
  srdrymn <- aggregate(daily_hist[,c("srdry")],by=list(month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  srdrysd <- aggregate(daily_hist[,c("srdry")],by=list(month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  
  ### wet days
  #srwetmn <- aggregate(daily_hist[,c("srwet")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  #srwetmn <- aggregate(srwetmn[,"x"],by=list(month=srwetmn$month),FUN=function(x) {mean(x,na.rm=T)})
  #srwetsd <- aggregate(daily_hist[,c("srwet")],by=list(year=daily_hist$year,month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  #srwetsd <- aggregate(srwetsd[,"x"],by=list(month=srwetsd$month),FUN=function(x) {mean(x,na.rm=T)})
  srwetmn <- aggregate(daily_hist[,c("srwet")],by=list(month=daily_hist$month),FUN=function(x) {mean(x,na.rm=T)})
  srwetsd <- aggregate(daily_hist[,c("srwet")],by=list(month=daily_hist$month),FUN=function(x) {sd(x,na.rm=T)})
  
  #create matrix with monthly data
  mth_data <- data.frame(month=1:12,sradmn=sradmn$x,sradsd=sradsd$x,srdrymn=srdrymn$x,
                         srdrysd=srdrysd$x,srwetmn=srwetmn$x,srwetsd=srwetsd$x)
  
  ### calculate monthly correction factors (because values will be linearly interpolated)
  mth_cf <- mth_data; mth_cf[,2:ncol(mth_cf)] <- NA
  for (icol in 2:ncol(mth_cf)) {
    for (imth in 1:12) {
      #imth <- 1
      prevmth <- imth-1; nextmth <- imth+1
      if (prevmth < 1) {prevmth <- 12}
      if (nextmth > 12) {nextmth <- 1}
      mth_cf[imth,icol] = mth_data[imth,icol] - (0.25 * (mth_data[prevmth,icol] + 2 * mth_data[imth,icol] + mth_data[nextmth,icol]))
    }
  }
  
  ### run the generator
  xprv <- 0
  srad_vals <- c() ; rada_vals <- raw_vals <- raw_intpol_val <- c()
  for (jday in 1:nrow(dates)) {
    #jday <- 1
    tdate <- paste(dates$date[jday])
    tmth <- as.numeric(unlist(strsplit(tdate,"-",fixed=T))[2])
    
    #linear interpolation, depending on whether we are past or not the middle of the month
    #the interpolation basis change, i.e. mid of previous and mid of actual vs.
    #mid of actual and mid of next.
    if (jday < mth_mids[tmth]) {
      prevmth <- tmth - 1
      if (prevmth < 1) {prevmth <- 12}
      prev_mid <- mth_mids[prevmth]
      this_mid <- mth_mids[tmth]
      if (prev_mid > this_mid) {prev_mid <- prev_mid - nrow(dates)}
      
      #interpolate mean and s.d. of all days, dry days and wet days
      sradmn_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$sradmn[tmth] - mth_data$sradmn[prevmth]) + mth_data$sradmn[prevmth]
      sradsd_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$sradsd[tmth] - mth_data$sradsd[prevmth]) + mth_data$sradsd[prevmth]
      srdrymn_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$srdrymn[tmth] - mth_data$srdrymn[prevmth]) + mth_data$srdrymn[prevmth]
      srdrysd_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$srdrysd[tmth] - mth_data$srdrysd[prevmth]) + mth_data$srdrysd[prevmth]
      srwetmn_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$srwetmn[tmth] - mth_data$srwetmn[prevmth]) + mth_data$srwetmn[prevmth]
      srwetsd_val <- (jday-prev_mid) / (this_mid-prev_mid) * (mth_data$srwetsd[tmth] - mth_data$srwetsd[prevmth]) + mth_data$srwetsd[prevmth]
    } else if (jday > mth_mids[tmth]) {
      nextmth <- tmth + 1
      if (nextmth > 12) {nextmth <- 1}
      next_mid <- mth_mids[nextmth]
      this_mid <- mth_mids[tmth]
      if (next_mid < this_mid) {next_mid <- next_mid + nrow(dates)}
      
      #interpolate mean and s.d. of all days, dry days and wet days
      sradmn_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$sradmn[nextmth] - mth_data$sradmn[tmth]) + mth_data$sradmn[tmth]
      sradsd_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$sradsd[nextmth] - mth_data$sradsd[tmth]) + mth_data$sradsd[tmth]
      srdrymn_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$srdrymn[nextmth] - mth_data$srdrymn[tmth]) + mth_data$srdrymn[tmth]
      srdrysd_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$srdrysd[nextmth] - mth_data$srdrysd[tmth]) + mth_data$srdrysd[tmth]
      srwetmn_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$srwetmn[nextmth] - mth_data$srwetmn[tmth]) + mth_data$srwetmn[tmth]
      srwetsd_val <- (jday-this_mid) / (next_mid-this_mid) * (mth_data$srwetsd[nextmth] - mth_data$srwetsd[tmth]) + mth_data$srwetsd[tmth]
    } else {
      #if in the middle of the month no need to interpolate
      sradmn_val <- mth_data$sradmn[tmth]
      sradsd_val <- mth_data$sradsd[tmth]
      srdrymn_val <- mth_data$srdrymn[tmth]
      srdrysd_val <- mth_data$srdrysd[tmth]
      srwetmn_val <- mth_data$srwetmn[tmth]
      srwetsd_val <- mth_data$srwetsd[tmth]
    }
    
    #apply correction factors
    sradmn_val <- sradmn_val + mth_cf$sradmn[tmth]
    sradsd_val <- sradsd_val + mth_cf$sradsd[tmth]
    srdrymn_val <- srdrymn_val + mth_cf$srdrymn[tmth]
    srdrysd_val <- srdrysd_val + mth_cf$srdrysd[tmth]
    srwetmn_val <- srwetmn_val + mth_cf$srwetmn[tmth]
    srwetsd_val <- srwetsd_val + mth_cf$srwetsd[tmth]
    
    #is it a wet or dry day
    train <- dates$prec[jday]
    if (train < 0.001) {iswet <- F} else {iswet <- T}
    
    #assign srad values if dry or if wet
    if (iswet) {
      srmn_val <- srwetmn_val
      srsd_val <- srwetsd_val
    } else {
      srmn_val <- srdrymn_val
      srsd_val <- srdrysd_val
    }
    
    #if missing because in recent history there were <= 1 wet or dry days in that month, replace
    #by mean and s.d. computed using all data
    if (is.na(srmn_val)) {srmn_val <- sradmn_val}
    if (is.na(srsd_val)) {srsd_val <- sradsd_val}
    
    #calculate extraterrestrial radiation
    #initialise
    t_val <- 2 * pi * (jday + 10) / 365
    c1_val <- cos(t_val)
    
    #declination
    dec_val <- -23.45 * c1_val
    ssin_val <- sin(pi/180 * dec_val) * sin(pi/180 * lat)
    ccos_val <- cos(pi/180 * dec_val) * cos(pi/180 * lat)
    soc_val <- ssin_val / ccos_val
    soc_val <- min(c(max(c(soc_val,-1)),1))
    
    #daylength
    dayl_val <- 12 + 24 * asin(soc_val) / pi
    dsinb_val <- 3600 * (dayl_val * ssin_val + 24 / pi * ccos_val * sqrt(1 - soc_val^2))
    sc_val <- 1368 * (1 + 0.033 * cos(2 * pi * jday / 365))
    
    #extra terrestrial radiation
    s0d_val <- sc_val * dsinb_val
    rada_val <- s0d_val / 1e6
    
    #acceptable lower and upper bounds for this site
    rc_val <- rada_val * 0.8 #upper bound
    srmin_val <- 0.2 * rc_val #lower bound
    
    #   DSSAT's version  of adding s.d. (i.e. noise) to the data
    #   k_val <- 1; e_val <- c()
    #   while (k_val <= 3) {
    #     rn1_val <- runif(1,min=0,max=1)
    #     rn2_val <- runif(1,min=0,max=1)
    #     v_val <- sqrt(-2 * log(rn1_val)) * cos(2 * pi * rn2_val)
    #     if (abs(v_val) <= 2.5) {
    #       e_val <- c(e_val,v_val)
    #       k_val <- k_val + 1
    #     }
    #   }
    #   
    #   r_val <- 0; rr_val <- 0
    #   for (j_val in 1:3) {
    #     r_val <- r_val + b_vals[j_val,3] * e_val[j_val]
    #     rr_val <- rr_val + a_vals[j_val,3] * xprv
    #   }
    #   x_val <- r_val + rr_val
    #   srad_val <- x_val*srsd_val + srmn_val
    
    #my version of adding noise to the data
    srad_val <- runif(1,-1,1)*srsd_val + srmn_val
    
    #record and cross-check value
    raw_val <- srad_val
    srad_val <- min(c(max(c(srad_val,srmin_val)),rc_val))
    
    #append objects
    srad_vals <- c(srad_vals, srad_val)
    rada_vals <- c(rada_vals, rada_val)
    raw_vals <- c(raw_vals, raw_val)
    raw_intpol_val <- c(raw_intpol_val,srmn_val)
    
    #srad value from previous day
    xprv <- srad_val
  }
  
  #put data into dates matrix
  dates$srad <- srad_vals
  #dates$rada <- rada_vals
  #dates$raw <- raw_vals
  #dates$intpol <- raw_intpol_val
  
  #return object
  return(dates)
}

