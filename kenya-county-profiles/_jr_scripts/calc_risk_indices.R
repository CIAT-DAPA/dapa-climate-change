#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#calculate risk indices based on weather data

### total rain
calc_totrain <- function(x,season_ini=1,season_end=365) {
  totrain <- sum(x$RAIN[season_ini:season_end],na.rm=T)
  return(totrain)
}


### number of rain days
calc_raindays <- function(x,season_ini=1,season_end=365,p_thresh=0.1) {
  raindays <- length(which(x$RAIN[season_ini:season_end] > p_thresh))
  return(raindays)
}


### maximum consecutive dry days
calc_max_cdd <- function(x,year=2000,season_ini=1,season_end=365,p_thresh=0.1) {
  cdd <- 0; cdd_seq <- c()
  for (i_x in season_ini:season_end) {
    if (x$RAIN[i_x] < p_thresh) {
      cdd <- cdd+1
    } else {
      cdd_seq <- c(cdd_seq, cdd)
      cdd <- 0
    }
  }
  max_cdd <- max(cdd_seq)
  return(max_cdd)
}


### mean consecutive dry days
calc_mean_cdd <- function(x,season_ini=1,season_end=365,p_thresh=0.1) {
  cdd <- 0; cdd_seq <- c()
  for (i_x in season_ini:season_end) {
    if (x$RAIN[i_x] < p_thresh) {
      cdd <- cdd+1
    } else {
      cdd_seq <- c(cdd_seq, cdd)
      cdd <- 0
    }
  }
  mean_cdd <- mean(cdd_seq[which(cdd_seq > 0)],na.rm=T)
  return(mean_cdd)
}


### number of rain days
calc_txxdays <- function(x,season_ini=1,season_end=365,t_thresh=30) {
  x$TDAY <- x$TMAX*0.75 + x$TMIN*0.25 #day temperature
  txxdays <- length(which(x$TDAY[season_ini:season_end] > t_thresh))
  return(txxdays)
}


### number of rain days
calc_tnndays <- function(x,season_ini=1,season_end=365,t_thresh=10) {
  x$TDAY <- x$TMAX*0.75 + x$TMIN*0.25 #day temperature
  tnndays <- length(which(x$TDAY[season_ini:season_end] < t_thresh))
  return(tnndays)
}


### calculate soilcap in mm
soilcap_calc <- function(x,minval,maxval) {
  rdepth <- max(c(x[4],minval)) #cross check
  rdepth <- min(c(rdepth,maxval)) #cross-check
  wc_df <- data.frame(depth=c(2.5,10,22.5,45,80,150),wc=(x[5:10])*.01)
  if (!rdepth %in% wc_df$depth) {
    wc_df1 <- wc_df[which(wc_df$depth < rdepth),]
    wc_df2 <- wc_df[which(wc_df$depth > rdepth),]
    y1 <- wc_df1$wc[nrow(wc_df1)]; y2 <- wc_df2$wc[1]
    x1 <- wc_df1$depth[nrow(wc_df1)]; x2 <- wc_df2$depth[1]
    ya <- (rdepth-x1) / (x2-x1) * (y2-y1) + y1
    wc_df <- rbind(wc_df1,data.frame(depth=rdepth,wc=ya),wc_df2)
  }
  wc_df <- wc_df[which(wc_df$depth <= rdepth),]
  wc_df$soilthick <- wc_df$depth - c(0,wc_df$depth[1:(nrow(wc_df)-1)])
  wc_df$soilcap <- wc_df$soilthick * wc_df$wc
  soilcp <- sum(wc_df$soilcap) * 10 #in mm
  return(soilcp)
}


#potential evapotranspiration
peest <- function(srad,tmin,tmax) {
  #constants
  albedo <- 0.2
  vpd_cte <- 0.7
  
  #soil heat flux parameters
  a_eslope=611.2
  b_eslope=17.67
  c_eslope=243.5
  
  #input parameters
  tmean <- (tmin+tmax)/2
  
  #net radiation
  rn = (1-albedo) * srad
  
  #soil heat flux
  eslope=a_eslope*b_eslope*c_eslope/(tmean+c_eslope)^2*exp(b_eslope*tmean/(tmean+c_eslope))
  
  #estimate vpd
  esat_min=0.61120*exp((17.67*tmin)/(tmin+243.5))
  esat_max=0.61120*exp((17.67*tmax)/(tmax+243.5))
  vpd=vpd_cte*(esat_max-esat_min) #kPa
  
  #Priestley-Taylor
  pt_const=1.26
  pt_fact=1
  vpd_ref=1
  psycho=62
  rho_w=997
  rlat_ht=2.26E6
  
  pt_coef=pt_fact*pt_const
  pt_coef = 1 + (pt_coef-1) * vpd / vpd_ref
  
  #*10^6? To convert fluxes MJ to J
  #rlat_ht? Latent heat flux to water flux
  #100/rho_w? Kg/m^2 to cm
  et_max=(pt_coef * rn * eslope/(eslope+psycho) * 10^6 / rlat_ht * 100/rho_w)*10 #in mm
  return(et_max)
}


#the two functions below estimate the ea/ep
#based on Jones (1987)
#ea/ep: actual to potential evapotranspiration ratio
eabyep_calc <- function(soilcp=100,cropfc=1,avail=50,rain,evap) {
  avail <- min(c(avail,soilcp))
  eratio <- eabyep(soilcp,avail)
  demand <- eratio*cropfc*evap
  result <- avail + rain - demand
  runoff <- result - soilcp
  avail <- min(c(soilcp,result))
  avail <- max(c(avail,0))
  runoff <- max(c(runoff,0))
  
  out <- data.frame(AVAIL=avail,DEMAND=demand,ERATIO=eratio,RAIN=rain,RUNOFF=runoff)
  
  return(out)
}


#ea/ep function
eabyep <- function(soilcp,avail) {
  percwt <- min(c(100,avail/soilcp*100))
  percwt <- max(c(1,percwt))
  eratio <- min(c(percwt/(97-3.868*sqrt(soilcp)),1))
  return(eratio)
}


#wrapper to calculate the water balance modeling variables
watbal_wrapper <- function(out_all,soilcp)  {
  out_all$ETMAX <- out_all$AVAIL <- out_all$ERATIO <- out_all$RUNOFF <- out_all$DEMAND <- out_all$CUM_RAIN <- NA
  for (d in 1:nrow(out_all)) {
    out_all$ETMAX[d] <- peest(out_all$SRAD[d],out_all$TMIN[d],out_all$TMAX[d])
    
    if (d==1) {
      out_all$CUM_RAIN[d] <- out_all$RAIN[d]
      sfact <- eabyep_calc(soilcp=soilcp,cropfc=1,avail=0,rain=out_all$RAIN[d],evap=out_all$ETMAX[d])
      out_all$AVAIL[d] <- sfact$AVAIL
      out_all$ERATIO[d] <- sfact$ERATIO
      out_all$RUNOFF[d] <- sfact$RUNOFF
      out_all$DEMAND[d] <- sfact$DEMAND
      
    } else {
      out_all$CUM_RAIN[d] <- out_all$CUM_RAIN[d-1] + out_all$RAIN[d]
      sfact <- eabyep_calc(soilcp=soilcp,cropfc=1,avail=out_all$AVAIL[d-1],rain=out_all$RAIN[d],evap=out_all$ETMAX[d])
      out_all$AVAIL[d] <- sfact$AVAIL
      out_all$ERATIO[d] <- sfact$ERATIO
      out_all$RUNOFF[d] <- sfact$RUNOFF
      out_all$DEMAND[d] <- sfact$DEMAND
    }
  }
  return(out_all)
}


#calculate number of water stress days
calc_wsdays <- function(x,season_ini=1,season_end=365,e_thresh=0.3) {
  wsdays <- length(which(x$ERATIO[season_ini:season_end] < e_thresh))
  return(wsdays)
}


#HTS1, HTS2, LETHAL: heat stress using tmax
calc_hts <- function(x,season_ini=1,season_end=365,t_thresh=35) {
  hts <- length(which(x$TMAX[season_ini:season_end] >= t_thresh))
  return(hts)
}


#CD: crop duration, if Tmean > (22, 23, 24) then CD=T-23, else CD=0
calc_cdur <- function(x,season_ini=1,season_end=365,t_thresh=35) {
  x$TMEAN <- (x$TMIN + x$TMAX) * 0.5
  tmean <- mean(x$TMEAN[season_ini:season_end], na.rm=T)
  if (tmean > t_thresh) {cdur <- tmean - t_thresh} else {cdur <- 0}
  return(cdur)
}


#DS2: max number of consecutive days Ea/Ep < 0.4, 0.5, 0.6
calc_cons_wsdays <- function(x,season_ini=1,season_end=365,e_thresh=0.4) {
  cdd <- 0; cdd_seq <- c()
  for (i_x in season_ini:season_end) {
    if (x$ERATIO[i_x] < e_thresh) {
      cdd <- cdd+1
    } else {
      cdd_seq <- c(cdd_seq, cdd)
      cdd <- 0
    }
  }
  cdd_seq <- c(cdd_seq, cdd)
  max_cdd <- max(cdd_seq)
  return(max_cdd)
}


#ATT: accum thermal time using capped top, Tb=7,8,9, To=30,32.5,35
calc_att <- function(x,season_ini=1,season_end=365,tb=10,to=20) {
  x$TMEAN <- (x$TMIN + x$TMAX) * 0.5
  att <- sapply(x$TMEAN[season_ini:season_end], ttfun, tb, to)
  att <- sum(att,na.rm=T)
  return(att)
}


#function to calc tt
ttfun <- function(tmean,tb,to) {
  if (tmean<to & tmean>tb) {
    teff <- tmean-tb
  } else if (tmean>=to) {
    teff <- to-tb
  } else if (tmean<=tb) {
    teff <- 0
  }
  return(teff)
}


#DLOSS: duration loss (difference between No. days to reach ATT_baseline in future vs. baseline)
calc_dloss <- function(x,season_ini,dur_b=110,att_b=5000,tb=10,to=20) {
  x$TMEAN <- (x$TMIN + x$TMAX) * 0.5
  att <- sapply(x$TMEAN[season_ini:(nrow(x))], ttfun, tb, to)
  att <- cumsum(att)
  estdur <- length(att[which(att < att_b)])
  dloss <- dur_b - estdur
  return(dloss)
}


#WES: wet early season if period between sowing and anthesis is above field cap. >= 50 % time
#     i.e. frequency of days if RUNOFF > 1
calc_wes <- function(x,season_ini,season_end,r_thresh=1) {
  wes <- length(which(x$RUNOFF[season_ini:season_end] > r_thresh))
  return(wes)
}


#BADSOW: no. days in sowing window +-15 centered at sdate with 0.05*SOILCP < AVAIL < 0.9*SOILCP
#        if this is < 3 then crop runs into trouble
calc_badsow <- function(x,season_ini,soilcp) {
  sow_i <- season_ini - 15; sow_f <- season_ini + 15
  if (sow_i < 1) {sow_i <- 1}; if (sow_f > 365) {sow_f <- 365}
  x <- x[sow_i:sow_f,]
  badsow <- length(which(x$AVAIL > (0.05*soilcp) & x$AVAIL < (0.9*soilcp)))
  return(badsow)
}


#BADHAR: no. days in harvest window (+25 after hdate) with AVAIL < 0.85*SOILCP
#        if this is < 3 then crop runs into trouble
calc_badhar <- function(x,season_end,soilcp) {
  har_i <- season_end
  har_f <- har_i + 25; if (har_f > 365) {har_f <- 365}
  x <- x[har_i:har_f,]
  badhar <- length(which(x$AVAIL < (0.85*soilcp)))
  return(badhar)
}
