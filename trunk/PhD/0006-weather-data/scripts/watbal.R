#Julian Ramirez-Villegas
#2012

#Estimates potential evapotranspiration based on the Priestley-Taylor equation
#based on Challinor et al. (2004)

#constants
albedo <- 0.2
vpd_cte <- 0.7

#soil heat flux parameters
a_eslope=611.2
b_eslope=17.67
c_eslope=243.5

#transform solar radiation from W/m2 to MJ/m2
#DAILYRAD(K)=DAILYRAD(K)*60*60*24/1000000

peest <- function(srad,tmin,tmax) {
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
  et_max=(pt_coef * rn * eslope/(eslope+psycho) * 10^6 / rlat_ht * 100/rho_w)*10
  return(et_max)
}


#the two functions below estimate the ea/ep
#based on Jones (1987)
#ea/ep: actual to potential evapotranspiration ratio
eabyep_calc <- function(soilcp=100,cropfc=1,avail=50,rain,evap) {
  avail <- min(avail,soilcp)
  eratio <- eabyep(soilcp,avail)
  demand <- eratio*cropfc*evap
  result <- avail + rain - demand
  runoff <- result - soilcp
  avail <- min(c(soilcp,result))
  avail <- max(c(avail,0))
  runoff <- max(runoff,0)
  
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

#######
#find the start and end days for the growing season
#find various possible starts and various possible ends
gsl_find <- function(eratio,ea_thresh=0.5,n_start=5,n_end=12,sd_default=1,ed_default=365) {
  start_day <- 0; sday_list <- 0
  end_day <- 0; eday_list <- 0
  ctr <- 0
  sctr <- 0; ectr <- 0
  for (d in 1:length(eratio)) {
    #determine start day
    if (start_day==0) {
      end_day <- 0
      if (eratio[d] > ea_thresh) {
        ctr <- ctr+1
      } else {
        ctr <- 0
      }
      if (ctr==n_start) {
        start_day <- d
        if (sctr==0) {
          sday_list <- start_day
        } else {
          sday_list <- c(sday_list,start_day)
        }
        cat("season started on day",start_day,"\n")
        ctr <- 0
        sctr <- sctr+1
      }
    }
    
    #closing the season
    if (end_day == 0 & start_day != 0) {
      if (eratio[d] <= ea_thresh) {
        ctr <- ctr+1
      } else {
        ctr <- 0
      }
      if (ctr==n_end) {
        if (ectr==0) {
          end_day <- d
          eday_list <- end_day
        } else {
          end_day <- d
          eday_list <- c(eday_list,end_day)
        }
        cat("season ended on day",d,"\n")
        ctr <- 0
        start_day <- 0
        ectr <- ectr+1
      }
    }
  }
  
  if (length(sday_list)>length(eday_list)) {eday_list <- c(eday_list,365)}
  
  #emergency to defaults
  if (length(sday_list)==1) {if (sday_list == 0) {sday_list <- sd_default}}
  if (length(eday_list)==1) {if (eday_list == 0) {eday_list <- ed_default}}
  
  out <- data.frame(START=sday_list,END=eday_list)
  return(out)
}


#wrapper to calculate the water balance modeling variables
watbal_wrapper <- function(out_all)  {
  for (d in 1:nrow(out_all)) {
    out_all$ETMAX[d] <- peest(out_all$SRAD[d],out_all$TMIN[d],out_all$TMAX[d])
    
    if (d==1) {
      out_all$CUM_RAIN[d] <- out_all$RAIN[d]
      sfact <- eabyep_calc(soilcp=100,cropfc=1,avail=0,rain=out_all$RAIN[d],evap=out_all$ETMAX[d])
      out_all$AVAIL[d] <- sfact$AVAIL
      out_all$ERATIO[d] <- sfact$ERATIO
      out_all$RUNOFF[d] <- sfact$RUNOFF
      out_all$DEMAND[d] <- sfact$DEMAND
      
    } else {
      out_all$CUM_RAIN[d] <- out_all$CUM_RAIN[d-1] + out_all$RAIN[d]
      sfact <- eabyep_calc(soilcp=100,cropfc=1,avail=out_all$AVAIL[d-1],rain=out_all$RAIN[d],evap=out_all$ETMAX[d])
      out_all$AVAIL[d] <- sfact$AVAIL
      out_all$ERATIO[d] <- sfact$ERATIO
      out_all$RUNOFF[d] <- sfact$RUNOFF
      out_all$DEMAND[d] <- sfact$DEMAND
    }
  }
  return(out_all)
}


#determine indicators
# 1. number of stress days (days with ea_thresh<0.15)
# 2. number of rain days
# 3. maximum number of consecutive dry days
# 4. number of days with temperature >10 (Challinor et al. 2004)
# 5. number of days with temperature >28
# 5. number of days with temperature >34
# 6. number of days with temperature >40
# 7. number of days with temperature >50
# 8. total rainfall during growing period
# 9. growing degree days with temperature above 10 Celsius

#calculate gs metrics
gs_metrics <- function(out_all,gs_sel,thresh=0.5,tbase=10,topt=28,tmax=50,tcrit=34,tlim=40) {
  stress_days <- 0; rain_days <- 0; cons_dd <- 0; ddays <- 0
  tb <- 0; to <- 0; tx <- 0; txcrit <- 0; txlim <- 0
  rain <- 0; gdd <- 0
  
  gs_tmean <- (out_all$TMIN + out_all$TMAX)/2
  gs_tmean <- mean(gs_tmean)
  
  #matrix of dates
  dgrid <- createDateGrid(year)
  dgrid$MONTH <- substr(dgrid$MTH.DAY,1,3)
  dgrid$MONTH <- as.numeric(gsub("M","",dgrid$MONTH))
  
  #calc Q1, Q2, Q3, Q4  raindays, rain, tmean
  days_q1 <- which(dgrid$MONTH %in% c(1:3))
  rain_q1 <- sum(out_all$RAIN[days_q1])
  tean_q1 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q1 <- mean(tean_q1[days_q1])
  rdays_q1 <- length(which(out_all$RAIN[days_q1] > 0))
  
  days_q2 <- which(dgrid$MONTH %in% c(4:6))
  rain_q2 <- sum(out_all$RAIN[days_q2])
  tean_q2 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q2 <- mean(tean_q2[days_q2])
  rdays_q2 <- length(which(out_all$RAIN[days_q2] > 0))
  
  days_q3 <- which(dgrid$MONTH %in% c(7:9))
  rain_q3 <- sum(out_all$RAIN[days_q3])
  tean_q3 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q3 <- mean(tean_q3[days_q3])
  rdays_q3 <- length(which(out_all$RAIN[days_q3] > 0))
  
  days_q4 <- which(dgrid$MONTH %in% c(10:12))
  rain_q4 <- sum(out_all$RAIN[days_q4])
  tean_q4 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q4 <- mean(tean_q4[days_q4])
  rdays_q4 <- length(which(out_all$RAIN[days_q4] > 0))
  
  #calc SEM1, SEM2
  days_s1 <- which(dgrid$MONTH %in% c(1:6))
  rain_s1 <- sum(out_all$RAIN[days_s1])
  tean_s1 <- (out_all$TMIN + out_all$TMAX)/2
  tean_s1 <- mean(tean_s1[days_s1])
  rdays_s1 <- length(which(out_all$RAIN[days_s1] > 0))
  
  days_s2 <- which(dgrid$MONTH %in% c(7:12))
  rain_s2 <- sum(out_all$RAIN[days_s2])
  tean_s2 <- (out_all$TMIN + out_all$TMAX)/2
  tean_s2 <- mean(tean_s2[days_s2])
  rdays_s2 <- length(which(out_all$RAIN[days_s2] > 0))
  
  #loop through growing season days
  for (gday in gs_sel$START:gs_sel$END) {
    if (out_all$ERATIO[gday] < thresh) {stress_days <- stress_days+1}
    if (out_all$RAIN[gday] > 0) {rain_days <- rain_days+1}
    if (out_all$RAIN[gday] == 0) {
      #cat(ddays," ")
      ddays <- ddays+1
      if (ddays >= cons_dd) {
        cons_dd <- ddays
      }
    } else {
      ddays <- 0
    }
    tmean <- (out_all$TMIN[gday]+out_all$TMAX[gday])/2
    if (tmean > 10) {tb <- tb+1}
    if (tmean > 28) {to <- to+1}
    if (tmean > 50) {tx <- tx+1}
    if (out_all$TMAX[gday] > 34) {txcrit <- txcrit+1}
    if (out_all$TMAX[gday] > 40) {txlim <- txlim+1}
    rain <- rain+out_all$RAIN[gday]
    
    if (tmean < tbase) {
      gdd <- gdd+0
    } else  if (tmean >= tbase & tmean <= topt) {
      gdd <- gdd + (tmean-tbase)
    } else if (tmean > topt) {
      gdd <- gdd + (topt-tbase)
    }
    
  }
  out <- data.frame(RAIN=rain,GDD=gdd,STRESS_DAYS=stress_days,RDAYS=rain_days,
                    X_CONS_DD=cons_dd,D_TB=tb,D_TO=to,D_TX=tx,
                    D_TXCRIT=txcrit,D_TXLIM=txlim,GS_TMEAN=gs_tmean,
                    Q1_TMEAN=tean_q1,Q1_RAIN=rain_q1,Q1_RDAYS=rdays_q1,
                    Q2_TMEAN=tean_q2,Q2_RAIN=rain_q2,Q2_RDAYS=rdays_q2,
                    Q3_TMEAN=tean_q3,Q3_RAIN=rain_q3,Q3_RDAYS=rdays_q3,
                    Q4_TMEAN=tean_q4,Q4_RAIN=rain_q4,Q4_RDAYS=rdays_q4,
                    S1_TMEAN=tean_s1,S1_RAIN=rain_s1,S1_RDAYS=rdays_s1,
                    S2_TMEAN=tean_s2,S2_RAIN=rain_s2,S2_RDAYS=rdays_s2)
  return(out)
}

