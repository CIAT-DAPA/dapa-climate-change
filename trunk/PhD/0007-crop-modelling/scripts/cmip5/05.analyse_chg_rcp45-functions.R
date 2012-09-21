#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012


#function to get the data
get_wth <- function(wthFil,century=1900) {
  wth <- read.fortran(wthFil,format=c("I5","F6","3F7"),skip=4)
  names(wth) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
  wth <- cbind(YEAR=(as.numeric(substr(wth$DATE,1,2))+century),JDAY=as.numeric(substr(wth$DATE,3,5)),wth)
  return(wth)
}

#function to wrap the watbal function
do_wbal <- function(wth) {
  wth$ETMAX <- NA; wth$AVAIL <- NA; wth$ERATIO <- NA
  wth$CUM_RAIN <- NA; wth$RUNOFF <- NA; wth$DEMAND <- NA
  wth <- watbal_wrapper(wth)
  wth$TMEAN <- (wth$TMAX + wth$TMIN)/2
  return(wth)
}

#function to get the metrics for a particular gs
do_metrics <- function(wth,gs_i,gs_f) {
  #select growing season
  wth <- wth[which(wth$JDAY >= gs_i & wth$JDAY <= gs_f),]
  row.names(wth) <- 1:nrow(wth)
  yr <- wth$YEAR[1]
  
  #calculate each metric
  #a. rainfall during groundnut growing season
  rain <- sum(wth$RAIN)
  
  #b. mean temperature during groundnut growing season
  tmen <- mean(wth$TMEAN)
  
  #c. number of days with rain > 0mm, 2mm, 5mm, 10mm, 15mm, 20mm
  rd_0 <- length(which(wth$RAIN>0))
  rd_2 <- length(which(wth$RAIN>2))
  rd_5 <- length(which(wth$RAIN>5))
  rd_10 <- length(which(wth$RAIN>10))
  rd_15 <- length(which(wth$RAIN>15))
  rd_20 <- length(which(wth$RAIN>20))
  
  #d. rainfall std, rainfall c.v.
  rstd <- sd(wth$RAIN)
  if (mean(wth$RAIN) == 0) {
    rcov <- rstd/1
  } else {
    rcov <- rstd/mean(wth$RAIN)
  }
  
  #e. number of days TMAX>34 (HTS)
  hts_34 <- length(which(wth$TMAX>34))
  
  #f. number of days TMAX>40 (HTS)
  hts_40 <- length(which(wth$TMAX>40))
  
  #g. number of days TMEAN>35 (TETRS)
  tetr_35 <- length(which(wth$TMEAN>35))
  
  #h. number of days TMEAN>47 (TETRS)
  tetr_47 <- length(which(wth$TMEAN>47))
  
  #i. tmean std, tmean c.v.
  tstd <- sd(wth$TMEAN)
  tcov <- tstd/tmen
  
  #j. number of days with Ea/Ep ratio < 0.25, 0.5, 0.75
  eratio_25 <- length(which(wth$ERATIO<0.25))
  eratio_50 <- length(which(wth$ERATIO<0.5))
  eratio_75 <- length(which(wth$ERATIO<0.75))
  
  #from Trnka et al. (2011) GCB
  #k. sum of global radiation of days with daily mean temperature >8, 
  #   daily minimum temperature >0, and ETRATIO>0.5
  #   (sum of effective global radiation)
  effsrad <- sum(wth$SRAD[which(wth$TMEAN > 8 & wth$TMIN > 0 & wth$ERATIO > 0.5)])
  
  #l. number of days with daily mean temperature >8, daily minimum
  #   temperature >0 and ERATIO>0.5
  #   (sum of effective growing days)
  effgd <- length(which(wth$TMEAN > 8 & wth$TMIN > 0 & wth$ERATIO > 0.5))
  
  #output row
  orow <- data.frame(YEAR=yr,SOW=gs_i,HAR=gs_f,RAIN=rain,RSTD=rstd,RCOV=rcov,RD.0=rd_0,
                     RD.2=rd_2,RD.5=rd_5,RD.10=rd_10,RD.15=rd_15,RD.20=rd_20,
                     HTS1=hts_34,HTS2=hts_40,TETR1=tetr_35,TETR2=tetr_47,
                     ERATIO.25=eratio_25,ERATIO.50=eratio_50,ERATIO.75=eratio_75,
                     TSTD=tstd,TMEN=tmen,TCOV=tcov,EFF.SRAD=effsrad,EFF.GD=effgd)
  
  return(orow)
}
