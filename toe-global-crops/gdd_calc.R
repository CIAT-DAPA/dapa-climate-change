#JRV Jan 2017
#Borrows from calc_risk_indices.R in p4s-csa/hh-analyses

#ATT: accum thermal time using capped top, specify tb and to
calc_att <- function(x,tb=10,to=20) {
  x$TMEAN <- (x$tmin + x$tmax) * 0.5
  att <- sapply(x$TMEAN, ttfun, tb, to)
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
