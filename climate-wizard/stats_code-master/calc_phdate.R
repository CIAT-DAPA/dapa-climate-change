#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#calculate planting date based on the following rules, papers:
#Marteau et al. (2011) doi:10.1016/j.agrformet.2011.05.018
#   * Sowing occurs on or after 2 wet days totalling >= 10 mm, after 15th April (73 % farmers)
#   * Resowing occurs if >= 7 days are dry (< 1mm) after sowing
#   * Latest planting date was 30th June +- 19 days
#   * Typical duration is 90-105 days (83 % farmers) and 120-150 days (13 % farmers)
#   * Harvesting is latest done by 27th Oct

#Ben Mohamed et al. (2002) doi:10.1023/A:1016189605188
#   * When rain in 3-day period is >= 25 mm and no dry spell of >= 7 days occurs in the next 
#     30-days
#   * End of season: rainy day after which 20-day rain is < 5 mm

#Wolf et al. (2015) doi:10.1016/j.agrformet.2015.08.262
#   * During DOY 160-200 cum rain >= 30 mm (sudano-sahelian and south zone, maize, Burkina Faso)
#   * During DOY 160-200 cum rain >= 30 mm (sudano-sahelian zone, short-dur sorghum, Burkina Faso)
#   * During DOY 160-200 cum rain >= 20 mm (sudano-sahelian zone, long-dur sorghum, Burkina Faso)
#   * During DOY 160-200 cum rain >= 2o mm (south zone, sorghum, Burkina Faso)


gcm <- "ACCESS1-0"
iDir <- paste0("D:/CIAT/Projects/wocat/AR5_Global_Daily_25k/", gcm, "/junk")
rcp <- "historical"
y <- 1950

prRs <- stack(paste0(iDir, "/prmm_day_BCSD_", rcp, "_r1i1p1_", gcm, "_", y, ".nc4"))
pdate_marteau(prRs, 105,201,p_thresh=1)


#planting date based on Marteau et al. (2011)
pdate_marteau <- function(x,mindate=105,maxdate=201,p_thresh=1) {
  #calculate rain of 2-days, if >= 10 then assume sowing on third day
  #then calculate number of days in next 7 days after sowing that have < 1 mm rain, 
  #assume resowing, and update sowing date
  pdate <- NA
  for (d in mindate:(nlayers(x)-1)) {
    #d <- mindate
    if (is.na(pdate)) {
      sum2day <- sum(x[[d:(d+1)]])
      if (sum2day >= 10) {
        pdate <- d+2
        rain7day <- x$RAIN[(pdate+1):(pdate+7)]
        rday7day <- length(which(rain7day < p_thresh))
        if (rday7day >= 7) {pdate <- pdate+7}
        if (pdate > maxdate) {pdate <- maxdate}
      }
    }
  }
  return(pdate)
}

#end of season date according to Mohamed et al. (2002)
hdate_mohamed <- function(x,mindate,maxdate,p_thresh=1) {
  #rainy day after which 20-day rain is < 5 mm
  #calculate 20-day rainfall, if < 5mm then assign as harvest date
  hdate <- NA
  for (d in mindate:(nrow(x)-19)) {
    if (is.na(hdate)) {
      if (x$RAIN[d] >= p_thresh) {
        sum20day <- sum(x$RAIN[(d):(d+19)])
        if (sum20day < 5) {
          hdate <- d
          if (hdate > maxdate) {hdate <- maxdate}
        }
      }
    }
  }
  #if (is.na(hdate)) {hdate <- maxdate}
  return(hdate)
}

#calculate hdate following Jones & Thornton (2009)
hdate_jones <- function(x,mindate,maxdate,e_thresh) {
  #season has ended once 12 consecutive nongrowing days (eratio<0.2, here) have occurred
  #calculate 12-day count, if < 5mm then assign as planting date
  hdate <- NA
  for (d in mindate:(nrow(x)-11)) {
    if (is.na(hdate)) {
      etseq <- x$ERATIO[(d):(d+11)]
      etcount <- length(which(etseq < e_thresh))
      if (etcount >= 12) {
        hdate <- d+11
        if (hdate > maxdate) {hdate <- maxdate}
      }
    }
  }
  return(hdate)
}


