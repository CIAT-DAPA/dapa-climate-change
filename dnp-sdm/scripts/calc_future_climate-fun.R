#JRV 2013
#CIAT / CCAFS

#1. function to calculate number of dry months
calc_drym <- function(x) {
  #x is the monthly climate that is used to calculate the relative entropy distribution
  if (length(which(is.na(x)))!=0) {
    ndry <- NA
  } else {
    ndry <- length(which(x < thresh))
  }
  return(ndry)
}

#2. a. function to calculate seasonality index
calcS <- function(x) {
  #x is the monthly climate that is used to calculate the relative entropy distribution
  if (length(which(is.na(x)))!=0) {
    s_ind <- NA
  } else {
    r_bar <- sum(x) #mean rainfall
    pm <- x / r_bar; pm <- pm[which(pm > 0)]; qm <- 1 / 12 #prob. distributions
    if (length(pm) >= 1) {
      d_ent <- pm / qm ; d_ent <- sum(pm * log(d_ent,base=2)) #rel. entropy [D=sum(pm*log2(pm/qm))]
      s_ind <- d_ent * (r_bar / r_max) #seasonality index
    } else {
      s_ind <- NA
    }
  }
  return(s_ind)
}

#2. b. function to calculate relative entropy
calcEnt <- function(x) {
  #x is the monthly climate that is used to calculate the relative entropy distribution
  if (length(which(is.na(x)))!=0) {
    d_ent <- NA
  } else {
    r_bar <- sum(x) #mean rainfall
    pm <- x / r_bar; pm <- pm[which(pm > 0)]; qm <- 1 / 12 #prob. distributions
    if (length(pm) >= 1) {
      d_ent <- pm / qm ; d_ent <- sum(pm * log(d_ent,base=2)) #rel. entropy [D=sum(pm*log2(pm/qm))]
    } else {
      d_ent <- NA
    }
  }
  return(d_ent)
}


