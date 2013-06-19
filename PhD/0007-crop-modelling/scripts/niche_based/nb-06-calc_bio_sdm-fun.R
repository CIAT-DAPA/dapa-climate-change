### make a number of bioclimatic layers for modelling groundnut using SDMs
### this is to be done for the 30s fitting data

###
#various functions that are needed
###

#apply a function by blocks to enhance memory use
apply_by_blocks <- function(clm_stk,sow_date,har_date,this_fun,...) {
  #determine maximum processing load
  bs <- blockSize(clm_stk, n=20, minblocks=2)
  cat("\nprocessing in: ", bs$n, " chunks \n", sep="")
  
  #output raster
  outraster <- raster(clm_stk)
  
  for (b in 1:bs$n) {
    #b <- 1
    cat(" ",round(b/bs$n*100,2),"%",sep="")
    
    iniCell <- 1+(bs$row[b]-1)*ncol(outraster)
    finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(outraster)
    allCells <- iniCell:finCell
    validCells <- allCells[which(!is.na(clm_stk[[1]][allCells]))]
    validXY <- xyFromCell(clm_stk,validCells)
    
    if (length(validCells) > 0) {
      rowVals <- extract(clm_stk,validCells)
      rowVals <- cbind(rowVals,sow=extract(sow_date,validXY))
      rowVals <- cbind(rowVals,har=extract(har_date,validXY))
      rasVals <- apply(rowVals, 1, this_fun, ...)
    } else {
      rasVals <- NA
    }
    outraster[validCells] <- rasVals
  }
  cat("\n")
  return(outraster)
}


#####
#function to calculate total seasonal rainfall
calc_totrain <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {gs <- c(Gf:12,1:Gi)} else {gs <- c(Gi:Gf)}
  
  #extract monthly climate
  monclim <- monclim[gs]
  
  #calculate and return
  seasrain <- sum(monclim)
  return(seasrain)
}
#####


#####
#function to calculate seasonality index
calc_sfeng <- function(x,...) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {gs <- c(Gf:12,1:Gi)} else {gs <- c(Gi:Gf)}
  
  #extract monthly climate
  monclim <- monclim[gs]
  
  r_max <- list(...)[[1]]
  #cat("r_max=",r_max,"\n")
  
  #monclim is the monthly climate that is used to calculate the relative entropy distribution
  if (length(which(is.na(monclim)))!=0) {
    s_ind <- NA
  } else {
    r_bar <- sum(monclim) #mean rainfall
    pm <- monclim / r_bar; pm <- pm[which(pm > 0)]; qm <- 1 / 12 #prob. distributions
    if (length(pm) >= 1) {
      d_ent <- pm / qm ; d_ent <- sum(pm * log(d_ent,base=2)) #rel. entropy [D=sum(pm*log2(pm/qm))]
      s_ind <- d_ent * (r_bar / r_max) #seasonality index
    } else {
      s_ind <- NA
    }
  }
  return(s_ind)
}
#####


#####
#function to calculate minimum seasonal rainfall
calc_minrain <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {gs <- c(Gf:12,1:Gi)} else {gs <- c(Gi:Gf)}
  
  #extract monthly climate
  monclim <- monclim[gs]
  
  #calculate and return
  minrain <- min(monclim)
  return(minrain)
}
#####


#####
#function to calculate minimum seasonal rainfall
calc_meantemp <- function(x,...) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {gs <- c(Gf:12,1:Gi)} else {gs <- c(Gi:Gf)}
  
  wfunc <- list(...)[[1]]
  
  #extract monthly climate
  monclim <- monclim[gs]
  
  #calculate and return
  if (wfunc == "mean") {meantemp <- mean(monclim)}
  if (wfunc == "min") {meantemp <- min(monclim)}
  if (wfunc == "max") {meantemp <- max(monclim)}
  
  return(meantemp)
}
#####


#####
#function to calculate minimum seasonal rainfall
calc_tcdays <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  if (har > 365) {har <- har - 365}
  
  #growing season start and end
  if (har < sow) {gs <- c(har:365,1:sow)} else {gs <- c(sow:har)}
  
  monclim <- c(monclim[12],monclim,monclim[1])
  dayclim <- linearise(monclim)[16:(365+15)]
  
  #extract daily climate
  dayclim <- dayclim[gs]
  
  #calculate and return
  tcritdays <- length(which(dayclim > 340))
  return(tcritdays)
}
#####


#to linearise monthly data
linearise <- function(input_vals) {
  day_mid <- c(-15,16,45,75,106,136,167,197,228,259,289,320,350,381)
  daily_vals <- rep(NA,times=(day_mid[14]-day_mid[1]+1))
  for (mth in 1:13) {
    deltawth <- (input_vals[mth+1]-input_vals[mth]) / (day_mid[mth+1]-day_mid[mth])
    
    for (d in day_mid[mth]:day_mid[mth+1]) {
      daily_vals[d+16] <- input_vals[mth] + deltawth*(d-day_mid[mth])
    }
  }
  return(daily_vals)
}

