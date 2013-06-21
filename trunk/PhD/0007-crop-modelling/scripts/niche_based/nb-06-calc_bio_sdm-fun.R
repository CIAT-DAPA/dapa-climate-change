### make a number of bioclimatic layers for modelling groundnut using SDMs
### this is to be done for the 30s fitting data

###
#various functions that are needed
###

#apply a function by blocks to enhance memory use
apply_by_blocks <- function(clm_stk,sow_date,har_date,this_fun,...) {
  #determine maximum processing load
  bs <- blockSize(clm_stk, n=nlayers(clm_stk)*1.5, minblocks=2)
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
      rowVals <- cbind(rowVals,validXY[,2])
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
    pm <- monclim / r_bar; pm <- pm[which(pm > 0)]; qm <- 1 / length(gs) #prob. distributions
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
#function to calculate mean, maximum and minimum growing season temperature
calc_meantemp <- function(x,...) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  
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
#function to calculate number of days with Tmax > 34 C
calc_tcdays <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  
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


#####
#function to calculate growing degree days (Tb=10, To=28, Tm=50)
calc_gdd <- function(x) {
  monclim <- x[1:12]
  sow <- x[13]; har <- x[14] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  
  #growing season start and end
  if (har < sow) {gs <- c(har:365,1:sow)} else {gs <- c(sow:har)}
  
  monclim <- c(monclim[12],monclim,monclim[1])
  dayclim <- linearise(monclim)[16:(365+15)]*.1
  
  #calculate
  gdd <- rep(0,times=length(dayclim))
  gdd[which(dayclim >= 10 & dayclim <= 28)] <- dayclim[which(dayclim >= 10 & dayclim <= 28)] - 10
  gdd[which(dayclim > 28 & dayclim < 50)] <- 28 - ((dayclim[which(dayclim > 28 & dayclim < 50)] - 28) / (50-28)) - 10
  gdd <- sum(gdd[gs])
  
  #return
  return(gdd)
}
#####


#### calculate vpd
#ESAT_MIN=0.61120*EXP((17.67*TMIN(IDAP))/(TMIN(IDAP)+243.5))     
#ESAT_MAX=0.61120*EXP((17.67*TMAX(IDAP))/(TMAX(IDAP)+243.5))     
#VPD=VPD_CTE*(ESAT_MAX-ESAT_MIN) !kPa
calc_vdp <- function(x) {
  montmin <- x[1:12]
  montmax <- x[13:24]
  sow <- x[25]; har <- x[26] #sowing and harvest dates
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  
  #growing season start and end
  if (har < sow) {gs <- c(har:365,1:sow)} else {gs <- c(sow:har)}
  
  montmin <- c(montmin[12],montmin,montmin[1])
  daytmin <- linearise(montmin)[16:(365+15)]*.1
  
  montmax <- c(montmax[12],montmax,montmax[1])
  daytmax <- linearise(montmax)[16:(365+15)]*.1
  
  #calculate
  vpd_cte <- 0.7
  esat_min=0.61120*exp((17.67*daytmin)/(daytmin+243.5))
  esat_max=0.61120*exp((17.67*daytmax)/(daytmax+243.5))
  vpd <- vpd_cte*(esat_max-esat_min) # kPa
  vpd <- sum(vpd[gs])
  
  #return
  return(vpd)
}
#####


#### calculate ETp using Priestley-Taylor
calc_etmax <- function(x) {
  montmin <- x[1:12]
  montmax <- x[13:24]
  sow <- x[25]; har <- x[26] #sowing and harvest dates
  lat <- x[27] #latitude of site
  
  #calculation constants
  albedo <- 0.2
  vpd_cte <- 0.7
  
  #soil heat flux parameters
  a_eslope=611.2
  b_eslope=17.67
  c_eslope=243.5
  
  #Priestley-Taylor constants
  pt_const=1.26
  pt_fact=1
  vpd_ref=1
  psycho=62
  rho_w=997
  rlat_ht=2.26E6
  
  #fix sowing date if missing
  if (is.na(sow) & is.na(har)) {sow <- 152}
  if (is.na(har)) {har <- sow+122}
  
  #growing season start and end
  if (har < sow) {gs <- c(har:365,1:sow)} else {gs <- c(sow:har)}
  
  #linearise monthly values
  montmin <- c(montmin[12],montmin,montmin[1])
  daytmin <- linearise(montmin)[16:(365+15)]*.1
  
  montmax <- c(montmax[12],montmax,montmax[1])
  daytmax <- linearise(montmax)[16:(365+15)]*.1
  
  #calculate solar radiation (in MJ m-2)
  daysrad <- bc(dates=createDateGrid(1985), lat, BCb=0.115, extraT = NULL, daytmax, daytmin, BCc=2, tal=0.76)
  
  #calculate
  daytmean <- (daytmin+daytmax)/2 #mean temperature
  dayrn = (1-albedo) * daysrad #net radiation (MJ m-2 is ok!)
  
  #soil heat flux
  eslope=a_eslope*b_eslope*c_eslope/(daytmean+c_eslope)^2*exp(b_eslope*daytmean/(daytmean+c_eslope))
  
  #estimate vpd (in kPa)
  esat_min=0.61120*exp((17.67*daytmin)/(daytmin+243.5))     
  esat_max=0.61120*exp((17.67*daytmax)/(daytmax+243.5))     
  vpd=vpd_cte*(esat_max-esat_min) #kPa
  
  pt_coef=pt_fact*pt_const
  pt_coef = 1 + (pt_coef-1) * vpd / vpd_ref
  
  #*10^6? To convert fluxes MJ to J
  #rlat_ht? Latent heat flux to water flux
  #100/rho_w? Kg/m^2 to cm
  #the last *10 is for mm (instead of cm)
  et_max=(pt_coef * dayrn * eslope/(eslope+psycho) * 10^6 / rlat_ht * 100/rho_w)*10 #in mm/day
  et_max <- sum(et_max[gs])
  
  #return
  return(et_max)
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

#Bristow-Campbell (1984) solar radiation model
bc <- function(dates, lat, BCb=0.11, extraT = NULL, Tmax, Tmin, BCc=2, tal=0.76) {
  days <- as.Date(dates$DATE,"%m-%d-%Y")
  i <- dates$DOY
  if (is.null(extraT)) extraT <- extrat(i = i, lat = radians(lat))$ExtraTerrestrialSolarRadiationDaily
  le <- length(Tmax)
  dtemp <- c(Tmax[-le] - (Tmin[-le] + Tmin[-1])/2, Tmax[le] - (Tmin[le - 1] + Tmin[le])/2)
  Zdtemp <- zoo(Tmax - Tmin, order.by = days) #Tmax - Tmin
  dtempM <- mean(as.numeric(aggregate(Zdtemp, by = format(time(Zdtemp), "%m"), FUN = mean, na.rm = TRUE)), na.rm = T)
  bc <- extraT * tal * (1 - exp(-BCb * (dtemp^BCc)/dtempM))
  return(bc)
}

#create date grid
createDateGrid <- function(year) {
  #Date grid (accounting to leap years). This is for merging with the station data to avoid gaps
  if (year%%4 == 0 & year%%100 != 0) { #This is a leap year
    date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:29,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$DATE <- paste(date.grid$MTH.STR,"-",date.grid$DAY.STR,"-",year,sep="")
    date.grid$MONTH <- NULL; date.grid$DAY <- NULL; date.grid$MTH.STR <- NULL; date.grid$DAY.STR <- NULL
  } else { #This is a non-leap year
    date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$DATE <- paste(date.grid$MTH.STR,"-",date.grid$DAY.STR,"-",year,sep="")
    date.grid$MONTH <- NULL; date.grid$DAY <- NULL; date.grid$MTH.STR <- NULL; date.grid$DAY.STR <- NULL
  }
  date.grid$DOY <- 1:nrow(date.grid) #Adding the Julian day
  return(date.grid)
}

#extraterrestral daily radiation
extrat <- function (i, lat) {
  rval <- list()
  rval$ExtraTerrestrialSolarRadiationDaily <- exd(i = i, lat = lat)
  rval$DayLength <- dayLength(i = i, lat = lat)
  return(rval)
}

#extraterrestrial daily radiation
exd <- function(i, lat, Con = 4.921) {
  if (abs(degrees(lat)) < 66.5) {
    Sd <- Con * 24/pi * corrEarthSunDist(i) * (sin(lat) * sin(solarDecl(i)) * daylightTimeFactor(lat = lat, i = i) + cos(lat) * cos(solarDecl(i)) * sin(daylightTimeFactor(lat = lat, i = i)))
  }
  if (abs(degrees(lat)) >= 66.5) {
    Sd <- vector()
    for (ii in 1:length(i)) {
      sdi <- sum(exh(i = i[ii], lat = lat)[exh(i = i[ii], lat = lat) > 0])
      Sd <- c(Sd, sdi)
    }
  }
  Sd
}

#solar declination
solarDecl <- function(i) {
  rod <- 0.4093 * sin((2 * pi * (284 + i))/365)
  return(rod)
}

dayLength <- function(lat, i) {
  if (abs(degrees(lat)) < 66.5) {DL <- 24 * daylightTimeFactor(lat, i)/pi}
  if (abs(degrees(lat)) >= 66.5) {
    DL <- c()
    for (ii in 1:length(i)) {
      DLi <- length(which(exh(i = i[ii], lat = lat) > 0))
      DL <- c(DL, DLi)
    }
  }
  return(DL)
}

#earth-sun distance for a day
corrEarthSunDist <- function (i) {
  d <- 1 + 0.0334 * cos(0.01721 * i - 0.0552)
  return(d)
}

#daylight factor
daylightTimeFactor <- function(lat, i) {
  ws <- acos(-tan(lat) * tan(solarDecl(i)))
  return(ws)
}

#radians to degrees
degrees <- function(radians) {deg <- radians * 180/pi; return(deg)}

#degrees to radians
radians <- function(degrees) {rad <- degrees * pi/180; return(rad)}


