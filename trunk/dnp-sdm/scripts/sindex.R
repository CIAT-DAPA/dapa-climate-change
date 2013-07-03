#JRV 2013
#CIAT / CCAFS
stop("!")

#load libraries
library(raster)

#working directory
#wd <- "/media/DATA/CIAT_work/DNP-biodiversity"
wd <- "/nfs/a102/eejarv/DNP-biodiversity"
setwd(wd)

##################
#Equations (from Feng et al. 2013 NCC)
#S = D*(R/Rmax)
#where
#S=seasonality index
#D=relative entropy in the rainfall distribution
#R=annual mean rainfall
#Rmax=maximum annual mean rainfall across the dataset

#D=sum(pm*log2(pm/qm))
#qm=1/12
#pm=Ri/R (for each pixel)
##################

#1. get annual total rainfall (climatological mean)
bioDir <- "./env-data/bioclim_gtiff"
rmean <- raster(paste(bioDir,"/bio_12.tif",sep=""))

#2. get normalising value from rmean raster
r_max <- rmean@data@max #max(rmean[],na.rm=T)

#3. a. function to calculate seasonality index
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

#3. b. function to calculate relative entropy
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


#4. a. load monthly data
mthDir <- "./env-data/prec_monthly_gtiff"
prec_stk <- stack(paste(mthDir,"/prec_",1:12,".tif",sep=""))

#4. b. calculate seasonality index
if (!file.exists(paste(bioDir,"/sind.tif",sep=""))) {
  sind_rs <- calc(prec_stk,calcS,filename=paste(bioDir,"/sind.tif",sep=""),format='GTiff')
}

#4. c. calculate relative entropy distribution
if (!file.exists(paste(bioDir,"/rent.tif",sep=""))) {
  rent_rs <- calc(prec_stk,calcEnt,filename=paste(bioDir,"/rent.tif",sep=""),format='GTiff')
}




