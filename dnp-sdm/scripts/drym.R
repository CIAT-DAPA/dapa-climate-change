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
#calculate total number of dry (R<100) months
##################

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


#4. load monthly data
bioDir <- "./env-data/bioclim_gtiff"
mthDir <- "./env-data/prec_monthly_gtiff"
prec_stk <- stack(paste(mthDir,"/prec_",1:12,".tif",sep=""))

#calculate indices
thresh <- 100
if (!file.exists(paste(bioDir,"/drymonths.tif",sep=""))) {
  cmths_rs <- calc(prec_stk,calc_drym,filename=paste(bioDir,"/drymonths.tif",sep=""),format='GTiff')
}



