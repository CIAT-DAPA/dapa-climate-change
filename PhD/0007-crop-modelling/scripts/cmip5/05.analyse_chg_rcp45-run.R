#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"

#name of crop and other details
cropName <- "gnut"
selection <- "v6"

#other directories
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
wthDir_obs <- paste(cropDir,"/inputs/ascii/wth",sep="")
wthDir_hist <- paste(cropDir,"/inputs/ascii/wth-cmip5_hist",sep="")
wthDir_rcp <- paste(cropDir,"/inputs/ascii/wth-cmip5_rcp45",sep="")

#list of gcms hist and future
gcmList_hist <- list.files(wthDir_hist,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp,pattern="_ENS_")

#merge both lists
gcmList <- gcmList_rcp[which(!gcmList_rcp %in% gcmList_hist)]

#matrix gridcells
cells <- read.csv(paste(cropDir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))

#cell to analyse
loc <- 529 #this is in northern Gujarat, one of the highest ccoef gridcells

#1. load all years of data into a data.frame for all GCMs
#       a. one data.frame for hist + obs, 
year <- 1966

#observed weather
wthFil_obs <- paste(wthDir_obs,"/rfd_",loc,"/ingc001001",year,".wth",sep="")
wth_obs <- read.fortran(wthFil_obs,format=c("I5","F6","3F7"),skip=4)
names(wth_obs) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
wth_obs$YEAR <- as.numeric(substr(wth_obs$DATE,1,2))
wth_obs$JDAY <- as.numeric(substr(wth_obs$DATE,3,5))

#cmip5 hist
wthFil_hist <- paste(wthDir_hist,"/rfd_",loc,"/ingc001001",year,".wth",sep="")
wth_obs <- read.fortran(wthFil_hist,format=c("I5","F6","3F7"),skip=4)
names(wth_obs) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
wth_obs$YEAR <- as.numeric(substr(wth_obs$DATE,1,2))
wth_obs$JDAY <- as.numeric(substr(wth_obs$DATE,3,5))



#       b. one data.frame for rcp45



#2. calculate few specific metrics for each of these time series
#   metrics are:
#      a. rainfall during groundnut growing season
#      b. mean temperature during groundnut growing season
#      c. number of days with rain > 0mm, 2mm, 5mm, 10mm, 15mm, 20mm
#      d. rainfall std, rainfall c.v.
#      e. number of days TMAX>34
#      f. number of days TMAX>40
#      g. number of days TMEAN>35
#      h. number of days TMEAN>47
#      i. tmean std, tmean c.v.
#      j. number of days with Ea/Ep ratio < 0.25, 0.5, 0.75
#      k. sum of global radiation of days with daily mean temperature >8, 
#         daily minimum temperature >0, and ETRATIO>0.5
#      l. number of days with daily mean temperature >8, daily minimum
#         temperature >0 and ERATIO>0.5

# wth_out$ETMAX <- NA; wth_out$AVAIL <- NA; wth_out$ERATIO <- NA
# wth_out$CUM_RAIN <- NA; wth_out$RUNOFF <- NA; wth_out$DEMAND <- NA
# wth_out <- watbal_wrapper(wth_out)
# wth_out <- wth_out[which(wth_out$JDAY >= sow & wth_out$JDAY <= har),]


#3. construct PDFs for these metrics






