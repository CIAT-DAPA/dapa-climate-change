#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014
stop("!")

#remap grid of WFD dataset and cut to African domain

#all data at: /nfs/a101/eejarv/quest-for-robustness/data/meteorology/wfd_data
#WFD:
#Tmax: Tmax_daily_WFD
#Tmin: Tmin_daily_WFD
#Prec: Rainf_daily_WFD_GPCC
#Srad: SWdown_daily_WFD

#WFDEI:
#Tmax: Tmax_daily_WFDEI
#Tmin: Tmin_daily_WFDEI
#Prec: Rainf_daily_WFDEI_GPCC
#Srad: SWdown_daily_WFDEI

#1. remapcon2
#2. sellonlatbox

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
wd <- "/nfs/a101/eejarv/quest-for-robustness"
#metDir <- paste(wd,"/data/meteorology/ISIMIP_wth",sep="")
metDir <- paste(wd,"/data/meteorology/wfd_data",sep="")
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#output directory
ometDir <- paste(wd,"/data/meteorology/baseline_climate",sep="")
if (!file.exists(ometDir)) {dir.create(ometDir)}

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

#determine extent to cut the resampled netcdf
bbox <- extent(yrs)
if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360

#details
var_list <- c("Rainf","SWdown","Tmax","Tmin")
dst_list <- c("WFD","WFDEI")
wfdei_yrs <- c(1979:2012)
wfd_yrs <- c(1950:2001)

dataset <- dst_list[1]
vname <- var_list[1]
if (vname == "Rainf") {suffix <- paste("_daily_",dataset,"_GPCC",sep="")} else {suffix <- paste("_daily_",dataset,sep="")}

idataDir <- paste(metDir,"/",vname,suffix,sep="")
odataDir <- paste(ometDir,"/",vname,suffix,sep="")
if (!file.exists(odataDir)) {dir.create(odataDir)}

years <- get(paste(tolower(dataset),"_yrs",sep=""))

#loop years and months
for (yr in years) {
  #yr <- years[1]
  for (mth in 1:12) {
    #mth <- 1
    cat("...processing year=",yr,"and month=",mth,"\n")
    tmth <- sprintf("%1$02d",mth)
    
    fname <- paste(vname,suffix,"_",yr,tmth,".nc",sep="")
    fnameout <- paste("afr_",vname,suffix,"_",yr,tmth,".nc",sep="")
    
    if (!file.exists(paste(odataDir,"/",fnameout,sep=""))) {
      setwd(idataDir)
      system(paste("cdo remapcon2,r320x160 ",fname," ",odataDir,"/",vname,"_remapped.nc",sep=""))
      setwd(odataDir)
      
      #cut to Africa
      system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," pr_remapped.nc ",fnameout,sep=""))
      
      #garbage collection
      system(paste("rm -f pr_remapped.nc",sep=""))
      setwd(wd)
    }
  }
}
