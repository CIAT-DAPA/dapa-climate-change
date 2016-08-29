#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014
# stop("!")
# Modified by Carlos Navarro May 2015

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

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf)


dirbase="/mnt/data_cluster_5/cropdata/"
dataset <- "agcfsr"

oDir <- paste0(dirbase,dataset,"/daily/nc-files")
wd <-  paste0(oDir,"/_primary_files")

ncList <- list.files(wd,full.names = FALSE)
# varraw=sapply(strsplit(ncList, '[_]'), "[[", 3)
# varraw=sapply(strsplit(varraw, '[.]'), "[[", 1)
# varraw=unique(varraw)
varraw=c("tmax")

year=sapply(strsplit(ncList, '[_]'), "[[", 2)
yearMin=min(as.numeric(year))
yearMax=max(as.numeric(year))

txtFile = paste(dirbase,"/","summary_obs.txt",sep="")
if(file.exists(txtFile)){
}else{
  opnFile <- file(txtFile, open="a")
  cat("id	name\tobsset_id\tvariable_id\tlocal_url\n", file=opnFile)
}

local_url=paste0("/cropdata/",dataset,"/daily/nc-files/")

if (dataset=="agmerra"){
  bsset_id=3
}else if (dataset=="grasp"){
  bsset_id=4
}else if (dataset=="agcfsr"){
  bsset_id=5
}else if (dataset=="princeton"){
  bsset_id=6
}else if (dataset=="ispam"){
  bsset_id=7
}else if (dataset=="nnrp"){
  bsset_id=8
}
i=0


for (var in varraw){
  cat("processing..",dataset,var,"\n")
  
  if (var == "prate") {
    varmod <- "prec"
    variable_id=1
  } else if (var == "srad") {
    varmod <- "srad"
    variable_id=5
  } else if (var == "rhstmax") {
    varmod <- "rhstmax"
    variable_id=10
  } else if (var == "tavg") {
    varmod <- "tavg"
    variable_id=3
  } else if (var == "tmax") {
    varmod <- "tmax"
    variable_id=2
  } else if (var == "tmin") {
    varmod <- "tmin"
    variable_id=4
  } else if (var == "wndspd") {
    varmod <- "wndspd"
    variable_id=9
  }  
  
  ncListVar <- list.files(wd, pattern=paste0(var,".*"),full.names = FALSE)
  ncListVar <- ncListVar[gsub(".nc4","",sapply(strsplit(basename(ncListVar), '[_]'), "[[", 3))==varmod]
  
  setwd(wd)
  
  system(paste("cdo -s -f nc mergetime ", paste(ncListVar, collapse=" "), " ", oDir, "/", var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax,"-temp.nc", sep=""))
  system(paste("nccopy -d9 ", oDir, "/", var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax,"-temp.nc", " ", oDir, "/", var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax,".nc", sep=""))
  unlink(paste(oDir, "/", var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax,"-temp.nc", sep=""))
  
  cat(paste(i+1, "\t", var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax,".nc", "\t",bsset_id, "\t", variable_id, "\t", local_url,"\n", sep=""), file=opnFile)
  
  cat(" ",paste0(var,"_daily_ts_",dataset,"_",yearMin,"_",yearMax," ..done\n"))
  
}
close.connection(opnFile) 
cat("DONE!")
