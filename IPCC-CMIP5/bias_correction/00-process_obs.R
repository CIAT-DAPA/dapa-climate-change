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

##################### PARA CONVERTIR A TIFF  #######################################

library(sp); library(raster); library(rgdal); library(maptools); library(ncdf4)


dirbase="U:/cropdata/" # "/mnt/data_cluster_5/cropdata/" # 
outdir="E:/data/temp" #  "/mnt/data_climatewizard/temp/" # 
dataset <-"princeton"# "agmerra" # "agcfsr" # "grasp" # 

wDir <-"E:/data/AR5_Global_Daily_25k/temp" # "/mnt/data_climatewizard/AR5_Global_Daily_25k/temp" # "D:/TEMP/temp_raster" # 
if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
rasterOptions(tmpdir= wDir)

outtiff=paste0(outdir,'/',dataset)
if (!file.exists(outtiff)) {dir.create(outtiff, recursive = TRUE)}

oDir <- paste0(dirbase,dataset,"/daily/nc-files")
wd <-  paste0(oDir,"/_primary_files")
setwd(wd)

ncList <- list.files(wd,full.names = FALSE)
if (dataset=="agmerra"){
  varraw=sapply(strsplit(ncList, '[_]'), "[[", 3)
  varraw=sapply(strsplit(varraw, '[.]'), "[[", 1)
  year=sapply(strsplit(ncList, '[_]'), "[[", 2)
}else if (dataset=="grasp"){
  varraw=sapply(strsplit(ncList, '[_]'), "[[", 1)
  year=gsub(".nc","",sapply(strsplit(ncList, '[_]'), "[[", 2))
}else if (dataset=="agcfsr"){
  bsset_id=5
}else if (dataset=="princeton"){
  varraw=sapply(strsplit(ncList, '[_]'), "[[", 1)
  year= sapply(strsplit(sapply(strsplit(ncList, '[_]'), "[[", 3), '[-]'), "[[", 1)
}else if (dataset=="ispam"){
  bsset_id=7
}else if (dataset=="nnrp"){
  bsset_id=8
}
varraw=unique(varraw)

yearMin=min(as.numeric(year))
yearMax=max(as.numeric(year))

for (var in varraw){
  cat("############ ",dataset,var," #######\n")
  
  if (var == "prate" ||var ==  "prcp") {
    varmod <- "prec"
    variable_id=1
  } else if (var == "srad" || var == "dswrf") {
    varmod <- "srad"
    variable_id=5
  } else if (var == "rhstmax") {
    varmod <- "rhstmax"
    variable_id=10
  } else if (var == "tavg") {
    varmod <- "tmean"
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
  if(varmod=="prec"||varmod=="tmax"||varmod=="tmin"||varmod=="srad"){
    ncListVar <- list.files(wd, pattern=paste0(var,".*"),full.names = FALSE)
    if (dataset=="agmerra" ){
      ncListVar <- ncListVar[gsub(".nc4","",sapply(strsplit(basename(ncListVar), '[_]'), "[[", 3))==var]
    } else if (dataset=="grasp"|| dataset=="princeton"){
      ncListVar <- ncListVar[sapply(strsplit(basename(ncListVar), '[_]'), "[[", 1)==var]
    }
    
    for(nc in ncListVar){
      # ncin <- nc_open(nc)
      # names(ncin$var)
      # rast<-raster(ncin, varname="prate")
      # variables = names(nc[['var']])
      # nc_close(ncin)
      cat('\nProcessing: ',dataset,basename(nc),'\n')
      if (dataset=="agmerra" ){
        year=sapply(strsplit(sapply(strsplit(basename(nc), '[.]'), "[[", 1), '[_]'), "[[", 2)
        tif=paste0(outtiff,'/',varmod,'_',year,'.tif')  
        if(!file.exists(tif)){ 
          nc2=paste0(outtiff,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'.nc2')
          if(!file.exists(nc2)){
            system(paste0("cdo -f nc copy ",wd,'/',nc," ",nc2),intern = T)
          }
          prec=stack(lapply(nc2, FUN=brick))
        }
      }else if (dataset=="princeton") {
        year= sapply(strsplit(sapply(strsplit(nc, '[_]'), "[[", 3), '[-]'), "[[", 1)
        tif=paste0(outtiff,'/',varmod,'_',year,'.tif')  
        if(!file.exists(tif)){ 
          nc2=paste0(outtiff,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'.nc')
          if(!file.exists(nc2)){
            system(paste0("cdo -f nc copy ",wd,'/',nc," ",nc2),intern = T)
          }
          prec=stack(lapply(nc2, FUN=brick))
        }        
        
      }else{
        year= sapply(strsplit(sapply(strsplit(nc, '[_]'), "[[", 3), '[-]'), "[[", 1)
        tif=paste0(outtiff,'/',varmod,'_',year,'.tif')      
        if(!file.exists(tif)){        
          prec=stack(lapply(nc, FUN=brick))
        }
      }
      tifFact=paste0(outtiff,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_fact.tif')
      tifint=paste0(outtiff,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_int32.tif')
      #tif=paste0(outtiff,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'.tif')
      #tif=paste0(outtiff,'/',varmod,'_',year,'.tif')      
      if(!file.exists(tif)){
        writeRaster(prec*100, tifFact, format='GTiff', overwrite=T)
        unlink(nc2)
        system(paste0("gdal_translate -ot Int32 -of GTiff ",tifFact," ",tifint),intern = T)
        system(paste0("gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1 ",tifint," ",tif),intern = T)
        unlink(tifFact) 
        unlink(tifint)
        cat(" .....done\n")
      }
    }    
  }
}  

##############################################################################################  
