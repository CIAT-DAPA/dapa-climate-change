#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT
stop("do not run please!")

library(raster); library(rgdal); library(maptools)

#6. Calculation of metrics as stated in methods

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
e40Dir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"


#source functions
source(paste(src.dir2,"/scripts/CMIP3-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#input directories and configurations
inputDD <- paste(mdDir,"/assessment/input-data",sep="")
outputDD <- paste(mdDir,"/assessment/output-data",sep="")

#administrative areas data
admDir <- paste(inputDD,"/adm-data",sep="")

#list of countries/regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:3,GCM=c("prec","tmean","dtr"),WCL=c("prec","tmean","dtr"),
                     CL_CRU=c("prec","tmean","dtr"),TS_CRU=c("pre","tmp","dtr"),
                     E40=c("prec","tasm","dtr"),CL_WST=c("rain","tean","dtr"),
                     TS_WST=c("pr","tas","dtr"))

#scaling factors to datasets per variable
scList <- data.frame(VID=1:3,GCM=c(1,1,1),WCL=c(1,1,1),
                     CL_CRU=c(1,1,1),TS_CRU=c(0.1,0.1,0.1),
                     E40=c(1,1,1),CL_WST=c(1,1,1),
                     TS_WST=c(1,0.1,0.1))

########################
### calculate dtr for all GCMs
########################
gcmList <- list.files(mdDir)
for (gcm in gcmList) {
  #gcm <- gcmList[1]
  gcmDir <- paste(mdDir,"/",gcm,sep="")
  yrDir <- paste(gcmDir,"/yearly_files",sep="")
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("XXXXXXXX processing gcm", gcm,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  yri <- 1961; yrf <- 2000
  for (yr in yri:yrf) {
    cat("year:",yr,":")
    #yr <- yri
    for (m in 1:12) {
      #m <- 1
      cat(m,". ",sep="")
      mth <- sprintf("%02d",m)
      tn <- paste(yrDir,"/",yr,"/tmin_",mth,".nc",sep="")
      tx <- paste(yrDir,"/",yr,"/tmax_",mth,".nc",sep="")
      
      if (file.exists(tn) & file.exists(tx)) {
        if (!file.exists(paste(yrDir,"/",yr,"/dtr_",mth,".nc",sep=""))) {
          rs1 <- raster(tn)
          rs2 <- raster(tx)
          rs <- rs2 - rs1
          rs <- writeRaster(rs,paste(yrDir,"/",yr,"/dtr_",mth,".nc",sep=""),
                            format="CDF",overwrite=T,varname="dtr",xname="lon",yname="lat",zname="time")
          rm(list=c("rs1","rs2","rs")); g=gc(); rm(g)
        }
      }
    }
    cat("\n")
  }
}


########################
### calculate 1961-2000 mean for CMIP3 models
########################
gcmList <- list.files(mdDir)
for (gcm in gcmList) {
  #gcm <- gcmList[1]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("XXXXXXXX processing gcm", gcm,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")  
  
  gcmDir <- paste(mdDir,"/",gcm,sep="")
  yr_iDir <- paste(gcmDir,"/yearly_files",sep="")
  cl_oDir <- paste(gcmDir,"/multiyr_avgs",sep="")
  
  yri <- 1961; yrf <- 2000
  rs_oDir <- paste(cl_oDir,"/",yri,"_",yrf,sep="")
  if (!file.exists(rs_oDir)) {dir.create(rs_oDir)}
  
  for (vn in c("prec","tmean","tmin","tmax","dtr")) {
    #vn <- "prec"
    cat("\nvariable:",vn,"\n")
    for (m in 1:12) {
      #m <- 1
      mth <- sprintf("%02d",m)
      fList <- paste(yr_iDir,"/",yri:yrf,"/",vn,"_",mth,".nc",sep="")
      fList <- as.character(sapply(fList,FUN=checkExists))
      fList <- fList[which(!is.na(fList))]
      
      if (length(fList)>0) {
        if (!file.exists(paste(rs_oDir,"/",vn,"_",mth,".nc",sep=""))) {
          rstk <- stack(fList)
          rs <- calc(rstk,fun=function(x) {mean(x,na.rm=T)})
          rs <- writeRaster(rs,paste(rs_oDir,"/",vn,"_",mth,".nc",sep=""),
                            format="CDF",overwrite=T,varname=vn,xname="lon",yname="lat",zname="time")
          rm(rstk); g=gc(); rm(g)
        }
      }
    }
  }
}


########################
### calculate MMM for yearly data
########################
gcmList <- list.files(mdDir)
moDir <- paste(mdDir,"/multi_model_mean/yearly_files",sep="")
if (!file.exists(moDir)) {dir.create(moDir,recursive=T)}

yri <- 1961; yrf <- 2000
for (yr in yri:yrf) {
  #yr <- yri
  oyrDir <- paste(moDir,"/",yr,sep="")
  if (!file.exists(oyrDir)) {dir.create(oyrDir)}
  
  #loop through variables and months
  for (vn in c("prec","tmean","tmin","tmax","dtr")) {
    #vn <- "prec"
    cat("year:",yr,": var :",vn,":")
    for (m in 1:12) {
      #m <- 1
      cat(m,". ",sep="")
      mth <- sprintf("%02d",m)
      fList <- paste(mdDir,"/",gcmList,"/yearly_files/",yr,"/",vn,"_",mth,".nc",sep="")
      fList <- as.character(sapply(fList,FUN=checkExists))
      fList <- fList[which(!is.na(fList))]
      
      if (!file.exists(paste(oyrDir,"/",vn,"_",mth,".nc",sep=""))) {
        #load rasters of different resolution into list
        mList <- list()
        xr <- c(); yr <- c()
        for (i in 1:length(fList)) {
          rs <- raster(fList[i])
          mList[[i]] <- rs
          xr <- c(xr,xres(rs)); yr <- c(yr,yres(rs))
        }
        
        mList_res <- list()
        sxr <- which(xr==min(xr))[1]
        for (i in 1:length(mList)) {
          mList_res[[i]] <- resample(mList[[i]],mList[[sxr]],method="ngb")
        }
        
        rstk <- stack(mList_res)
        rs <- calc(rstk,fun=function(x) {mean(x,na.rm=T)})
        rs <- writeRaster(rs,paste(oyrDir,"/",vn,"_",mth,".nc",sep=""),
                          format="CDF",overwrite=T,varname=vn,xname="lon",yname="lat",zname="time")
        rm(list=c("rstk","rs","mList","mList_res")); g=gc(); rm(g)
      }
    }
    cat("\n")
  }
}


########################
### calculate MMM for climatological means
########################
moDir <- paste(mdDir,"/multi_model_mean/multiyr_avgs/1961_2000",sep="")
if (!file.exists(moDir)) {dir.create(moDir,recursive=T)}

#loop through variables and months
for (vn in c("prec","tmean","tmin","tmax","dtr")) {
  #vn <- "prec"
  cat("var :",vn,":")
  for (m in 1:12) {
    #m <- 1
    cat(m,". ",sep="")
    mth <- sprintf("%02d",m)
    fList <- paste(mdDir,"/",gcmList,"/multiyr_avgs/1961_2000/",vn,"_",mth,".nc",sep="")
    fList <- as.character(sapply(fList,FUN=checkExists))
    fList <- fList[which(!is.na(fList))]
    
    if (!file.exists(paste(moDir,"/",vn,"_",mth,".nc",sep=""))) {
      #load rasters of different resolution into list
      mList <- list()
      xr <- c(); yr <- c()
      for (i in 1:length(fList)) {
        rs <- raster(fList[i])
        mList[[i]] <- rs
        xr <- c(xr,xres(rs)); yr <- c(yr,yres(rs))
      }
      
      mList_res <- list()
      sxr <- which(xr==min(xr))[1]
      for (i in 1:length(mList)) {
        mList_res[[i]] <- resample(mList[[i]],mList[[sxr]],method="ngb")
      }
      
      rstk <- stack(mList_res)
      rs <- calc(rstk,fun=function(x) {mean(x,na.rm=T)})
      rs <- writeRaster(rs,paste(oyrDir,"/",vn,"_",mth,".nc",sep=""),
                        format="CDF",overwrite=T,varname=vn,xname="lon",yname="lat",zname="time")
      rm(list=c("rstk","rs","mList","mList_res")); g=gc(); rm(g)
    }
  }
  cat("\n")
}


#################################################################################
#################################################################################
#processes to complete
gcmList <- list.files(mdDir)
gcmList <- c(gcmList,paste("multi_model_mean"))
isoList <- regions$ISO
procList <- expand.grid(GCM=gcmList,ISO=isoList)



#################################################################################
#################################################################################
######################### REVISION FOR ERL PAPER ################################
#################################################################################
#################################################################################
#a. mean climates: for each area using the values of GCM gridcells and the mean
#                  values of the datasets calculate the following
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse

#climatology data
clWCL <- paste(inputDD,"/wcl-data",sep="")
clCRU <- paste(inputDD,"/cru-data",sep="")
clE40 <- e40Dir
clWST <- paste(inputDD,"/wcl-weather-stations",sep="")

#check those that are done already
procList <- check_done(procList,"x_rev2.proc")

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("e40Dir")
sfExport("inputDD")
sfExport("outputDD")
sfExport("clWCL")
sfExport("clCRU")
sfExport("clE40")
sfExport("clWST")
sfExport("admDir")
sfExport("vnList")
sfExport("scList")
sfExport("procList")
sfExport("regions")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),mean_climate_skill_revised2))

#stop the cluster
sfStop()


#######################################################################
#b. interannual variability: calculate the VI as in Scherrer 2011
#                            so that the issue of non-matching series is overcome

#specify initial and final years
yi <- 1961
yf <- 2000

#time series data
tsWST <- paste(inputDD,"/all-weather-stations",sep="")
tsE40 <- e40Dir
tsCRU <- paste(inputDD,"/cru-ts-data",sep="")

#check those that are done already
procList <- check_done(procList,"z_rev.proc")

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("e40Dir")
sfExport("inputDD")
sfExport("outputDD")
sfExport("tsCRU")
sfExport("tsE40")
sfExport("tsWST")
sfExport("admDir")
sfExport("vnList")
sfExport("scList")
sfExport("procList")
sfExport("regions")
sfExport("yi")
sfExport("yf")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),interannual_vi_revised))

#stop the cluster
sfStop()

