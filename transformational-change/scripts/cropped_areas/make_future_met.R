#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015
stop("!")

library(raster); library(ncdf)

#working dir
bdir <- "/mnt/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
wd <- paste(bdir,"/modelling/Cul_de_sacs",sep="")

#output directories
climdir_out <- paste(wd,"/meteorology",sep="")
wcl_inp <- paste(climdir_out,"/wcl_hist",sep="")
cru_inp <- paste(climdir_out,"/cru_hist",sep="")
wcl_out <- paste(climdir_out,"/wcl_futclim_bc",sep="")
cru_out <- paste(climdir_out,"/cru_futclim_bc",sep="")

#climate data dirs
cmip_dir <- "/mnt/data_cluster_2/gcm/cmip5/raw/monthly"

#lists of variables
rcp_list <- c("rcp60","rcp85")
gcm_list60 <- list.files(paste(bdir,"/climate/4dup/diss_africa_1975s_yearly_cru/rcp60",sep=""),pattern="_")
gcm_list85 <- list.files(paste(bdir,"/climate/4dup/diss_africa_1975s_yearly_cru/rcp85",sep=""),pattern="_")
gcm_list <- gcm_list85[which(gcm_list85 %in% gcm_list60)]

ndaysmth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#for (dset in c("cru","wcl")) {
  dset <- "wcl"
  idir <- get(paste(dset,"_inp",sep=""))
  odir <- get(paste(dset,"_out",sep=""))
  if (!file.exists(odir)) {dir.create(odir)}
  
  for (rcp in rcp_list) {
    #rcp <- rcp_list[2]
    for (gcm in gcm_list) {
      #gcm <- gcm_list[2]
      if (dset == "cru") {iniper <- "1971_2000"} else {iniper <- "1961_1990"}
      idir_his <- paste(cmip_dir,"/historical/",gcm,"/r1i1p1/average/",iniper,sep="") #1,2...12
      idir_rcp <- paste(cmip_dir,"/",rcp,"/",gcm,"/r1i1p1/monthly-files",sep="") #./year/01,02...12
      
      for (year in 2006:2099) {
        #year <- 2006
        out_dir <- paste(odir,"/",rcp,"/",gcm,"/r1i1p1/",year,sep="")
        if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}
        
        cat("...processing dataset=",dset,"/ rcp=",rcp,"/ gcm=",gcm,"/ year=",year,"\n")
        
        for (mth in 1:12) {
          #mth <- 1
          for (ivar in c("prec","tmean","tmin")) {
            #ivar <- "prec"
            if (!file.exists(paste(out_dir,"/",ivar,"_",mth,".tif",sep=""))) {
              #read in historical
              #rs_his <- raster(paste(idir_his,"/",ivar,"_",mth,".nc",sep=""))
              nc <- open.ncdf(paste(idir_his,"/",ivar,"_",mth,".nc",sep=""))
              ncdata <- get.var.ncdf(nc,varid=nc$var[[1]]) #get data from nc connection
              brs <- raster(nrow=nc$dim$latitude$len,ncol=nc$dim$longitude$len,xmn=-180,xmx=180,ymn=-90,ymx=90) #base raster
              brs[] <- t(ncdata[,])
              rs_his <- brs
              nc <- close.ncdf(nc); rm(brs); rm(ncdata); rm(nc)
              
              #read in rcp
              #rs_rcp <- raster(paste(idir_rcp,"/",year,"/",ivar,"_",sprintf("%02d",mth),".nc",sep=""),stopIfNotEqualSpaced=F)
              #rs_rcp <- rotate(rs_rcp)
              if (ivar == "prec") {nc_vname <- "pr"}
              if (ivar == "tmean") {nc_vname <- "tas"}
              if (ivar == "tmin") {nc_vname <- "tasmin"}
              
              nc <- open.ncdf(paste(idir_rcp,"/",year,"/",ivar,"_",sprintf("%02d",mth),".nc",sep=""))
              ncdata <- get.var.ncdf(nc,varid=nc$var[[nc_vname]]) #get data from nc connection
              brs <- raster(nrow=nc$dim$lat$len,ncol=nc$dim$lon$len,xmn=0,xmx=360,ymn=-90,ymx=90) #base raster
              if (length(dim(ncdata)) == 3) {ncdata <- ncdata[,,1]}
              brs[] <- t(ncdata[,]); brs <- flip(brs,direction='y'); brs <- rotate(brs)
              rs_rcp <- brs
              nc <- close.ncdf(nc); rm(brs); rm(ncdata); rm(nc)
              
              #read observations
              rs_obs <- raster(paste(idir,"/",ivar,"_",mth,".tif",sep=""))
              
              #unit conversion
              if (ivar == "prec") {
                rs_rcp <- rs_rcp * 3600 * 24 * ndaysmth[mth] #kg m-2 s-1 --> mm month-1
              } else {
                rs_rcp <- (rs_rcp - 273.15) * 10 #K --> C*10
                rs_his <- rs_his * 10 #C --> C*10
              }
              
              #resample gcm data onto obs grid
              rs_his <- resample(rs_his, rs_obs, method="ngb")
              rs_rcp <- resample(rs_rcp, rs_obs, method="ngb")
              if (ivar == "prec") {
                rs_his[which(rs_his[]<=0)] <- 0.001; rs_rcp[which(rs_rcp[] < 0)] <- 0
              }
              
              #calculate delta and apply to obs
              if (ivar == "prec") {
                del_rs <- (rs_rcp - rs_his) / (rs_his)
                fut_bc <- rs_obs + del_rs * rs_obs
              } else {
                del_rs <- rs_rcp - rs_his
                fut_bc <- rs_obs + del_rs
              }
              fut_bc <- writeRaster(fut_bc, paste(out_dir,"/",ivar,"_",mth,".tif",sep=""),format="GTiff",overwrite=T)
            }
          }
        }
      }
    }
  }
#}




