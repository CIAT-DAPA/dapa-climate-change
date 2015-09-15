#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2015
stop("!")

########################################################################
#This script will check and fix errors in extracted GCM data. Errors sought are:
#1. values of -273.15 (i.e. < -273), to be filled with mean from above and below
#2. values of NA (or xxe+xx), to be filled with mean from above and below
########################################################################

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
#wd <- "~/Leeds-work/rice-future-tpe"
gcm_idir <- "/mnt/data_cluster_2/gcm/cmip5/raw/daily"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
#gcm_odir <- "~/scratch/gcm_meteorology"
#if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#variable list
varlist <- c("prec", "tmax", "tmin", "srad")

for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[4])
  wst_name <- gsub(".","",wst,fixed=T)
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  cat("...checking wst=",wst_name,"\n")
  
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,sep="")
  gcmlist <- list.files(paste(wst_odir,"/gcm",sep=""))
  
  #remove GCMs without some of the variables
  gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
  gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
  gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
  gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]
  
  for (gcm in gcmlist) {
    #gcm <- "miroc_esm"
    for (vname in varlist) {
      #vname <- varlist[2]
      for (sce in c("historical","rcp26","rcp45","rcp60","rcp85")) {
        #sce <- "historical"
        tdata <- read.table(paste(wst_odir,"/gcm/",gcm,"/raw_ts_",sce,"_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        tmiss <- c(which(tdata$value < -272), which(tdata$value > 1e5))
        
        if (length(tmiss) != 0) {
          cat("\n.......................\n")
          cat("...found missing data...\n")
          cat("...weather station=",wst,"\n")
          cat("...gcm=",gcm,"\n")
          cat("...variable=",vname,"\n")
          cat("...scenario=",sce,"\n")
          cat("...number of missing points=",length(tmiss),"\n")
          cat("...actual data:\n")
          print(tdata[tmiss,])
          cat(".......................\n")
          for (imiss in tmiss) {
            #imiss <- tmiss[1]
            tdata$value[imiss] <- (tdata$value[(imiss-1)] + tdata$value[(imiss+1)]) * 0.5
          }
          write.table(tdata, paste(wst_odir,"/gcm/",gcm,"/raw_ts_",sce,"_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""), 
                      sep=" ",row.names=F, quote=F)
        }
      }
    }
  }
}





