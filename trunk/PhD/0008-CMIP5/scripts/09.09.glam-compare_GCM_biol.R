#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#run GLAM parameters using perturbed crop parameters

library(raster)
library(maptools); data(wrld_simpl)

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"


#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM


#input directories and model
cropName <- "gnut"
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")

#directories of runs
unp_rundir <- paste(glam_dir,"/model-runs/unperturbed_calib_ygp_ipdate",sep="")
bio_rundir <- paste(glam_dir,"/model-runs/bio_runs",sep="")
gcm_rundir <- paste(glam_dir,"/model-runs/gcm_runs",sep="")


#load observed mean and sd yield
ym_obs <- raster(paste(unp_rundir,"/calib_results_spat/y_obs.asc",sep=""))
ys_obs <- raster(paste(unp_rundir,"/calib_results_spat/ysd_obs.asc",sep=""))


#load unperturbed mean and sd yield
ym_unp <- raster(paste(unp_rundir,"/calib_results_spat/y_pred.asc",sep=""))
ys_unp <- raster(paste(unp_rundir,"/calib_results_spat/ysd_pred.asc",sep=""))
rmse_unp <- raster(paste(unp_rundir,"/calib_results_spat/rmse.asc",sep=""))
mse1_unp <- raster(paste(unp_rundir,"/calib_results_spat/mse1.asc",sep=""))

#deviation (ratio of simulated to observed yield)
rt_ym <- ym_unp/ym_obs
rt_ys <- ys_unp/ys_obs

df_all <- data.frame(RUN="CTRL",P="CTRL",RMSE=rmse_unp[which(!is.na(rmse_unp[]))],
                     MSE1=mse1_unp[which(!is.na(mse1_unp[]))],
                     YM_NORM=rt_ym[which(!is.na(rt_ym[]))],
                     YS_NORM=rt_ys[which(!is.na(rt_ys[]))])

#GCM data loading
gcmList <- list.files(gcm_rundir)
gcmList <- gcmList[-grep("x.proc",gcmList)]

for (gcm in gcmList) {
  cat(gcm,"\n")
  #gcm <- gcmList[1]
  
  ym_gcm <- raster(paste(gcm_rundir,"/",gcm,"/calib_results_spat/y_pred.asc",sep=""))
  ys_gcm <- raster(paste(gcm_rundir,"/",gcm,"/calib_results_spat/ysd_pred.asc",sep=""))
  rmse_gcm <- raster(paste(gcm_rundir,"/",gcm,"/calib_results_spat/rmse.asc",sep=""))
  mse1_gcm <- raster(paste(gcm_rundir,"/",gcm,"/calib_results_spat/mse1.asc",sep=""))
  
  rt_ym <- ym_gcm/ym_obs
  rt_ys <- ys_gcm/ys_obs
  
  df_all <- rbind(df_all,data.frame(RUN="CLIM",P=gcm,RMSE=rmse_gcm[which(!is.na(rmse_gcm[]))],
                                    MSE1=mse1_gcm[which(!is.na(mse1_gcm[]))],
                                    YM_NORM=rt_ym[which(!is.na(rt_ym[]))],
                                    YS_NORM=rt_ys[which(!is.na(rt_ys[]))]))
}


#biological perturbations data loading
bioList <- list.files(bio_rundir)

for (bio in bioList) {
  #bio <- bioList[1]
  cat(bio,"\n")
  
  ym_bio <- raster(paste(bio_rundir,"/",bio,"/calib_results_spat/y_pred.asc",sep=""))
  ys_bio <- raster(paste(bio_rundir,"/",bio,"/calib_results_spat/ysd_pred.asc",sep=""))
  rmse_bio <- raster(paste(bio_rundir,"/",bio,"/calib_results_spat/rmse.asc",sep=""))
  mse1_bio <- raster(paste(bio_rundir,"/",bio,"/calib_results_spat/mse1.asc",sep=""))
  
  rt_ym <- ym_bio/ym_obs
  rt_ys <- ys_bio/ys_obs
  
  df_all <- rbind(df_all,data.frame(RUN="CROP",P=bio,RMSE=rmse_bio[which(!is.na(rmse_bio[]))],
                                    MSE1=mse1_bio[which(!is.na(mse1_bio[]))],
                                    YM_NORM=rt_ym[which(!is.na(rt_ym[]))],
                                    YS_NORM=rt_ys[which(!is.na(rt_ys[]))]))
}


###output folders
out_sum <- paste(glam_dir,"/_summary",sep="")
if (!file.exists(out_sum)) {dir.create(out_sum)}

#write the previously generated tables
write.csv(df_all,paste(out_sum,"/all_unp_gcm_bio.csv",sep=""),quote=T,row.names=F)
#df_all <- read.csv(paste(out_sum,"/all_unp_gcm_bio.csv",sep=""))

#calculate medians of all climate models and parameter perturbations 
df_all$MSE1 <- sqrt(df_all$MSE1)
ctl <- subset(df_all,RUN=="CTRL")
gcm <- subset(df_all,RUN=="CLIM")
bio <- subset(df_all,RUN=="CROP")

fld <- "RMSE"
tiff(paste(out_sum,"/Fig3a1.tif",sep=""),res=300,pointsize=12,
     units="px",compression="lzw",height=1500,width=1500)
par(mar=c(4,5,1,1))
boxplot(df_all[,fld] ~ df_all$RUN,ylab="RMSE (kg/ha)",pch=NA,
        cex=0.75,las=2,ylim=c(0,1200),col="grey 80",notch=F,font.lab=21,font.axis=21)
for (gcm_ens in unique(gcm$P)) {
  this_data <- gcm[which(gcm$P == paste(gcm_ens)),fld]
  points(2,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
  cat(paste(gcm_ens),round(mean(this_data,na.rm=T),2),"\n")
}

for (bio_run in unique(bio$P)) {
  this_data <- bio[which(bio$P == paste(bio_run)),fld]
  points(3,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
  cat(paste(bio_run),round(mean(this_data,na.rm=T),2),"\n")
}
grid()
dev.off()

fld <- "MSE1"
tiff(paste(out_sum,"/Fig3a2.tif",sep=""),res=300,pointsize=12,
     units="px",compression="lzw",height=1500,width=1500)
par(mar=c(4,5,1,1))
boxplot(df_all[,fld] ~ df_all$RUN,ylab="RMSE with r=1 (kg/ha)",pch=NA,
        cex=0.75,las=2,ylim=c(0,1200),col="grey 80",notch=F,font.lab=21,font.axis=21)
for (gcm_ens in unique(gcm$P)) {
  this_data <- gcm[which(gcm$P == paste(gcm_ens)),fld]
  points(2,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
  cat(paste(gcm_ens),round(mean(this_data,na.rm=T),2),"\n")
}

for (bio_run in unique(bio$P)) {
  this_data <- bio[which(bio$P == paste(bio_run)),fld]
  points(3,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
  cat(paste(bio_run),round(mean(this_data,na.rm=T),2),"\n")
}
grid()
dev.off()


fld <- "YM_NORM"
tiff(paste(out_sum,"/Fig3b.tif",sep=""),res=300,pointsize=12,
     units="px",compression="lzw",height=1500,width=1500)
par(mar=c(4,5,1,1))
boxplot(df_all[,fld] ~ df_all$RUN,ylab="Mean yield (normalized by observed)",
        pch=NA,cex=0.75,las=2,ylim=c(0,2.5),col="grey 80",notch=F,font.lab=21,font.axis=21)
for (gcm_ens in unique(gcm$P)) {
  this_data <- gcm[which(gcm$P == paste(gcm_ens)),fld]
  points(2,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
}
for (bio_run in unique(bio$P)) {
  this_data <- bio[which(bio$P == paste(bio_run)),fld]
  points(3,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
}
grid()
dev.off()


fld <- "YS_NORM"
tiff(paste(out_sum,"/Fig3c.tif",sep=""),res=300,pointsize=12,
     units="px",compression="lzw",height=1500,width=1500)
par(mar=c(4,5,1,1))
boxplot(df_all[,fld] ~ df_all$RUN,ylab="SD yield (normalized by observed)",
        pch=NA,cex=0.75,las=2,ylim=c(0,2.5),col="grey 80",notch=F,font.lab=21,font.axis=21)
for (gcm_ens in unique(gcm$P)) {
  this_data <- gcm[which(gcm$P == paste(gcm_ens)),fld]
  points(2,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
}
for (bio_run in unique(bio$P)) {
  this_data <- bio[which(bio$P == paste(bio_run)),fld]
  points(3,mean(this_data,na.rm=T),pch=15,col="red",cex=0.75)
}
grid()
dev.off()




# windows()
# plot(df_all$YM_NORM,df_all$YS_NORM,pch=20,xlim=c(0,5),ylim=c(0,5))
# points(gcm$YM_NORM,gcm$YS_NORM,pch=20,col="red")
# points(bio$YM_NORM,bio$YS_NORM,pch=20,col="blue")
# points(ctl$YM_NORM,ctl$YS_NORM,pch=20,col="grey 50")



