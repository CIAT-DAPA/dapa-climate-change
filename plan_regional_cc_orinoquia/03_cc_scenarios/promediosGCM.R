
####raster de referencia a 1 grado de resolucion
require(raster)
ref<-raster(xmn=-83,xmx=-34,ymn=-40,ymx=14,res=1)
ref[]<-1:ncell(ref)

####
rcp="rcp85"
baseDir="T:/gcm/cmip5/raw/monthly"
ens="r1i1p1"
basePer="1981_2005"
oDir=paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/GCM_historical/")

##GCM historicos
curDir <- paste(baseDir, "/historical", sep="")
gcmList <- list.dirs(curDir, recursive = FALSE, full.names =F)

for(i in 1:length(gcmList)){
# Path of each ensemble
curEnsDir <- paste(curDir, "/", gcmList[i], "/", ens, sep="")

# Average directory
curAvgDir <- paste(curEnsDir, "/average/", basePer, sep="")

if (file.exists(curAvgDir)){
#archivos de variables
arc<-list.files(curAvgDir,full.names=T)

#leer archivos
rasters<-stack(lapply(arc,FUN=raster))

###archivos de salida a 1 grado de resolucion
names<-list.files(curAvgDir,full.names=F)
for(j in 1:length(names)){
out<-resample(rasters[[j]],ref)


outDir<-paste0(oDir,"/",gcmList[i])
if (!file.exists(outDir)) {dir.create(outDir, recursive=T)}

writeRaster(out,paste0(outDir,"/",names[j]),format='CDF', overwrite=F)
}
}
}

##GCM futuro
rcp="rcp85"
futDir <- paste(baseDir, "/", rcp, sep="")

period<- c("2030")

# Define start and end year
staYear <- as.integer(period)
endYear <- as.integer(period) + 29


curDir <- paste(baseDir, "/historical", sep="")
gcmList <- list.dirs(curDir, recursive = FALSE, full.names =F)
gcmListf <- list.dirs(paste0(baseDir,"/",rcp), recursive = FALSE, full.names =F)

for(i in 1:length(gcmList)){
futAvgDir <- paste(futDir, "/", gcmListf[i], "/", ens, "/average/", staYear, "_", endYear, sep="")

# Path of each ensemble
curEnsDir <- paste(curDir, "/", gcmList[i], "/", ens, sep="")

# Average directory
curAvgDir <- paste(curEnsDir, "/average/", basePer, sep="")

if (file.exists(futAvgDir)){

  #archivos de variables
  arcfut<-list.files(futAvgDir,full.names=T)
  
  #leer archivos
  rastersfut<-stack(lapply(arcfut,FUN=raster))
  
  ###archivos de salida a 1 grado de resolucion
  names<-list.files(futAvgDir,full.names=F)
  for(j in 1:length(names)){
    outfut<-resample(rastersfut[[j]],ref)
  
  dirfut<-paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/GCM_fut/",rcp)
  if (!file.exists(dirfut)) {dir.create(dirfut, recursive=T)}
  
  dirfutrcp<-paste0(dirfut,"/",gcmListf[i])
  if (!file.exists(dirfutrcp)) {dir.create(dirfutrcp, recursive=T)}  
  
  writeRaster(outfut,paste0(dirfutrcp,"/",names[j]),format='CDF', overwrite=F)
}
}
}




##ensamblajes historico
gcms<- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")
iDir=paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/GCM_historical")
gcmList <-paste0(iDir,"/",gcms)

for(i in 1:12){
  var1<-paste0(gcmList,"/prec_",i,".nc")
  var2<-paste0(gcmList,"/tmax_",i,".nc")
  var3<-paste0(gcmList,"/tmin_",i,".nc")
  
  prec<-stack(lapply(var1,raster))
  tmax<-stack(lapply(var2,raster))
  tmin<-stack(lapply(var3,raster))
  
  prec_mean<-mean(prec)
  tmax_mean<-mean(tmax)
  tmin_mean<-mean(tmin)
  
#   prec_sd<-sd(prec)
#   tmax_sd<-sd(tmax)
#   tmin_sd<-sd(tmin)
  
  writeRaster(prec_mean,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/prec_",i,".nc"),format='CDF',overwrite=F)
  writeRaster(tmax_mean,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/tmax_",i,".nc"),format='CDF',overwrite=F)
  writeRaster(tmin_mean,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/tmin_",i,".nc"),format='CDF',overwrite=F)  
  
#   writeRaster(prec_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/prec_sd_",i,".nc"),format='CDF',overwrite=F)
#   writeRaster(tmax_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/tmax_sd_",i,".nc"),format='CDF',overwrite=F)
#   writeRaster(tmin_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_historical/tmin_sd_",i,".nc"),format='CDF',overwrite=F)  

}


##ensamblajes futuros
require(raster)
rcp="rcp85"
oDir=paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/GCM_fut/")
gcms<- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")
gcmList <-paste0(oDir,rcp,"/",gcms)

ooDir<-paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_14GCM_fut/",rcp)
if (!file.exists(ooDir)) {dir.create(ooDir, recursive=T)}  

for(i in 1:12){
#if (file.exists(gcmList)){
  
  var1<-paste0(gcmList,"/prec_",i,".nc")
  var2<-paste0(gcmList,"/tmax_",i,".nc")
  var3<-paste0(gcmList,"/tmin_",i,".nc")
  
  prec<-stack(lapply(var1,raster))
  tmax<-stack(lapply(var2,raster))
  tmin<-stack(lapply(var3,raster))
  
  prec_mean<-mean(prec)
  tmax_mean<-mean(tmax)
  tmin_mean<-mean(tmin)
  
  writeRaster(prec_mean,paste0(ooDir,"/prec_",i,".nc"),format='CDF',overwrite=F)
  writeRaster(tmax_mean,paste0(ooDir,"/tmax_",i,".nc"),format='CDF',overwrite=F)
  writeRaster(tmin_mean,paste0(ooDir,"/tmin_",i,".nc"),format='CDF',overwrite=F)  
}
#}
  
#   prec_sd<-sd(prec)
#   tmax_sd<-sd(tmax)
#   tmin_sd<-sd(tmin)
#   
#   writeRaster(prec_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_GCM_historical/prec_",i,"_std.nc"),format='CDF',overwrite=F)
#   writeRaster(tmax_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_GCM_historical/tmax_",i,"_std.nc"),format='CDF',overwrite=F)
#   writeRaster(tmin_sd,paste0("D:/col-cormacarena/interpolation_america/anomalias_GCM/GCM_resolution_1g/average_GCM_historical/tmin_",i,"_std.nc"),format='CDF',overwrite=F)  
#   
}
}









