### Author: Julian Ardila
### Date: January 2016

####raster de referencia a 1 grado de resolucion
require(raster)
ref<-raster(xmn=-114,xmx=-34,ymn=-40,ymx=25,res=1)
ref[]<-1:ncell(ref)


##################################################################
## Historical
baseDir <- "T:/gcm/cmip5/raw/monthly"
ens <- "r1i1p1"
basePer <- "1981_2005"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico/raw/historical")

##GCM historicos
curDir <- paste(baseDir, "/historical", sep="")
# gcmList <- list.dirs(curDir, recursive = FALSE, full.names =F)
gcmList <- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", 
             "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", 
             "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")

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


###########################################################################################################
##### GCM futuro
rcpList <- c("rcp26", "rcp45","rcp85")
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico/raw"
baseDir <- "T:/gcm/cmip5/raw/monthly"
ens <-"r1i1p1"
period <- "2030"

for (rcp in rcpList){

  futDir <- paste(baseDir, "/", rcp, sep="")
  dirfut<-paste0(oDir, "/", rcp)
  
  # Define start and end year
  staYear <- as.integer(period)
  endYear <- as.integer(period) + 29
  
  
  curDir <- paste(baseDir, "/historical", sep="")
  # gcmList <- list.dirs(curDir, recursive = FALSE, full.names =F)
  # gcmListf <- list.dirs(paste0(baseDir,"/",rcp), recursive = FALSE, full.names =F)
  gcmList <- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", 
               "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", 
               "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")
  
  for(i in 1:length(gcmList)){
  
    futAvgDir <- paste(futDir, "/", gcmList[i], "/", ens, "/average/", staYear, "_", endYear, sep="")
    
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
        
        if (!file.exists(dirfut)) {dir.create(dirfut, recursive=T)}
        
        dirfutrcp<-paste0(dirfut,"/",gcmList[i])
        if (!file.exists(dirfutrcp)) {dir.create(dirfutrcp, recursive=T)}  
        
        writeRaster(outfut,paste0(dirfutrcp,"/",names[j]),format='CDF', overwrite=F)
      }
    }
  }
  
}


#################################################################################################
##ensamblajes historico
require(raster)
gcmList <- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", 
             "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", 
             "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")
iDir<- paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico/raw/historical")

gcmList <-paste0(iDir,"/",gcmList)
varList <- c("prec", "tmax", "tmin")
oDir <- paste0(iDir, "/ensemble")
if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (var in varList){
  for(i in 1:12){
    
    varStk<-paste0(gcmList,"/", var, "_",i,".nc")
    varStk<-stack(lapply(varStk,raster))
    varStk_mean<-mean(varStk)
    
    writeRaster(varStk_mean, paste0(oDir, "/", var, "_",i,".nc"),format='CDF',overwrite=F)
    
  }
}
 

####################################################################################################
##Ensamblajes futuros

require(raster)
iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico/raw"
gcmList <- c("bcc_csm1_1", "ncar_ccsm4", "csiro_mk3_6_0", "gfdl_cm3", "giss_e2_h", "giss_e2_r", 
             "mohc_hadgem2_es", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_miroc5", "miroc_esm", 
             "miroc_esm_chem", "mri_cgcm3", "ncc_noresm1_m")
rcpList <- c("rcp26","rcp45","rcp85")
varList <- c("prec", "tmax", "tmin")

for (rcp in rcpList) {
  
  gcmPath <-paste0(iDir, "/", rcp,"/",gcmList)
  
  oDir<-paste0(iDir, "/", rcp, "/ensemble")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (var in varList){
    
    for(i in 1:12){
      
      if (!file.exists(paste0(oDir, "/", var, "_",i,".nc"))){
        varStk<-paste0(gcmPath,"/", var, "_",i,".nc")
        varStk<-stack(lapply(varStk,raster))
        varStk_mean<-mean(varStk)
        
        writeRaster(varStk_mean, paste0(oDir, "/", var, "_",i,".nc"),format='CDF',overwrite=F)
      }
      
    }
  }
}


###############################################################################################
## Resampled anomalies

require(raster)

rcp <- "rcp85"
msk <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/_region/alt-prj-ame.asc"
iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico/raw"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico"
# shape<-"D:/col-cormacarena/interpolation_america/shape_america/sa0.shp"
corr = 0.1
conf = 0.98

curAvgDir <- paste0(iDir, "/historical/ensemble")
futAvgDir <- paste0(iDir, "/", rcp, "/ensemble")
oResDir <- paste0(oDir, "/", rcp)
if (!file.exists(oResDir)) {dir.create(oResDir, recursive=T)}
varList <- c("prec", "tmax", "tmin")

mask <- raster(msk) * 0 + 1 
rs <- extent(raster(mask))

# Loop around variables
for (var in varList) {
  
  dataMatrix <- c("rcp","model","var")
  
  # Loop around months
  for (mth in 1:12) {
    
    outRes <- paste0(oResDir, "/", var, "_", mth, ".tif")
    cat(outRes)
    if (!file.exists(outRes)){
      
      curAvgNc <- raster(paste(curAvgDir, "/", var, "_", mth, ".nc", sep=""))
      futAvgNc <- raster(paste(futAvgDir, "/", var, "_", mth, ".nc", sep=""))
      
      if (var == "prec"){
        
        curAvgNc[curAvgNc[]<corr] <- corr
        futAvgNc[futAvgNc[]<corr] <- corr
        anomNc <- ((futAvgNc - curAvgNc)/(curAvgNc))*100
        
        centroids=rasterToPoints(anomNc)
        df <- data.frame(centroids)
        value=df[,3]
        
        qdev = quantile(value,conf,na.rm=T)
        qdev = data.frame(id=names(qdev), values=unname(qdev), stringsAsFactors=FALSE)
        
        values(anomNc) <- ifelse(values(anomNc) >=qdev$values, qdev$values, values(anomNc)) 
        
      } else {
        
        anomNc <- ( futAvgNc - curAvgNc ) * 10
        
      }
      
      # Cortar
      anomNc <- crop(anomNc, rs)
      
      # Resample
      anomRes <- resample(anomNc, mask ,method="bilinear")
      anomNc <- writeRaster(anomRes, outRes, format="GTiff", overwrite=F, datatype='INT2S')
      
    }
    
  } #mth
} # var




###############################################################################################
## Agregación anual

require(raster)
rcp <- "rcp26"
msk <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/_region/alt-prj-ame.asc"
mask <- raster("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask_tropico.tif")

bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico"
iDir <- paste0(bDir , "/", rcp)

varList <- c("prec", "tmax", "tmin")

for (var in varList){
  
  if (!file.exists(paste0(iDir, "/", var, "_ann.tif"))){
    
    cat(rcp, var)
    stk <- stack(paste0(iDir, "/", var, "_", 1:12, ".tif"))
    varStk_mean <- mean(stk)
    
    rsCrop <- crop(varStk_mean, extent(mask))
    mask2 <- crop(mask, extent(rsCrop))
    rsMask <- mask(rsCrop, mask2)
    
    varStk_mean <- writeRaster(rsMask, paste0(iDir, "/", var, "_ann.tif"), format="GTiff", overwrite=F, datatype='INT2S')
    
  }
}




