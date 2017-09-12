require(raster)
require(ncdf)

rcpList=c('rcp26','rcp45','rcp85')
# rcp <- "rcp85"
dirgcm <-  "T:/gcm/cmip5/raw/daily"
ens="r1i1p1"
basePer="1961_1990"
mask="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask_rectangle.tif"
# ncmask='D:/jetarapues/Request/Request_cnavarro/cormacarena/mask_rectangle.nc'
dirout="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/hur"
var='hur'
# gcmlist=c("bcc_csm1_1","csiro_mk3_6_0","gfdl_cm3","ipsl_cm5a_lr","ipsl_cm5a_mr","miroc_esm","miroc_esm_chem","miroc_miroc5","mohc_hadgem2_cc","mri_cgcm3","ncar_ccsm4")
gcmlist=c("bcc_csm1_1","csiro_mk3_6_0","ipsl_cm5a_lr","ipsl_cm5a_mr","miroc_esm_chem","miroc_miroc5")
#gcmlist <-  list.dirs(paste0(dirgcm,"/", rcp), recursive = FALSE, full.names = FALSE) 

bbox <- extent(raster(mask))

# r <- raster(nrow=687, ncol=925,)
# r[] <- 1
# extent(r) <- extent(bbox@xmin+360,bbox@xmax+360,bbox@ymin,bbox@ymax)
# writeRaster(r, 'D:/jetarapues/Request/Request_cnavarro/cormacarena/mask_rectangle.nc', format="CDF", overwrite=T)   


for(rcp in rcpList){
  
  if (rcp == "historical"){dirrcp <- paste0(dirgcm, "/", rcp)} else {dirrcp <- paste0(dirgcm, "/", rcp)}
  
  for (gcm in gcmlist){
    
    Hncvar <- list.files(path=paste0(dirgcm, "/historical/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)
    ncvar <- list.files(path=paste0(dirrcp, "/", gcm, "/r1i1p1"), pattern=paste0(var, "_day*"), full.names=TRUE)    
    
    if (length(ncvar) > 0 && length(Hncvar) > 0){
      
      cat(basename(gcm),rcp,'\n')
      dirtempHist <- paste0(dirout,"/historical/", basename(gcm))
      dirnorm<- paste0(dirout,"/historical/", basename(gcm),"/1981_2005")
      
      if (!file.exists(dirtempHist)) {dir.create(dirtempHist, recursive=T)}    
      if (!file.exists(dirnorm)) {dir.create(dirnorm, recursive=T)}
      
      ncsel=paste0("D:/jetarapues/Request/Request_cnavarro/cormacarena/", "/historical_", var, "_1981_2005_day_sely.nc")
      nccut=paste0(dirtempHist, "/historical_", var, "_1981_2005_day_cut.nc")
      ncmon=paste0(dirtempHist, "/historical_", var, "_1981_2005_day_mon.nc")
      ncnor=paste0(dirtempHist, "/historical_", var, "_1981_2005_day_ymon.nc")
      ncres=paste0(dirnorm, "/historical_", var, "_1981_2005_day_nor.nc")
      
      if (!file.exists(ncres)) {
        
        #         system(paste("D:/jetarapues/cdo/cdo.exe -sellevidx,2 -seldate,1981-01-01,2005-12-31 ", Hncvar, " ", ncsel, sep=""))
        #         system(paste("D:/jetarapues/cdo/cdo.exe sellonlatbox,",bbox@xmin+360-5,",",bbox@xmax+360+5,",",bbox@ymin-5,",",bbox@ymax+5," ", ncsel, " ", nccut,sep=""))
        #         system(paste("D:/jetarapues/cdo/cdo.exe monavg ",nccut, " ", ncmon,sep=""))
        #         system(paste("D:/jetarapues/cdo/cdo.exe ymonavg ",ncmon, " ", ncnor,sep=""))
        #         system(paste("D:/jetarapues/cdo/cdo.exe remapbil,",ncmask,' ',ncnor, " ", ncres,sep=""))
        system(paste("cdo splitmon ",ncnor, " ", dirnorm,'/',var,'_',sep=""))
      }
      
      dirtempFut = paste0(dirout,'/',rcp,"/", basename(gcm))
      dirnorm2 <- paste0(dirout,'/',rcp,"/", basename(gcm),"/2030_2059")
      
      if (!file.exists(dirtempFut)) {dir.create(dirtempFut, recursive=T)}  
      if (!file.exists(dirnorm2)) {dir.create(dirnorm2, recursive=T)}  
      
      ncsel=paste0("D:/jetarapues/Request/Request_cnavarro/cormacarena/", rcp, "/", basename(gcm), "/",rcp,"_", var, "_2030_2059_day_sely.nc")
      nccut=paste0(dirtempFut, "/",rcp,"_", var, "_2030_2059_day_cut.nc")
      ncmon=paste0(dirtempFut, "/",rcp,"_", var, "_2030_2059_day_mon.nc")
      ncnor=paste0(dirtempFut, "/",rcp,"_", var, "_2030_2059_day_ymon.nc")
      ncres=paste0(dirnorm2, "/",rcp,"_", var, "_2030_2059_day_nor.nc")
      
#       if (!file.exists(ncsel)) {
#         system(paste("D:/jetarapues/cdo/cdo.exe -sellevidx,2 -seldate,2030-01-01,2059-12-31 ", ncvar, " ", ncsel, sep=""))
#       }
#       if (!file.exists(nccut)) {
#         system(paste("D:/jetarapues/cdo/cdo.exe sellonlatbox,",bbox@xmin+360-5,",",bbox@xmax+360+5,",",bbox@ymin-5,",",bbox@ymax+5," ", ncsel, " ", nccut,sep=""))
#       }
#       if (!file.exists(ncmon)) {
#         system(paste("D:/jetarapues/cdo/cdo.exe monavg ",nccut, " ", ncmon,sep=""))
#       }
#       if (!file.exists(ncnor)) {
#         system(paste("D:/jetarapues/cdo/cdo.exe ymonavg ",ncmon, " ", ncnor,sep=""))
#       }
      #       if (!file.exists(ncres)) {
      #         system(paste("D:/jetarapues/cdo/cdo.exe remapbil,",ncmask,' ',ncnor, " ", ncres,sep=""))
      #       }
      if (!file.exists(paste0(dirnorm2,'/',var,'_12.nc'))) {
        system(paste("cdo splitmon ",ncnor, " ", dirnorm2,'/',var,'_',sep=""))
      }
      
      cat('..Change',basename(gcm),rcp,'\n')
      diranom <- paste0(dirout,"/anomalies/",rcp,'/',basename(gcm))
      if (!file.exists(diranom)) {dir.create(diranom, recursive=T)}  
      
      mon=c(paste0(0,1:9),10:12)
      for(i in 1:12){
        
        if (!file.exists(paste0(diranom,'/',var,'_',i,'.nc'))) {
          hist=raster(paste0(dirnorm,'/',var,'_',mon[i],'.nc'))
          fut= raster(paste0(dirnorm2,'/',var,'_',mon[i],'.nc'))
          #         histP=rasterToPoints(hist)
          #         futP=rasterToPoints(fut)
          #         summary(futP[,3]-histP[,3])
          anom=(fut-hist)/hist
          xmin(anom) <- xmin(anom) - 360
          xmax(anom) <- xmax(anom) - 360
          anom <- resample(anom, raster(mask))
          #         extent(anom) <- extent(bbox@xmin,bbox@xmax,bbox@ymin,bbox@ymax)
          out=writeRaster(anom, paste0(diranom,'/',var,'_',i,'.nc'), format="CDF", overwrite=T)   
        }
      }
      
    }
  }
}

for(rcp in rcpList){

  for(i in 1:12){

    # ens<-ens[which(file.exists(ens))]
    ensemble=mean(stack(paste0(dirout,'/anomalies/',rcp,'/',gcmlist,'/',var, "_",i,'.nc')))
    direns=paste0(dirout,'/anomalies/',rcp,'/ensemble')
    if (!file.exists(direns)) {dir.create(direns, recursive=T)}
    out=writeRaster(ensemble, paste0(direns,'/',var,'_',i,'.asc'))
  }
}
