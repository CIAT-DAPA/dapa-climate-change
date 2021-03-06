dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/IPSL-CM5A-MR/"
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/MIROC-ESM-CHEM/"
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/IPSL-CM5A-LR/"
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/MIROC-ESM/"

ncListVar <- list.files(dirbase,pattern=paste0(".nc"),recursive = T,full.names = TRUE)
for(nc in ncListVar){
  if(!file.exists(paste0(dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4"))){
    system(paste("nccopy -d9 -k4 ", nc, " ", dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4", sep=""))
    if(file.exists(paste0(dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4"))){
      unlink(nc) 
      cat(basename(nc),"...done!\n")
    }else{cat(" -> Error de compresion\n")}
  }else{cat(basename(nc),"...Exist!\n")}
}

################  para comprimir archivo a NC4

#gcmlist<-c('ACCESS1-0', 'bcc-csm1-1', 'BNU-ESM', 'CanESM2', 'CCSM4', 'CESM1-BGC', 'CNRM-CM5', 'CSIRO-Mk3-6-0')
gcmlist<-c('IPSL-CM5A-MR', 'MIROC-ESM-CHEM','IPSL-CM5A-LR', 'MIROC-ESM')

dirgcm= "/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/" #"Z:/data/AR5_Global_Daily_25k/out_stats/"
gcmlist <-  list.dirs(dirgcm, recursive = FALSE, full.names = FALSE) 
gcmi=1
gcmf=1
gcmlist <-  list.dirs(dirgcm, recursive = FALSE, full.names = FALSE) 
for(gcm in gcmlist[gcmi:gcmf]){
  dirbase=paste0(dirgcm,gcm)
  ncListVar <- list.files(dirbase,pattern=paste0(".nc"),recursive = T,full.names = TRUE)
  for(nc in ncListVar){
    if(file_ext(nc)=="nc"){
      if(length(strsplit(basename(nc), '[.]')[[1]])>2){
        if(!file.exists(paste0(gsub(file_ext(nc),"",nc),"nc4"))){
          cat(basename(nc))
          system(paste("nccopy -d9 -k4 ", nc, " ", paste0(gsub(file_ext(nc),"",nc),"nc4"), sep=""))
          if(file.exists(paste0(gsub(file_ext(nc),"",nc),"nc4"))){
            unlink(nc) 
            cat("...done!\n")
          }else{cat(" -> Error de compresion\n")}
        }else{cat(basename(nc),"...Exist!\n");unlink(nc)}       
      }else{
        if(!file.exists(paste0(dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4"))){
          cat(basename(nc))
          system(paste("nccopy -d9 -k4 ", nc, " ", dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4", sep=""))
          if(file.exists(paste0(dirname(nc), "/", sapply(strsplit(basename(nc), '[.]'), "[[", 1),".nc4"))){
            unlink(nc) 
            cat("...done!\n")
          }else{cat(" -> Error de compresion\n")}
        }else{cat(basename(nc),"...Exist!\n");unlink(nc)}      
      }
    }
  }
}
################  para chekear archivos
dirbase="E:/data/AR5_Global_Daily_25k/out_stats/" # "/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/"
gcmi=1
gcmf=22
gcmlist <-  list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
check_ndate=c()
for(gcm in gcmlist[gcmi:gcmf]){
  dirnc=paste0(dirbase,gcm)
  ncListVar <- list.files(dirnc,pattern=paste0(".nc"),recursive = T,full.names = TRUE)
  for(nc in ncListVar){
    t1 <- try(system(paste0("cdo ndate ",nc), intern = TRUE))
    n=as.numeric(t1[2])
    cat("-->Run ",basename(nc),"\n")
    #if(n!=94 && n!=56){
    #if(n==94 || n==56){
      ext=sapply(strsplit(basename(nc), '[.]'), "[[", 2)
      rcp=sapply(strsplit(basename(nc), '[_]'), "[[", 3)
      check_ndate=rbind(check_ndate,cbind(rcp,gcm,dirname(nc),basename(nc),ext,n)) 
      cat("-->!Error ndate: ",basename(nc),"\n")
    #}
  }
}
write.csv(check_ndate, paste0(dirbase, "/errores_ndates2.csv"), row.names = F)

checkExist=c()
scenarios = c("historical","rcp45","rcp85")
gcms=list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
indices = c("CD18", "CDD", "GD10", "HD18", "PTOT", "R02", "R5D", "SDII", "TNN", "TXX","HWDI","R02","SDII","") 
['txavg', 'tnavg', 'tas', 'txx', 'tnn', 'ptot', 'r02','sdii', 'hwdi', 'gsl']
for (index in indices){
  for (scenario in scenarios){
    if(scenario=="historical"){
      period="1950-2005"
    }else{period="2006-2099"}
      cat('-->Run: ',index,scenario,period,'\n')
      nclist=paste0(index,"_BCSD_",scenario,"_",gcms,"_",period,".nc4")
      pos<-which(!file.exists(paste0(dirbase,"/",gcms,"/",nclist)))
      nclist<-nclist[pos]
      gcmlist<-gcms[pos]
      #checkExist=c(checkExist,nclist)
      n=length(gcmlist)
      checkExist=rbind(checkExist,cbind(index=rep(index,n),rcp=rep(scenario,n),gcmlist)) 
  }
}
write.csv(checkExist, paste0(dirbase, "/faltante.csv"), row.names = F)

################## decargar datos corruptos
require(R.utils)
require(stringr)
require("RCurl")

dirbase="Z:/data/AR5_Global_Daily_25k"#"E:/data/AR5_Global_Daily_25k/out_stats/" 
ncListVar <- list.files(dirbase,pattern = "\\.nc$",recursive = F,full.names = TRUE)

for(nc in ncListVar){
  sizeNC=file.info(nc)$size/1000000
  if(sizeNC<100){
    cat(basename(nc),'\n')
    unlink(nc) 
  }
}

listurl=read.csv(paste0(dirbase,"/nex-gddp-nccs-ftp-files.csv"), stringsAsFactors = F,encoding ="latin1",header = T)


t=grepl("tas_day_BCSD_rcp45_r1i1p1_bcc-csm1-1_2074.nc", listurl[,1])

which(t) 
which(listurl[,1] %in% grep("tas_day_BCSD_rcp45_r1i1p1_bcc-csm1-1_2074.nc",listurl[,1], value = TRUE))


output="S:/observed/gridded_products/chirps/daily"
setwd(output)
year="2016"
files=getURL(paste0("http://nasanex.s3.amazonaws.com/NEX-GDDP/BCSD/",year,"/"),verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
files=unlist(strsplit(files, "\r\n"))
for(file in files){
  url <- paste("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/",file,sep="")
  monfile <- basename(file)
  if(!file.exists(paste0(output,'/',gsub(".gz","",monfile)))){
    download.file(url, monfile)
    system(paste0("7z e ",output,"/",monfile," -o",output))
    unlink(monfile)  
  }
}

################  Convert to geotiff  # server: climate

library(sp); library(raster); library(rgdal); library(maptools); library(ncdf4);library(tools);
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats" # "E:/data/AR5_Global_Daily_25k/out_stats" #  
outdir="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats_tiff" # "E:/data/AR5_Global_Daily_25k/out_stats_tiff" # 
mask="/mnt/data_cluster_4/admin_boundaries/masks_world/mask15m" # "S:/admin_boundaries/masks_world/mask15m" # 
gcmi=9#11#8#5#3 #4# #14 #15 #16 #17 #18
gcmf=9#13#10#7#4 #6# #14 #15

wDir <- "/mnt/data_climatewizard/AR5_Global_Daily_25k/temp"
if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
rasterOptions(tmpdir= wDir)

gcmlist <-  list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
for(gcm in gcmlist[gcmi:gcmf]){
  ncListVar <- list.files(paste0(dirbase,'/',gcm),pattern=paste0(".nc4"),recursive = T,full.names = TRUE)
  dirtif <- paste0(outdir, "/", gcm)
  if (!file.exists(dirtif)){dir.create(dirtif)} 
  for(nc in ncListVar){
    tif=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'.tif')      
    if(!file.exists(tif)){
      cat('\nProcessing: ',basename(nc),'\n')
      tifraw=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_raw.tif')
      tifFact=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_fact.tif')
      tifint=paste0(dirtif,'/',sapply(strsplit(basename(nc), '[.]'), "[[", 1),'_int32.tif')
    
      prec=stack(lapply(nc, FUN=brick))
      prec=rotate(prec)
      prec=crop(prec,raster(mask))
      prec=mask(prec, raster(mask))      
      writeRaster(prec*100,tifFact , format='GTiff', overwrite=T)
      #system(paste0("gdal_translate -ot Int16 -a_nodata -9999 -of GTiff ",tifFact," ",tifint),intern = T)
      system(paste0("gdal_translate -ot Int32 -of GTiff ",tifFact," ",tifint),intern = T)
      #system(paste0("gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1 ",tifraw," ",tif),intern = T)
      system(paste0("gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1 ",tifint," ",tif),intern = T)
      unlink(tifFact) 
      unlink(tifint)
      cat(" .....done\n")
    }
  }
}

extract(raster(nc,band=1),cbind(284,-3))
extract(raster(tif,band=1),cbind(-85,-3))

################  calculate ensemble
wDir <-  "/mnt/data_climatewizard/AR5_Global_Daily_25k/temp" #"F:/data/AR5_Global_Daily_25k/temp" #
dirbase="/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats" # "F:/data/AR5_Global_Daily_25k/out_stats" # 

if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
rasterOptions(tmpdir= wDir)
scenarios = c("historical","rcp45","rcp85")
gcms=list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
#var_stat = ['txavg', 'tnavg', 'txx', 'tnn', 'gd10', 'hd18', 'cd18', 'ptot', 'cdd', 'r02', 'r5d', 'sdii']
indices = c("CD18", "CDD", "GD10", "HD18", "PTOT", "R02", "R5D", "SDII", "TNN", "TXX") 
for (index in indices){
  for (scenario in scenarios){
    if(scenario=="historical"){
      period="1950-2005"
    }else{period="2006-2099"}
    direns=paste0(dirbase,"/ensemble")
    if (!file.exists(direns)) {dir.create(direns, recursive=T)}
    
    outNcAvg=paste0(dirbase,"/ensemble/",index,"_BCSD_",scenario,"_ensemble_",period,".nc4")
    if(!file.exists(outNcAvg)){
      cat('\nProcessing: ',index,scenario,period,'\n')
      nclist=paste0(index,"_BCSD_",scenario,"_",gcms,"_",period,".nc4")
      pos<-which(file.exists(paste0(dirbase,"/",gcms,"/",nclist)))
      nclist<-nclist[pos]
      gcmlist<-gcms[pos]
      
      stacknc=paste(paste0(dirbase,"/",gcmlist,"/",nclist),collapse = " ")
      system(paste("cdo -f nc4 -ensavg ",stacknc,outNcAvg))
    }    
  }
}


################  comprobar ndatos
dirbase="Z:/data/AR5_Global_Daily_25k/out_stats/"
gcmlist <-  list.dirs(dirbase, recursive = FALSE, full.names = FALSE) 
checkndate=rbind()
for(gcm in gcmlist){
  dirbase=paste0(dirbase,gcm)
  ncListVar <- list.files(dirbase,pattern=paste0(".nc"),recursive = T,full.names = TRUE)
  for(nc in ncListVar){
    rcp=strsplit(basename(nc), '[_]')[[1]][3]
    period=strsplit(basename(nc), '[_]')[[1]][5]
    ndate=as.numeric(system(paste0("cdo ndate ", nc),intern = T)[2])    
    if(length(strsplit(basename(nc), '[.]')[[1]])>2){
      if(file_ext(nc)=="nc4"){
        timestep=strsplit(basename(nc), '[.]')[[1]][2]
        cat(basename(nc),"\n")
        chedate="error"
        if(rcp=="historical"){
          if(timestep=="monthly" && ndate==672){chedate="ok"}else{unlink(nc)}
          if(timestep=="quarter" && ndate==560){chedate="ok"}else{unlink(nc)}
          if(timestep=="seasonal" && ndate==224){chedate="ok"}else{unlink(nc)}
        }else{
          if(timestep=="monthly" && ndate>=1076){chedate="ok"}else{unlink(nc)}
          # if(timestep=="quarter" && ndate==560){chedate="ok"}
          # if(timestep=="seasonal" && ndate==224){chedate="ok"}          
        }
        checkndate=rbind(checkndate,cbind(name=basename(nc),"nc4",ndate,chedate))
      }else{cat(basename(nc),"...nc!\n")}       
    }else{
      if(file_ext(nc)=="nc4"){
        chedate="error"
        if(rcp=="historical"){
          if(ndate>=56&&ndate<=57){chedate="ok"}else{unlink(nc)}
        }else{
          if(ndate>=90&&ndate<=100){chedate="ok"}else{unlink(nc)}
        }    
        checkndate=rbind(checkndate,cbind(name=basename(nc),"nc4",ndate,chedate))
      }
      # else{
        # chedate="error"
        # if(rcp=="historical"){
        #   if(ndate==56&&ndate<=57){chedate="ok"}
        # }else{
        #   if(ndate>=90&&ndate<=100){chedate="ok"}
        # }    
        # checkndate=rbind(checkndate,cbind(name=basename(nc),"nc",ndate,chedate))        
        # cat(basename(nc),"...nc!\n")}        
    }
  }
}

####### move files
dirbase="Z:/data/AR5_Global_Daily_25k/out_stats/"
nclist <-  list.files(dirbase,pattern=paste0(".nc"),recursive = F,full.names = TRUE)
for(nc in nclist){
  model=strsplit(basename(nc), '[_]')[[1]][6] 
  system(paste0("robocopy ",dirbase," ",dirbase,model," ",basename(nc)," /mov /z"),intern=T)
}

################  para renombrar NC4 a nc

dirgcm= "Z:/data/AR5_Global_Daily_25k/out_stats/" # "/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/" # 
gcmi=1
gcmf=22
gcmlist <-  list.dirs(dirgcm, recursive = FALSE, full.names = FALSE) 
for(gcm in gcmlist[gcmi:gcmf]){
  dirbase=paste0(dirgcm,gcm)
  ncListVar <- list.files(dirbase,pattern=paste0(".nc"),recursive = T,full.names = TRUE)
  for(nc in ncListVar){
    if(file_ext(nc)=="nc4"){
      ncNew=paste0(gsub(file_ext(nc),"",nc),"nc")
      file.rename(nc,ncNew)
      cat(basename(nc),'..done\n')
    }
  }
}

################  datos para EVAN
dirgcm= "Z:/data/AR5_Global_Daily_25k/out_stats/" # "/mnt/data_climatewizard/AR5_Global_Daily_25k/out_stats/" # 
gcmi=1
gcmf=1
#gcmlist <-  list.dirs(dirgcm, recursive = FALSE, full.names = FALSE) 
ncListVar <- list.files(dirgcm,pattern = "SDII",recursive = T,full.names = TRUE)
basename(ncListVar[grep(".monthly\historical", ncListVar)])

paste(c("historical",".monthly"),collapse="|")

which(basename(ncListVar) %in% grep("historical",basename(ncListVar), value = TRUE))


for(gcm in gcmlist[gcmi:gcmf]){
  dirbase=paste0(dirgcm,gcm)

  for(nc in ncListVar){

  }
}

