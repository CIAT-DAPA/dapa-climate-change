#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

### make a number of bioclimatic layers for modelling groundnut using SDMs
### this is to be done for the 30s fitting data

#load libraries
library(raster); library(rgdal); library(zoo)

#source directory
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/niche_based"
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/niche_based"
source(paste(src.dir,"/nb-06-calc_bio_sdm-fun.R",sep=""))

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
calDir <- paste(bDir,"/calendar",sep="")
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")

outDir <- paste(clmDir,"/bio_ind_30s",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

#load sowing and harvest dates
sowd <- raster(paste(calDir,"/plant_doy_ind_jt.tif",sep=""))
hard <- raster(paste(calDir,"/harvest_doy_ind_jt.tif",sep=""))

#load monthly rainfall
rain_stk <- stack(paste(clmDir,"/wcl_ind_30s/prec_",1:12,".tif",sep=""))

#####################################
#### 0. Total annual rainfall
#####################################
if (!file.exists(paste(outDir,"/annrain.tif",sep=""))) {
  totrain <- calc(rain_stk,fun=sum)
  writeRaster(totrain,paste(outDir,"/annrain.tif",sep=""),format="GTiff")
} else {
  totrain <- raster(paste(outDir,"/annrain.tif",sep=""))
}


#####################################
#### 1. Total seasonal rainfall
#####################################
#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/seasrain.tif",sep=""))) {
  seasrain <- apply_by_blocks(rain_stk,sowd,hard,calc_totrain)
  writeRaster(seasrain,paste(outDir,"/seasrain.tif",sep=""),format="GTiff")
} else {
  seasrain <- raster(paste(outDir,"/seasrain.tif",sep=""))
}

#get maximum rainfall for normalising
rx <- seasrain@data@max

#####################################
#### 2. Feng et al. (2013)'s seasonality index
#####################################
#run the calculation
if (!file.exists(paste(outDir,"/sindex.tif",sep=""))) {
  sindex <- apply_by_blocks(rain_stk,sowd,hard,calc_sfeng,rx)
  writeRaster(sindex,paste(outDir,"/sindex.tif",sep=""),format="GTiff")
} else {
  sindex <- raster(paste(outDir,"/sindex.tif",sep=""))
}

#####################################
#### 3. minimum rainfall during growing season
#####################################
#run the calculation
if (!file.exists(paste(outDir,"/minrain.tif",sep=""))) {
  sminrain <- apply_by_blocks(rain_stk,sowd,hard,calc_minrain)
  writeRaster(sminrain,paste(outDir,"/minrain.tif",sep=""),format="GTiff")
} else {
  sminrain <- raster(paste(outDir,"/minrain.tif",sep=""))
}


#####################################
#### 4. mean/min/max temperature growing season
#####################################
tmen_stk <- stack(paste(clmDir,"/wcl_ind_30s/tmean_",1:12,".tif",sep=""))
tmax_stk <- stack(paste(clmDir,"/wcl_ind_30s/tmax_",1:12,".tif",sep=""))
tmin_stk <- stack(paste(clmDir,"/wcl_ind_30s/tmin_",1:12,".tif",sep=""))

#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/minmintemp.tif",sep=""))) {
  #average of monthly mean temperatures
  smeantemp <- apply_by_blocks(tmen_stk,sowd,hard,calc_meantemp,"mean")
  writeRaster(smeantemp,paste(outDir,"/meanmeantemp.tif",sep=""),format="GTiff")
  
  #maximum of monthly maximum temperatures
  smaxtemp <- apply_by_blocks(tmax_stk,sowd,hard,calc_meantemp,"max")
  writeRaster(smaxtemp,paste(outDir,"/maxmaxtemp.tif",sep=""),format="GTiff")
  
  #minimum of monthly minimum temperatures
  smintemp <- apply_by_blocks(tmin_stk,sowd,hard,calc_meantemp,"min")
  writeRaster(smintemp,paste(outDir,"/minmintemp.tif",sep=""),format="GTiff")
} else {
  smeantemp <- raster(paste(outDir,"/meanmeantemp.tif",sep=""))
  smaxtemp <- raster(paste(outDir,"/maxmaxtemp.tif",sep=""))
  smintemp <- raster(paste(outDir,"/minmintemp.tif",sep=""))
}


#####################################
#### 5. number of days with Tmax > 34C
#####################################
#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/daystcrit.tif",sep=""))) {
  #calculate
  daystcrit <- apply_by_blocks(tmax_stk,sowd,hard,calc_tcdays)
  writeRaster(daystcrit,paste(outDir,"/daystcrit.tif",sep=""),format="GTiff")
} else {
  daystcrit <- raster(paste(outDir,"/daystcrit.tif",sep=""))
}


#####################################
#### 6. calculate growing season GDD
#####################################
#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/totgdd.tif",sep=""))) {
  #calculate
  totgdd <- apply_by_blocks(tmen_stk,sowd,hard,calc_gdd)
  writeRaster(totgdd,paste(outDir,"/totgdd.tif",sep=""),format="GTiff")
} else {
  totgdd <- raster(paste(outDir,"/totgdd.tif",sep=""))
}


#####################################
#### 7. calculate growing season total VPD
#####################################
tnx_stk <- stack(c(paste(clmDir,"/wcl_ind_30s/tmin_",1:12,".tif",sep=""),
                   paste(clmDir,"/wcl_ind_30s/tmax_",1:12,".tif",sep="")))
if (!file.exists(paste(outDir,"/totvpd.tif",sep=""))) {
  totvpd <- apply_by_blocks(tnx_stk,sowd,hard,calc_vdp)
  writeRaster(totvpd,paste(outDir,"/totvpd.tif",sep=""),format="GTiff")
} else {
  totvpd <- raster(paste(outDir,"/totvpd.tif",sep=""))
}


#####################################
#### 8. calculate total potential evapotranspiration
#####################################
if (!file.exists(paste(outDir,"/setmax.tif",sep=""))) {
  totetmax <- apply_by_blocks(tnx_stk,sowd,hard,calc_etmax)
  writeRaster(totetmax,paste(outDir,"/setmax.tif",sep=""),format="GTiff")
} else {
  totetmax <- raster(paste(outDir,"/setmax.tif",sep=""))
}


#####################################
#### 9. calculate drought index using etmax and seasrain
#####################################
if (!file.exists(paste(outDir,"/dindex.tif",sep=""))) {
  totetmax <- raster(paste(outDir,"/setmax.tif",sep=""))
  seasrain <- raster(paste(outDir,"/seasrain.tif",sep=""))
  if (totetmax@data@min == 0) {
    dindex <- (totetmax-seasrain)/(totetmax+1) * 100
  } else {
    dindex <- (totetmax-seasrain)/totetmax * 100
  }
  writeRaster(dindex,paste(outDir,"/dindex.tif",sep=""),format="GTiff")
} else {
  dindex <- raster(paste(outDir,"/dindex.tif",sep=""))
}




