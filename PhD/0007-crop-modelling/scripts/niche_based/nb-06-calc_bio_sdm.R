#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

### make a number of bioclimatic layers for modelling groundnut using SDMs
### this is to be done for the 30s fitting data

#load libraries
library(raster); library(rgdal)

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

#load monthly rainfall
rain_stk <- stack(paste(clmDir,"/wcl_ind_30s/prec_",1:12,".tif",sep=""))
if (!file.exists(paste(outDir,"/totrain.tif",sep=""))) {
  totrain <- calc(rain_stk,fun=sum)
  writeRaster(totrain,paste(outDir,"/totrain.tif",sep=""),format="GTiff")
} else {
  totrain <- raster(paste(outDir,"/totrain.tif",sep=""))
}

#load sowing and harvest dates
sowd <- raster(paste(calDir,"/plant_ind.tif",sep=""))
hard <- raster(paste(calDir,"/harvest_ind.tif",sep=""))


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
tmin_stk <- stack(paste(clmDir,"/wcl_ind_30s/tmean_",1:12,".tif",sep=""))
tmax_stk <- stack(paste(clmDir,"/wcl_ind_30s/tmean_",1:12,".tif",sep=""))

#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/mintemp.tif",sep=""))) {
  #calculate
  smeantemp <- apply_by_blocks(rain_stk,sowd,hard,calc_meantemp,"mean")
  writeRaster(smeantemp,paste(outDir,"/mintemp.tif",sep=""),format="GTiff")
  
  smintemp <- apply_by_blocks(rain_stk,sowd,hard,calc_meantemp,"min")
  writeRaster(smintemp,paste(outDir,"/mintemp.tif",sep=""),format="GTiff")
  
  smintemp <- apply_by_blocks(rain_stk,sowd,hard,calc_meantemp,"min")
  writeRaster(smintemp,paste(outDir,"/mintemp.tif",sep=""),format="GTiff")
  
  
  
} else {
  sminrain <- raster(paste(outDir,"/minrain.tif",sep=""))
}




