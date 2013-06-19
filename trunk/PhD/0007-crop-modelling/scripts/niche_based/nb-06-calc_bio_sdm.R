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
  #calculate
  seasrain <- apply_by_blocks(rain_stk,sowd,hard,calc_totrain)
  
  #write seasonal rainfall total raster
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
  #calculate
  sindex <- apply_by_blocks(rain_stk,sowd,hard,calc_sfeng,rx)
  
  #write seasonal rainfall total raster
  writeRaster(sindex,paste(outDir,"/sindex.tif",sep=""),format="GTiff")
} else {
  sindex <- raster(paste(outDir,"/sindex.tif",sep=""))
}


test_function("test",r_max)


#####################################
#### 3. minimum rainfall during growing season
#####################################

#function to calculate total seasonal rainfall
calc_minrain <- function(x) {
  cell <- x[1]
  lon <- x[2]; lat <- x[3] #longitude
  sow <- x[4]; har <- x[5] #sowing and harvest dates
  
  #growing season start and end
  Gi <- ceiling(sow/30); if (Gi > 12) {Gi <- 12}
  Gf <- ceiling(har/30); if (Gf>12) {Gf <- Gf-12}
  if (Gf < Gi) {rs_list <- c(Gf:12,1:Gi)} else {rs_list <- c(Gi:Gf)}
  
  #extract monthly climate
  monclim <- rain_stk[cell]
  monclim <- as.numeric(monclim[,rs_list])
  
  #calculate and return
  minrain <- sum(monclim)
  return(minrain)
}
#####

#calculate total seasonal rainfall and write raster
if (!file.exists(paste(outDir,"/minrain.tif",sep=""))) {
  #calculate
  sminrain <- apply(xy,1,calc_minrain)
  
  #get into raster
  minrain_rs <- raster(totrain)
  minrain_rs[xy$cell] <- sminrain
  
  #write seasonal rainfall total raster
  writeRaster(minrain_rs,paste(outDir,"/minrain.tif",sep=""),format="GTiff")
} else {
  minrain_rs <- raster(paste(outDir,"/minrain.tif",sep=""))
}






