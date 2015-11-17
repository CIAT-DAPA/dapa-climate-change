#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015
stop("!")

##exclusion of forest from SPAM and Ecocrop data
library(raster); library(maptools); library(rgdal); library(sp); library(PresenceAbsence)

#working dir
bdir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
#bdir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
wd <- paste(bdir,"/modelling/Cul_de_sacs",sep="")
data_dir <- paste(wd,"/model_data",sep="")
runs_dir <- paste(wd,"/model_runs",sep="")
eco_dir <- paste(runs_dir,"/cru_hist",sep="")

#m3 dir
m3_dir <- "~/Leeds-work/cul-de-sacs/M3_data_africa"

#list of crops
crop_list <- paste(read.table(paste(data_dir,"/crop_parameters.tab",sep=""),header=T)$Crop[c(1:6,9:10,17)])

#30 min eco results
eco_crops = stack(paste(eco_dir,"/",crop_list,"_suit.tif",sep=""))

#forest raster
forest <- raster(paste(paste(data_dir,"/forest.tif",sep="")))

####crop rasters including forest areas
m3_crops <- list.files(m3_dir, full.names=F, recursive=T)
m3_stk <- c()
for (tfil in m3_crops) {
  #tfil <- m3_crops[5]
  
  if (!file.exists(paste(m3_dir,"/",gsub("_5min.nc.zip","_M3_af.tif",tfil),sep=""))) {
    #extract netcdf
    setwd(m3_dir); system(paste("7z x ",tfil,sep=""))
    
    #get nc name
    ncfil <- gsub(".zip","",tfil)
    if (length(grep("fmillet",tfil)) != 0) {ncfil <- "millet_5min.nc"}
    if (length(grep("pmillet",tfil)) != 0) {ncfil <- "millet_5min.nc"}
    
    #load nc file
    rs <- raster(ncfil, level=1)
    
    #resample raster
    rs <- resample(rs, eco_crops[[1]], method="ngb")
    rs[which(rs[] == 0)] <- NA
    rs <- writeRaster(rs, paste(m3_dir,"/",gsub("_5min.nc.zip","_M3_af.tif",tfil),sep=""),format="GTiff")
    
    #remove nc file
    system(paste("rm -f ",ncfil,sep=""))
  } else {
    rs <- raster(paste(m3_dir,"/",gsub("_5min.nc.zip","_M3_af.tif",tfil),sep=""))
  }
  
  #forest exclusion
  if (!file.exists(paste(m3_dir,"/",gsub("_5min.nc.zip","_M3_af_forest_excl.tif",tfil),sep=""))) {
    rs_for <- rs
    rs_for[which(forest[]==1)] <- NA  
    rs_for <- writeRaster(rs_for, paste(m3_dir,"/",gsub("_5min.nc.zip","_M3_af_forest_excl.tif",tfil),sep=""),format="GTiff")
  }
}



