#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the Ben's soil data and convert to Iizumi's grid of 1.125 x 1.125

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
soDir <- paste(wd,"/data/soils",sep="")
sdataDir <- "~/Leeds-work/datasets/soils"
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

#read in soil data
rll_rs <- raster(paste(sdataDir,"/Glam_soil_bjp/Glam_FAO_SOIL.nc",sep=""),varname="rll")
rll_rs <- crop(rll_rs, yrs)
rll_rs <- resample(rll_rs, yrs, method="bilinear")
rll_rs <- writeRaster(rll_rs, paste(soDir, "/rll_lr.tif",sep=""), format="GTiff")

dul_rs <- raster(paste(sdataDir,"/Glam_soil_bjp/Glam_FAO_SOIL.nc",sep=""),varname="dul")
dul_rs <- crop(dul_rs, yrs)
dul_rs <- resample(dul_rs, yrs, method="bilinear")
dul_rs <- writeRaster(dul_rs, paste(soDir, "/dul_lr.tif",sep=""), format="GTiff")

sat_rs <- raster(paste(sdataDir,"/Glam_soil_bjp/Glam_FAO_SOIL.nc",sep=""),varname="sat")
sat_rs <- crop(sat_rs, yrs)
sat_rs <- resample(sat_rs, yrs, method="bilinear")
sat_rs <- writeRaster(sat_rs, paste(soDir, "/sat_lr.tif",sep=""), format="GTiff")

#calculate asw from dul-rll
asw_rs <- dul_rs - rll_rs
asw_rs <- writeRaster(asw_rs, paste(soDir, "/asw_lr.tif",sep=""), format="GTiff")


####
#load and calculate for Shangguan et al. (2014) soil data
sshanDir <- paste(sdataDir,"/shangguan2014",sep="")

#list files
flist <- list.files(sshanDir, pattern="\\.zip")

#process all files
for (fil in flist) {
  #fil <- flist[1]
  cat("...processing file",fil,"\n")
  
  #first uncompress file as needed
  ncfil <- gsub("\\.zip","\\.nc",fil)
  if (!file.exists(paste(sshanDir,"/",ncfil,sep=""))) {
    setwd(sshanDir)
    system(paste("unzip ",fil,sep=""))
    setwd("~")
  }
  
  filname <- gsub("\\.zip","",fil)
  
  #loop levels (soil depth)
  for (i in 1:4) {
    cat("...processing layer",i,"\n")
    if (!file.exists(paste(soDir,"/",filname,"_",i,".tif",sep=""))) {
      #read in raster
      rs <- stack(paste(sshanDir,"/",ncfil,sep=""))
      rs <- rs[[i]]
      
      #crop to Africa
      rs <- crop(rs, yrs)
      
      #resample to 1.125
      rs <- resample(rs, yrs, method="bilinear")
      #plot(rs, zlim=c(0,60))
      
      #write raster file
      rs <- writeRaster(rs,paste(soDir,"/",filname,"_",i,".tif",sep=""),format="GTiff")
    }
  }
  
  if (file.exists(paste(sshanDir,"/",ncfil,sep=""))) {
    system(paste("rm -f ",sshanDir,"/",ncfil,sep=""))
  }
}

###
#load the four layers for each of the limits and calculate mean
fnames <- c("sat","dul","rll")
for (i in 1:3) {
  #i <- 1
  cat("...processing",fnames[i],"\n")
  if (!file.exists(paste(soDir,"/",fnames[i],"_lr_shangguan2014.tif",sep=""))) {
    flist <- list.files(soDir,pattern=paste("VMC",i,sep=""))
    rstk <- stack(paste(soDir,"/",flist,sep=""))
    rs <- calc(rstk[[1:6]], fun=function(x) {mean(x,na.rm=T)})
    rs <- writeRaster(rs, paste(soDir,"/",fnames[i],"_lr_shangguan2014.tif",sep=""),format="GTiff")
    rm(rs)
  }
}

#available soil water
asw_rs <- raster(paste(soDir,"/dul_lr_shangguan2014.tif",sep="")) - raster(paste(soDir,"/rll_lr_shangguan2014.tif",sep=""))
asw_rs <- writeRaster(asw_rs, paste(soDir, "/asw_lr_shangguan2014.tif",sep=""), format="GTiff")

