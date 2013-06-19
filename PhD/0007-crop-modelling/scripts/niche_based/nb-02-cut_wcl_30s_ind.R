#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

#load libraries
library(raster); library(rgdal)

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
wclDir <- "/media/PhD_data_01/Clim_30s/WCl_30s"

owclDir <- paste(clmDir,"/wcl_ind_30s",sep="")
if (!file.exists(owclDir)) {dir.create(owclDir)}

#load indian extent
msk <- raster(paste(clmDir,"/wcl_ind_2_5min/prec_1.tif",sep=""))
msk <- extent(msk)

#loop months and variables and crop the 30s data
for (m in 1:12) {
  #m <- 1
  for (vn in c("prec","tmean","tmax","tmin")) {
    #vn <- "prec"
    cat(vn,":",m,"\n")
    if (!file.exists(paste(owclDir,"/",vn,"_",m,".tif",sep=""))) {
      rs <- raster(paste(wclDir,"/",vn,"_",m,sep=""))
      rs <- crop(rs,msk)
      writeRaster(rs,paste(owclDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
      rm(rs); g=gc(); rm(g)
    }
  }
}


