#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("!")

### prepare the Sacks calendar for necessary analyses.

#load libraries
library(raster); library(rgdal)

#i/o directories
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
calDir <- paste(bDir,"/calendar",sep="")
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")

#load indian extent
msk <- raster(paste(clmDir,"/wcl_ind_2_5min/prec_1.tif",sep=""))
msk <- extent(msk)

#load sowdate and harvest date
sdate <- raster(paste(calDir,"/plant.tif",sep=""))
hdate <- raster(paste(calDir,"/harvest.tif",sep=""))

#crop to Indian extent
sdate <- crop(sdate,msk)
hdate <- crop(hdate,msk)

#fix the errors in those districts over Orissa state
sdate_cor <- sdate
sdate_cor[which(sdate[] ==  354.5)] <- 140.5
sdate_cor[which(sdate[] ==  312.0)] <- 140.5
plot(sdate_cor,col=rev(terrain.colors(14))) #verify

hdate_cor <- hdate
hdate_cor[which(hdate[] ==  46.0)] <- 245.5
hdate_cor[which(hdate[] ==  112.5)] <- 245.5
plot(hdate_cor,col=rev(terrain.colors(14))) #verify

#write rasters
writeRaster(sdate_cor,paste(calDir,"/plant_ind.tif",sep=""),format="GTiff")
writeRaster(hdate_cor,paste(calDir,"/harvest_ind.tif",sep=""),format="GTiff")

#make a raster of lgp
xy <- as.data.frame(xyFromCell(sdate_cor,which(!is.na(sdate_cor[]))))
xy <- cbind(cell=which(!is.na(sdate_cor[])),xy)
xy$sow <- extract(sdate_cor,xy[,c("x","y")])
xy$har <- extract(hdate_cor,xy[,c("x","y")])

#function to calculate lgp from sowing and harvest date
lgp_calc <- function(x) {
  sowd <- x[1]
  hard <- x[2]
  if (sowd <= hard) {
    lgp <- hard-sowd+1
  } else {
    lgp <- hard+365-(sowd)+1
  }
  return(lgp)
}

#calculation
xy$lgp <- apply(xy[,c("sow","har")],1,FUN=lgp_calc)

#create and write raster
lgp <- raster(sdate_cor)
lgp[xy$cell] <- xy$lgp
writeRaster(lgp,paste(calDir,"/lgp_ind.tif",sep=""),format="GTiff")

