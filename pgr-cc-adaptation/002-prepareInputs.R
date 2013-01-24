#Julian Ramirez-Villegas
#Jan 2012

library(raster)

#source dir
src.dir <- "~/Repositories/dapa-climate-change/trunk/pgr-cc-adaptation"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"

source(paste(src.dir2,"/GHCND-GSOD-functions.R",sep=""))

#####################
#Harvested area
#Three crops: rice, wheat and millet
#Cut to Europe and Africa
#Aggregated to 0.5 degree

#extents
eur_xt <- extent(-12,43,36,72) #Europe
afr_xt <- extent(-20,52,-36,37) #Africa

#input dir
bDir <- "/mnt/a17/eejarv/pgr-cc-adaptation"
dataDir <- paste(bDir,"/crop-data",sep="")

#here process crop
crop_name <- "Rice"
cropDir <- paste(dataDir,"/",crop_name,".harea",sep="")

if (!file.exists(paste(cropDir,"/",tolower(crop_name),"_harea_glo.tif",sep=""))) {
  rs <- raster(paste(cropDir,"/",tolower(crop_name),"_harea.asc",sep=""))
  rs <- aggregate(rs,fact=6)
  
  #europe
  rs_eur <- crop(rs,eur_xt)
  rs_eur[which(rs_eur[] == 0)] <- NA
  rs_eur <- writeRaster(rs_eur,paste(cropDir,"/",tolower(crop_name),"_harea_eur.tif",sep=""),format="GTiff")
  
  #africa
  rs_afr <- crop(rs,afr_xt)
  rs_afr[which(rs_afr[] == 0)] <- NA
  rs_afr <- writeRaster(rs_afr,paste(cropDir,"/",tolower(crop_name),"_harea_afr.tif",sep=""),format="GTiff")
  
  #globe
  rs[which(rs[] == 0)] <- NA
  rs <- writeRaster(rs,paste(cropDir,"/",tolower(crop_name),"_harea_glo.tif",sep=""),format="GTiff")
}


#########
#Sowing and harvesting dates:
#Three crops: rice, wheat and millet
#Planting dates from Sacks et al. (2010) cut to Europe and Africa
#Aggregated to 0.5 degree
#Cut to harvested area
crop_name <- "Rice.2"
cropDir <- paste(dataDir,"/",crop_name,".crop.calendar.fill",sep="")

if (crop_name == "Rice.2") {
  ha_name <- "Rice"
} else if (crop_name == "Wheat.Winter") {
  ha_name <- "Wheat"
} else {
  ha_name <- crop_name
}

#aggregate the planting and harvest dates
for (rsname in c("plant","harvest")) {
  if (!file.exists(paste(cropDir,"/",rsname,"_month.tif",sep=""))) {
    rs <- raster(paste(cropDir,"/",rsname,".asc",sep=""))
    rs <- aggregate(rs,fact=6)
    ha_rs <- raster(paste(dataDir,"/",ha_name,".harea/",tolower(ha_name),"_harea_glo.tif",sep=""))
    rs[which(is.na(ha_rs[]))] <- NA
    
    #convert to month
    vals <- as.numeric(sapply(rs[which(!is.na(rs[]))],find_month))
    rs[which(!is.na(rs[]))] <- vals
    rs <- writeRaster(rs,paste(cropDir,"/",rsname,"_month.tif",sep=""),format="GTiff")
  }
}



