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


#################################################################
############## prepare the JT calendar
#################################################################
jtDir <- "/mnt/a101/earak/GeneticRes_Beans_Africa/planting_harvest_dates/jtglobal"

#load sowdate and harvest date
sdate <- raster(paste(jtDir,"/2000_sta.asc",sep=""))
lgp <- raster(paste(jtDir,"/2000_lgp.asc",sep=""))

#crop to Indian extent
sdate <- crop(sdate,msk)
lgp <- crop(lgp,msk)

#calculate harvest date
df <- as.data.frame(xyFromCell(sdate,1:ncell(sdate)))
df <- cbind(cell=1:ncell(sdate),df)
df$sta <- extract(sdate,cbind(x=df$x,y=df$y))
df$lgp <- extract(lgp,cbind(x=df$x,y=df$y))

#function to calculate harvest date
calc_har <- function(x) {
  a <- x[1] #a is sowing date
  b <- x[2] #b is lgp value
  if (is.na(a) | is.na(b)) {
    z <- NA
  } else if (a == 0 & b == 0) {
    z <- NA
  } else {
    z <- a + b
    if (z > 365) {z <- z - 365}
  }
  return(z)
}

#calculate harvest date
df$end <- apply(df[,c("sta","lgp")],1,FUN=calc_har)

#set to NA those that are zero in both sta and lgp
df$sta[which(is.na(df$end))] <- NA; df$lgp[which(is.na(df$end))] <- NA

#matrix of dates
mths <- as.Date(0:364,origin="2002-01-01")
mths <- data.frame(date=mths)
mths$mth <- sapply(mths$date,FUN=function(x) {unlist(strsplit(paste(x),"-",fixed=T))[2]})
mths$dom <- sapply(mths$date,FUN=function(x) {unlist(strsplit(paste(x),"-",fixed=T))[3]})
mths$doy <- 1:365
mths$date <- NULL; mths$mth <- as.numeric(mths$mth); mths$dom <- as.numeric(mths$dom)

#function to find month using day of year
find_month <- function(x,mths) {
  x <- round(x,0)
  if (is.na(x) | x < 0) {
    y <- NA
  } else if (x > 365) {
    y <- 12
  } else {
    y <- as.numeric(mths$mth[which(mths$doy == x)])
  }
  return(y)
}

df$sta.mth <- as.numeric(sapply(df$sta,FUN=find_month,mths))
df$end.mth <- as.numeric(sapply(df$end,FUN=find_month,mths))

#put back into rasters
sta_mth <- raster(sdate); end_mth <- raster(sdate)
sta_mth[cellFromXY(sta_mth,cbind(x=df$x,y=df$y))] <- df$sta.mth
end_mth[cellFromXY(end_mth,cbind(x=df$x,y=df$y))] <- df$end.mth

sta_doy <- raster(sdate); end_doy <- raster(sdate)
sta_doy[cellFromXY(sta_doy,cbind(x=df$x,y=df$y))] <- df$sta
end_doy[cellFromXY(end_doy,cbind(x=df$x,y=df$y))] <- df$end

#write the rasters
writeRaster(sta_mth,paste(calDir,"/plant_mth_ind_jt.tif",sep=""),format="GTiff")
writeRaster(end_mth,paste(calDir,"/harvest_mth_ind_jt.tif",sep=""),format="GTiff")

writeRaster(sta_doy,paste(calDir,"/plant_doy_ind_jt.tif",sep=""),format="GTiff")
writeRaster(end_doy,paste(calDir,"/harvest_doy_ind_jt.tif",sep=""),format="GTiff")

######
###### make 1dd versions of these
#load 1dd indian mask
msk <- raster(paste(clmDir,"/imd_cru_climatology_1dd/1960_2000/prec_1.tif",sep=""))

#load sow / harvest dates (Jones & Thornton)
sta_jot <- raster(paste(calDir,"/plant_doy_ind_jt.tif",sep=""))
end_jot <- raster(paste(calDir,"/harvest_doy_ind_jt.tif",sep=""))

#load sow / harvest dates (Sacks et al.)
sta_sck <- raster(paste(calDir,"/plant_ind.tif",sep=""))
end_sck <- raster(paste(calDir,"/harvest_ind.tif",sep=""))

#resample
sta_jota <- aggregate(sta_jot,fact=(xres(msk)/xres(sta_jot)),FUN=function(x) {modal(x,na.rm=T)})
sta_jotb <- resample(sta_jot,msk,method="bilinear")

end_jota <- aggregate(end_jot,fact=(xres(msk)/xres(end_jot)),FUN=function(x) {modal(x,na.rm=T)})
end_jotb <- resample(end_jot,msk,method="bilinear")

sta_scka <- aggregate(sta_sck,fact=(xres(msk)/xres(sta_sck)),FUN=function(x) {modal(x,na.rm=T)})
sta_sckb <- resample(sta_sck,msk,method="bilinear")

end_scka <- aggregate(end_sck,fact=(xres(msk)/xres(end_sck)),FUN=function(x) {modal(x,na.rm=T)})
end_sckb <- resample(end_sck,msk,method="bilinear")


#xy object
xy <- as.data.frame(xyFromCell(msk,which(!is.na(msk[]))))
xy <- cbind(cell=which(!is.na(msk[])),xy)
xy$sta_jota <- extract(sta_jota,xy[,c("x","y")])
xy$sta_jotb <- extract(sta_jotb,xy[,c("x","y")])

xy$end_jota <- extract(end_jota,xy[,c("x","y")])
xy$end_jotb <- extract(end_jotb,xy[,c("x","y")])

xy$sta_scka <- extract(sta_scka,xy[,c("x","y")])
xy$sta_sckb <- extract(sta_sckb,xy[,c("x","y")])

xy$end_scka <- extract(end_scka,xy[,c("x","y")])
xy$end_sckb <- extract(end_sckb,xy[,c("x","y")])

#final values
xy$sta_jot <- xy$sta_jotb; xy$sta_jot[which(is.na(xy$sta_jotb))] <- xy$sta_jota[which(is.na(xy$sta_jotb))]
xy$end_jot <- xy$end_jotb; xy$end_jot[which(is.na(xy$end_jotb))] <- xy$end_jota[which(is.na(xy$end_jotb))]
xy$sta_sck <- xy$sta_sckb; xy$sta_sck[which(is.na(xy$sta_sckb))] <- xy$sta_scka[which(is.na(xy$sta_sckb))]
xy$end_sck <- xy$end_sckb; xy$end_sck[which(is.na(xy$end_sckb))] <- xy$end_scka[which(is.na(xy$end_sckb))]



#final objects
sta_jot <- raster(msk); sta_jot[xy$cell] <- xy$sta_jot
end_jot <- raster(msk); end_jot[xy$cell] <- xy$end_jot

sta_sck <- raster(msk); sta_sck[xy$cell] <- xy$sta_sck
end_sck <- raster(msk); end_sck[xy$cell] <- xy$end_sck

#write rasters
writeRaster(sta_jot,paste(calDir,"/plant_doy_ind_jt_1dd.tif",sep=""),format="GTiff")
writeRaster(end_jot,paste(calDir,"/harvest_doy_ind_jt_1dd.tif",sep=""),format="GTiff")
writeRaster(sta_sck,paste(calDir,"/plant_ind_1dd.tif",sep=""),format="GTiff")
writeRaster(end_sck,paste(calDir,"/harvest_ind_1dd.tif",sep=""),format="GTiff")



