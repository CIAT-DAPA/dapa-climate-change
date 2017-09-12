#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jan 2016
stop("!")

#Make crop calendars using ToE paper 1x1 grid, for cultivated areas only

#load libraries
require(ncdf4); require(maps); require(fields); require(raster)
require(maptools); data(wrld_simpl)

#directories
wd <- "~/Leeds-work/ToE-global-crops-seasons"
m3_dir <- "~/Leeds-work/datasets/yield_data/Monfreda2008/NetCDF"

#crop list
clist_ag <- c("Maize","Rice","Wheat","Soybeans")
clist_m3 <- c("maize","rice","wheat","soybean")

#fix file sent my Maisa so that it is lat,lon instead of lon,lat. Also rename variables
#from Longitude to longitude, and Latitude to latitude
setwd(paste(wd,"/data",sep=""))
system("ncpdq -a latitude,longitude pr_clim_4TOE.nc mask_4TOE.nc")
system("ncrename -h -O -v Latitude,latitude mask_4TOE.nc")
system("ncrename -h -O -v Longitude,longitude mask_4TOE.nc")
setwd("~")

#read base mask file
ncw <- nc_open(paste(wd,"/data/mask_4TOE.nc",sep=""))
msk <- ncvar_get(ncw,start=c(1,1),count=c(ncw$dim$longitude$len,ncw$dim$latitude$len))
image.plot(msk[,],zlim=c(0,12),main=NA)
nc_close(ncw)

msk1 <- raster(paste(wd,"/data/mask_4TOE.nc",sep=""))
msk1[which(!is.na(msk1[]))] <- 1
msk2 <- msk1
msk2[1:ncell(msk2)] <- 1:ncell(msk2)

#load harvested area and resample it to the 1x1 resolution
ahar_crops <- prod_crops <- list()
for (tcrop in clist_m3) {
  #tcrop <- clist_m3[1]
  cat("reading in and resampling harvested area for crop=",tcrop,"\n")
  
  #unzip file
  setwd(m3_dir)
  system(paste("unzip ",tcrop,"_5min.nc.zip",sep=""))
  
  #load raster
  ahar_rs <- raster(paste(tcrop,"_5min.nc",sep=""),level=1)
  ahar_rs[which(ahar_rs[] == 0)] <- NA
  
  #resample
  ahar_xy <- as.data.frame(xyFromCell(ahar_rs,1:ncell(ahar_rs)))
  ahar_xy$ahar_rs <- extract(ahar_rs, ahar_xy[,c("x","y")])
  ahar_xy$msk1 <- extract(msk1, ahar_xy[,c("x","y")]) #msk1 gives where Maisa mask is NA vs valid
  ahar_xy$msk2 <- extract(msk2, ahar_xy[,c("x","y")]) #msk2 gives grid cell number
  ahar_xy <- ahar_xy[which(!is.na(ahar_xy$msk2)),] #if even msk2 is.na is because pixel is outside
  ahar_agg <- aggregate(ahar_xy[,c("ahar_rs","msk1","msk2")],by=list(msk2=ahar_xy$msk2),FUN=function(x) {mean(x,na.rm=T)})
  ahar_agg$ahar_rs[which(is.na(ahar_agg$msk1))] <- NA
  ahar_crops[[tcrop]] <- raster(msk1)
  ahar_crops[[tcrop]][] <- ahar_agg$ahar_rs
  
  #area in ha
  parea <- area(ahar_crops[[tcrop]],na.rm=T) #in km2
  parea <- parea * 100 #in ha
  ahar_crops[[tcrop]] <- ahar_crops[[tcrop]] * parea #actual area (rather than fraction, in ha)
  
  #yield to get production
  yield_rs <- raster(paste(tcrop,"_5min.nc",sep=""),level=2) #ton ha-1
  yield_rs[which(is.na(ahar_rs[]))] <- NA
  
  #resample yield
  yield_xy <- as.data.frame(xyFromCell(yield_rs,1:ncell(yield_rs)))
  yield_xy$yield_rs <- extract(yield_rs, yield_xy[,c("x","y")])
  yield_xy$msk1 <- extract(msk1, yield_xy[,c("x","y")]) #msk1 gives where Maisa mask is NA vs valid
  yield_xy$msk2 <- extract(msk2, yield_xy[,c("x","y")]) #msk2 gives grid cell number
  yield_xy <- yield_xy[which(!is.na(yield_xy$msk2)),] #if even msk2 is.na is because pixel is outside
  yield_agg <- aggregate(yield_xy[,c("yield_rs","msk1","msk2")],by=list(msk2=yield_xy$msk2),FUN=function(x) {mean(x,na.rm=T)})
  yield_agg$yield_rs[which(is.na(yield_agg$msk1))] <- NA
  prod_crops[[tcrop]] <- raster(msk1)
  prod_crops[[tcrop]][] <- yield_agg$yield_rs #in ton ha-1
  prod_crops[[tcrop]] <- prod_crops[[tcrop]] * ahar_crops[[tcrop]] #in ton
  
  #delete netcdf and set another wd
  system(paste("rm -f ",tcrop,"_5min.nc",sep=""))
  setwd("~")
}

#load planting and harvest dates and resample them to the 1x1 resolution
#removing non-harvested areas
pday_crops <- hday_crops <- list()
for (i in 1:length(clist_ag)) {
  #i <- 1
  tcrop <- clist_ag[i]
  tcrop_m3 <- clist_m3[i]
  cat("reading in and resampling crop calendar for crop=",tcrop,"\n")
  
  #load calendar data
  pday_rs <- raster(paste(wd,"/AGMIP_GROWING_SEASON.HARM.version1.25/",tcrop,"_rf_growing_season_dates_v1.25.nc4",sep=""),varname="planting day")
  hday_rs <- raster(paste(wd,"/AGMIP_GROWING_SEASON.HARM.version1.25/",tcrop,"_rf_growing_season_dates_v1.25.nc4",sep=""),varname="harvest day")
  pday_rs[which(pday_rs[] < 0)] <- NA
  hday_rs[which(hday_rs[] < 0)] <- NA
  
  #resample
  cal_xy <- as.data.frame(xyFromCell(pday_rs,1:ncell(pday_rs)))
  cal_xy$pday_rs <- extract(pday_rs, cal_xy[,c("x","y")])
  cal_xy$hday_rs <- extract(hday_rs, cal_xy[,c("x","y")])
  cal_xy$ahar_rs <- extract(ahar_crops[[tcrop_m3]], cal_xy[,c("x","y")])
  cal_xy$msk1 <- extract(msk1, cal_xy[,c("x","y")]) #msk1 gives where Maisa mask is NA vs valid
  cal_xy$msk2 <- extract(msk2, cal_xy[,c("x","y")]) #msk2 gives grid cell number
  cal_xy <- cal_xy[which(!is.na(cal_xy$msk2)),] #if even msk2 is.na is because pixel is outside
  cal_agg <- aggregate(cal_xy[,c("pday_rs","hday_rs","ahar_rs","msk1","msk2")],by=list(msk2=cal_xy$msk2),FUN=function(x) {mean(x,na.rm=T)})
  cal_agg$pday_rs[which(is.na(cal_agg$msk1))] <- NA
  cal_agg$pday_rs[which(is.na(cal_agg$ahar_rs))] <- NA
  cal_agg$hday_rs[which(is.na(cal_agg$msk1))] <- NA
  cal_agg$hday_rs[which(is.na(cal_agg$ahar_rs))] <- NA
  pday_crops[[tcrop]] <- hday_crops[[tcrop]] <- raster(msk1)
  pday_crops[[tcrop]][] <- cal_agg$pday_rs
  hday_crops[[tcrop]][] <- cal_agg$hday_rs
}

#create countries raster
coun_rs <- rasterize(wrld_simpl, msk1, field="UN")

#writing netcdf files per crop with 3 variables: (1) ahar; (2) pday; (3) hday
for (i in 1:length(clist_ag)) {
  #i <- 1
  tcrop_ag <- clist_ag[i]
  tcrop_m3 <- clist_m3[i]
  
  cat("creating NetCDF file for crop=",tcrop_ag,"\n")
  
  #missing value
  mv <- 1.e20
  
  #dimension specification
  dim_lon <- ncdim_def("lon","degrees_east",seq(-179,180,len=360))
  dim_lat <- ncdim_def("lat","degrees_north",seq(-90,90,len=181))
  
  #variable specification
  ncp <- ncvar_def("planting_day","julian days",list(dim_lon,dim_lat),mv,
                   longname="GGCMI planting day MIRCA2000, SAGE, LPJmL",
                   compression=9)
  nch <- ncvar_def("harvest_day","julian days",list(dim_lon,dim_lat),mv,
                   longname="GGCMI harvest day MIRCA2000, SAGE, LPJmL",
                   compression=9)
  nca <- ncvar_def("harvested_area","ha",list(dim_lon,dim_lat),mv,
                   longname="M3 crops harvested area",
                   compression=9)
  ncd <- ncvar_def("production","tons",list(dim_lon,dim_lat),mv,
                   longname="M3 crops total production",
                   compression=9)
  ncy <- ncvar_def("countries","value",list(dim_lon,dim_lat),mv,
                   longname="Country number",
                   compression=9)
  
  #create file in hard drive
  ncfg <- nc_create(paste(wd,"/data/growing_season_prod_countries_",tcrop_ag,".nc",sep=""),list(ncp,nch,nca,ncd,ncy))
  
  #putting attributes
  ncatt_put(ncfg,varid=0,"title","Ag-GRID GGCMI harmonization planting dates and M3 harvested area and production")
  ncatt_put(ncfg,varid=0,"comment1","non-cropped areas have been made NA based on M3 harvested area")
  ncatt_put(ncfg,varid=0,"comment2","production computed by harvested area fraction * physical area of grid cell * M3 yield")
  ncatt_put(ncfg,varid=0,"comment3","countries follow wrld_simple rasterised to 1x1 degree")
  ncatt_put(ncfg,varid=0,"comment4","data was aggregated to 1x1 resolution by averaging")
  ncatt_put(ncfg,varid=0,"comment5","file written by J. Ramirez-Villegas, Jul 2016")
  
  #preparing data for NC files
  mapp <- matrix(nrow=360,ncol=181,data=pday_crops[[tcrop_ag]][],byrow=F)
  #image.plot(mapp[,181:1],zlim=c(0,365),main=NA)
  maph <- matrix(nrow=360,ncol=181,data=hday_crops[[tcrop_ag]][],byrow=F)
  #image.plot(maph[,181:1],zlim=c(0,365),main=NA)
  mapa <- matrix(nrow=360,ncol=181,data=ahar_crops[[tcrop_m3]][],byrow=F)
  #image.plot(mapa[,181:1],zlim=c(0,9e5),main=NA)
  mapd <- matrix(nrow=360,ncol=181,data=prod_crops[[tcrop_m3]][],byrow=F)
  #image.plot(mapd[,181:1],zlim=c(0,5e6),main=NA)
  mapy <- matrix(nrow=360,ncol=181,data=coun_rs[],byrow=F)
  #image.plot(mapy[,181:1],zlim=c(0,894),main=NA)
  mapp[is.na(mapp)] <- mv
  maph[is.na(maph)] <- mv
  mapa[is.na(mapa)] <- mv
  mapd[is.na(mapd)] <- mv
  mapy[is.na(mapy)] <- mv
  
  #writing data to NC files
  ncvar_put(ncfg,ncp,mapp[,181:1])
  ncvar_put(ncfg,nch,maph[,181:1])
  ncvar_put(ncfg,nca,mapa[,181:1])
  ncvar_put(ncfg,ncd,mapd[,181:1])
  ncvar_put(ncfg,ncy,mapy[,181:1])
  
  #closing nc file
  nc_close(ncfg)
}

write.csv(wrld_simpl@data, paste(wd,"/data/country_IDs.csv",sep=""),row.names=F,quote=T)

