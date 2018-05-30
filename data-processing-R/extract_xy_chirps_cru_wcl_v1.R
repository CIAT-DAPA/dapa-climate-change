# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Email: j.e.tarapues@cgiar.org
# Date: April 27th, 2018
# Version: 1.0
# Purpose: extract values of CHIRPS V2 (5 KM), CRU V4 (50 km), WorldClim V2 (1 km) from coordinates
# How does it to run: only modify YEARS,DATASETS and list of coordinates that you need to extract (lines 13-25).
# ----------------------------------------------------------------------------------

library(jsonlite,curl) 

# selection YEARS: yi=start year yf=end year
yi=2017
yf=2018

#Select DATASETS true/false:
chirps="true" #1980-Actual
chirp="false"
wcl="true" # Worldclim
cru="true" #1901-2016

# list of points with coordinates in columns id,lon,lat
listcoor=data.frame(id=c(1,2),lon=c(-75.021,-76),lat=c(4,4.5))
## Option of upload file of points structure (include name columns): id,lon,lat
#listcoor= read.table("D:/temp/xy.txt",, sep=",", na.strings = "", header = TRUE) 

service="http://maprooms.ciat.cgiar.org/CCAFS-Climate/chirps/data-graphics-chirps.php?"
dateday <- format(seq(as.Date(paste0(yi,'/',"1/1")), as.Date(paste0(yf,'/',"12/31")), "days") ,"%Y-%m-%d")
datemon <- format(seq(as.Date(paste0(yi,'/',"1/1")), as.Date(paste0(yf,'/',"12/31")), "months") ,"%Y-%m")
mon <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
years=yi:yf

datalist=list()
datalist[[1]]=data.frame();datalist[[2]]=data.frame();datalist[[3]]=data.frame();datalist[[4]]=data.frame()
datalist[[5]]=data.frame();datalist[[6]]=data.frame();datalist[[7]]=data.frame();datalist[[8]]=data.frame();datalist[[9]]=data.frame();datalist[[10]]=data.frame();datalist[[11]]=data.frame();datalist[[12]]=data.frame()
for(i in 1:nrow(listcoor)){
  lon=listcoor$lon[i]
  lat=listcoor$lat[i]
  value=fromJSON(paste0(service,"lon=",lon,"&lat=",lat,"&yi=",yi,"&yf=",yf,"&mi=1&mf=12","&ch_chirps=",chirps,"&ch_chirp=",chirp,"&ch_wcl=",wcl,"&ch_cru=",cru))
  datalist[[1]]=rbind(datalist[[1]],data.frame(id=listcoor$id[i],date=dateday[1:length(value$prec$data)],value=value$prec$data))
  datalist[[2]]=rbind(datalist[[2]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$monthly$data)],value=value$monthly$data))
  datalist[[3]]=rbind(datalist[[3]],data.frame(id=listcoor$id[i],date=mon,value=value$clim$data))
  datalist[[4]]=rbind(datalist[[4]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_prec$data))
  datalist[[5]]=rbind(datalist[[5]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_tmin$data))
  datalist[[6]]=rbind(datalist[[6]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_tmax$data))
  datalist[[7]]=rbind(datalist[[7]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$rainy$data)],value=value$rainy$data))
  datalist[[8]]=rbind(datalist[[8]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$wetdays$data)],value=value$wetdays$data))  
  datalist[[9]]=rbind(datalist[[9]],data.frame(id=listcoor$id[i],date=years,value=value$annual$data))
  if(length(value$cru_prec$data)>0){
    datalist[[10]]=rbind(datalist[[10]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$cru_prec$data)],value=value$cru_prec$data))
    datalist[[11]]=rbind(datalist[[11]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$cru_tmin$data)],value=value$cru_tmin$data))
    datalist[[12]]=rbind(datalist[[12]],data.frame(id=listcoor$id[i],date=datemon[1:length(value$cru_tmax$data)],value=value$cru_tmax$data))
  }
}

###########
# RESULTS
###########

# Daily chirps v2 resol 5km
datalist[[1]]

# Monthly chirps v2 resol 5km
datalist[[2]]

# climatology chirps v2 resol 5km
datalist[[3]]

# woldclim v2 prec 1km
datalist[[4]]

# woldclim v2 tmin 1km
datalist[[5]]

# woldclim v2 tmax 1km
datalist[[6]]

# rainy days per month - using chirps v2
datalist[[7]]

# maximum of consecutive rainy days per month - using chirps v2
datalist[[8]]

# annuals chirps v2
datalist[[9]]

# cru v4 prec 50km
datalist[[10]]

# cru v4  tmin 50km
datalist[[11]]

# cru v4  tmax 50km
datalist[[12]]
