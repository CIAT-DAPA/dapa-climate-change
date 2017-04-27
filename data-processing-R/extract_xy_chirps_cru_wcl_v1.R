# ---------------------------------------------------------------------------------
# Author: Jaime Tarapues
# Date: April 27th, 2017
# Version: 1.0 (Test version)
# Purpose: extract values of CHIRPS V2 (5 KM), CRU V4 (50 km), WorldClim V2 (1 km)
# ----------------------------------------------------------------------------------

library(jsonlite)

# selección de años yi=año inciio yf=año final
yi=2010
yf=2010

# listado de coordenadas en columnas id,lon,lat
listcoor=data.frame(id=c(1,2),lon=c(-75.021,-76),lat=c(4,4.5))

# Para usarlo por fuera de ciat usar:
#service="http://181.118.144.158/stations/php/data-graphics-chirps.php?"
service="http://172.22.52.8/stations/php/data-graphics-chirps.php?"
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
  value=fromJSON(paste0(service,"lon=",lon,"&lat=",lat,"&yi=",yi,"&yf=",yf,"&mi=1&mf=12"))
  datalist[[1]]=rbind(datalist[[1]],data.frame(id=listcoor$id[i],date=dateday,value=value$prec$data))
  datalist[[2]]=rbind(datalist[[2]],data.frame(id=listcoor$id[i],date=datemon,value=value$monthly$data))
  datalist[[3]]=rbind(datalist[[3]],data.frame(id=listcoor$id[i],date=mon,value=value$clim$data))
  datalist[[4]]=rbind(datalist[[4]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_prec$data))
  datalist[[5]]=rbind(datalist[[5]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_tmin$data))
  datalist[[6]]=rbind(datalist[[6]],data.frame(id=listcoor$id[i],date=mon,value=value$wcl_tmax$data))
  datalist[[7]]=rbind(datalist[[7]],data.frame(id=listcoor$id[i],date=datemon,value=value$rainy$data))
  datalist[[8]]=rbind(datalist[[8]],data.frame(id=listcoor$id[i],date=datemon,value=value$wetdays$data))  
  datalist[[9]]=rbind(datalist[[9]],data.frame(id=listcoor$id[i],date=years,value=value$annual$data))  
  datalist[[10]]=rbind(datalist[[10]],data.frame(id=listcoor$id[i],date=datemon,value=value$cru_prec$data))
  datalist[[11]]=rbind(datalist[[11]],data.frame(id=listcoor$id[i],date=datemon,value=value$cru_tmin$data))
  datalist[[12]]=rbind(datalist[[12]],data.frame(id=listcoor$id[i],date=datemon,value=value$cru_tmax$data))
  
}

# Datos diarios de chirps v2 resol 5km
datalist[[1]]

# Datos mensuales de chirps v2 resol 5km
datalist[[2]]

# climatología de chirps v2 resol 5km
datalist[[3]]

# woldclim v2 prec 1km
datalist[[4]]

# woldclim v2 tmin 1km
datalist[[5]]

# woldclim v2 tmax 1km
datalist[[6]]

# dias lluviosos por mes chirps v2
datalist[[7]]

# max. dias lluviosos consecutivos por mes chirps
datalist[[8]]

# datos anuales chirps v2
datalist[[9]]

# cru v4 prec 50km
datalist[[10]]

# cru v4  tmin 50km
datalist[[11]]

# cru v4  tmax 50km
datalist[[12]]