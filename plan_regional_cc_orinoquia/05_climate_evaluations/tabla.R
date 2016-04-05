
info<-read.csv("C:/Users/jardila/Desktop/estaciones Rdata/info_var_tmax/info_var_tmax.csv")##carpeta donde esta contenido el archivo de informacion de variable
infomes<-read.csv("C:/Users/jardila/Desktop/grar_mpas/datos_completos/month_tmax_imput.csv")
ruta<-"C:/Users/jardila/Desktop/estaciones Rdata/Rdata_tmax"################carpeta donde estan contenidos los archios .Rdata
txt=list.files(ruta)
rutas=paste(ruta,"/",txt,sep="")
##rutas1=rutas[29:37]##para prec
station.num=9



cat('\n', '\n', "       *** STATISTICAL/EXPLORATORY ANALYSIS Script ***", '\n', '\n')
# Retrieving and Loading climate information from 'station' Objects
#station.num <- as.numeric(readline("[Enter Number of Stations to analize] "))
while(is.na(station.num)==TRUE) {station.num <- ask.station.num() }
stat.list.full <- stat.list <- vector("list", station.num)
period <- vector("list", station.num)
n.y <- starting <- end <- station.name <- x <- y <- elev <- numeric()
#readline("[Press Enter to select the stations (*.RData) to include in the analysis] ")
library(tcltk)


for(i in 1:station.num){
  Filters <- matrix(c("R", ".RData", "All files", "*"), 1, 2, byrow = TRUE)
  #   load(if(interactive())
  # tk_choose.files(caption="Select the *.RData file to load", filter = Filters)); rm(Filters)
  data<-load(rutas[i+1])##para las otras
  #data<-load(rutas1[i])###para prec
  stat.list.full[[i]] <- stat.list[[i]] <- stndata$val
  period[[i]] <- stndata$yy; country=stndata$country
  country <- stndata$country; Var <- stndata$obs.name; units<-stndata$unit
  station.name[i] <- stndata$station; n.y[i] <- length(stat.list[[i]][,1])
  starting[i] <- stndata$start; end[i] <- starting[i]+(n.y[i]-1)
  x[i] <- stndata$lon; y[i] <- stndata$lat; elev[i] <- stndata$alt
  ###########################                  
  cat(paste(station.name[i], ":", i, "of", station.num, "stations loaded\n"))
  if(i==station.num){cat('\n\r')}
  flush.console()
  ###########################
  
}
print(cbind(station.name, starting, end))

####promedios de estacion
listas<-vector("list",station.num)
for(i in 1:station.num){
  media.mensual<- apply(stat.list.full[[i]], 2, mean)
  listas[[i]]<-media.mensual
}

tmax<-do.call(rbind, listas)

####latitudes
l=info$lat
latitudes<-cbind(l)

####longitudes
l<-info$long
longitudes<-cbind(l)

####altitud
l<-info$Elevation
altitud<-cbind(l)


##########country
country=rep("Colombia",9)

###names
l<-paste0("",info$Name,"")
names<-cbind(l)


##########id
ID<-seq(1,9,1)


l<-info$codigo
OLD_ID<-cbind(l)


##########source
SOURCE=rep("IDEAM",9)

##nyear
nyear=rep(35,9)

nombres<-c("ID","SOURCE","OLD_ID","NAME","COUNTRY","LAT","LON","ALT","Jan", "Feb", "Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec","NYEAR")
base<-data.frame(ID,SOURCE,OLD_ID,names,country,latitudes,longitudes,altitud,tmax,nyear)
names(base)<-nombres

write.csv(base,"table_tmax.csv",row.names=F)

##############################################################################################tmin
info<-read.csv("C:/Users/jardila/Desktop/estaciones Rdata/info_var_tmin/info_var_tmin.csv")##carpeta donde esta contenido el archivo de informacion de variable
infomes<-read.csv("C:/Users/jardila/Desktop/grar_mpas/datos_completos/month_tmin_imput.csv")
ruta<-"C:/Users/jardila/Desktop/estaciones Rdata/Rdata_tmin"################carpeta donde estan contenidos los archios .Rdata
txt=list.files(ruta)
rutas=paste(ruta,"/",txt,sep="")
##rutas1=rutas[29:37]##para prec
station.num=9



cat('\n', '\n', "       *** STATISTICAL/EXPLORATORY ANALYSIS Script ***", '\n', '\n')
# Retrieving and Loading climate information from 'station' Objects
#station.num <- as.numeric(readline("[Enter Number of Stations to analize] "))
while(is.na(station.num)==TRUE) {station.num <- ask.station.num() }
stat.list.full <- stat.list <- vector("list", station.num)
period <- vector("list", station.num)
n.y <- starting <- end <- station.name <- x <- y <- elev <- numeric()
#readline("[Press Enter to select the stations (*.RData) to include in the analysis] ")
library(tcltk)


for(i in 1:station.num){
  Filters <- matrix(c("R", ".RData", "All files", "*"), 1, 2, byrow = TRUE)
  #   load(if(interactive())
  # tk_choose.files(caption="Select the *.RData file to load", filter = Filters)); rm(Filters)
  data<-load(rutas[i+1])##para las otras
  #data<-load(rutas1[i])###para prec
  stat.list.full[[i]] <- stat.list[[i]] <- stndata$val
  period[[i]] <- stndata$yy; country=stndata$country
  country <- stndata$country; Var <- stndata$obs.name; units<-stndata$unit
  station.name[i] <- stndata$station; n.y[i] <- length(stat.list[[i]][,1])
  starting[i] <- stndata$start; end[i] <- starting[i]+(n.y[i]-1)
  x[i] <- stndata$lon; y[i] <- stndata$lat; elev[i] <- stndata$alt
  ###########################                  
  cat(paste(station.name[i], ":", i, "of", station.num, "stations loaded\n"))
  if(i==station.num){cat('\n\r')}
  flush.console()
  ###########################
  
}
print(cbind(station.name, starting, end))

####promedios de estacion
listas<-vector("list",station.num)
for(i in 1:station.num){
  media.mensual<- apply(stat.list.full[[i]], 2, mean)
  listas[[i]]<-media.mensual
}

tmax<-do.call(rbind, listas)

####latitudes
l=info$lat
latitudes<-cbind(l)

####longitudes
l<-info$long
longitudes<-cbind(l)

####altitud
l<-info$Elevation
altitud<-cbind(l)


##########country
country=rep("Colombia",9)

###names
l<-paste0("",info$Name,"")
names<-cbind(l)


##########id
ID<-seq(1,9,1)


l<-info$codigo
OLD_ID<-cbind(l)


##########source
SOURCE=rep("IDEAM",9)

##nyear
nyear=rep(35,9)

nombres<-c("ID","SOURCE","OLD_ID","NAME","COUNTRY","LAT","LON","ALT","Jan", "Feb", "Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec","NYEAR")
base<-data.frame(ID,SOURCE,OLD_ID,names,country,latitudes,longitudes,altitud,tmax,nyear,row.names = NULL)
names(base)<-nombres

write.csv(base,"table_tmin.csv",row.names=F)

##############################################################################################prec
info<-read.csv("C:/Users/jardila/Desktop/estaciones Rdata/info_var_prec/info_var_prec.csv")##carpeta donde esta contenido el archivo de informacion de variable
infomes<-read.csv("C:/Users/jardila/Desktop/grar_mpas/datos_completos/month_prec_imput.csv")
ruta<-"C:/Users/jardila/Desktop/estaciones Rdata/Rdata_prec"################carpeta donde estan contenidos los archios .Rdata
txt=list.files(ruta)
rutas=paste(ruta,"/",txt,sep="")
##rutas1=rutas[29:37]##para prec
station.num=46



cat('\n', '\n', "       *** STATISTICAL/EXPLORATORY ANALYSIS Script ***", '\n', '\n')
# Retrieving and Loading climate information from 'station' Objects
#station.num <- as.numeric(readline("[Enter Number of Stations to analize] "))
while(is.na(station.num)==TRUE) {station.num <- ask.station.num() }
stat.list.full <- stat.list <- vector("list", station.num)
period <- vector("list", station.num)
n.y <- starting <- end <- station.name <- x <- y <- elev <- numeric()
#readline("[Press Enter to select the stations (*.RData) to include in the analysis] ")
library(tcltk)


for(i in 1:station.num){
  Filters <- matrix(c("R", ".RData", "All files", "*"), 1, 2, byrow = TRUE)
  #   load(if(interactive())
  # tk_choose.files(caption="Select the *.RData file to load", filter = Filters)); rm(Filters)
  data<-load(rutas[i+1])##para las otras
  #data<-load(rutas1[i])###para prec
  stat.list.full[[i]] <- stat.list[[i]] <- stndata$val
  period[[i]] <- stndata$yy; country=stndata$country
  country <- stndata$country; Var <- stndata$obs.name; units<-stndata$unit
  station.name[i] <- stndata$station; n.y[i] <- length(stat.list[[i]][,1])
  starting[i] <- stndata$start; end[i] <- starting[i]+(n.y[i]-1)
  x[i] <- stndata$lon; y[i] <- stndata$lat; elev[i] <- stndata$alt
  ###########################                  
  cat(paste(station.name[i], ":", i, "of", station.num, "stations loaded\n"))
  if(i==station.num){cat('\n\r')}
  flush.console()
  ###########################
  
}
print(cbind(station.name, starting, end))

####promedios de estacion
listas<-vector("list",station.num)
for(i in 1:station.num){
  media.mensual<- apply(stat.list.full[[i]], 2,mean)
  listas[[i]]<-media.mensual
}

prec<-do.call(rbind, listas)

####latitudes
l=info$lat
latitudes<-cbind(l)

####longitudes
l<-info$long
longitudes<-cbind(l)

####altitud
l<-info$Elevation
altitud<-cbind(l)


##########country
country=rep("Colombia",46)

###names
l<-paste0("",info$Name,"")
names<-cbind(l)


##########id
ID<-seq(1,46,1)


l<-info$codigo
OLD_ID<-cbind(l)


##########source
SOURCE=rep("IDEAM",46)

##nyear
nyear=rep(35,46)

nombres<-c("ID","SOURCE","OLD_ID","NAME","COUNTRY","LAT","LON","ALT","Jan", "Feb", "Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec","NYEAR")
base<-data.frame(ID,SOURCE,OLD_ID,names,country,latitudes,longitudes,altitud,prec,nyear,row.names = NULL)
names(base)<-nombres

write.csv(base,"table_prec.csv",row.names=F)
