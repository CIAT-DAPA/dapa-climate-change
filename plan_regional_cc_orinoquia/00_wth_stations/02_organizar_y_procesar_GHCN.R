


dir<-list.files("D:/col-cormacarena/interpolation_america/datos_estaciones_raw/rain-per-year",full.names=T)
dir=dir[82:110]
datos<-lapply(dir,read.csv)



estation_select<-which(datos[[1]][,4]<24 & datos[[1]][,4]>-40 & datos[[1]][,5]< -30 & datos[[1]][,5]>-120)

##########crea las bases de datos con las estaciones seleccionadas
datos_listfull<-vector("list",length(datos))
for(i in 1:length(datos)){
datos_select<-datos[[i]][estation_select,]
datos_listfull[[i]]<-datos_select
}

##########crea bases de datos de una estacion por lista
list_stat<-vector("list",length(estation_select))

for(j in 1:length(estation_select)){
  listas<-vector("list",length(datos))
for(i in 1:length(datos)){
list<-datos_listfull[[i]][j,7:18]
listas[[i]]<-list
}
list_stat[[j]]<-do.call(rbind, listas)
}

####matrix que contiene todas las estaciones en columnas
stat<-matrix("NA",348,length(list_stat))
for(k in 1:length(list_stat)){
a<-cbind(c(do.call(rbind,list_stat[[k]][,1:length(list_stat[[1]][1,])])))
stat[,k]<-a
}

####base de datos final
mth=rep(seq(1:12),29)
year=rep(seq(1:12),29)

stations<-data.frame(cbind(mth),cbind(year),stat)
names(stations)<-c("Month","Year",array(datos_listfull[[1]][,1]))

write.csv(stations,"stations_ghcn_rain.csv",row.names=F)

#########################################seleccionar estaciones por porcentaje de NA (por lo menos 30% de datos completos)

data<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/porcentajes llenos.csv")


pos<-which(data[349,]>.30)
datos<-data[,pos]
write.csv(datos,"estations_select.csv",row.names=F)

######################################acomodar datos para crear tabla de interpolacion

data1<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/estations_select.csv")
data<-data1[,3:length(names(data1))]

ene=seq(1,348,12)
feb=seq(2,348,12)
mar=seq(3,348,12)
abr=seq(4,348,12)
may=seq(5,348,12)
jun=seq(6,348,12)
jul=seq(7,348,12)
ago=seq(8,348,12)
sep=seq(9,348,12)
oct=seq(10,348,12)
nov=seq(11,348,12)
dic=seq(12,348,12)

#meses<-c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
promedios<-matrix("NA",length(names(data)),12)


a<-cbind(apply(data[ene,],2,function(x) mean(x,na.rm=T)))
b<-cbind(apply(data[feb,],2,function(x) mean(x,na.rm=T)))
c<-cbind(apply(data[mar,],2,function(x) mean(x,na.rm=T)))
d<-cbind(apply(data[abr,],2,function(x) mean(x,na.rm=T)))
e<-cbind(apply(data[may,],2,function(x) mean(x,na.rm=T)))
f<-cbind(apply(data[jun,],2,function(x) mean(x,na.rm=T)))
g<-cbind(apply(data[jul,],2,function(x) mean(x,na.rm=T)))
h<-cbind(apply(data[ago,],2,function(x) mean(x,na.rm=T)))
i<-cbind(apply(data[sep,],2,function(x) mean(x,na.rm=T)))
j<-cbind(apply(data[oct,],2,function(x) mean(x,na.rm=T)))
k<-cbind(apply(data[nov,],2,function(x) mean(x,na.rm=T)))
l<-cbind(apply(data[dic,],2,function(x) mean(x,na.rm=T)))



nombres<-c("Jan", "Feb", "Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec")
promedios<-data.frame(a,b,c,d,e,f,g,h,i,j,k,l)
names(promedios)<-nombres

write.csv(promedios,"table_rain.csv",row.names=F)


