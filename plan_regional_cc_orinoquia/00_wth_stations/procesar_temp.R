



tmin1<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmin/tmin.csv")
tmin=tmin1[,4:15]

for(i in 1:12){
pos<-which(tmin[,i]==-9999)
tmin[pos,i]<-"NA"
}

for(i in 1:12){
  pos<-which(tmin[,i]>5000)
  tmin[pos,i]<-"NA"
}

for(i in 1:12){
  pos<-which(tmin[,i] < -4000)
  tmin[pos,i]<-"NA"
}

write.csv(tmin,"tmin1_proc.csv",row.names=F)

##########################
tmax1<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmax/tmax.csv")
tmax=tmax1[,4:15]

for(i in 1:12){
  pos<-which(tmax[,i]==-9999)
  tmax[pos,i]<-"NA"
}

for(i in 1:12){
  pos<-which(tmax[,i]>5000)
  tmax[pos,i]<-"NA"
}

for(i in 1:12){
  pos<-which(tmax[,i] < (-4000))
  tmax[pos,i]<-"NA"
}

write.csv(tmax,"tmax_proc.csv",row.names=F)


##################################promedios por estacion
info<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmax/coordenadas_stations_select.csv")
tmax<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmax/tmax_qc.csv")

promedios<-vector("list",length(info$ID))
for(j in 1:length(info$ID)){
station<-tmax[grep(info$ID[j],tmax[,1]),]
average<-apply(station[,4:15],2,function(x) mean(x,na.rm=T))
promedios[[j]]<-average
}

datos<-do.call(rbind,promedios)
datos_out<-data.frame(info$ID,round(datos/100,2))

write.csv(datos_out,"D:/col-cormacarena/interpolation_america/stations_ame/tmax/averages.csv",row.names=F)


##################################promedios por estacion
info<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmin/coordenadas_stations_select.csv")
tmin<-read.csv("D:/col-cormacarena/interpolation_america/stations_ame/tmin/tmin_qc.csv")

promedios<-vector("list",length(info$ID))
for(j in 1:length(info$ID)){
  station<-tmin[grep(info$ID[j],tmin[,1]),]
  average<-apply(station[,4:15],2,function(x) mean(x,na.rm=T))
  promedios[[j]]<-average
}

datos<-do.call(rbind,promedios)
datos_out<-data.frame(info$ID,round(datos/100,2))

write.csv(datos_out,"D:/col-cormacarena/interpolation_america/stations_ame/tmin/averages.csv",row.names=F)
###############################################









