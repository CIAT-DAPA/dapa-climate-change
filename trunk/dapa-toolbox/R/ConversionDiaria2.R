#############Conversion a escala diaria de información horaria.

library(chron)
library(stringr)

###Lectura de los archivos
ruta="//dapadfs/workspace_cluster_3/Aeps/2013/MADR/Clima/SERIES_ORIGINALES/Llanos/Horarias_QC1_OK/"
Destino="//dapadfs/workspace_cluster_3/Aeps/2013/MADR/Clima/SERIES_ORIGINALES/Llanos/Diarias_para_QC/"

files <- list.files(ruta, pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
Data.all.files <- lapply(paste(ruta,"/",files,sep="",dec="."),function(x){read.table(x,header=T,sep="\t")})
names(Data.all.files)=nom.files

Data.all.filesNAFree=Data.all.files
Data.all.files.OK=Data.all.files
Serie.diaria=Data.all.files

for(i in 1:length(Data.all.files)){
  
  # Crear series de trabajo
Data.all.filesNAFree[[i]]=Data.all.files[[i]][which(!is.na(Data.all.files[[i]]$Value)),]
Data.all.files.OK[[i]]=Data.all.filesNAFree[[i]]
  
  # Lectura de fechas y horas
Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y-%m-%d")
Data.all.files.OK[[i]]$Hour=times(Data.all.filesNAFree[[i]]$Hour)

  # Lectura de la variable
VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))
VAR=toupper(VAR)

  # Calculo de las medianas de intervalo de tiempo
medianas=aggregate(Data.all.files.OK[[i]]$DIFF~Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]],median)
colnames(medianas)=c("Date", "Median")

if(VAR=="TMAX"){
  # Definir fechas usables  
  Hmin=times(strftime(strptime("5:58 AM","%I:%M %p"),"%H:%M:00"))
  Hmax=times(strftime(strptime("6:02 PM","%I:%M %p"),"%H:%M:00"))
  minutos.dia=720
  Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour>Hmin & Data.all.files.OK[[i]]$Hour<Hmax,]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, max)
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="TMIN"){
  # Definir fechas usables  
  Hmin=times(strftime(strptime("6:02 AM","%I:%M %p"),"%H:%M:00"))
  Hmax=times(strftime(strptime("5:58 PM","%I:%M %p"),"%H:%M:00"))
  minutos.dia=720
  Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour<Hmin | Data.all.files.OK[[i]]$Hour>Hmax,]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, min)
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="RHUM"){
  # Definir fechas usables  
  minutos.dia=1440
  Data.in.time=Data.all.files.OK[[i]]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, mean)
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="RAIN"){
  # Definir fechas usables  
  minutos.dia=1440
  Data.in.time=Data.all.files.OK[[i]]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, sum)
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="ESOL"){
  Hmin=times(strftime(strptime("4:28 AM","%I:%M %p"),"%H:%M:00"))
  Hmax=times(strftime(strptime("9:32 PM","%I:%M %p"),"%H:%M:00"))
  minutos.dia=1020
  Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour>Hmin & Data.all.files.OK[[i]]$Hour<Hmax,]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  
  UNIT=substring(nom.files[i],nchar(nom.files[i])-8, nchar(nom.files[i])-5)
  UNIT=toupper(UNIT)
  if(UNIT=="WAM2"){
    SerieOPccm2=SerieOP$Value*SerieOP$DIFF*60/4.18/10000
    SerieOP$Value=SerieOPccm2
  }
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, sum)
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}
  # Generacion del reporte
  resul=c(registros.totales, NAentrada, NAentrada/registros.totales*100, registros.totales.dias, registros.rescatados, registros.rescatados/registros.totales.dias*100)
  Tabla.fin=as.data.frame(resul,row.names=c("Numero de datos totales", "Numero de NA en entrada", "% NA en entrada", "Dias con datos entrada", "Dias rescatables", "% dias rescatables"))
  write.csv(Tabla.fin,paste0(Destino,nom.files[i], "_ResumenTransfDia.csv"))
  write.table(Serie.diaria[[i]],paste0(Destino,nom.files[i], "_DiarioReady.txt"), sep="\t")

}