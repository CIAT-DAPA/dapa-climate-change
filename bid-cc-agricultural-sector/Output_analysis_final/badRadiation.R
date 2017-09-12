#Check radiation values by model

#get list of models
path = ('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/')
models = list.files()
ind.bad = grep('txt',models)
models = models[-ind.bad]

for (j in 1:length(models))  {
  load(paste(path,models[j],'/2020_2049/bc_prec_2020_2049_daily.Rdat',sep=''))
  print(paste(models[j],quantile(dataMatrixFut[,4:10961],na.rm=T)))
  #print(sum(as.matrix(dataMatrixFut[,4:10961])==0,na.rm=T))
  print(sum(as.matrix(dataMatrixFut[,4:10961])<0,na.rm=T))
  print(sum(is.na(as.matrix(dataMatrixFut[,4:10961]))))
}

j=1
test = raster(paste(path,models[j],'/2020_2049/by_month/prec_2020_01.nc',sep=''))

#Look at WFD & tmin/tmax
path = ('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/wfd/')
load(paste(path,'Prec.Rdat',sep=''))

#Look at tmin/ tmax
library('snowfall')
modelo = models[j]
Tiempo = '2020_2049'
load("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/coordenadas.RDat")

lecturabandas=function(data){
  setwd(paste("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/",modelo,"/2020_2049","/by_month",sep=""))
  
  data=paste(data)
  lectura=raster(paste(data),band=T)
  dias=sapply(1:lectura@ file@nbands, function(i) raster(paste(data),band=i) )
  return(stack(dias))
  
}

sfExport("lecturabandas")

Meses=c(paste("_0",sep="",1:9,".nc"),paste("_",sep="",10:12,".nc"))

Años_TemMax<-paste("tmax_20",sep="",21:49)
Años_TemMin<-paste("tmin_20",sep="",21:49)

SerieAnual_Tmax<-lapply(1:29,function(i) paste(Años_TemMax[i],Meses,sep=""))
SerieAnual_Tmin<-lapply(1:29,function(i) paste(Años_TemMin[i],Meses,sep=""))

sfExport("SerieAnual_Tmax","SerieAnual_Tmin")

cat("Cargando Archivos nc",modelo,"2021_2049","\n",sep="  ")
Raster_TempMax=sfLapply(1:29,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmax[[j]][i])))
cat("Cargando Archivos nc",modelo,"2021_2049","\n",sep="  ")
Raster_TempMin=sfLapply(1:29,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmin[[j]][i])))

sfExport("Raster_TempMax","Raster_TempMin")
sfExport("Coordenadas")

Raster_TempMax <- lapply(Raster_TempMax,FUN=stack)
Raster_TempMin <- lapply(Raster_TempMin,FUN=stack)

cat("Extrayendo Valores de Temperatura Maxima",modelo,"2021_2049","\n",sep="  ")
TempMax <- sfLapply(Raster_TempMax, FUN=extract,Coordenadas)
cat("Extrayendo Valores de Temperatura Minima",modelo,"2021_2049","\n",sep="  ")
TempMin <- sfLapply(Raster_TempMin, FUN=extract,Coordenadas)
