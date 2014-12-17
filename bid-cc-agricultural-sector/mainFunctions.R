

## Lecturas de los dias (bandas)
 
 lecturabandas=function(data){
  data=paste(data)
  lectura=raster(paste(data),band=T)
  dias=sfSapply(1:lectura@ file@nbands, function(i) raster(paste(data),band=i) )
  return(stack(dias))
  
}




## para extraer las fechas de las grillas se construye la siguiente funcion (extrae las fechas en dias julianos)

extraerfechas=function(data,año){
  tamaño=sapply(1:12,function(i)dim(data[[año]][[i]])[3])
  fecha=unlist(lapply(1:length(tamaño),function(j) sapply(1:tamaño[j],function(i) paste(data[[año]][[j]][[i]]@z))))
  origen<-paste(fecha[1])
  tmp <- as.POSIXlt(paste(fecha), format = "%Y%m%d")
  dias<-julian(as.Date(tmp),origin = as.Date(origen,"%Y%m%d"))+1
  tenvalues<-which(dias<10)
  pos_1=which(dias<10)
  c0=str_sub(fecha,3,4)
  c0=c0[1]
  c1=sapply(1:length(pos_1),function(i) paste(c0,"00",sep="",pos_1[i]))
  pos_2=which(dias>=10 & dias<100)
  c2=sapply(1:length(pos_2),function(i) paste(c0,"0",sep="",pos_2[i]))
  pos_3=which(dias>=100)
  c3=sapply(1:length(pos_3),function(i) paste(c0,"",sep="",pos_3[i]))
  fecha<-c(c1,c2,c3)
  fecha<-substr(fecha,1,5)   
  return(fecha)
  
}

## Extraer Valores de un Pixel por Año

# Extraervalores_grilla=function(data,año,grilla){
 # tamdias=sfSapply(1:12,function(i) dim(data[[año]][[i]])[3])
 # return(unlist(sfSapply(1:length(tamdias),function(j) sfSapply(1:tamdias[j],function(i) as.vector(rasterToPoints(data[[año]][[j]][[i]])[grilla,3])) )))
  
#}

#sfLapply(1:2,function(i)  Extraervalores_grilla(Raster_precipitacion,i,coordenadas[1,]) )


Extraervalores_grilla=function(data,año,grilla){
  tamdias=sfSapply(1:12,function(i) dim(data[[año]][[i]])[3])
  value<-sfSapply(1:length(tamdias),function(j) sfSapply(1:tamdias[j],function(i) as.vector(extract(data[[año]][[j]][[i]],grilla) )))
  return(unlist(value))
  
  
}
# save(soil_coord,file="/mnt/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/soil_coord.Rdat")


Extraervalores_grilla=function(data,año,grilla){
  tamdias=sapply(1:12,function(i) dim(data[[año]][[i]])[3])
  value<-sapply(1:length(tamdias),function(j) sapply(1:tamdias[j],function(i) as.vector(extract(data[[año]][[j]][[i]],grilla) )))
  return(unlist(value))
  
  
}

## Crea Directorios dada una ruta

Create_dir<-function(path){
  
  dir.create(path,showWarnings=F,recursive=T)
  
  
}



## Creacion de Archivos de Clima

WriteWTH<-function(año,Srad,Tmax,Tmin,Prec){
yrs<-año
yrs2<-(yrs*100):((yrs*(100))+365)
yrs<-(yrs*1000):((yrs*(1000))+365)

yrs<-yrs[-1]
yrs2<-yrs2[-1]
Prec<-pmax(unlist(Prec),0)
Srad<-(pmax(unlist(Srad),0))/11.5740741 
Tmax<-unlist(Tmax)
Tmin<-unlist(Tmin)
sink(paste("JBID",yrs2[1],".WTH",sep=""))
##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
cat(paste("*WEATHER DATA :"),paste("BID"))
cat("\n")
cat("\n")
cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
cat("\n")
cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.2f %5.2f %5.2f","JBID", 3.482, -76.350, -99,22.7, 0.4, 0, 0))
cat("\n")
cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
cat("\n")
cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f",yrs,Srad,Tmax,Tmin,Prec)),sep="\n")
sink()
}




########################### Funciones para tipos de Suelo ##############################@@@@


TiposSoilinWise<-function(data,path){
  k=0
  j=0
  for(i in 1:length(data)){
  test <- try(strsplit(data[i],split=path),silent=TRUE)
  if(length(test[[1]])==1 && test[[1]][1]!="*"){
    j[i]=i
  }
  else(k[i]=i)
    
  }
  return(k)
}



## Funcion para tener solo el Codigo de tipo de suelo de Wise
SustraerTipoSuelo<-function(data){
  typeWise<-substr(strsplit(data," ")[[1]][1],2,nchar(strsplit(data," ")[[1]][1]))
return(typeWise)
}



## Funcion para escribir el archivo de suelo  (data=Archivo Wise, Cell5m=identificador del archivo soilprofile)
read_oneSoilFile<-function(data,path){
 i=1
  test <- try(strsplit(data[i],split=path),silent=TRUE)
  soils <- NULL;

while(length(test[[1]])==1){
 soils <- rbind(soils,data[i]);
  i=i+1
  test <- try(strsplit(data[i],split=path),silent=TRUE)
  
   }
return(soils);
}




Extraer.SoilDSSAT<-function(valores,pos){


## which(Soil_profile[,"CELL5M"]==valores[1])
posicion<-which(Soil_profile[,"CELL5M"]==valores[length(valores)])    ## Posicion coincidencia valores id suelo con el identificador WISE
if(length(posicion)==0){
  valores1<-valores+1  
}
else{

celdas_id_Wise<-Soil_profile[posicion,]                                           ## Celdas y porcentaje del archivo de suelo DSSAT  
Posicion_Pct<-which(celdas_id_Wise[,"SharePct"]==max(celdas_id_Wise[,"SharePct"]))   ## Se eligue la celda con mayor porcentaje
Ref_for_Soil<-celdas_id_Wise[Posicion_Pct,2][1]

condicion<-which(Cod_Ref_and_Position_Generic==paste(Ref_for_Soil))
if(length(condicion)>=1){
  Wise_Position<-Cod_Ref_and_Position_Generic[which(Cod_Ref_and_Position_Generic[,1]==paste(Ref_for_Soil)),]
  Soil_file<-read_oneSoilFile(Soil_Generic[Wise_Position[,"Position_CodigoSueloGeneric"]:length(wise)],path)
  return(write(Soil_file,file=paste("celda",pos,".Sol",sep="")))
}

else{
Wise_Position<-Cod_Ref_and_Position[which(Cod_Ref_and_Position[,1]==paste(Ref_for_Soil)),]

Soil_file<-read_oneSoilFile(wise[Wise_Position[,"Position_CodigoSueloWise"]:length(wise)],path)
return(write(Soil_file,file=paste("celda",pos,".Sol",sep="")))
}
}
}




