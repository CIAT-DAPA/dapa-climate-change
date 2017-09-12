
Extraer_Srad_or_Prec <- function(Tiempo,modelo,Variable,cantidad){

cat(modelo,sep="\n")
  
  
  if(Tiempo=="Presente"){
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/",modelo,"/1971_2000",sep=""))
    años <- c(1971:2000)
    if(Variable=="Precipitacion"){
      cat("Cargando Precipitacion....... 1971-2000","\n",sep="  ")
      load("bc_prec_1950_2000_daily.Rdat")  
      
    }
    if(Variable=="Radiacion"){
      cat("Cargando Radiacion........ 1971-2000","\n",sep="  ")
      load("bc_rsds_1950_2000_daily.Rdat") 
    }
    
    
    
  }
  
  if(Tiempo=="Futuro"){
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/",modelo,"/2020_2049",sep=""))
    años <- c(2021:2049)
    if(Variable=="Precipitacion"){
      cat("Cargando Precipitacion....... 2021:2049","\n",sep="  ")
      load("bc_rsds_2020_2049_daily.Rdat")  ## Objeto que se desear extraer (precipitacion,radiacion) cambiar a ser necesario
    }
    if(Variable=="Radiacion"){
      cat("Cargando Radiacion........ 2021:2049","\n",sep="  ")
      load("bc_prec_2020_2049_daily.Rdat")  ## Objeto que se desear extraer (precipitacion,radiacion) cambiar a ser necesario
    }
    
  }
  
  
  cat("Cargando Funciones Necesarias","\n",sep="  ")
  
  
  añosToRun <- function(inicial,final){
    
    y<-seq(as.Date(paste(inicial,"-01-01",sep="")),
           as.Date(paste(final,"-12-31",sep="")),by=1)
    return(y)
    
  }
  
  años_mod <- lapply(1:length(años),function(i) añosToRun(años[i],años[i]))
  
  
  ExtractMatrix <- function(data,fecha){
    
    fecha <- paste(fecha)
    valores <- data[,fecha]
    return(valores)
    
    
  }

  cat("Cargando el Cluster","\n")
  sfExportAll()
  if(Tiempo=="Presente"){
    
    if(Variable=="Precipitacion"){
      cat("Extrayendo Precipitacion",modelo,"1971_2000","\n",sep="  ")
      Prec <- sfLapply(1:cantidad,function(i) sfLapply(1:30,function(j) ExtractMatrix(dataMatrix[i,],años_mod[[j]])))
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Presente",sep=""))
      save(Prec,file="Precipitacion.RDat")
      
    }
    if(Variable=="Radiacion"){
      cat("Extrayendo Radiacion",modelo,"1971_20000","\n",sep="  ")
      Srad <- sfLapply(1:cantidad,function(i) sfLapply(1:30,function(j) ExtractMatrix(dataMatrix[i,],años_mod[[j]])))
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Presente",sep=""))
      save(Srad,file="Radiacion.RDat")
    }
    
    
    
  }
  
  if(Tiempo=="Futuro"){
    
    if(Variable=="Precipitacion"){
      cat("Extrayendo Precipitacion",modelo,"2021_2049","\n",sep="  ")
      Prec <- sfLapply(1:cantidad,function(i) sfLapply(1:29,function(j) ExtractMatrix(dataMatrixFut[i,],años_mod[[j]])))
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Futuro",sep=""))
      save(Prec,file="Precipitacion.RDat")
      
    }
    
    if(Variable=="Radiacion"){
      cat("Extrayendo Radiacion",modelo,"2021_2049","\n",sep="  ")
      Srad <- sfLapply(1:cantidad,function(i) sfLapply(1:29,function(j) ExtractMatrix(dataMatrixFut[i,],años_mod[[j]])))
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Futuro",sep=""))
      save(Srad,file="Radiacion.RDat")
      
    }
    
    
    
  }
  cat("Proceso Terminado",modelo,"\n",sep="  ")
rm(list=ls())  
  
}





Extraer_Temperatura <- function(modelo,Tiempo){
  

  
  if(Tiempo=="Presente"){
    
    
    
    
    lecturabandas=function(data){
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/",modelo,"/1971_2000","/by_month",sep=""))
      
      data=paste(data)
      lectura=raster(paste(data),band=T)
      dias=sapply(1:lectura@ file@nbands, function(i) raster(paste(data),band=i) )
      return(stack(dias))
      
    }
    
    sfExport("lecturabandas")
    
    Meses <- c(paste("_0",sep="",1:9,".nc"),paste("_",sep="",10:12,".nc"))
    Años_TemMax <- c(paste("tmax_19",sep="",71:99),paste("tmax_2000"))
    Años_TemMin <- c(paste("tmin_19",sep="",71:99),paste("tmin_2000"))
    
    
    SerieAnual_Tmax <- lapply(1:30,function(i) paste(Años_TemMax[i],Meses,sep=""))
    SerieAnual_Tmin <- lapply(1:30,function(i) paste(Años_TemMin[i],Meses,sep=""))
    
    sfExport("SerieAnual_Tmax","SerieAnual_Tmin")
    cat("Cargando Temperatura maxima y minima",modelo,"\n",sep="  ")
    Raster_TempMax <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmax[[j]][i])))
    Raster_TempMin <- sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmin[[j]][i])))
    
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Presente",sep="")) 
    cat("Guardando informacion","\n",sep="  ")
    save(Raster_TempMax,Raster_TempMin,file="Temp.Rdat")
    
    rm(list=ls())
    
    cat("Load Informacion","\n",sep="  ")
    load("Temp.Rdat") 
    load("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/coordenadas.RDat")
    sfExport("Raster_TempMax","Raster_TempMin")
    sfExport("Coordenadas")
    
    Raster_TempMax <- lapply(Raster_TempMax,FUN=stack)
    Raster_TempMin <- lapply(Raster_TempMin,FUN=stack)
    
    cat("Extrayendo Temperatura Maxima",modelo,"1971_2000","\n",sep="  ")
    TempMax <- sfLapply(Raster_TempMax, FUN=extract,Coordenadas)
    cat("Extrayendo Temperatura Minima",modelo,"1971_2000","\n",sep="  ")
    TempMin <- sfLapply(Raster_TempMin, FUN=extract,Coordenadas)
    
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Presente",sep="")) ## Para guardar los datos
    save(TempMax,TempMin,file=paste(modelo,".RDat",sep=""))
    
  }
  
  if(Tiempo=="Futuro"){
    
    
    
    lecturabandas=function(data){
      setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/",modelo,"/2020_2049","/by_month",sep=""))
      
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
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Futuro",sep="")) ## Para guardar los datos
    
    save(Raster_TempMax,Raster_TempMin,file="Temp.Rdat")
    
    
    rm(Raster_TempMax,Raster_TempMin,SerieAnual_Tmax,SerieAnual_Tmin)
    
    cat("Load Informacion Temperatura",modelo,"2021_2049","\n",sep="  ")
    load("Temp.Rdat") 
    load("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/coordenadas.RDat")
    
   # sfSource("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/_scripts/mainFunctions.R")
    sfExport("Raster_TempMax","Raster_TempMin")
    sfExport("Coordenadas")
    
    Raster_TempMax <- lapply(Raster_TempMax,FUN=stack)
    Raster_TempMin <- lapply(Raster_TempMin,FUN=stack)
    
    cat("Extrayendo Valores de Temperatura Maxima",modelo,"2021_2049","\n",sep="  ")
    TempMax <- sfLapply(Raster_TempMax, FUN=extract,Coordenadas)
    cat("Extrayendo Valores de Temperatura Minima",modelo,"2021_2049","\n",sep="  ")
    TempMin <- sfLapply(Raster_TempMin, FUN=extract,Coordenadas)
    
    setwd(paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/14-ObjectsR/",modelo,"/Futuro",sep="")) ## Para guardar los datos
    
    save(TempMax,TempMin,file=paste(modelo,".RDat",sep=""))
    
    
    
  }
  cat("Proceso Finalizado",modelo,"\n",sep="  ")
  rm(list=ls())
  
}


