## Librerias
library(raster)
library (sp)
library (rgdal)
library (maps)
library (mapproj)
library(stringr)    ## libreria necesaria para las funciones de tipo caracter
library(date)       ## configuracion de fecha tipo dia juliano




######################################## Directorio para los Archivos Necesarios ####################################################

dirScripts<-"//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/_scripts"

ModelGcm<-"//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"

## Directorio Carpetas Corridas DSSAT
DirDSSAT<-"//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/DSSAT-R"

######################################### Funciones Necesarias para las corridas ####################################################
source(paste(dirScripts,"/","mainFunctions.R",sep=""))  

########################################     Listo todos los modelos (GCM's)     ####################################################

Gcms<-list.files(ModelGcm)
Gcms
length(Gcms)    ## Son 21 Modelos 
Cultivos<-c("Bean","Soy","Rice","Maize","Potato")   ## Cultivos a Simular 


lapply(paste(DirDSSAT,"/",Cultivos,sep=""),FUN=Create_dir)   ## Se crean lis directorios de trabajo para cada uno de los cultivos
sapply(1:length(Cultivos),function(i) lapply(paste(paste(DirDSSAT,"/",Cultivos[i],sep=""),"/",Gcms,sep=""),FUN=Create_dir))

