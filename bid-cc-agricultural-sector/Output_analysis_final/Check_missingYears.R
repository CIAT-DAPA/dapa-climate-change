rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#escoger parametro de interes
params = c('HWAH','MDAT','PDAT','NDCH')  #,'PDAT','EPCM')  #duración, rendimiento y otro variables

#fijar cultivo aquí
#c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
c=3  #soy
#c=4  #frijol
#c=5 #trigo
# material = 'M-6101'

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Soya-3cultivars_withCO2'
carp.res.secano = 'Soya-3cultivars_withCO2'

#fijar variedad
#variedades = c('INRA','H6','FM6') #maiz
#variedades = c('LOW.TEMP','IR72','IR8')  #arroz
variedades = c('MSMATGROUP','DonMario','Hutcheson')  #soya
#variedades = c('ICTAOstua','Carioca','A193','Manitou')  #frijol
#variedades = c('ChinaBBA','KauzBA','AVALON')  #trigo

#otros datos fijos
regions= c('MEX','CEN','AND','BRA','SUR')
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
anos = 1:29  #fijar años para analizar aquí (leave extra year at end)
anos2 = 1971:1999

#get list of climate models
models = read.table(paste(path.root,'ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)  #_documentos/
models = rbind(models,'Historical baseline','Future multi-GCM average')
p=dim(models)[1]
colnames(models) = 'Models'  #hack for now (machetazo)

#Load pixel id's
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#initialize data matrices
dat_secano = array(NA,dim=c(dim(crop_secano)[1],30,10,length(variedades)))  #initialize arrays
dat_riego = array(NA,dim=c(dim(crop_riego)[1],30,10,length(variedades)))

#loop through varieties & models
for (v in 1:length(variedades))  {
  for (m in c(1:10)) {

    #load, unlist and extract yield data to arrays (gridcells x years x params)
    if (m<=9)  {
      load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
      Run.secano = Run
      load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
      Run.riego = Run
      yr.start = 2020
    }  else{
      load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_WFD.Rdat',sep=''))
      Run.secano = Run
      load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_WFD.Rdat',sep=''))
      Run.riego = Run
      yr.start = 1970
    }
    
    #unlist everything into matrices
    for (j in 1:length(Run.secano))  {
      if (is.null(dim(Run.secano[[j]])))  {
        dat_secano[j,,] = NA
      }  else{
        ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-yr.start  #index based on simulation start date
        if (ind.s[1]==0)  {ind.s = ind.s +1}
        dat_secano[j,ind.s,m,v] = as.numeric(as.character(Run.secano[[j]][,params[1]]))
        dat_secano[j,1:28,m,v] = as.numeric(as.character(Run.secano[[j]][,params[1]]))
      }
    }
    
    for (j in 1:length(Run.riego))  {
      if (is.null(dim(Run.riego[[j]])))  {
        dat_riego[j,,] = NA
      }  else{
        ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-yr.start
        if (ind.r[1]==0)  {ind.r = ind.r +1}
        dat_riego[j,ind.r,m,v] = as.numeric(as.character(Run.riego[[j]][,params[1]]))
      }
    }
    
    print(paste(models[m,],variedades[v],apply(dat_secano[,,m,v],2,function(x) sum(is.na(x)==F))))
    print(paste(models[m,],variedades[v],apply(dat_riego[,,m,v],2,function(x) sum(is.na(x)==F))))  #print values per year
  }
}

