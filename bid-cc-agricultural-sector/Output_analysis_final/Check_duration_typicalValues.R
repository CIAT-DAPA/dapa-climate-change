#Look at typical durations for WFD

rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#escoger parametro de interes
params = c('HWAH','NDCH')  #,'PDAT','EPCM')  #duración, rendimiento y otro variables

#fijar cultivo aquí
#c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
#c=3  #soy
#c=4  #frijol
c=5 #trigo
# material = 'M-6101'

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Trigo-3cultivars_withCO2'
carp.res.secano = 'Trigo-3cultivars_withCO2'

#fijar variedad
#variedades = c('INRA','H6','FM6') #maiz
#variedades = c('LOW.TEMP','IR72','IR8')  #arroz
#variedades = c('MSMATGROUP','DonMario','Hutcheson')  #soya
#variedades = c('ICTAOstua','Carioca','A193','Manitou')  #frijol
variedades = c('ChinaBBA','AC_Barrie','Avalon')

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
dat_secano = array(NA,dim=c(dim(crop_secano)[1],30,length(params),length(variedades)))  #initialize arrays
dat_riego = array(NA,dim=c(dim(crop_riego)[1],30,length(params),length(variedades)))

#loop through varieties
for (v in 1:length(variedades))  {
  
  #load, unlist and extract WFD yield data to arrays (gridcells x years x params)
  load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_WFD.Rdat',sep=''))
  Run.secano = Run
  load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_WFD.Rdat',sep=''))
  Run.riego = Run
  
  #unlist everything into matrices
  for (j in 1:length(Run.secano))  {
    if (is.null(dim(Run.secano[[j]])))  {
      dat_secano[j,,,] = NA
    }  else{
      for (p2 in 1:length(params)) {
        #ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-1970  #index based on simulation start date
        dat_secano[j,1:dim(Run.secano[[j]])[1],p2,v] = as.numeric(as.character(Run.secano[[j]][,params[p2]]))
      }
    }
  }
  
  for (j in 1:length(Run.riego))  {
    if (is.null(dim(Run.riego[[j]])))  {
      dat_riego[j,,,] = NA
    }  else{
      for (p2 in 1:length(params)) {
        #ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-1970
        dat_riego[j,1:dim(Run.secano[[j]])[1],p2,v] = as.numeric(as.character(Run.riego[[j]][,params[p2]]))
      }
    }
  }
}

#check data by year
apply(dat_riego[,,1,3],2,function(x) sum(is.na(x)==F))  #yield, variety 3
apply(dat_secano[,,1,3],2,function(x) sum(is.na(x)==F))

#Find crop failures & then put NA for duration
#separate yield & duration first
yld_secano = dat_secano[,anos,1,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = dat_riego[,anos,1,]
dur_secano = dat_secano[,anos,2,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
dur_riego = dat_riego[,anos,2,]

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego,c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano,c(1,3),mean,na.rm=T)
thresh = mean(yld_secano,na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
dur_secano[is.na(yld_secano)|yld_secano<thresh] = NA  #exclude bad yields from durations
dur_riego[is.na(yld_riego)|yld_riego<thresh] = NA
wfd.fail.r = apply(yld_riego,c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
wfd.fail.s = apply(yld_secano,c(1,3),function(x) sum(x<thresh,na.rm=T))

#check durations by variety
for (v in 1:length(variedades))  {
  print(variedades[v])
  print(quantile(c(c(dur_secano[,,v]),c(dur_riego[,,v])),na.rm=T))
}

#highest-yielding variety
wfd.r.high = apply(wfd.r,1,which.max)
wfd.s.high = apply(wfd.s,1,which.max)

#high-yielding and no more than 4 crop failures (15%)
wfd.s.best = mat.or.vec(dim(wfd.s)[1],1)
for (j in 1:dim(wfd.s)[1])  {
  sortYield = sort.list(wfd.s[j,],dec=T) 
  if (wfd.fail.s[j,sortYield[1]]<=4) {wfd.s.best[j] = sortYield[1]}  else{
    if (wfd.fail.s[j,sortYield[2]]<=4) {wfd.s.best[j] = sortYield[2]}  else{
      if (wfd.fail.s[j,sortYield[3]]<=4) {wfd.s.best[j] = sortYield[3]}  else{
        wfd.s.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
      }
    }
  }
}

wfd.r.best = mat.or.vec(dim(wfd.r)[1],1)
for (j in 1:dim(wfd.r)[1])  {
  sortYield = sort.list(wfd.r[j,],dec=T) 
  if (wfd.fail.r[j,sortYield[1]]<=4) {wfd.r.best[j] = sortYield[1]}  else{
    if (wfd.fail.r[j,sortYield[2]]<=4) {wfd.r.best[j] = sortYield[2]}  else{
      if (wfd.fail.r[j,sortYield[3]]<=4) {wfd.r.best[j] = sortYield[3]}  else{
        wfd.r.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
      }
    }
  }
}

#Select yield (& durations) for best variety for every pixel 
yld_secano.2 = yld_secano  #make backup with varieties
yld_riego.2 = yld_riego
dur_secano.2 = dur_secano  #make backup with varieties
dur_riego.2 = dur_riego
yld_secano = array(NA,dim=c(dim(crop_secano)[1],length(anos)))  #pixels x years  
yld_riego = array(NA,dim=c(dim(crop_riego)[1],length(anos)))
dur_secano = array(NA,dim=c(dim(crop_secano)[1],length(anos)))  #pixels x years  
dur_riego = array(NA,dim=c(dim(crop_riego)[1],length(anos)))
for (k in 1:length(variedades))  {
  yld_secano[wfd.s.best==k,] = yld_secano.2[wfd.s.best==k,,k]
  yld_riego[wfd.r.best==k,] = yld_riego.2[wfd.r.best==k,,k]
  dur_secano[wfd.s.best==k,] = dur_secano.2[wfd.s.best==k,,k]
  dur_riego[wfd.r.best==k,] = dur_riego.2[wfd.r.best==k,,k]
}

#look at range of durations
quantile(dur_secano,na.rm=T)
quantile(dur_riego,na.rm=T)
