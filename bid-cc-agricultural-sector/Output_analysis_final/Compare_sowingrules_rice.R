rm(list=ls())
#Aggregate yields to country level to calculate % changes in yield and area at risk of crop failure

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library('gplots')

#fijar cultivo aquí
#c=1  #maize
c=2  #rice
#c=3  #soy
#c=4  #frijol
#c=5 #trigo

#fijar variedad
#variedades = c('INRA','H6','FM6') #maiz
variedades = c('LOW.TEMP','IR72','IR8')  #arroz
#variedades = c('PIO9202','MSMATGROUP','DonMario')  #soya
#variedades = c('ICTAOstua','Carioca','A193','Manitou')  #frijol
#variedades = c('ChinaBBA','KauzBA','AVALON')  #wheat

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory D:/tobackup/BID/Resultados_DSSAT/
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Arroz-3cultivars_withCO2'
carp.res.secano = 'Arroz-3cultivars_withCO2'

#otros datos fijos
regions= c('MEX','CEN','AND','BRA','SUR')
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
anos = 1:28  #fijar años para analizar aquí
anos2 = 1971:1998

#Load pixel id's
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))  #08-Cells_toRun/
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#get list of climate models
models = read.table(paste(path.root,'ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)  #_documentos/
models = rbind(models,'Historical baseline','Future multi-GCM average')
p=dim(models)[1]
colnames(models) = 'Models'  #hack for now (machetazo)

#load, unlist and extract data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,(p-1),length(variedades)))  #10 GCM's + baseline
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,(p-1),length(variedades)))

for (v in 1:length(variedades))  {
  for (m in c(10))  {  #fija indices de modelos para incluir
    
    print(m)
    
    #Load & extract baseline data from list
    if (m<=9) {
      load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_IR72_',models[m,],'_.Rdat',sep=''))
    }  else {load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_IR72_WFD_ventana',v,'.Rdat',sep=''))}
    Run.secano = Run
    if (m<=9) {
      load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
    }  else {load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_WFD.Rdat',sep=''))}
    Run.riego = Run
    
    #unlist everything into matrices
    secano = array(NA,dim=c(length(Run.secano),30))  #initialize arrays
    for (j in 1:length(Run.secano))  {
      if (is.null(dim(Run.secano[[j]])))  {
        secano[j,,] = NA
      }  else{
        secano[j,1:dim(Run.secano[[j]])[1]] = as.numeric(as.character(Run.secano[[j]][,'HWAH']))
      }
    }
    riego = array(NA,dim=c(length(Run.riego),30))
    for (j in 1:length(Run.riego))  {
      if (is.null(dim(Run.riego[[j]])))  {
        riego[j,,] = NA
      }  else{
        riego[j,1:dim(Run.riego[[j]])[1]] = as.numeric(as.character(Run.riego[[j]][,'HWAH']))
      }
    }
    
    #place results in array
    yld_secano[,,m,v] = secano
    yld_riego[,,m,v] = riego
    
  }
}

#Descartar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,,]
yld_secano[yld_secano==-99|yld_secano==-99.9|yld_secano==9999999] = 0  #set crop failures to 0 yield
yld_riego[yld_riego==-99|yld_riego==-99.9|yld_riego==9999999] = 0

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego[,,(p-1),],c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1),],c(1,3),mean,na.rm=T)
thresh = mean(yld_secano[,,(p-1),],na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
wfd.fail.r = apply(yld_riego[,,(p-1),],c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
wfd.fail.s = apply(yld_secano[,,(p-1),],c(1,3),function(x) sum(x<thresh,na.rm=T))

#area-weighted average yield
sum(wfd.s[,1]*crop_secano$secano.area)/sum(crop_secano$secano.area)
sum(wfd.s[,2]*crop_secano$secano.area)/sum(crop_secano$secano.area)
sum(wfd.s[,3]*crop_secano$secano.area)/sum(crop_secano$secano.area)

#highest-yielding variety
wfd.r.high = apply(wfd.r,1,which.max)
wfd.s.high = apply(wfd.s,1,which.max)

#least crop failures
wfd.r.lowFail = apply(wfd.fail.r,1,which.min)
wfd.s.lowFail = apply(wfd.fail.s,1,which.min)

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

#look at one "variety' at a time
yld_secano = yld_secano[,,,3]  #change sowing rule here
yld_riego = yld_riego[,,,2]  #2nd variety is IR72

#Filter out bad pixels with excess crop failures
#contar # de NA y 0 en rendimiento
riego.fallas = apply(yld_riego,c(1,3),function(x) sum(is.na(x)|x==0))
secano.fallas = apply(yld_secano,c(1,3),function(x) sum(is.na(x)|x==0))

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas[,10]>=14)  #half of total years
ind.bad.s = which(secano.fallas[,10]>=14)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,,]
crop_riego2 = crop_riego  #make backup of original
crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,,]
crop_secano2 = crop_secano  #make backup of original
crop_secano = crop_secano[-ind.bad.s,]}

#get list of countries
countries = unique(c(as.character(crop_riego$country),as.character(crop_secano$country)))

#Calculate weighted averages by model & compare to WFD
area.riego = replicate(length(anos),crop_riego$riego.area)  #keep same dimension as yield matrix
area.secano = replicate(length(anos),crop_secano$secano.area)

#Aggregate to country scale for all models & years (across riego & secano)
yld.country = array(NA,dim=c(length(countries),length(anos),10))  #initialize matrices (years x country x model )
dimnames(yld.country)[1] = list(countries)
dimnames(yld.country)[2] = list(anos2) 
dimnames(yld.country)[3] = list(models[1:10,])
for (r in 1:length(countries))  {  #loop by country
  ind.reg.r = which(crop_riego$country==countries[r])
  ind.reg.s = which(crop_secano$country==countries[r])
  
  if (length(c(ind.reg.r,ind.reg.s))>=1)  {
    for (m in 1:10)  {
      prod_riego.m = yld_riego[ind.reg.r,,m]*area.riego[ind.reg.r,]  #calculate production by pixel
      prod_secano.m = yld_secano[ind.reg.s,,m]*area.secano[ind.reg.s,]
      prod_all = rbind(prod_riego.m,prod_secano.m)  #stack riego & secano
      area_all = rbind(area.riego[ind.reg.r,],area.secano[ind.reg.s,])  #same for area
      yld.country[r,,m] = apply(prod_all,2,sum)/apply(area_all,2,sum)  #sum production at country-scale & divide out by total area
    }
  }
}

#calculate interannual means in each period
yld.country.m = apply(yld.country,c(1,3),mean,na.rm=T)
print(round(yld.country.m[order(rownames(yld.country.m)),10]))

##Aggregate area with crop failures to country-scale
#first identify crop failures by pixel & year
thresh = mean(yld_secano[,,10])*0.05  #5% of mean rainfed yield is considered "failure"
riego.fail = (yld_riego<=thresh | is.na(yld_riego))*1
secano.fail = (yld_secano<=thresh | is.na(yld_secano))*1

fail.country = array(NA,dim=c(length(countries),length(anos),10))  #initialize matrices (years x country x model )
dimnames(fail.country)[1] = list(countries)
dimnames(fail.country)[2] = list(anos2) 
dimnames(fail.country)[3] = list(models[1:10,])
for (r in 1:length(countries))  {  #loop by country
  ind.reg.r = which(crop_riego$country==countries[r])
  ind.reg.s = which(crop_secano$country==countries[r])
  
  if (length(c(ind.reg.r,ind.reg.s))>=1)  {
    for (m in 1:10)  {
      fail_riego.m = riego.fail[ind.reg.r,,m]*area.riego[ind.reg.r,]  #calculate area with crop failure
      fail_secano.m = secano.fail[ind.reg.s,,m]*area.secano[ind.reg.s,]
      fail_all = rbind(fail_riego.m,fail_secano.m)  #stack riego & secano
      area_all = rbind(area.riego[ind.reg.r,],area.secano[ind.reg.s,])
      fail.country[r,,m] = apply(fail_all,2,sum)/apply(area_all,2,sum)*100  #sum area with crop failure at country-scale, divide by total area to get %
    }
  }
}

#calculate interannual means in each period
fail.country.m = apply(fail.country,c(1,3),mean)
print(round(fail.country.m[order(rownames(fail.country.m)),10],2))
