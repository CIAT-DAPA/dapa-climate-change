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
        ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-1970  #index based on simulation start date
        if (ind.s[1]==0)  {ind.s = ind.s +1}
        dat_secano[j,ind.s,p2,v] = as.numeric(as.character(Run.secano[[j]][,params[p2]]))
      }
    }
  }
  
  for (j in 1:length(Run.riego))  {
    if (is.null(dim(Run.riego[[j]])))  {
      dat_riego[j,,,] = NA
    }  else{
      for (p2 in 1:length(params)) {
        ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-1970
        if (ind.r[1]==0)  {ind.r = ind.r +1}
        dat_riego[j,ind.r,p2,v] = as.numeric(as.character(Run.riego[[j]][,params[p2]]))
      }
    }
  }
}

#check data by year
apply(dat_riego[,,1,3],2,function(x) sum(is.na(x)==F))  #yield for 3rd variety
apply(dat_secano[,,1,2],2,function(x) sum(is.na(x)==F))

#Descartar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = dat_secano[,anos,1,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = dat_riego[,anos,1,]
yld_secano[yld_secano==-99] = 0  #re-emplazar -99 con 0 para rendimiento
yld_riego[yld_riego==-99] = 0
fechas_secano = dat_secano[,anos,2:4,]
fechas_riego = dat_riego[,anos,2:4,]
fechas_secano[fechas_secano==-99] = NA  #re-emplazar -99 con NA para fechas de siembra y cosecha
fechas_riego[fechas_riego==-99] = NA

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego,c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano,c(1,3),mean,na.rm=T)
thresh = mean(yld_secano,na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
wfd.fail.r = apply(yld_riego,c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
wfd.fail.s = apply(yld_secano,c(1,3),function(x) sum(x<thresh,na.rm=T))

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

#just pick highest-yielding by growth habit for dry bean
if (cultivos[c]=='frijol')  {   #check highest yielding by country according to growth habit
  habit = read.table('D:/Tobackup/BID/cultivar.files/Bean_growthHabit.csv',header=T,sep=',')
  for (t in 1:2) {
    eval(parse(text=paste("meso = match(crop_",treat[t],"$country,habit$X[habit$Mesoamerican==1])",sep="")))
    ind.meso = which(is.na(meso)==F)
    ind.andean = which(is.na(meso))
    eval(parse(text=paste("meso.high = apply(wfd.", substr(treat[t],1,1),"[ind.meso,1:2],1,which.max)",sep="")))
    eval(parse(text=paste("andean.high = apply(wfd.", substr(treat[t],1,1),"[ind.andean,3:4],1,which.max)+2",sep="")))
    eval(parse(text=paste("wfd.", substr(treat[t],1,1),".best[ind.meso] = meso.high",sep="")))
    eval(parse(text=paste("wfd.", substr(treat[t],1,1),".best[ind.andean] = andean.high",sep="")))
  }
}

#Select yield (& dates) for best variety for every pixel 
yld_secano.2 = yld_secano  #make backup with varieties
yld_riego.2 = yld_riego
fechas_secano.2 = fechas_secano  #make backup with varieties
fechas_riego.2 = fechas_riego
yld_secano = array(NA,dim=c(dim(crop_secano)[1],length(anos)))  #pixels x years  
yld_riego = array(NA,dim=c(dim(crop_riego)[1],length(anos)))
fechas_secano = array(NA,dim=c(dim(crop_secano)[1],length(anos),3))  #pixels x years  
fechas_riego = array(NA,dim=c(dim(crop_riego)[1],length(anos),3))  #pixels x years x params

# #to pick just one variety
# yld_secano = yld_secano.2[,,2]
# yld_riego = yld_riego.2[,,2]
# fechas_secano = fechas_secano.2[,,,2]
# fechas_riego = fechas_riego.2[,,,2]

#optimal variety
for (k in 1:length(variedades))  {
  yld_secano[wfd.s.best==k,] = yld_secano.2[wfd.s.best==k,,k]
  yld_riego[wfd.r.best==k,] = yld_riego.2[wfd.r.best==k,,k]
  fechas_secano[wfd.s.best==k,,] = fechas_secano.2[wfd.s.best==k,,,k]
  fechas_riego[wfd.r.best==k,,] = fechas_riego.2[wfd.r.best==k,,,k]
}

#contar # de 0 en rendimiento
riego.fallas = apply(yld_riego,1,function(x) sum(is.na(x)|x==0))
secano.fallas = apply(yld_secano,1,function(x) sum(is.na(x)|x==0))

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas>14)
ind.bad.s = which(secano.fallas>14)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,]
fechas_riego = fechas_riego[-ind.bad.r,,]
yld_riego.2 = yld_riego.2[-ind.bad.r,,]
crop_riego2 = crop_riego  #make backup of original
crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,]
fechas_secano = fechas_secano[-ind.bad.s,,]
yld_secano.2 = yld_secano.2[-ind.bad.s,,]
crop_secano2 = crop_secano  #make backup of original
crop_secano = crop_secano[-ind.bad.s,]}

#extract mode of planting & harvest months (across years)
siembra.s = apply(fechas_secano[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
siembra.s[sapply(siembra.s, is.null)] <- NA
siembra.s = as.numeric(unlist(siembra.s))
siembra.r = apply(fechas_riego[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
siembra.r[sapply(siembra.r, is.null)] <- NA
siembra.r = as.numeric(unlist(siembra.r))

cosecha.s = apply(fechas_secano[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
cosecha.s[sapply(cosecha.s, is.null)] <- NA
cosecha.s = as.numeric(unlist(cosecha.s))
cosecha.r = apply(fechas_riego[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
cosecha.r[sapply(cosecha.r, is.null)] <- NA
cosecha.r = as.numeric(unlist(cosecha.r))

#shift years for harvest in next year (Feb or after)
colnames(yld_riego) = anos2
colnames(yld_secano) = anos2

ind.harv.s = which(cosecha.s<siembra.s )  # & cosecha.s>=2 
yld_secano[ind.harv.s,2:length(anos)] = yld_secano[ind.harv.s,1:(length(anos)-1)]  #put in following year when harvest month earlier than sowing 
yld_secano[ind.harv.s,1] = NA  #reset first year to NA

ind.harv.r = which(cosecha.r<siembra.r )   #& cosecha.r>=2
yld_riego[ind.harv.r,2:length(anos)] = yld_riego[ind.harv.r,1:(length(anos)-1)]  #put in following year when harvest month earlier than sowing
yld_riego[ind.harv.r,1] = NA  #reset first year to NA

#exception for Chilean wheat!
ind.chile.s = which(crop_secano$country=='Chile' & cosecha.s>=11) 
yld_secano[ind.chile.s,2:length(anos)] = yld_secano[ind.chile.s,1:(length(anos)-1)]  #put in following year when harvest in Nov/Dec 
yld_secano[ind.chile.s,1] = NA  #reset first year to NA

# ind.chile.r = which(crop_riego$country=='Chile' & cosecha.r>=11) 
# yld_riego[ind.chile.r,2:length(anos)] = yld_riego[ind.chile.r,1:(length(anos)-1)]  #put in following year when harvest in Nov/Dec 
# yld_riego[ind.chile.r,1] = NA  #reset first year to NA

#check NA's by year again
apply(yld_secano,2,function(x) sum(is.na(x)==T))
apply(yld_riego,2,function(x) sum(is.na(x)==T))

#Save out yields for comparison with Iizumi
save(yld_riego,crop_riego,file=paste(path.res,'ylds_evaluacion/',cultivos[c],'_yld_pixel_riego.Rdat',sep=''))
save(yld_secano,crop_secano,file=paste(path.res,'ylds_evaluacion/',cultivos[c],'_yld_pixel_secano.Rdat',sep=''))

#get list of countries
countries = unique(c(as.character(crop_riego$country),as.character(crop_secano$country)))

#Calculate weighted averages by model & compare to WFD
area.riego = replicate(length(anos),crop_riego$riego.area)  #keep same dimension as yield matrix
area.secano = replicate(length(anos),crop_secano$secano.area)

#Aggregate to country scale for WFD in each model & year (across riego & secano)
yld.country = array(NA,dim=c(length(anos),length(countries)))  #initialize matrices
colnames(yld.country) = countries
rownames(yld.country) = anos2    
for (r in 1:length(countries))  {  #loop by country
  ind.reg.r = which(crop_riego$country==countries[r])
  ind.reg.s = which(crop_secano$country==countries[r])
  
  if (length(c(ind.reg.r,ind.reg.s))>=1)  {
    for (y in 1:dim(yld.country)[1])  { #loop through years
      ylds.y = c(yld_riego[ind.reg.r,y],yld_secano[ind.reg.s,y])
      #ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
      areas.y = c(area.riego[ind.reg.r,y],area.secano[ind.reg.s,y])
      yld.country[y,r] = sum(ylds.y*areas.y)/sum(areas.y)    
    }
  }
}

#aggregate area by country
secano.area = aggregate(crop_secano$secano.area,by=list(crop_secano$country),FUN=sum)
riego.area = aggregate(crop_riego$riego.area,by=list(crop_riego$country),FUN=sum)
area.merge = merge(secano.area, riego.area, by = "Group.1", all = TRUE)
area.merge[is.na(area.merge)]=0
area.merge$total = area.merge[,2]+area.merge[,3]
colnames(area.merge) = c('country','secano','riego','total')

#Save results to csv
eval(parse(text=paste("write.csv(yld.country,file='D:/tobackup/BID/FAOSTAT_validation/country.ylds_",cultivos[c],"_optVariety_cosecha.csv')",sep="")))
eval(parse(text=paste("write.csv(area.merge,file='D:/tobackup/BID/FAOSTAT_validation/country.areas_",cultivos[c],".csv')",sep="")))

