rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#escoger parametro de interes
params = c('HWAH','MDAT','SDAT','NDCH')  #,'PDAT','EPCM')  #duración, rendimiento y otro variables

#fijar cultivo aquí
#c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
#c=3  #soy
c=4  #frijol
#c=5 #trigo
# material = 'M-6101'

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Frijol-30-Octubre_noCO2'
carp.res.secano = 'Frijol-30-Octubre_noCO2'

#otros datos fijos
regions= c('MEX','CEN','AND','BRA','SUR')
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
anos = 1:27  #fijar años para analizar aquí (leave extra year at end)
anos2 = 1971:1997

#Load pixel id's
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#load, unlist and extract WFD yield data to arrays (gridcells x years x params)
load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_WFD.Rdat',sep=''))
Run.secano = Run
load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_WFD.Rdat',sep=''))
Run.riego = Run
  
#unlist everything into matrices
dat_secano = array(NA,dim=c(length(Run.secano),30,length(params)))  #initialize arrays
for (j in 1:length(Run.secano))  {
  if (is.null(dim(Run.secano[[j]])))  {
    dat_secano[j,,] = NA
  }  else{
    for (p2 in 1:length(params)) {
      ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-1970
      dat_secano[j,ind.s,p2] = as.numeric(as.character(Run.secano[[j]][,params[p2]]))
    }
  }
}

dat_riego = array(NA,dim=c(length(Run.riego),30,length(params)))
for (j in 1:length(Run.riego))  {
  if (is.null(dim(Run.riego[[j]])))  {
    dat_riego[j,,] = NA
  }  else{
    for (p2 in 1:length(params)) {
      ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-1970
      dat_riego[j,ind.r,p2] = as.numeric(as.character(Run.riego[[j]][,params[p2]]))
    }
  }
}

#check data by year
apply(dat_riego[,,4],2,function(x) sum(is.na(x)==F))
apply(dat_secano[,,4],2,function(x) sum(is.na(x)==F))
  
#Quitar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = dat_secano[,anos,1]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = dat_riego[,anos,1]
yld_secano[yld_secano==-99] = 0  #re-emplazar -99 con 0 para rendimiento
yld_riego[yld_riego==-99] = 0
fechas_secano = dat_secano[,anos,2:4]
fechas_riego = dat_riego[,anos,2:4]
fechas_secano[fechas_secano==-99] = NA  #re-emplazar -99 con NA para fechas de siembra y cosecha
fechas_riego[fechas_riego==-99] = NA

#contar # de NA y 0 en rendimiento
riego.fallas = apply(yld_riego,1,function(x) sum(x==0|is.na(x)))
secano.fallas = apply(yld_secano,1,function(x) sum(x==0|is.na(x)))

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas>12)
ind.bad.s = which(secano.fallas>12)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,]
fechas_riego = fechas_riego[-ind.bad.r,,]
crop_riego2 = crop_riego  #make backup of original
crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,]
fechas_secano = fechas_secano[-ind.bad.s,,]
crop_secano2 = crop_secano  #make backup of original
crop_secano = crop_secano[-ind.bad.s,]}

#extract mode of simulation start & harvest months (across years)
siembra.s = as.numeric(apply(fechas_secano[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1]))  
siembra.r = as.numeric(apply(fechas_riego[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1]))  

cosecha.s = as.numeric(apply(fechas_secano[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1]))  
cosecha.r = as.numeric(apply(fechas_riego[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1]))  

#alternative way to get harvest dates & months
sem.s = as.numeric(format(strptime(fechas_secano[,,2],'%Y %j'),'%j'))
sem.s = matrix(sem.s, nrow = dim(fechas_secano)[1], byrow = F)
cos.s = sem.s+fechas_secano[,,3]
cos.s[which(cos.s>366)] = cos.s[which(cos.s>366)]-365
cosecha.s = as.numeric(apply(cos.s,1,function(x) names(sort(table(format(strptime(x,'%j'),'%m')),dec=T))[1]))  

sem.r = as.numeric(format(strptime(fechas_riego[,,2],'%Y %j'),'%j'))
sem.r = matrix(sem.r, nrow = dim(fechas_riego)[1], byrow = F)
cos.r = sem.r+fechas_riego[,,3]
cos.r[which(cos.r>366)] = cos.r[which(cos.r>366)]-365
cosecha.r = as.numeric(apply(cos.r,1,function(x) names(sort(table(format(strptime(x,'%j'),'%m')),dec=T))[1]))  

#shift years for harvest in next year (Feb or after)
colnames(yld_riego) = anos2
colnames(yld_secano) = anos2

ind.harv.s = which(cosecha.s<siembra.s )  # & cosecha.s>=2
yld_secano[ind.harv.s,2:length(anos)] = yld_secano[ind.harv.s,1:(length(anos)-1)]  #put in following year when harvest month earlier than sowing and in Feb or after
yld_secano[ind.harv.s,1] = NA  #reset first year to NA

ind.harv.r = which(cosecha.r<siembra.r)   #  & cosecha.r>=2
yld_riego[ind.harv.r,2:length(anos)] = yld_riego[ind.harv.r,1:(length(anos)-1)]  #put in following year when harvest month earlier than sowing
yld_riego[ind.harv.r,1] = NA  #reset first year to NA

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
eval(parse(text=paste("write.csv(yld.country,file='D:/tobackup/BID/FAOSTAT_validation/country.ylds_",cultivos[c],"_cosecha.csv')",sep="")))
eval(parse(text=paste("write.csv(area.merge,file='D:/tobackup/BID/FAOSTAT_validation/country.areas_",cultivos[c],".csv')",sep="")))

