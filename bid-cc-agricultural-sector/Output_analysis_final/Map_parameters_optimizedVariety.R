rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#escoger parametros de interes
params = c('HWAH','EPCM')  #HWAH (yield) should always be the first

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

#Load mapas de Latinoamerica
eval(parse(text=paste('Map_LatinAmerica<-shapefile("',path.root,'Latino_America.shp")',sep='')))  #03-Map_LatinAmerica/
Map_LatinAmerica1<- fortify(Map_LatinAmerica)

#set graphing limits
xlim.c=c(-116,-35)
ylim.c=c(-54,32)
cex.c = 0.5

#get list of climate models
models = read.table(paste(path.root,'ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)  #_documentos/
models = rbind(models,'Historical baseline','Future multi-GCM average')
p=dim(models)[1]
colnames(models) = 'Models'  #hack for now (machetazo)

#load, unlist and extract data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,(p-1),length(variedades),length(params)))  #10 GCM's + baseline
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,(p-1),length(variedades),length(params)))

for (v in 1:length(variedades))  {
  for (m in c(1:10))  {  #fija indices de modelos para incluir
    
    print(m)
    
    #Load & extract baseline data from list
    if (m<=9) {
      load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
    }  else {load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_WFD.Rdat',sep=''))}
    Run.secano = Run
    if (m<=9) {
      load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
    }  else {load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedades[v],'_WFD.Rdat',sep=''))}
    Run.riego = Run
    
    #unlist everything into matrices
    secano = array(NA,dim=c(length(Run.secano),30,length(params)))  #initialize arrays
    for (j in 1:length(Run.secano))  {
      if (is.null(dim(Run.secano[[j]])))  {
        secano[j,,] = NA
      }  else{
        for (p2 in 1:length(params)) {
          secano[j,1:dim(Run.secano[[j]])[1],p2] = as.numeric(as.character(Run.secano[[j]][,params[p2]]))
        }
      }
    }
    riego = array(NA,dim=c(length(Run.riego),30,length(params)))
    for (j in 1:length(Run.riego))  {
      if (is.null(dim(Run.riego[[j]])))  {
        riego[j,,] = NA
      }  else{
        for (p2 in 1:length(params)) {
          riego[j,1:dim(Run.riego[[j]])[1],p2] = as.numeric(as.character(Run.riego[[j]][,params[p2]]))
        }
      }
    }
    
    #place results in array
    yld_secano[,,m,v,] = secano
    yld_riego[,,m,v,] = riego
    
  }
}

#Descartar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,,,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,,,]
yld_secano[yld_secano==-99|yld_secano==-99.9|yld_secano==9999999] = NA  #re-emplazar -99 con NA
yld_riego[yld_riego==-99|yld_riego==-99.9|yld_riego==9999999] = NA

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego[,,(p-1),,1],c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1),,1],c(1,3),mean,na.rm=T)
thresh = mean(yld_secano[,,(p-1),,1],na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
wfd.fail.r = apply(yld_riego[,,(p-1),,1],c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
wfd.fail.s = apply(yld_secano[,,(p-1),,1],c(1,3),function(x) sum(x<thresh,na.rm=T))

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

#Select variety with highest yields for every pixel, all params 
yld_secano.2 = yld_secano  #make backup with varieties
yld_riego.2 = yld_riego
yld_secano = array(NA,dim=c(dim(crop_secano)[1],length(anos),(p-1),length(params)))  #pixels x years x models x params
yld_riego = array(NA,dim=c(dim(crop_riego)[1],length(anos),(p-1),length(params)))
for (k in 1:length(variedades))  {
  yld_secano[wfd.s.best==k,,,] = yld_secano.2[wfd.s.best==k,,,k,]
  yld_riego[wfd.r.best==k,,,] = yld_riego.2[wfd.r.best==k,,,k,]
}

#Filter out bad pixels with excess crop failures
#contar # de NA y 0 en rendimiento
riego.fallas = apply(yld_riego[,,,1],c(1,3),function(x) sum(is.na(x)|x==0))
secano.fallas = apply(yld_secano[,,,1],c(1,3),function(x) sum(is.na(x)|x==0))

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas[,10]>=14)  #half of total years
ind.bad.s = which(secano.fallas[,10]>=14)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,,,]
crop_riego2 = crop_riego  #make backup of original
crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,,,]
crop_secano2 = crop_secano  #make backup of original
crop_secano = crop_secano[-ind.bad.s,]}

#Set variable to graph here!!!
var.g = 2  #que variable quieres graficar en los mapas??

#Calculate multi-model mean, % changes, and changes relative to sd
wfd.r = apply(yld_riego[,,(p-1),var.g],1,mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1),var.g],1,mean,na.rm=T)
models.r = apply(yld_riego[,,1:(p-2),var.g],c(1,3),mean,na.rm=T)  #multi-annual means
models.s = apply(yld_secano[,,1:(p-2),var.g],c(1,3),mean,na.rm=T)

#repeat wfd for 9 models
wfd.r2 = replicate(9,wfd.r)
wfd.s2 = replicate(9,wfd.s)

#calculate % change from each model to WFD
if (params[var.g]=='TMINA'|params[var.g]=='TMAXA'|params[var.g]=='Stress_water_all'|params[var.g]=='Stress_nitrogen_all') {  
  pct.ch.r = (models.r-wfd.r2)   #solo calcular diferencia
  pct.ch.s = (models.s-wfd.s2)
} else {
  pct.ch.r = (models.r-wfd.r2)/(wfd.r2+0.1)*100  #calcular % de cambio
  pct.ch.s = (models.s-wfd.s2)/(wfd.s2+0.1)*100  #avoid divide by zero errors
}

quantile(c(pct.ch.r,pct.ch.s),c(.05,.95),na.rm=T)
quantile(c(wfd.s,models.s),c(.05,.95),na.rm=T)
quantile(c(wfd.r,models.r,wfd.s,models.s),c(.05,.95),na.rm=T)

#Bootstrapping on percent change to get confidence intervals
#have to bootstrap for every pixel?
pct.ch.s2 = array(NA,dim=c(dim(crop_secano)[1],3))  #mean plus CI
# for (j in 1:dim(crop_secano)[1]) {
#   boot = mat.or.vec(500,1)
#   for (b in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
#                      boot[b] = mean(pct.ch.s[j,boot.ind])
#   }
#   pct.ch.s2[j,1:2] = quantile(boot,c(.025,.975),na.rm=T)
# }

pct.ch.r2 = array(NA,dim=c(dim(crop_riego)[1],3))
# for (j in 1:dim(crop_riego)[1]) {
#   boot = mat.or.vec(500,1)
#   for (b in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
#                      boot[b] = mean(pct.ch.r[j,boot.ind])
#   }
#   pct.ch.r2[j,1:2] = quantile(boot,c(.025,.975),na.rm=T)
# }

#calculate multi-model means for % change and yields
pct.ch.s2[,3] = apply(pct.ch.s,1,mean,na.rm=T)
pct.ch.r2[,3] = apply(pct.ch.r,1,mean,na.rm=T)
mmm.s = apply(models.s,1,mean,na.rm=T)
mmm.r = apply(models.r,1,mean,na.rm=T)

#add another column to pct.ch putting NA for non-significant changes
pct.ch.r2 = cbind(pct.ch.r2,pct.ch.r2[,3])
ind.na.r = which(pct.ch.r2[,1]<0 & pct.ch.r2[,2]>0)
pct.ch.r2[ind.na.r,4] = NA

pct.ch.s2 = cbind(pct.ch.s2,pct.ch.s2[,3])
ind.na.s = which(pct.ch.s2[,1]<0 & pct.ch.s2[,2]>0)
pct.ch.s2[ind.na.s,4] = NA

#Add column names & reorder columns
pct.ch.r2 = cbind(pct.ch.r2[,3:4],pct.ch.r2[,1:2])  #re-order columns
pct.ch.s2 = cbind(pct.ch.s2[,3:4],pct.ch.s2[,1:2])
pct.ch_cols = c('PctCh.MMM','SigCh.MMM','PctCh.lower','PctCh.upper')
colnames(pct.ch.s2) = pct.ch_cols
colnames(pct.ch.r2) = pct.ch_cols
models = rbind(models,'PctCh.MMM','SigCh.MMM','PctCh.lower','PctCh.upper')  #concatenate for plotting purposes

for (t in 1:2)  {  #loop through riego (t=1)/ secano (t=2)
  #t=2  #secano
  print(t)
  for (m in c((p+1)))  {  #(p-1):(p+1):  WFD, multi-model mean y % cambio (4 tipos)
    #m=11
    
    #multi-annual means per model
    if (m<=(p-1))  {
      eval(parse(text=paste('promedio = apply(yld_',treat[t],'[,,m,var.g],1,mean,na.rm=T)',sep='')))  #average in zeros??  
    }  else{  if(m==p) {  #multi-model mean
      eval(parse(text=paste('promedio = mmm.',substr(treat[t],1,1),sep='')))
    }  else{ if(m>=(p+1)) {  #% change
      eval(parse(text=paste('promedio = pct.ch.',substr(treat[t],1,1),'2[,m-p]',sep='')))  #change column with loop
    }  
    }
    }
    
    if(m<=p)  {  #for models + WFD
      #promedio[promedio==0] = NA  #set 0 values to NA
      limits2 = quantile(c(yld_riego[,,,var.g],yld_secano[,,,var.g]),c(.05,.95),na.rm=T)  #limites flexibles segun el cultivo
      if (params[var.g]=='Stress_nitrogen_all') {
        limits2 = c(0,0.45)  #stress indices
      } 
      if (params[var.g]=='Stress_water_all') {
        limits2 = c(0,0.32)  #stress indices
      } 
      if (params[var.g]=='NDCH') {
        limits2 = c(75,175)  #duration
      } 
      #limits2 = c(430,13800)  #limites fijados
      promedio[which(promedio<limits2[1] & promedio>0)] = limits2[1]  #reset end points of data
      promedio[which(promedio>limits2[2])] = limits2[2]
      
      eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
      if (params[var.g]=='Stress_water_all'|params[var.g]=='Stress_nitrogen_all') {
        color_scale = colorRampPalette(c('forestgreen','gold2','red'), space="rgb")(25)
      }  else{
        color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
      }
      labs2 = 'Yield \n(Kg/ha)'
      if (params[var.g]=='NDCH') {
        labs2='Days'
      }
    }  else{  #for % change plots
      if (m>=(p+1)) {dat = c(pct.ch.r,pct.ch.s)}
      #limits2 = quantile(dat[dat<0],c(.025),na.rm=T)  
      limits2 = max(abs(quantile(dat,c(.05,.95),na.rm=T)))
      limits2 = sort(c(limits2,-1*limits2))  #set positive equal to opposite of negative
      if (params[var.g]=='TMAXA'|params[var.g]=='TMINA') {
        limits2 = c(0,3.5)  #temperatura
      }  
      if (params[var.g]=='HWAH'|params[var.g]=='YPEM') {
        limits2 = c(-50,50)  #rend y WUE
      }  
      if (params[var.g]=='NDCH') {
        limits2 = c(-12,12)  #duración
      } 
      if (params[var.g]=='Stress_water_all'|params[var.g]=='Stress_nitrogen_all') {
        limits2 = c(-0.15,0.15)  #stress indices
      } 
      if (params[var.g]=='EPCM') {
        limits2 = c(-25,25)  #stress indices
      } 
      if (params[var.g]=='IRCM') {
        dat = pct.ch.r
        limits2 = max(abs(quantile(dat,c(.1,.9),na.rm=T)))
        limits2 = sort(c(limits2,-1*limits2))
        limits2 = c(-1000,1000)
      } 
      #limits2 = c(-25,25)  #hard-code limits instead
      promedio[promedio<limits2[1]] = limits2[1]  #reset end points
      promedio[promedio>limits2[2]] = limits2[2]
      eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
      if (params[var.g]=='TMAXA'|params[var.g]=='TMINA') {
        color_scale = colorRampPalette(c('white','orange','red'), space="rgb")(15)
        #color_scale = colorRampPalette(c('darkgreen','green','white','yellow'), space="rgb")(15)
      }  else { if (params[var.g]=='Stress_nitrogen_all'|params[var.g]=='Stress_water_all') {
        color_scale = colorRampPalette(c('forestgreen','green','lightgreen','white','orange','orangered','red'), space="rgb")(15)
      }  else{
        color_scale = colorRampPalette(c('red','orangered','orange','white','lightgreen','green','forestgreen'), space="rgb")(15)
      }
        
      }
      labs2 = ''
    }
    
    y=ggplot() +
      geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="gray90", fill="gray70" )+
      geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      coord_equal() +
      geom_raster(data=df, aes(x=Long, y=Lat,fill=yield))+
      ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n',models[m,],sep=''))+
      scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "blue")+ # limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
      theme_bw()+
      labs(fill=labs2)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size=14),
            legend.title = element_text(face="bold",size=14),
            legend.background = element_blank(),
            legend.key = element_blank(),
            plot.title = element_text(face="bold", size=18),
            panel.border = element_blank(),
            axis.ticks = element_blank())
    #plot(y)
    ggsave(filename=paste(path.root,"resultados_graficas/",cultivos[c],"_",treat[t],"_bestVariety_",models[m,],"_",params[var.g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
  }
}  