rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#escoger parametro de interes
params = c('NDCH','HWAH','TMAXA','TMINA','Stress_water_all','PDAT')  #,'PDAT','EPCM','H.AM','HWUM')  #duración, rendimiento y otro variables

#fijar cultivo aquí
#c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
#c=3  #soy
#c=4  #frijol
c=5 #trigo
variedad = 'AVALON'

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Trigo-3cultivars_withCO2'
carp.res.secano = 'Trigo-3cultivars_withCO2'

#otros datos fijos
regions= c('MEX','CEN','AND','BRA','SUR')
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
anos = 1:28  #fijar años para analizar aquí
anos2 = 1971:1998

#Load pixel id's
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#Load mapas de Latinoamerica
eval(parse(text=paste('Map_LatinAmerica<-shapefile("',path.root,'Latino_America.shp")',sep='')))
Map_LatinAmerica1<- fortify(Map_LatinAmerica)

#get list of climate models
models = read.table(paste(path.root,'ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)
models = rbind(models,'Historical baseline','Future multi-GCM average')
p=dim(models)[1]
colnames(models) = 'Models'  #hack for now (machetazo)

#load, unlist and extract yield data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,(p-1),length(params)))  #10 GCM's + baseline
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,(p-1),length(params)))

for (m in c(1:10))  {  #fija indices de modelos para incluir
  
  print(m)
  
  #Load & extract baseline data from list
  if (m<=9) {
    load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedad,'_',models[m,],'_.Rdat',sep=''))
  }  else {load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedad,'_WFD.Rdat',sep=''))}
  Run.secano = Run
  if (m<=9) {
    load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedad,'_',models[m,],'_.Rdat',sep=''))
  }  else {load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedad,'_WFD.Rdat',sep=''))}
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
  yld_secano[,,m,] = secano
  yld_riego[,,m,] = riego
  
}

#Descartar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,,]
yld_secano[yld_secano==-99|yld_secano==-99.9|yld_secano==9999999] = NA  #re-emplazar -99 con NA
yld_riego[yld_riego==-99|yld_riego==-99.9|yld_riego==9999999] = NA
# yld_secano[which(yld_secano==0)] = NA  #reemplazar 0 con NA también
# yld_riego[which(yld_riego==0)] = NA

#filtro en duración (a valores realísticos)
yld_secano[yld_secano[,,,1]>250]=NA  #first variable should be duration!
yld_riego[yld_riego[,,,1]>250]=NA

#contar # de NA y 0 en rendimiento
riego.fallas = apply(yld_riego[,,,2],c(1,3),function(x) sum(is.na(x)|x==0))
secano.fallas = apply(yld_secano[,,,2],c(1,3),function(x) sum(is.na(x)|x==0))
#hist(riego.fallas[,11],breaks=20)
#hist(secano.fallas[,11],breaks=20)

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas[,10]>14)
ind.bad.s = which(secano.fallas[,10]>14)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,,,]
                          crop_riego2 = crop_riego  #make backup of original
                          crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,,,]
                          crop_secano2 = crop_secano  #make backup of original
                          crop_secano = crop_secano[-ind.bad.s,]}

#Graficas comparando variables
x.var = 4  #cual variable quieres graficar?
xlim.c = quantile(c(yld_secano[,,10,x.var],yld_riego[,,10,x.var]),c(.01,.99),na.rm=T)
ylim.c = quantile(c(yld_secano[,,10,2],yld_riego[,,10,2]),c(0.01,.99),na.rm=T)
#xlim.c = c(0,5000)  #hard-coded values?
xlim.c = c(1,24)
ylim.c = c(0,10000)

plot(yld_secano[,,10,x.var],yld_secano[,,10,2],xlab=params[x.var],ylab='Yield',xlim=xlim.c,ylim=ylim.c)
test = lm(c(yld_secano[,,10,2])~c(yld_secano[,,10,x.var])+eval(c(yld_secano[,,10,x.var]^2)),x=T)
points(test$x[,2],test$fitted,pch=20,col='orange')
abline(v=quantile(test$x[,2],.05,na.rm=T),lwd=2,col='purple')
abline(v=quantile(test$x[,2],.5,na.rm=T),lwd=2,col='purple',lty=2)
abline(v=quantile(test$x[,2],.95,na.rm=T),lwd=2,col='purple')
title('Secano')

plot(yld_riego[,,10,x.var],yld_riego[,,10,2],xlab=params[x.var],ylab='Yield',xlim=xlim.c,ylim=ylim.c)
test = lm(c(yld_riego[,,10,2])~c(yld_riego[,,10,x.var])+eval(c(yld_riego[,,10,x.var]^2)),x=T)
points(test$x[,2],test$fitted,pch=20,col='red')
abline(v=quantile(test$x[,2],.05,na.rm=T),lwd=2,col='purple')
abline(v=quantile(test$x[,2],.5,na.rm=T),lwd=2,col='purple',lty=2)
abline(v=quantile(test$x[,2],.95,na.rm=T),lwd=2,col='purple')
title('riego')

## Algorithm to get confidence intervals on pixel-level % changes:
#Calculate multi-annual means for each model
#Calculate % change betwen each GCM and WFD for multi-annual means
#Bootstrapping on these 10 values per pixel to get 2.5 & 97.5 confidence intervals
#Calculate multi-model mean on % changes for comparison to CI

#Set variable to graph here!!!
var.g = 6  #que variable quieres graficar en los mapas??

#Calculate multi-model mean, % changes, and changes relative to sd
wfd.r = apply(yld_riego[,,(p-1),var.g],1,mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1),var.g],1,mean,na.rm=T)
models.r = apply(yld_riego[,,1:(p-2),var.g],c(1,3),mean,na.rm=T)  #multi-annual means
models.s = apply(yld_secano[,,1:(p-2),var.g],c(1,3),mean,na.rm=T)

#repeat wfd for 9 models
wfd.r2 = replicate(9,wfd.r)
wfd.s2 = replicate(9,wfd.s)

#calculate % change from each model to WFD
if (params[var.g]=='TMINA'|params[var.g]=='TMAXA'|params[var.g]=='Stress_water_all'|params[var.g]=='Stress_nitrogen_all'|params[var.g]=='IRCM') {  
  pct.ch.r = (models.r-wfd.r2)   #solo calcular diferencia
  pct.ch.s = (models.s-wfd.s2)
} else {
  pct.ch.r = (models.r-wfd.r2)/wfd.r2*100  #calcular % de cambio
  pct.ch.s = (models.s-wfd.s2)/wfd.s2*100
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
  for (m in c((p-1):(p+1)))  {  #(p-1):(p+1):  WFD, multi-model mean y % cambio (4 tipos)
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
        limits2 = c(-0.1,0.1)  #stress indices
      } 
      if (params[var.g]=='IRCM') {
        dat = pct.ch.r
        limits2 = max(abs(quantile(dat,c(.05,.95),na.rm=T)))
        limits2 = sort(c(limits2,-1*limits2))
        limits2 = c(-50,50)
      } 
      #limits2 = c(-25,25)  #hard-code limits instead
      promedio[promedio<limits2[1]] = limits2[1]  #reset end points
      promedio[promedio>limits2[2]] = limits2[2]
      eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
      if (params[var.g]=='TMAXA'|params[var.g]=='TMINA') {
        color_scale = colorRampPalette(c('white','orange','red'), space="rgb")(15)
        #color_scale = colorRampPalette(c('darkgreen','green','white','yellow'), space="rgb")(15)
      }  else { if (params[var.g]=='Stress_nitrogen_all'|params[var.g]=='Stress_water_all') {
        color_scale = colorRampPalette(c('forestgreen','green','white','orange','red'), space="rgb")(15)
      }  else{
        color_scale = colorRampPalette(c('red','orange','white','green','forestgreen'), space="rgb")(15)
      }
        
      }
      labs2 = ''
    }
    
    y=ggplot() +
      geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
      geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      coord_equal() +
      geom_raster(data=df, aes(x=Long, y=Lat,fill=yield))+
      ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n',models[m,],sep=''))+
      scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "white")+ # limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
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
    ggsave(filename=paste(path.root,"resultados_graficas/",cultivos[c],"_",treat[t],"_",variedad,"_",models[m,],"_",params[var.g],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
  }
}  

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %

#check for 0's (or low yields)
riego_zero = array(NA,dim=c(30,(p-1)))
secano_zero = array(NA,dim=c(30,(p-1)))
thresh = 0  #median(yld_secano,na.rm=T)*.01  #1% of median yield
for (m in 1:(p-1))  {
  for (j in anos)  {
    riego_zero[j,m] = sum(yld_riego[,j-1,m,2]<=thresh| is.na(yld_riego[,j-1,m,2]))  #number of pixels
    secano_zero[j,m] = sum(yld_secano[,j-1,m,2]<=thresh| is.na(yld_secano[,j-1,m,2]))  #fix up j-1!
    #     riego_na[j,m] = sum(is.na(yld_riego[,j,m])==F)
    #     secano_na[j,m] = sum(is.na(yld_secano[,j,m])==F)
  }
}
riego_zero2 = apply(riego_zero,2,mean,na.rm=T)/dim(yld_riego)[1]*100
secano_zero2 = apply(secano_zero,2,mean,na.rm=T)/dim(yld_secano)[1]*100
# riego_zero2 = apply(riego_zero/riego_na*100,2,mean,na.rm=T)   #dim(yld_riego)[1]*100
# secano_zero2 = apply(secano_zero/secano_na*100,2,mean,na.rm=T)
names(riego_zero2) = models$Models[1:(p-1)]
names(secano_zero2) = models$Models[1:(p-1)]

#mirar tasa de fallas
round(riego_zero2,2)
round(secano_zero2,2)

#barplot of % failures in future vs. WFD
par(mai=c(1.25,1.25,0.5,0.5))  #fijar los margenes de la gráfica
par(ask=T)  #preguntar antes de seguir a la próxima gráfica en un loop
#par(ask=F)
for (t in 1:2)  {  #riego, secano
  eval(parse(text=paste("barplot(",treat[t],"_zero2[1:(p-2)],xaxt='n',ylim=c(0,35),ylab='% Crop Failures')",sep='')))
  eval(parse(text=paste("abline(h=",treat[t],"_zero2[(p-1)],col='red',lty=2,lwd=2)",sep='')))
  eval(parse(text=paste("abline(h=mean(",treat[t],"_zero2[1:(p-2)],na.rm=T),lty=2,lwd=2.5)",sep='')))
  title(paste(cultivos.en[c],' (',treat.en[t],')',sep=''))
  text(x=seq(1,p,1.2),y=-1,cex=1,labels = models$Models[1:(p-2)], xpd=TRUE, srt=40, pos=2)
  legend('topright',c('GCMs - future','Baseline period','Multi-model mean'),bty='n',col=c('grey','red','black'),pch=c(15,-1,-1),lty=c(0,2,2),lwd=c(0,2,2.5))
}
