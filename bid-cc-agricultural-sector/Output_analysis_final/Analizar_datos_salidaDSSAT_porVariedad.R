#Visualize DSSAT runs
#limpiar workspace
rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#fijar cultivo aquí
#c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
#c=3  #soy
c=4  #frijol
#c=5 #trigo
# material = 'M-6101'

#fijar variedad & variable para analizar
variedad = 'Carioca'
variable = 'HWAH'  #HWAH = yield, 'NDCH' = duration

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory D:/tobackup/BID/Resultados_DSSAT/
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
carp.res.riego = 'Frijol-3cultivars_withCO2/SYMBI_Y'
carp.res.secano = 'Frijol-3cultivars_withCO2/SYMBI_Y'

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

#load, unlist and extract yield data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,(p-1)))  #10 GCM's + baseline
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,(p-1)))

for (m in c(1:10))  {  #fija indices de modelos para incluir
  
  print(m)

  #Load & extract baseline data from list
  if (m<=9)  {
    load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedad,'_',models[m,],'_.Rdat',sep=''))
    Run.secano = Run
    load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedad,'_',models[m,],'_.Rdat',sep=''))
    Run.riego = Run
    yr.start = 2020
  }  else{
    load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedad,'_WFD.Rdat',sep=''))
    Run.secano = Run
    load(paste(path.res,cultivos[c],'/',carp.res.riego,'/_',cultivos[c],'_Riego_',variedad,'_WFD.Rdat',sep=''))
    Run.riego = Run
    yr.start = 1970
  }
  
  #unlist everything into matrices
  secano = array(NA,dim=c(length(Run.secano),30))  #initialize arrays
  for (j in 1:length(Run.secano))  {
    if (is.null(dim(Run.secano[[j]])))  {
      secano[j,] = 0
    }  else{
      ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-yr.start
      if (ind.s[1]==0)  {ind.s = ind.s +1}  #shift for simulations starting in prior year
      secano[j,ind.s] = Run.secano[[j]][,variable]
    }
  }
  riego = array(NA,dim=c(length(Run.riego),30))
  for (j in 1:length(Run.riego))  {
    if (is.null(dim(Run.riego[[j]])))  {
      riego[j,] = 0
    }  else{
      ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-yr.start
      if (ind.r[1]==0)  {ind.r = ind.r +1}
      riego[j,ind.r] = Run.riego[[j]][,variable]
    }
  }
  
  #place results in array
   yld_secano[,,m] = secano
   yld_riego[,,m] = riego
  
}

#check data points in each year
apply(yld_secano[,,2],2,function(x) sum(is.na(x)==F))
apply(yld_secano[,,10],2,function(x) sum(is.na(x)==F))

#Quitar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,]
yld_secano[yld_secano==-99] = 0  #re-emplazar -99 con 0 for legitimate crop failures
yld_riego[yld_riego==-99] = 0

#contar # de NA y 0 en rendimiento
riego.fallas = apply(yld_riego,c(1,3),function(x) sum(is.na(x)|x==0))
secano.fallas = apply(yld_secano,c(1,3),function(x) sum(is.na(x)|x==0))
# hist(riego.fallas[,10],breaks=20)
# hist(secano.fallas[,10],breaks=20)

# #check mean yields by model
# test = apply(yld_secano[,,10],1,mean)
# sum(test*crop_secano$secano.area)/sum(crop_secano$secano.area)

#filtrar pixeles con muchas fallas en WFD
ind.bad.r = which(riego.fallas[,10]>=14)  #half of total years
ind.bad.s = which(secano.fallas[,10]>=14)
if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,,]
crop_riego2 = crop_riego  #make backup of original
crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,,]
crop_secano2 = crop_secano  #make backup of original
crop_secano = crop_secano[-ind.bad.s,]}

#look at distribution of values in WFD
quantile(yld_secano[,,10],c(.1,.25,.5,.75,.9),na.rm=T)
quantile(yld_riego[,,10],c(.1,.25,.5,.75,.9),na.rm=T)

## Algorithm to get confidence intervals on pixel-level % changes:
#Calculate multi-annual means for each model
#Calculate % change betwen each GCM and WFD for multi-annual means
#Bootstrapping on these 10 values per pixel to get 2.5 & 97.5 confidence intervals
#Calculate multi-model mean on % changes for comparison to CI

#Calculate multi-model mean, % changes, and changes relative to sd
wfd.r = apply(yld_riego[,,(p-1)],1,mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1)],1,mean,na.rm=T)
models.r = apply(yld_riego[,,1:(p-2)],c(1,3),mean,na.rm=T)  #multi-annual means
models.s = apply(yld_secano[,,1:(p-2)],c(1,3),mean,na.rm=T)

#repeat wfd for 9 models
wfd.r2 = replicate(9,wfd.r)
wfd.s2 = replicate(9,wfd.s)

#calculate % change from each model to WFD
pct.ch.r = (models.r-wfd.r2)/wfd.r2*100
pct.ch.r[which(wfd.r2==0)] = NA  #set pixels with 0 yield in WFD to NA
pct.ch.s = (models.s-wfd.s2)/wfd.s2*100
pct.ch.s[which(wfd.s2==0)] = NA  #set pixels with 0 yield in WFD to NA

#Bootstrapping on percent change to get confidence intervals
#have to bootstrap for every pixel?
pct.ch.s2 = array(NA,dim=c(dim(crop_secano)[1],5))  #two sets of CI plus mean
for (j in 1:dim(crop_secano)[1]) {
  boot = mat.or.vec(500,1)
  for (b in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
                     boot[b] = mean(pct.ch.s[j,boot.ind])
  }
  pct.ch.s2[j,1:2] = quantile(boot,c(.1,.9),na.rm=T)
  pct.ch.s2[j,3:4] = quantile(boot,c(.025,.975),na.rm=T)
}

pct.ch.r2 = array(NA,dim=c(dim(crop_riego)[1],5))
for (j in 1:dim(crop_riego)[1]) {
  boot = mat.or.vec(500,1)
  for (b in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
                     boot[b] = mean(pct.ch.r[j,boot.ind])
  }
  pct.ch.r2[j,1:2] = quantile(boot,c(.1,.9),na.rm=T)
  pct.ch.r2[j,3:4] = quantile(boot,c(.025,.975),na.rm=T)
}

#calculate multi-model means for % change and yields
pct.ch.s2[,5] = apply(pct.ch.s,1,mean,na.rm=T)
pct.ch.r2[,5] = apply(pct.ch.r,1,mean,na.rm=T)
mmm.s = apply(models.s,1,mean,na.rm=T)
mmm.r = apply(models.r,1,mean,na.rm=T)

#add another column to pct.ch putting NA for non-significant changes
pct.ch.r2 = cbind(pct.ch.r2,pct.ch.r2[,5])
ind.na.r = which(pct.ch.r2[,3]<0 & pct.ch.r2[,4]>0)
pct.ch.r2[ind.na.r,6] = NA

pct.ch.s2 = cbind(pct.ch.s2,pct.ch.s2[,5])
ind.na.s = which(pct.ch.s2[,3]<0 & pct.ch.s2[,4]>0)
pct.ch.s2[ind.na.s,6] = NA

#Add column names & reorder columns
pct.ch.r2 = cbind(pct.ch.r2[,5:6],pct.ch.r2[,1:4])  #re-order columns
pct.ch.s2 = cbind(pct.ch.s2[,5:6],pct.ch.s2[,1:4])
pct.ch_cols = c('PctCh.MMM','SigCh.MMM','PctCh.10.lower','PctCh.90.upper','PctCh.2.5.lower','PctCh.97.5.upper')
colnames(pct.ch.s2) = pct.ch_cols
colnames(pct.ch.r2) = pct.ch_cols
models = rbind(models,'PctCh.MMM','SigCh.MMM','PctCh.10.lower','PctCh.90.upper','PctCh.2.5.lower','PctCh.97.5.upper')  #concatenate for plotting purposes

for (t in 2:2)  {  #loop through riego (t=1)/ secano (t=2)
  #t=2  #secano
  print(t)
  for (m in c(p-1,p+2))  {  #(p-1):(p+1),(p+3):(p+4)  WFD, multi-model mean y % cambio (4 tipos)
    #m=11
    
    #multi-annual means per model
    if (m<=(p-1))  {
      eval(parse(text=paste('promedio = apply(yld_',treat[t],'[,,m],1,mean,na.rm=T)',sep='')))  #average in zeros??  
    }  else{  if(m==p) {  #multi-model mean
      eval(parse(text=paste('promedio = mmm.',substr(treat[t],1,1),sep='')))
    }  else{ if(m>=(p+1)) {  #% change
      eval(parse(text=paste('promedio = pct.ch.',substr(treat[t],1,1),'2[,m-p]',sep='')))  #change column with loop
    }  
    }
    }
    
    if(m<=p)  {  #for models + WFD
      promedio[promedio==0] = NA  #set 0 values to NA
      #These limits should be calculated on wfd.s/r & mmm.s/r
      #limits2 = quantile(c(yld_riego,yld_secano),c(.025,.975),na.rm=T)  #limites flexibles segun el cultivo
      limits2 = quantile(c(wfd.s,wfd.r,mmm.s,mmm.r),c(.05,.95),na.rm=T)
      limits2 = c(500,11000)  #limites fijados
      promedio[which(promedio<limits2[1] & promedio>0)] = limits2[1]  #reset end points of data
      promedio[which(promedio>limits2[2])] = limits2[2]
      
      eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
      color_scale = colorRampPalette(c('red','gold2','forestgreen'), space="rgb")(25) 
      labs2 = 'Yield \n(Kg/ha)'
    }  else{  #for % change plots
      if (m>=(p+1)) {dat = c(pct.ch.r2[,1],pct.ch.s2[,1])}
      limits2 = quantile(dat[dat<0],c(.025),na.rm=T)  
      limits2 = c(limits2,-1*limits2)  #set positive equal to opposite of negative
      limits2 = c(-51.1,51.1)  #hard-code limits instead
      promedio[promedio=='NaN'] = NA
      promedio[promedio<limits2[1]] = limits2[1]  #reset end points
      promedio[promedio>limits2[2]] = limits2[2]
      eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
      color_scale = colorRampPalette(c('red','orangered','orange','white','lightgreen','green','forestgreen'), space="rgb")(20)
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
    ggsave(filename=paste(path.root,"resultados_graficas/",cultivos[c],"_",treat[t],"_",variedad,'_',models[m,],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)  #_documentos/graficas
  }
}  

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %

#check for 0's (or low yields)
riego_zero = array(NA,dim=c(28,(p-1)))
secano_zero = array(NA,dim=c(28,(p-1)))
# riego_na = array(NA,dim=c(30,(p-1)))
# secano_na = array(NA,dim=c(30,(p-1)))
thresh = 0  #median(yld_secano,na.rm=T)*.01  #1% of median yield
for (m in 1:(p-1))  {
  for (j in anos)  {
    riego_zero[j,m] = sum(yld_riego[,j,m]<=thresh| is.na(yld_riego[,j,m]))  #number of pixels
    secano_zero[j,m] = sum(yld_secano[,j,m]<=thresh| is.na(yld_secano[,j,m]))  #fix up j-1!
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

#AGGREGATE RESULTS TO FPU SCALE
#get list of FPU's
FPUs = unique(c(as.character(crop_riego$New_FPU),as.character(crop_secano$New_FPU)))

#Calculate weighted averages by model & compare to WFD
area.riego = replicate(length(anos),crop_riego$riego.area)
area.secano = replicate(length(anos),crop_secano$secano.area)

#find areas where WFD consistently failing and discard these from aggregation
zeros.wfd.r = apply(yld_riego[,,(p-1)],1,function(x) sum(x==0,na.rm=T))
zeros.wfd.s = apply(yld_secano[,,(p-1)],1,function(x) sum(x==0,na.rm=T))

#Aggregate to FPU scale for each model & year
yld.FPU.r = array(NA,dim=c(p-1,length(anos),length(FPUs)))  #initialize matrices
yld.FPU.s = array(NA,dim=c(p-1,length(anos),length(FPUs)))

#aggregation
for (t in 1:2)  {  #loop by treatment
  for (r in 1:length(FPUs))  {  #then by FPU
   eval(parse(text=paste("ind.reg = which(crop_",treat[t],"$New_FPU==FPUs[r] & zeros.wfd.",substr(treat[t],1,1),"<=14)",sep='')))
    
    if (length(ind.reg)>1)  {
      for (j in 1:(p-1))  {   #loop through models
        for (y in (anos))  { #loop through years 
          eval(parse(text=paste("ylds.y = yld_",treat[t],"[ind.reg,y,j]",sep=''))) 
          #ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
          eval(parse(text=paste("areas.y = area.",treat[t],"[ind.reg,y]",sep=''))) 
          eval(parse(text=paste("yld.FPU.",substr(treat[t],1,1),"[j,y,r] = sum(ylds.y*areas.y)/sum(areas.y)",sep='')))    
        }
      }
    }
  }
}

# Calculate interannual means & CV's
yld.FPU.mean.r = apply(yld.FPU.r,c(1,3),function(x) mean(x))
yld.FPU.mean.s = apply(yld.FPU.s,c(1,3),function(x) mean(x))
yld.FPU.CV.r = apply(yld.FPU.r,c(1,3),function(x) sd(x)/mean(x)*100)
yld.FPU.CV.s = apply(yld.FPU.s,c(1,3),function(x) sd(x)/mean(x)*100)

#calculate changes
WFD.FPU.mean.r = t(replicate(9,yld.FPU.mean.r[10,]))  #repeat WFD for 10 models
WFD.FPU.mean.s = t(replicate(9,yld.FPU.mean.s[10,]))
WFD.FPU.CV.r = t(replicate(9,yld.FPU.CV.r[10,]))
WFD.FPU.CV.s = t(replicate(9,yld.FPU.CV.s[10,]))
FPU.pctCh.r = (yld.FPU.mean.r[1:9,]-WFD.FPU.mean.r)/WFD.FPU.mean.r*100
FPU.pctCh.s = (yld.FPU.mean.s[1:9,]-WFD.FPU.mean.s)/WFD.FPU.mean.s*100
FPU.CVCh.r = (yld.FPU.CV.r[1:9,]-WFD.FPU.CV.r)
FPU.CVCh.s = (yld.FPU.CV.s[1:9,]-WFD.FPU.CV.s)

#multi-model means
FPU.pctCh.mmm.r = apply(FPU.pctCh.r,2,mean,na.rm=T)  #remove missing models
FPU.pctCh.mmm.s = apply(FPU.pctCh.s,2,mean,na.rm=T)
FPU.CVCh.mmm.r = apply(FPU.CVCh.r,2,mean,na.rm=T)
FPU.CVCh.mmm.s = apply(FPU.CVCh.s,2,mean,na.rm=T)

#Plot mean yield changes vs. baseline CV
plot(yld.FPU.CV.r[10,],FPU.pctCh.mmm.r,xlab='Baseline IAV',ylab='Mean yield change')
lines(0:60,seq(0,-60,-1),col='green',lwd=2)
title('Riego')
plot(yld.FPU.CV.s[10,],FPU.pctCh.mmm.s,xlab='Baseline IAV',ylab='Mean yield change')
lines(0:60,seq(0,-60,-1),col='green',lwd=2)
title('Secano')

#Plot mean yield vs. IAV changes
#plot(FPU.CVCh.mmm.r,FPU.pctCh.mmm.r,xlab='Change in IAV',ylab='Change in mean yields',xlim=c(0,max(FPU.CVCh.mmm.r,na.rm=T)+2),ylim=c(min(FPU.pctCh.mmm.r,na.rm=T)-2,0))
plot(FPU.CVCh.mmm.r,FPU.pctCh.mmm.r,xlab='Change in IAV (%)',ylab='Change in mean yields (%)',xlim=c(0,50),ylim=c(-60,0))
abline(h=-25,lty=3)
abline(v=10,lty=3)
title('Riego')
hotspots.r = which(FPU.CVCh.mmm.r>10 & FPU.pctCh.mmm.r<(-25))
points(FPU.CVCh.mmm.r[hotspots.r],FPU.pctCh.mmm.r[hotspots.r],pch=15,col='purple')
text(FPU.CVCh.mmm.r[hotspots.r],FPU.pctCh.mmm.r[hotspots.r],FPUs[hotspots.r],pos=1,cex=0.9)

#plot(FPU.CVCh.mmm.s,FPU.pctCh.mmm.s,xlab='Change in IAV',ylab='Change in mean yields',xlim=c(0,max(FPU.CVCh.mmm.s,na.rm=T)+2),ylim=c(min(FPU.pctCh.mmm.s,na.rm=T)-2,0))
plot(FPU.CVCh.mmm.s,FPU.pctCh.mmm.s,xlab='Change in IAV',ylab='Change in mean yields',xlim=c(0,50),ylim=c(-60,0))
abline(h=-25,lty=3)
abline(v=10,lty=3)
title('Secano')
hotspots.s = which(FPU.CVCh.mmm.s>10 & FPU.pctCh.mmm.s<(-25))
points(FPU.CVCh.mmm.s[hotspots.s],FPU.pctCh.mmm.s[hotspots.s],pch=16,col='purple')
text(FPU.CVCh.mmm.s[hotspots.s],FPU.pctCh.mmm.s[hotspots.s],FPUs[hotspots.s],cex=0.9,pos=1,offset=0.2)

#POINT-SCALE PLOTS
#Plot mean yield changes vs. initial interannual CV for WFD
#(identify areas where projected yield declines are higher than interannual variability)
par(ask=F)
wfd.s.sd = apply(yld_secano[,,(p-1)],1,sd,na.rm=T)
wfd.s.cv = wfd.s.sd/wfd.s*100
plot(wfd.s.cv,pct.ch.s2[,1],xlim=c(0,100),ylim=c(-65,50),xlab='WFD CV',ylab='% yield change (MMM)')
abline(h=0)
lines(0:60,seq(0,-60,-1),col='green',lwd=2)
ind.hotspot = which(pct.ch.s2[,1]<wfd.s.cv*-1)
text(5,-50,paste(round(sum(crop_secano[ind.hotspot,'secano.area'])/sum(crop_secano[,'secano.area'])*100,2),'%'))
aggregate(crop_secano[ind.hotspot,'secano.area'],by=list(crop_secano[ind.hotspot,'country']),FUN=sum)
title(paste(cultivos[c],'- secano'))

wfd.r.sd = apply(yld_riego[,,(p-1)],1,sd,na.rm=T)
wfd.r.cv = wfd.r.sd/wfd.r*100
plot(wfd.r.cv,pct.ch.r2[,1],xlim=c(0,100),ylim=c(-65,50),xlab='WFD CV',ylab='% yield change (MMM)')
abline(h=0)
lines(0:60,seq(0,-60,-1),col='green',lwd=2)
ind.hotspot = which(pct.ch.r2[,1]<wfd.r.cv*-1)
text(5,-50,paste(round(sum(crop_riego[ind.hotspot,'riego.area'])/sum(crop_riego[,'riego.area'])*100,2),'%'))
aggregate(crop_riego[ind.hotspot,'riego.area'],by=list(crop_riego[ind.hotspot,'country']),FUN=sum)
title(paste(cultivos[c],'- riego'))

#PLot yield changes vs. change in CV
models.r.cv = apply(yld_riego[,,1:(p-2)],c(1,3),function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)  #multi-annual means
models.s.cv = apply(yld_secano[,,1:(p-2)],c(1,3),function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)
wfd.r.cv2 = replicate(9,wfd.r.cv)
wfd.s.cv2 = replicate(9,wfd.s.cv)
CV.ch.r = models.r.cv - wfd.r.cv2
CV.ch.s = models.s.cv - wfd.s.cv2
CV.ch.r.mmm = apply(CV.ch.r,1,mean,na.rm=T)
CV.ch.s.mmm = apply(CV.ch.s,1,mean,na.rm=T)

par(ask=F)
plot(CV.ch.s.mmm,pct.ch.s2[,3],xlim=c(-50,100),ylim=c(-65,50),xlab='Change in CV',ylab='% yield change (MMM)')
ind.good.s = which(CV.ch.s.mmm>(-50) & CV.ch.s.mmm<100 & pct.ch.s2[,3]>(-65) & pct.ch.s2[,3]<50)
mod.s = lm(pct.ch.s2[ind.good.s,3]~CV.ch.s.mmm[ind.good.s]+eval(CV.ch.s.mmm[ind.good.s]^2),x=T)
abline(h=0)
points(mod.s$x[,2],mod.s$fitted,pch=20,col='red')
title(paste(cultivos[c],'- secano'))

plot(CV.ch.r.mmm,pct.ch.r2[,3],xlim=c(-50,100),ylim=c(-65,50),xlab='Change in CV',ylab='% yield change (MMM)')
ind.good.r = which(CV.ch.r.mmm>(-50) & CV.ch.r.mmm<100 & pct.ch.r2[,3]>(-65) & pct.ch.r2[,3]<50)
mod.r = lm(pct.ch.r2[ind.good.r,3]~CV.ch.r.mmm[ind.good.r]+eval(CV.ch.r.mmm[ind.good.r]^2),x=T)
abline(h=0)
points(mod.r$x[,2],mod.r$fitted,pch=20,col='red')
title(paste(cultivos[c],'- riego'))

#AGGREGATION TO 6 REGIONS
#Calculate overall mean yield changes for entire region (then sub-regions)
#load physical area from SPAM to weight yield changes

#Calculate weighted averages by model & compare to WFD
area.riego = replicate(length(anos),crop_riego$riego.area)
area.secano = replicate(length(anos),crop_secano$secano.area)

#find areas where WFD consistently failing and discard these from aggregation
zeros.wfd.r = apply(yld_riego[,,(p-1)],1,function(x) sum(x==0,na.rm=T))
zeros.wfd.s = apply(yld_secano[,,(p-1)],1,function(x) sum(x==0,na.rm=T))

#initialize arrays
yld.ag.r = array(NA,dim=c(8,6))  #8 indicators x 6 regions
yld.ag.s = array(NA,dim=c(8,6))  

for (j in 1:6)  {  #5 regions + all
  for (t in 1:2)  {  #loop through riego/ secano
    
    #subset regions
    if (j<=5) {eval(parse(text=paste("ind.reg = which(crop_",treat[t],"$Region==regions[j] & zeros.wfd.",substr(treat[t],1,1),"<=14)",sep='')))
    }   else{
      eval(parse(text=paste("ind.reg = which(zeros.wfd.",substr(treat[t],1,1),"<=14)",sep='')))
    }
    
    if (length(ind.reg)>1)  {  #at least 2 pixels
      
      #hacer el promedio ponderado por área de los rendimientos
      mean.ylds = array(NA,dim=c((p-1),length(anos)))  #models x years
      for (m in 1:(p-1))  {  
        for (y in (anos-1))  {   #fix anos-1!
          eval(parse(text=paste("ylds.y = yld_",treat[t],"[ind.reg,y,m]",sep=''))) 
          #ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0
          eval(parse(text=paste("areas.y = area.",treat[t],"[ind.reg,y]",sep=''))) 
          mean.ylds[m,y] = sum(ylds.y*areas.y)/sum(areas.y)  #promedio ponderado
        }    
      }
      mean.ylds = apply(mean.ylds,1,mean,na.rm=T)  #take inter-annual mean 
      
      wfd = mean.ylds[p-1]  #WFD
      mmm = mean(mean.ylds[1:(p-2)],na.rm=T)  #multi-model mean
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[1,j] = wfd",sep='')))
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[2,j] = mmm",sep='')))
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[3,j] = (mmm - wfd)/wfd*100",sep='')))  #% diff: riego
      
      #calculate across-model range of % changes
      ch = mat.or.vec((p-2),1)
      for (m in 1:(p-2))  {
        ch[m] = (mean.ylds[m] - mean.ylds[p-1] )/mean.ylds[p-1] *100
      }
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[4:5,j] = range(ch,na.rm=T)",sep='')))
      
      #try bootstrapping across models to get CI
      boot = mat.or.vec(500,1)
      for (b in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
                         boot[b] = mean(mean.ylds[boot.ind])
      }
      boot.ch = (boot - mean.ylds[p-1] )/mean.ylds[p-1]  * 100
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[6:7,j] = quantile(boot.ch,c(.025,.975),na.rm=T)",sep='')))
      eval(parse(text=paste("yld.ag.",substr(treat[t],1,1),"[8,j] = sum(crop_",treat[t],"[ind.reg,'",treat[t],".area'])",sep='')))
      
    }
  }
}  

#label results and save
rownames(yld.ag.r) = c('WFD','MMM','%Ch','Range.ch.l','Range.ch.u','CI.l','CI.u','Area')
rownames(yld.ag.s) = rownames(yld.ag.r)
colnames(yld.ag.r) = c(regions,'LAC')
colnames(yld.ag.s) = colnames(yld.ag.r)

eval(parse(text=paste(cultivos[c],".ch.r = yld.ag.r ",sep='')))
eval(parse(text=paste(cultivos[c],".ch.s = yld.ag.s ",sep='')))
eval(parse(text=paste("save(",cultivos[c],".ch.r,file='",path.res,"summaries/",cultivos[c],".ch.r.Rdat')",sep="")))  #guardar matrices
eval(parse(text=paste("save(",cultivos[c],".ch.s,file='",path.res,"summaries/",cultivos[c],".ch.s.Rdat')",sep="")))

#Mirar resultados regionales
round(yld.ag.r,2)
round(yld.ag.s,2)

#CALCULATE REGIONAL IAV
#try plotting boxplots with interannual variability by model
# have to aggregate to LAM or regions first
#look at physical area from SPAM to weight yield changes
#Calculate weighted averages by model & compare to WFD
boxplot.r = array(NA,dim=c(p-1,length(anos),6))  #initialize matrices
boxplot.s = array(NA,dim=c(p-1,length(anos),6))

#loop by region
for (t in 1:2)  {  #then by treatment
  for (r in 1:6)  {
    if (r<=5) {eval(parse(text=paste("ind.reg = which(crop_",treat[t],"$Region==regions[r] & zeros.wfd.",substr(treat[t],1,1),"<=14)",sep='')))
    }   else{
      eval(parse(text=paste("ind.reg = which(zeros.wfd.",substr(treat[t],1,1),"<=12)",sep='')))          
    }
    if (length(ind.reg)>1)  {
      for (j in 1:(p-1))  {   #loop through models
        for (y in (anos-1))  { #loop through years, fix anos-1!
          eval(parse(text=paste("ylds.y = yld_",treat[t],"[ind.reg,y,j]",sep=''))) 
          ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
          eval(parse(text=paste("areas.y = area.",treat[t],"[ind.reg,y]",sep=''))) 
          eval(parse(text=paste("boxplot.",substr(treat[t],1,1),"[j,y,r] = sum(ylds.y*areas.y)/sum(areas.y)",sep='')))    
        }
      }
    }
  }
}

dimnames(boxplot.r)[1] = list(models$Models[1:(p-1)])
dimnames(boxplot.r)[3] = list(c(regions,'LAC'))
dimnames(boxplot.s)[1] = list(models$Models[1:(p-1)])
dimnames(boxplot.s)[3] = list(c(regions,'LAC'))

par(ask=F)
if (cultivos[c]=='maiz') {ylim.c = c(2000,9000)}  #hard-code ranges of plots
if (cultivos[c]=='arroz') {ylim.c = c(0,6600)}
if (cultivos[c]=='soya') {ylim.c = c(500,1700)}
if (cultivos[c]=='trigo') {ylim.c = c(0,8000)}
if (cultivos[c]=='frijol') {ylim.c = c(0,2500)}

#Create plots of interannual variability for Latin American region
#riego
boxplot(t(boxplot.r[c(10,1:9),,6]),xaxt='n',ylim=ylim.c)
text(x=seq(1,p,1.1),y=ylim.c[1] + 50,cex=1,labels = dimnames(boxplot.r)[[1]][c(10,1:9)], xpd=TRUE, srt=40, pos=2)
title(paste('Inter-annual variability for region: ',cultivos[c],' (riego)',sep=''))

#secano
boxplot(t(boxplot.s[c(10,1:9),,6]),xaxt='n',ylim=ylim.c)
text(x=seq(1,p,1.1),y=ylim.c[1] + 50,cex=1,labels = dimnames(boxplot.s)[[1]][c(10,1:9)], xpd=TRUE, srt=40, pos=2)
title(paste('Inter-annual variability for region: ',cultivos[c],' (secano)',sep=''))

#Calculate inter-annual variability (standard dev) for regions
yld.CV.r = array(NA,dim=c(6,6))  #initialize matrices
yld.CV.s = array(NA,dim=c(6,6))
for (t in 1:2)  {
  #t=1
  for (r in 1:6)  {
  
    eval(parse(text=paste("test = sum(is.na(boxplot.",substr(treat[t],1,1),"[,1,r]))",sep='')))
    if (test==0)  {  #check for NA's in 1st year
      
      eval(parse(text=paste("yld.cv = apply(boxplot.",substr(treat[t],1,1),"[,,r],1,function(x) sd(x,na.rm=T)/mean(x,na.rm=T))*100",sep='')))
      
      eval(parse(text=paste("yld.CV.",substr(treat[t],1,1),"[1,r] = yld.cv[(p-1)]",sep='')))  #WFD
      eval(parse(text=paste("yld.CV.",substr(treat[t],1,1),"[2,r] = mean(yld.cv[1:(p-2)],na.rm=T)",sep='')))  #Multi-model mean
      
      eval(parse(text=paste("yld.CV.",substr(treat[t],1,1),"[3:4,r] = range(yld.cv[1:(p-2)],na.rm=T)",sep='')))  #range of models
      
      boot = mat.or.vec(500,1)  #bootstrap results
      for (j in 1:500)  {boot.ind = sample(1:(p-2),(p-2),replace=T)
                         boot[j] = mean(yld.cv[boot.ind])}
      eval(parse(text=paste("yld.CV.",substr(treat[t],1,1),"[5:6,r] = quantile(boot,c(.025,.975),na.rm=T)",sep='')))
    }
  }
}

dimnames(yld.CV.r)[1] = list(c('WFD','MMM','Range.ch.l','Range.ch.u','CI.l','CI.u'))
dimnames(yld.CV.r)[2] = list(c(regions,'LAC'))
dimnames(yld.CV.s)[1] = list(c('WFD','MMM','Range.ch.l','Range.ch.u','CI.l','CI.u'))
dimnames(yld.CV.s)[2] = list(c(regions,'LAC'))

#put results in data frame and save
eval(parse(text=paste(cultivos[c],".cv.r = yld.CV.r ",sep='')))
eval(parse(text=paste(cultivos[c],".cv.s = yld.CV.s ",sep='')))
eval(parse(text=paste("save(",cultivos[c],".cv.r,file='",path.res,"summaries/",cultivos[c],".cv.r.Rdat')",sep="")))
eval(parse(text=paste("save(",cultivos[c],".cv.s,file='",path.res,"summaries/",cultivos[c],".cv.s.Rdat')",sep="")))

#mirar resultados
round(yld.CV.r,2)
round(yld.CV.s,2)
round(yld.CV.r[2,] - yld.CV.r[1,],1)
round(yld.CV.s[2,] - yld.CV.s[1,],1)
