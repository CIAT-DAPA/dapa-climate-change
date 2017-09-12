#Look at % difference in yield between top 2 yielders

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
#c=4  #frijol
c=5 #trigo
# material = 'M-6101'

#fijar variedad
#variedades = c('LOW.TEMP','IR72','IR8')  #arroz
#variedades = c('INRA','H6','FM6') #maiz
#variedades = c('PIO9202','MSMATGROUP','DonMario')  #soya
#variedades = c('ICTAOstua','Carioca','A193','Manitou')  #frijol
variedades = c('ChinaBBA','KauzBA','AVALON')

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory D:/tobackup/BID/Resultados_DSSAT/
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
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,(p-1),length(variedades)))  #pixels x years x models x varieties
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,(p-1),length(variedades)))

for (v in 1:length(variedades))  {
  for (m in c(10))  {  #fija indices de modelos para incluir
    
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
    secano = array(NA,dim=c(length(Run.secano),30))  #initialize arrays
    for (j in 1:length(Run.secano))  {
      if (is.null(dim(Run.secano[[j]])))  {
        secano[j,] = 0
      }  else{
        if (m==10) {
          ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-1970
        }  else {
          ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-2020
        }
        
        secano[j,ind.s] = Run.secano[[j]][,'HWAH']
      }
    }
    riego = array(NA,dim=c(length(Run.riego),30))
    for (j in 1:length(Run.riego))  {
      if (is.null(dim(Run.riego[[j]])))  {
        riego[j,] = 0
      }  else{
        if (m==10) {
          ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-1970
        }  else {
          ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-2020
        }
        riego[j,ind.r] = Run.riego[[j]][,'HWAH']
      }
    }
    
    #place results in array
    yld_secano[,,m,v] = secano
    yld_riego[,,m,v] = riego
    
  }
}

#check data points in each year
apply(yld_secano[,,10,1],2,function(x) sum(is.na(x)==F))

#Quitar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,,]
yld_secano[yld_secano==-99] = 0  #re-emplazar -99 con 0 for legitimate crop failures
yld_riego[yld_riego==-99] = 0

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego[,,(p-1),],c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano[,,(p-1),],c(1,3),mean,na.rm=T)

#sort top yielders
wfd.r.sort = t(apply(wfd.r,1,function(x) order(x,decreasing=T)))  #columns are 1st, 2nd, 3rd rank
wfd.s.sort = t(apply(wfd.s,1,function(x) order(x,decreasing=T)))

#Select yields of top 2 yielders
wfd.s.2 = array(NA,dim=c(dim(crop_secano)[1],2))  #pixels x 2 varieties
wfd.r.2 = array(NA,dim=c(dim(crop_riego)[1],2))
for (j in 1:2)  {  #top & 2nd yielders
  for (k in 1:length(variedades))  {  #by varieties
    wfd.s.2[wfd.s.sort[,j]==k,j] = wfd.s[wfd.s.sort[,j]==k,k]
    wfd.r.2[wfd.r.sort[,j]==k,j] = wfd.r[wfd.r.sort[,j]==k,k]
  }
}

#Calculate % diff in yield between top 2 yielders (benefit provided by top yielder compared to next nearest)
diff.s = (wfd.s.2[,1] - wfd.s.2[,2])/wfd.s.2[,2]*100
diff.r = (wfd.r.2[,1] - wfd.r.2[,2])/wfd.r.2[,2]*100

quantile(diff.s,c(.01,.05,.25,.5,.75,.95,.99),na.rm=T)
quantile(diff.r,c(.01,.05,.25,.5,.75,.95,.99),na.rm=T)

#map these differences
limits2 = quantile(c(diff.r,diff.s),c(.05,.95),na.rm=T)  
for (t in 1:2) {
  eval(parse(text=paste('diff = diff.',substr(treat[t],1,1),sep='')))
  diff[diff=='Inf'] = NA
  diff[diff<limits2[1]] = limits2[1]  #reset end points
  diff[diff>limits2[2]] = limits2[2]
  eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],diff=diff)",sep='')))
  color_scale = colorRampPalette(c('white','orange','orangered','red'), space="rgb")(12)
  labs2 = ''
  y=ggplot() +
    geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="gray90", fill="gray70" )+
    geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
    coord_equal() +
    geom_raster(data=df, aes(x=Long, y=Lat,fill=diff))+
    ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n','% Diff b/w top yielders',sep=''))+
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
  ggsave(filename=paste(path.root,"resultados_graficas/",cultivos[c],"_",treat[t],"_diff_top2yielders_.png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)  #_documentos/graficas
}
