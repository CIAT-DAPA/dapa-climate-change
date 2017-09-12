#Comparar resultados WFD con SPAM

#load libraries
library('raster')
library('ncdf4')

#fijar cultivo aquí
 c=1  #maize
 variedades = c('INRA','H6','FM6') #maiz
# c=2  #rice
# variedades = c('IR72','LOW.TEMP','IR8')  #arroz
#c=3  #soy
#variedades = c('DonMario','PIO9202','MSMATGROUP')
#c=4  #frijol
#c=5 #wheat

#Set paths
# path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory D:/tobackup/BID/Resultados_DSSAT/
# path.root = "D:/tobackup/BID/"; #'Z:/'  #root path
path.res = "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/12-Resultados/"  #results directory
path.root = "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"   #root path
carp.res.riego = 'Maiz-3cultivars_withCO2'
carp.res.secano = 'Maiz-3cultivars_withCO2'

#otros datos fijos
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
anos = 1:28  #fijar años para analizar aquí

#Load pixel id's
# eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))  #08-Cells_toRun/
# eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#Load mapas de Latinoamerica
eval(parse(text=paste('Map_LatinAmerica<-shapefile("',path.root,'Latino_America.shp")',sep='')))  #03-Map_LatinAmerica/
#eval(parse(text=paste('Map_LatinAmerica<-shapefile("',path.root,'03-Map_LatinAmerica/Latino_America.shp")',sep='')))

#Cargar WFD para las variedades y promediar todos los años
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,length(variedades)))  #pixels x years x models x varieties
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,length(variedades)))

for (v in 1:length(variedades))  {
    m = 10
    print(m)
    
    #Load & extract baseline data from list
    if (m<=9) {
      load(paste(path.res,cultivos[c],'/',carp.res.secano,'/_',cultivos[c],'_Secano_',variedades[v],'_',models[m,],'_.Rdat',sep=''))
    }  else {load(paste(path.res,cultivos[c],'/',carp.res.secano,'/old/_',cultivos[c],'_Secano_',variedades[v],'_WFD.Rdat',sep=''))}
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
    yld_secano[,,v] = secano
    yld_riego[,,v] = riego
    
}

#optimize variety
#Quitar los años sin datos climáticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones técnicas!
yld_secano = yld_secano[,anos,]  #incluir los indices de los años "buenos" aquí con datos por todos los pixeles
yld_riego = yld_riego[,anos,]
yld_secano[yld_secano==-99] = 0  #re-emplazar -99 con 0 for legitimate crop failures
yld_riego[yld_riego==-99] = 0

#Identify best variety in historical baseline (higher mean yields & less crop failures)
#across all 3 varieties
wfd.r = apply(yld_riego,c(1,3),mean,na.rm=T)  #multi-annual means
wfd.s = apply(yld_secano,c(1,3),mean,na.rm=T)
thresh = mean(yld_secano,na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
wfd.fail.r = apply(yld_riego,c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
wfd.fail.s = apply(yld_secano,c(1,3),function(x) sum(x<thresh,na.rm=T))

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
  habit = read.table(paste("'",path.root,"_documentos/Bean_growthHabit.csv'",sep=""),header=T,sep=',')
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

#Select yield for best variety for every pixel 
secano = array(NA,dim=c(dim(crop_secano)[1],length(anos)))  #pixels x years x models 
riego = array(NA,dim=c(dim(crop_riego)[1],length(anos)))
secano[wfd.s.best==1,] = yld_secano[wfd.s.best==1,,1]
secano[wfd.s.best==2,] = yld_secano[wfd.s.best==2,,2]
secano[wfd.s.best==3,] = yld_secano[wfd.s.best==3,,3]
riego[wfd.r.best==1,] = yld_riego[wfd.r.best==1,,1]
riego[wfd.r.best==2,] = yld_riego[wfd.r.best==2,,2]
riego[wfd.r.best==3,] = yld_riego[wfd.r.best==3,,3]

# #filtrar pixeles con muchas fallas en WFD
# riego.fallas = apply(riego,1,function(x) sum(is.na(x)|x==0))
# secano.fallas = apply(secano,1,function(x) sum(is.na(x)|x==0))
# ind.bad.r = which(riego.fallas>12)
# ind.bad.s = which(secano.fallas>12)
# if (length(ind.bad.r)>0) {riego = riego[-ind.bad.r,]
#                           crop_riego2 = crop_riego  #make backup of original
#                           crop_riego = crop_riego[-ind.bad.r,]}
# if (length(ind.bad.s)>0) {secano = secano[-ind.bad.s,]
#                           crop_secano2 = crop_secano  #make backup of original
#                           crop_secano = crop_secano[-ind.bad.s,]}

#Promedios interanuales con y sin ceros/ fallas
secano1 = secano
secano1[secano1==-99] = 0  #re-emplazar -99 con 0
riego1 = riego
riego1[riego1==-99] = 0
secano.WFD.0 = apply(secano1,1,function(x) mean(x,na.rm=T))  #includes crop failures
riego.WFD.0 = apply(riego1,1,function(x) mean(x,na.rm=T))

secano2 = secano
secano2[secano2==-99|secano2==0] = NA  #re-emplazar -99 con NA
riego2 = riego
riego2[riego2==-99|riego2==0] = NA
secano.WFD.NA = apply(secano2,1,function(x) mean(x,na.rm=T))  #excludes crop failures
riego.WFD.NA = apply(riego2,1,function(x) mean(x,na.rm=T))

#Cargar SPAM, agregar a 0.5 grados y cortar pixeles de interes
#Load SPAM2005 rasters
eval(parse(text=paste("riego.SPAM = raster('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/yield/spam2005v2r0_yield_",cultivos.en[c],"_irrigated.nc')",sep='')))
eval(parse(text=paste("secano.SPAM = raster('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/yield/spam2005v2r0_yield_",cultivos.en[c],"_rainfed.nc')",sep='')))
eval(parse(text=paste("riego.phys.area = raster('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_",cultivos.en[c],"_irrigated.nc')",sep='')))
eval(parse(text=paste("secano.phys.area = raster('/mnt/workspace_cluster_3/bid-cc-agricultural-sector/07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_",cultivos.en[c],"_rainfed.nc')",sep='')))

#calculate production
riego.prod = riego.SPAM * riego.phys.area
secano.prod = secano.SPAM * secano.phys.area

#aggregate from 5-arc minute (or 1/12 degree) to half degree
riego.prod.p5 = aggregate(riego.prod, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
secano.prod.p5 = aggregate(secano.prod, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
riego.area.p5 = aggregate(riego.phys.area, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
secano.area.p5 = aggregate(secano.phys.area, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)

#derive yield as production/area
riego.yld.p5 = riego.prod.p5/riego.area.p5
secano.yld.p5 = secano.prod.p5/secano.area.p5

# #hacer graficas de Latinoamerica
# riego.yld.p5.LAM = mask(riego.yld.p5,Map_LatinAmerica)

#Pull out points in SPAM rasters
riego.pts.p5 = rasterToPoints(riego.yld.p5)  
secano.pts.p5 = rasterToPoints(secano.yld.p5)  

#Cut out coordinates in Latin America by crop

#match to SPAM data
ind_LAM_riego = match(paste(crop_riego$x,crop_riego$y),paste(round(riego.pts.p5[,'x'],2),round(riego.pts.p5[,'y'],2)))
riego.pts.LAM = riego.pts.p5[ind_LAM_riego,]

ind_LAM_secano = match(paste(crop_secano$x,crop_secano$y),paste(round(secano.pts.p5[,'x'],2),round(secano.pts.p5[,'y'],2)))
secano.pts.LAM = secano.pts.p5[ind_LAM_secano,]

#Calcular la correlación espacial
cor(secano.WFD.NA,secano.pts.LAM[,3],use='pairwise.complete.obs')
#cor(secano.WFD.0,secano.pts.LAM[,3],use='pairwise.complete.obs')
cor(secano.WFD.NA,secano.pts.LAM[,3],use='pairwise.complete.obs',method='kendall')
cor(secano.WFD.NA,secano.pts.LAM[,3],use='pairwise.complete.obs',method='spearman')
png(filename=paste(path.root,'_documentos/graficas/',cultivos[c],'_',treat[2],'.cor.SPAM_optimalVariety.png',sep=''),width=5, height=5, res=400,units='in')
plot(secano.WFD.NA,secano.pts.LAM[,3])
lines(1:20000,1:20000)
text(max(secano.WFD.NA,na.rm=T)-500,max(secano.pts.LAM[,3],na.rm=T)-100,paste('Corr =',round(cor(secano.WFD.NA,secano.pts.LAM[,3],use='pairwise.complete.obs'),2)),cex=0.8)
dev.off()

cor(riego.WFD.NA,riego.pts.LAM[,3],use='pairwise.complete.obs')
#cor(riego.WFD.0,riego.pts.LAM[,3],use='pairwise.complete.obs')
cor(riego.WFD.NA,riego.pts.LAM[,3],use='pairwise.complete.obs',method='kendall')
cor(riego.WFD.NA,riego.pts.LAM[,3],use='pairwise.complete.obs',method='spearman')
png(filename=paste(path.root,'_documentos/graficas/',cultivos[c],'_',treat[1],'.cor.SPAM_optimalVariety.png',sep=''),width=5, height=5, res=400,units='in')
plot(riego.WFD.NA,riego.pts.LAM[,3])
lines(1:20000,1:20000)
text(max(riego.WFD.NA,na.rm=T)-500,max(riego.pts.LAM[,3],na.rm=T)-100,paste('Corr =',round(cor(riego.WFD.NA,riego.pts.LAM[,3],use='pairwise.complete.obs'),2)),cex=0.8)
dev.off()

#calcular desviaciones estanderes
sd(secano.WFD.NA,na.rm=T)/mean(secano.WFD.NA,na.rm=T)*100  #WFD
sd(secano.pts.LAM[,3],na.rm=T)/mean(secano.pts.LAM[,3],na.rm=T)*100  #SPAM

sd(riego.WFD.NA,na.rm=T)/mean(riego.WFD.NA,na.rm=T)*100
sd(riego.pts.LAM[,3],na.rm=T)/mean(riego.pts.LAM[,3],na.rm=T)*100
