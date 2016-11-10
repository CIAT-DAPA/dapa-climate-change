#Visualize DSSAT runs

#Load libraries
library('raster')

#Load pixel id's & maps
load("Z:/08-Cells_toRun/matrices_cultivo/Maize_riego.Rdat")
load("Z:/08-Cells_toRun/matrices_cultivo/Maize_secano.Rdat")
Map_LatinAmerica<-shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")

#get list of climate models
setwd('Z:/01-climate-data/bc_0_5deg_lat/')
models = list.files()
models = c('WFD',models)
setwd('Z:/12-Resultados/')

#load, unlist and extract yield data to arrays (gridcells x years x models)
#initialize arrays
maiz_secano = array(NA,dim=c(dim(crop_secano)[1],30,24))  #23 GCM's + baseline
maiz_riego = array(NA,dim=c(dim(crop_riego)[1],30,24))

#for (m in 1:(length(models)+1))  {
for (m in c(1,2,7,12))  {  
  if(m==1)  {
    #Load & extract baseline data from list
    load('Z:/12-Resultados/Maiz/1971-1998/IB0006_IRNA_Secano.Rdat')
    Run.secano = Run
    load('Z:/12-Resultados/Maiz/1971-1998/IB0006_IRNA_Riego.Rdat')
    Run.riego = Run
  } else{    
    #Load future yields from models
    eval(parse(text=paste("load('Z:/12-Resultados/Maiz/2021-2048/IB0006_IRNA_Secano",models[m],".Rdat')",sep='')))
    Run.secano = Run
    eval(parse(text=paste("load('Z:/12-Resultados/Maiz/2021-2048/IB0006_IRNA_Riego",models[m],".Rdat')",sep='')))
    Run.riego = Run
  }
  
  #unlist everything into matrices
  secano = array(NA,dim=c(length(Run.secano),30))  #initialize arrays
  for (j in 1:length(Run.secano))  {
    secano[j,1:length(Run.secano[[j]])] = Run.secano[[j]]
  }
  riego = array(NA,dim=c(length(Run.riego),30))
  for (j in 1:length(Run.riego))  {
    riego[j,1:length(Run.riego[[j]])] = Run.riego[[j]]
  }
  
  #place results in array
  maiz_secano[,,m] = secano
  maiz_riego[,,m] = riego
  
  print(m)
}

#Graph results
data_seq = quantile(c(maiz_riego,maiz_secano),seq(0,1,.05),na.rm=T)
data_seq = c(0,data_seq[which(data_seq>0)[1]:length(data_seq)])  #get rid of repeating zeros
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))

cols = col_pal[cut(maiz_secano[,1,2],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_secano[,1:2], col = cols, pch=16, cex=0.5)
#title('Maize (secano):  Linea Base')
title(paste('Maiz (secano): ',models[2],sep=''))

cols = col_pal[cut(maiz_riego[,1,2],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_riego[,1:2], col = cols, pch=16, cex=0.5)
#title('Maize (riego):  Linea Base')
title(paste('Maiz (riego): ',models[2],sep=''))

#check for 0's and NA's
riego_zero = array(NA,dim=c(30,24))
riego_NA = array(NA,dim=c(30,24))
secano_zero = array(NA,dim=c(30,24))
secano_NA = array(NA,dim=c(30,24))
for (m in c(1,2,7,12))  {
  for (j in 1:30)  {
    riego_zero[j,m] = sum(maiz_riego[,j,m]==0)
    secano_zero[j,m] = sum(maiz_secano[,j,m]==0)
    riego_NA[j,m] = sum(is.na(maiz_riego[,j,m]))
    secano_NA[j,m] = sum(is.na(maiz_secano[,j,m]))
  }
}

