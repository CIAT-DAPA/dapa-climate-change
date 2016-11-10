#Combine N, sowing date, country information in matrices by crop & rainfed/ irrigated

#Static data
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat','Bean')
k=1

#Load SPAM matrices
eval(parse(text=paste("load('Z:/07-SPAM_data/SPAM2005/physical.area/RS/",crops[k],"_RS.Rdat')",sep='')))

#Load pixel ID's & countries
load('Z:/14-ObjectsR/id_coordinates.Rdat')

#Match together
ind_ID = match(paste(round(crop_RS[,'x'],2),round(crop_RS[,'y'],2)),paste(round(id_pixel[,'x'],2),round(id_pixel[,'y'],2)))
crop_RS$country = id_pixel[ind_ID,'country']
crop_RS$ID = id_pixel[ind_ID,'ID']

#Separate rainfed & irrigated (eliminating rows without area or sowing dates)
ind_secano = which(is.na(crop_RS$ID)==F & crop_RS$secano.area>0 & is.na(crop_RS$mirca.rf.start)==F)
ind_riego = which(is.na(crop_RS$ID)==F & crop_RS$riego.area>0 & is.na(crop_RS$mirca.irr.start)==F)
crop_secano = crop_RS[ind_secano,c('x','y','ID','country','secano.area','total.area','secano.pct','mirca.rf.start','mirca.rf.end','N.app.rf')]
crop_riego = crop_RS[ind_riego,c('x','y','ID','country','riego.area','total.area','riego.pct','mirca.irr.start','mirca.irr.end','N.app.irr')]
colnames(crop_secano)[8:10] = c('mirca.start','mirca.end','N.app')
colnames(crop_riego)[8:10] = c('mirca.start','mirca.end','N.app')

#Create fields with N applications (TO DO)
if (k==1)  {  #Maize
  crop_secano[c('N.app.0d','N.app.40d')] <- NA  #initialize fields
  crop_riego[c('N.app.0d','N.app.40d')] <- NA
  
  ind_low = which(crop_secano$N.app<120)
  ind_high = which(crop_secano$N.app>=120)
  crop_secano$N.app.0d[ind_low] = crop_secano$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_secano$N.app.0d[ind_high] = crop_secano$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_secano$N.app.40d[ind_high] = crop_secano$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
  
  ind_low = which(crop_riego$N.app<120)
  ind_high = which(crop_riego$N.app>=120)
  crop_riego$N.app.0d[ind_low] = crop_riego$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_riego$N.app.0d[ind_high] = crop_riego$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_riego$N.app.40d[ind_high] = crop_riego$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
}

if (k==2)  {  #Potato 
  crop_secano[c('N.app.0d','N.app.35d')] <- NA  #initialize fields
  crop_riego[c('N.app.0d','N.app.35d')] <- NA
  
  ind_low = which(crop_secano$N.app<75)
  ind_high = which(crop_secano$N.app>=75)
  crop_secano$N.app.0d[ind_low] = crop_secano$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_secano$N.app.0d[ind_high] = crop_secano$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_secano$N.app.35d[ind_high] = crop_secano$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
  
  ind_low = which(crop_riego$N.app<75)
  ind_high = which(crop_riego$N.app>=75)
  crop_riego$N.app.0d[ind_low] = crop_riego$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_riego$N.app.0d[ind_high] = crop_riego$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_riego$N.app.35d[ind_high] = crop_riego$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
  
  }

if (k==3)  {  #Rice
  crop_secano[c('N.app.0d','N.app.30d')] <- NA  #initialize fields
  crop_riego[c('N.app.0d','N.app.30d')] <- NA
  
  ind_low = which(crop_secano$N.app<75)
  ind_high = which(crop_secano$N.app>=75)
  crop_secano$N.app.0d[ind_low] = crop_secano$N.app[ind_low]  #100% at 30 DDS for low-input
  crop_secano$N.app.0d[ind_high] = crop_secano$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_secano$N.app.30d[ind_high] = crop_secano$N.app[ind_high]*0.5 #50% at 30 DDS for high-input
  
  ind_low = which(crop_riego$N.app<75)
  ind_high = which(crop_riego$N.app>=75)
  crop_riego$N.app.0d[ind_low] = crop_riego$N.app[ind_low]  #100% at 30 DDS for low-input
  crop_riego$N.app.0d[ind_high] = crop_riego$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_riego$N.app.30d[ind_high] = crop_riego$N.app[ind_high]*0.5 #50% at 30 DDS for high-input
}

if (k==4)  {  #Soybean (DOUBLE-CHECK THIS, ESP. FOR RIEGO)
  crop_secano[c('N.app.21d')] <- NA  #initialize fields
  crop_riego[c('N.app.21d')] <- NA
  
  crop_secano$N.app.21d = crop_secano$N.app  #100% at 0 DDS 
  crop_riego$N.app.21d = crop_riego$N.app  #100% at 0 DDS 
}

#Eliminate original N.app columns
crop_secano$N.app = NULL
crop_riego$N.app = NULL

#Add row numbers from climate data
load("Z:/14-ObjectsR/coordenadas.RDat")
Coincidencias <- match(paste(round(crop_riego[,"x"],2),round(crop_riego[,"y"],2)),paste(round(Coordenadas[,"x"],2),round(Coordenadas[,"y"],2)))
crop_riego <- data.frame(crop_riego,Coincidencias)
Coincidencias <- match(paste(round(crop_secano[,"x"],2),round(crop_secano[,"y"],2)),paste(round(Coordenadas[,"x"],2),round(Coordenadas[,"y"],2)))
crop_secano <- data.frame(crop_secano,Coincidencias)

#Save results
eval(parse(text=paste("save(crop_secano,file='Z:/08-Cells_toRun/matrices_cultivo/",crops[k],"_secano.Rdat')",sep='')))
eval(parse(text=paste("save(crop_riego,file='Z:/08-Cells_toRun/matrices_cultivo/",crops[k],"_riego.Rdat')",sep='')))

#Plot remaining points
library('raster')
Map_LatinAmerica<-shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")
par(mar=c(2.1,2.1,2.1,1.1))

data_seq = quantile(c(crop_riego$riego.area,crop_secano$secano.area),seq(0,1,.05),na.rm=T)
data_seq = c(0,data_seq[which(data_seq>0)[1]:length(data_seq)])  #get rid of repeating zeros
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))

ind.good = which(crop_riego$riego.area>0)
cols = col_pal[cut(crop_riego$riego.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_riego[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Irrigated pixels')

ind.good = which(crop_secano$secano.area>0)
cols = col_pal[cut(crop_secano$secano.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_secano[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Rainfed pixels')