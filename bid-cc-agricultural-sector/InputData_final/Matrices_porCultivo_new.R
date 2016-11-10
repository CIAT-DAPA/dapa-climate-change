#Combine N, sowing date, country information in matrices by crop & rainfed/ irrigated

#load libraries
library(maptools)
library(sp)

#static data
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat','Bean')
cultivos = c('maiz','papa','arroz','soya','trigo','frijol')
k=3
treat = c('riego','secano')
regions = c('AND','BRA','CEN','MEX','SUR')
path <- "Z:/"

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

#Create fields with N applications
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

if (k==4)  {  #Soybean 
  crop_secano[c('N.app.0d')] <- NA  #initialize fields
  crop_riego[c('N.app.0d')] <- NA
  
  crop_secano$N.app.0d = crop_secano$N.app  #100% at 0 DDS 
  crop_riego$N.app.0d = crop_riego$N.app  #100% at 0 DDS 
}

if (k==5)  {  #Wheat
  crop_secano[c('N.app.0d','N.app.30d')] <- NA  #initialize fields
  crop_riego[c('N.app.0d','N.app.30d')] <- NA
  
  ind_low = which(crop_secano$N.app<120)
  ind_high = which(crop_secano$N.app>=120)
  crop_secano$N.app.0d[ind_low] = crop_secano$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_secano$N.app.0d[ind_high] = crop_secano$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_secano$N.app.30d[ind_high] = crop_secano$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
  
  ind_low = which(crop_riego$N.app<120)
  ind_high = which(crop_riego$N.app>=120)
  crop_riego$N.app.0d[ind_low] = crop_riego$N.app[ind_low]  #100% at 0 DDS for low-input
  crop_riego$N.app.0d[ind_high] = crop_riego$N.app[ind_high]*0.5  #50% at 0 DDS for high-input
  crop_riego$N.app.30d[ind_high] = crop_riego$N.app[ind_high]*0.5 #50% at 40 DDS for high-input
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

#Add FPU
# Load FPU shape file
worldFPU <- readShapePoly(fn="Z:/15_FPUs/fpu_shp/fpu.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

for (t in 1:2)  {  #loop through treatments
  # Organizing data to identify FPU categories for all coordinates
  if (t==1)  {crop = crop_riego}  else{crop = crop_secano}
  xy <- cbind(crop$x, crop$y)
  occ <- SpatialPoints(xy); rm(xy)
  proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # Extract FPU categories
  overFPU <- sp::over(occ,worldFPU)
  overFPU <- overFPU[,c("New_FPU","NFPU_INT","New_Basin","Basin_Name")]
  
  # Create new file with FPU categories
  crop <- cbind(crop,overFPU)
  if (t==1)  {crop_riego = crop}  else{crop_secano = crop}
}

#Add regions
#Assign missing FPU's (on coastlines)
na.riego = which(is.na(crop_riego$New_FPU))
na.secano = which(is.na(crop_secano$New_FPU))

#Fix coastline cells manually
crop_riego[which(round(crop_riego$x,2)==-97.75 & round(crop_riego$y,2)==23.25),'New_FPU'] = 'MIM_MEX'
crop_riego[which(round(crop_riego$x,2)==-105.25 & round(crop_riego$y,2)==21.25),'New_FPU'] = 'MIM_MEX'
crop_riego[which(round(crop_riego$x,2)==-88.25 & round(crop_riego$y,2)==15.75),'New_FPU'] = 'GTM_GTM'
crop_riego[which(round(crop_riego$x,2)==-71.25 & round(crop_riego$y,2)==9.25),'New_FPU'] = 'ORI_VEN'
crop_riego[which(round(crop_riego$x,2)==-59.25 & round(crop_riego$y,2)==-38.75),'New_FPU'] = 'SAL_ARG'

crop_secano[which(round(crop_secano$x,2)==-97.75 & round(crop_secano$y,2)==23.25),'New_FPU'] = 'MIM_MEX'
crop_secano[which(round(crop_secano$x,2)==-105.25 & round(crop_secano$y,2)==21.25),'New_FPU'] = 'MIM_MEX'
crop_secano[which(round(crop_secano$x,2)==-88.25 & round(crop_secano$y,2)==15.75),'New_FPU'] = 'GTM_GTM'
crop_secano[which(round(crop_secano$x,2)==-65.75 & round(crop_secano$y,2)==10.25),'New_FPU'] = 'ORI_VEN'
crop_secano[which(round(crop_secano$x,2)==-71.25 & round(crop_secano$y,2)==9.25),'New_FPU'] = 'ORI_VEN'
crop_secano[which(round(crop_secano$x,2)==-59.25 & round(crop_secano$y,2)==-38.75),'New_FPU'] = 'SAL_ARG'

#Add region classification
regiones = list(MEX=c('MIM_MEX','RIG_MEX','UME_MEX'),
                CEN=c('YUC_MEX','GTM_GTM','BLZ_BLZ','SLV_SLV','HND_HND','NIC_NIC','CRI_CRI','PAN_PAN','CRB_CRB','DOM_DOM','HTI_HTI','CUB_CUB','JAM_JAM'),
                AND=c('AMA_BOL','PAR_BOL','AMA_COL','NWS_COL','ORI_COL','AMA_ECU','NWS_ECU','AMA_PER','PEC_PER','ORI_VEN'),
                BRA=c('AMA_BRA','NEB_BRA','SAN_BRA','TOC_BRA','PAR_BRA','GSA_GSA','RVE_VEN'),
                SUR=c('PAR_ARG','RIC_ARG','SAL_ARG','TIE_ARG','URU_BRA','CHC_CHL','PAR_PRY','URU_URY')
)

#add region column to data frames
crop_riego$Region = NA
crop_secano$Region = NA

for (j in 1:5)  {  #loop through regions
  for (t in 1:2)  {
    eval(parse(text=paste("reg.T = crop_",treat[t],"$New_FPU %in% regiones[[",j,"]]",sep="")))
    eval(parse(text=paste("crop_",treat[t],"[which(reg.T==T),'Region'] = names(regiones)[",j,"]",sep="")))
  }
}

#LOOK FOR PIXELS OUTSIDE THESE REGIONS & DISCARD
ind.bad.r = which(is.na(crop_riego$Region))
ind.bad.s = which(is.na(crop_secano$Region))
if (length(ind.bad.r)>0)  {crop_riego = crop_riego[-ind.bad.r,]}
if (length(ind.bad.s)>0)  {crop_secano = crop_secano[-ind.bad.s,]}

#Add elevation
load('Z:/01-climate-data/elevation/elevation_LAM.rdat')
ind.elev.r = match(paste(round(crop_riego$x,2),round(crop_riego$y,2)),paste(round(elev.full[,1],2),round(elev.full[,2],2)))
ind.elev.s = match(paste(round(crop_secano$x,2),round(crop_secano$y,2)),paste(round(elev.full[,1],2),round(elev.full[,2],2)))
crop_riego$elev = elev.full[ind.elev.r,3]
crop_secano$elev = elev.full[ind.elev.s,3]

#Add varieties por region
#cargar variedades
# variedades.riego = read.csv('Z:/_documentos/Cultivares_porRegion2_R_riego.csv',header=T,stringsAsFactors=F)
# variedades.secano = read.csv('Z:/_documentos/Cultivares_porRegion2_R_secano.csv',header=T,stringsAsFactors=F)
# 
# for (t in 1:2)  {  #loop through treatments
#   
#   ind.var = grep(crops[k],variedades.riego$cultivares,ignore.case=T)  #filas para cultivo c, asumir mismo orden para riego y secano
#   
#   #Crear columnas con variedad
#   eval(parse(text=paste("crop_",treat[t],"$variedad.1 = NA",sep="")))
#   eval(parse(text=paste("crop_",treat[t],"$variedad.2 = NA",sep="")))
#   eval(parse(text=paste("crop_",treat[t],"$variedad.3 = NA",sep="")))
#   
#   for (r in 1:5)  {  #loop through regions
#     eval(parse(text=paste("crop_",treat[t],"$variedad.1[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[1],regions[r]]",sep="")))
#     eval(parse(text=paste("crop_",treat[t],"$variedad.2[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[2],regions[r]]",sep="")))
#     eval(parse(text=paste("crop_",treat[t],"$variedad.3[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[3],regions[r]]",sep="")))
#   }
# }

#Agregar variedades por altitud/ latitud
variedades.alt = read.csv('Z:/_documentos/Cultivares_porAltitud.csv',header=T,stringsAsFactors=F)
crop_secano$variedad.1 = NA
crop_riego$variedad.1 = NA
for (t in 1:2) {  #riego/ secano
  ind.v = which(variedades.alt$Cultivo==cultivos[k]&variedades.alt$Treat==treat[t])
  var.alt = variedades.alt[ind.v,]
  for (j in 1:7)  {  #bandas de elevación
    for (l in 1:5) {  #bandas de latitud
      lat.lower = as.numeric(substr(colnames(var.alt)[l+4],2,3))
      lat.upper = as.numeric(substr(colnames(var.alt)[l+4],5,6))
      eval(parse(text=paste("ind.pixel = which(abs(crop_",treat[t],"$y)>=lat.lower & abs(crop_",treat[t],"$y)<lat.upper & crop_",treat[t],"$elev>=var.alt[j,'Elev.l'] & crop_",treat[t],"$elev<=var.alt[j,'Elev.u'])",sep="")))
      print(paste(treat[t],lat.lower,lat.upper,var.alt[j,'Elev.l'],var.alt[j,'Elev.u'],length(ind.pixel)))  #mirar cuantos pixeles en cada categoria
      if (length(ind.pixel)>0)  {
        eval(parse(text=paste("crop_",treat[t],"$variedad.1[ind.pixel] = var.alt[j,l+4]",sep="")))
      }  
    } 
  }
}

#Descartar filas afuera de 45N a 45S o faltando N.app
ind.r = which(crop_riego$y<(-45)|crop_riego$y>45|is.na(crop_riego$N.app.0d))
ind.s = which(crop_secano$y<(-45)|crop_secano$y>45|is.na(crop_secano$N.app.0d))
if (length(ind.r)>0) {crop_riego = crop_riego[-ind.r,]}
if (length(ind.s)>0) {crop_secano = crop_secano[-ind.s,]}

#Ajustar fechas de siembra "manualmente" para maiz en Ecuador
pixels.EC = read.csv('Z:/08-Cells_toRun/matrices_cultivo/maiz_Ecuador_fechas.csv',header=T)
if (k==1) {  #maiz en Ecuador
  ind.EC = match(paste(pixels.EC[,1],pixels.EC[,2]),paste(crop_secano[,1],crop_secano[,2]))
  crop_secano[ind.EC,'mirca.start'] = crop_secano[ind.EC,'mirca.start'] - 60
}

#save results
eval(parse(text=paste('save(crop_riego,file="',path,'08-Cells_toRun/matrices_cultivo/',crops[k],'_riego.Rdat")',sep='')))
eval(parse(text=paste('save(crop_secano,file="',path,'08-Cells_toRun/matrices_cultivo/',crops[k],'_secano.Rdat")',sep='')))

# crop_secano = crop_secano[ind.EC,]
# eval(parse(text=paste('save(crop_secano,file="',path,'08-Cells_toRun/matrices_cultivo/',crops[k],'_secano.EC.Rdat")',sep='')))
