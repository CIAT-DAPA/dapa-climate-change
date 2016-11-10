## Code's Jeison based en Code's Sharon Gourdji
## July 2015
## Generate planting dates for rainfed and irrigated frijol
## Packages necessary

library(raster)
library(ncdf4)
library(rgdal)
library(ggplot2)
library(ncdf4)
library(RColorBrewer)  #next option
library(lattice)
library(maptools)
library(sp)

path <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"  #Z:/ for Windows; /mnt/workspace_cluster_3/bid-cc-agricultural-sector/

Map_LatinAmerica <- shapefile(paste0(path, "03-Map_LatinAmerica/Latino_America.shp"))  ## Map Latin America

#Set static variables
cultivos = c('Maize','Bean','Potato','Rice','Soybean','Wheat')
vars = c('area','prod')
cultivos[2]
c <- 2
cultivos[c]  ## Cultivo para frijol
treat = c('riego','secano')
regions = c('AND','BRA','CEN','MEX','SUR')

#crops
crops = 'Bean'
crops2 = 'Bean'


#Load fechas de siembra de EcoCrop
  eval(parse(text = paste0('crop_plant<-raster(', "'", path, "11-EcoCrop_runs/bean-EcoCrop_fechasSiembra/", "common_bean_mode_forest_exc_gsrain.tif", "'", ")")))
  plot(crop_plant)
  #extent(crop_plant) <- extent(prod.LA.5)
 #plot(stack(crop_plant, prod.LA.5))

  crop_plant <- round(crop_plant)
  meses <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30)
  acumulado <- cumsum(meses)
  dia_siembra <- acumulado - 15
   
  
  ## Cambiar a dias la plantacion (dia juliano)
  crop_plant[crop_plant==0] <- dia_siembra[1]
  crop_plant[crop_plant==1] <- dia_siembra[2]
  crop_plant[crop_plant==2] <- dia_siembra[3]
  crop_plant[crop_plant==3] <- dia_siembra[4]
  crop_plant[crop_plant==4] <- dia_siembra[5]
  crop_plant[crop_plant==5] <- dia_siembra[6]
  crop_plant[crop_plant==6] <- dia_siembra[7]
  crop_plant[crop_plant==7] <- dia_siembra[8]
  crop_plant[crop_plant==8] <- dia_siembra[9]
  crop_plant[crop_plant==9] <- dia_siembra[10]
  crop_plant[crop_plant==10] <- dia_siembra[11]
  crop_plant[crop_plant==11] <- dia_siembra[12]
  
  ## Cargar areas de SPAM para secano y riego; se puede filtrar tanto para area como para rendimiento o Produccion
#plot(crop_plant)

for (t in 1:2)  {
  if (t==1) {   #riego
    bean_riego <- raster(paste0(path, "07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_bean_irrigated.nc"), level = 1)
  }  else {  #secano
    bean_secano <- raster(paste0(path, "07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_bean_rainfed.nc"), level = 1)
  }
  
  ## Agregar a la resolucion del 0.5 grados
  eval(parse(text=paste('area.LA.5 = aggregate(bean_',treat[t],', fact=6, fun=sum, expand=TRUE, na.rm=TRUE)',sep='')))  #porque los centros no estan en .25/.75 grados??
  
  #Crop to Latin American region
  area.LA.5 = crop(area.LA.5,Map_LatinAmerica)
    
  #Ordenar Area & extraer celdas que contribuyen a 95% de area
  pos_=which(area.LA.5[]!="NA")   ## Posiciones donde se Encuentran los valores
  valores=area.LA.5[][pos_]                     ## Valores sin NA del Raster
  Base=cbind(valores,pos_)                        ## Matriz de valores asociadas con las posciones en el raster   
  valores.sort=Base[order(Base[,1],decreasing = T),]   ## Se ordenan los valores de mayor a menor
  valores.sort2 = cumsum(valores.sort[,1])         ## Suma acumulada de los valores de produccion
  
  Cortar=which(valores.sort2<sum(valores)*0.95)   #dentro de 95% de area sembrada
  Cortar2=which(valores.sort2>sum(valores)*0.95)  #arriba de 95%
  
  valores.sort[Cortar2,1]<-NA
  
  area.LA.5[][valores.sort[,2]]<-valores.sort[,1]
  
  ##area.LA.5 <- mask(area.LA.5, Map_LatinAmerica)
  area.La.coord <- rasterToPoints(area.LA.5)
  #area.La.coord <- data.frame(area.La.coord, add_areas)
  colnames(area.La.coord) <- c("x", "y", "area")
  
  area.La.coord <- round(area.La.coord, 2)
  EcoCrop <- rasterToPoints(crop_plant)
  EcoCrop <- round(EcoCrop, 2)
  ind.cal.bean = match(paste(area.La.coord[,'x'], area.La.coord[,'y'],sep=''), paste(EcoCrop[,'x'],EcoCrop[,'y'],sep=''))
  ## head(area.La.coord[which(!is.na(ind.cal.bean.secano)),])
  
  eval(parse(text=paste('crop_',treat[t],' <- EcoCrop[ind.cal.bean, ]',sep='')))
  eval(parse(text=paste('crop_',treat[t],'<- na.omit(crop_',treat[t],')',sep='')))
  eval(parse(text=paste('crop_',treat[t],' <- data.frame(crop_',treat[t],')',sep='')))
  eval(parse(text=paste('crop_',treat[t],' <- merge(crop_',treat[t],', area.La.coord, by = c("x", "y"))',sep='')))
  
}

#actualizar nombres de columnas
crop_riego$mirca.start = crop_riego$layer 
crop_secano$mirca.start = crop_secano$layer
crop_riego$riego.area = crop_riego$area
crop_secano$secano.area = crop_secano$area
crop_riego = subset(crop_riego, select=-c(layer,area))
crop_secano = subset(crop_secano, select=-c(layer,area))

# #Make the points a dataframe for ggplot
# df <- data.frame(crop_secano)
# colnames(df) <- c("Longitude", "Latitude", "Plant", "Area")
# 
# Map_LatinAmerica1<- fortify(Map_LatinAmerica)
# ggplot() +
#   geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
#   geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
#   coord_equal() +
#   geom_raster(data=df, aes(x=Longitude, y=Latitude, fill = Plant))+
#   ggtitle("Dias de Siembra Secano para aquellos que cuenten con el 95% de las areas total")+
#   scale_fill_gradient2(low = "yellow", mid = "darkgreen", high = "darkred")

### Agregar fertilizantes
#Load GGCMI fertilizer data
crop_riego$N.app.0d = 60
crop_secano$N.app.0d = 30

### Agregar FPU's
fpu.file = paste(path,"15_FPUs/fpu_shp/fpu.shp",sep="")
worldFPU <- readShapePoly(fn=fpu.file,proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
## Objeto que contiene las coordenadas
# coord_climate <- raster(paste(path,'01-climate-data/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_11.nc',sep=''))   
# coord_climate <- rasterToPoints(coord_climate)
load(paste(path,"14-ObjectsR/coordenadas.RDat",sep=""))

for (t in 1:2)  {
  eval(parse(text=paste('xy <- cbind(crop_',treat[t],'$x, crop_',treat[t],'$y)',sep='')))
  occ <- SpatialPoints(xy); rm(xy)
  proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # Extract FPU categories
  overFPU <- sp::over(occ,worldFPU)
  overFPU <- overFPU[,c("New_FPU","New_Basin","Basin_Name", "New_Region","Region_Nam")]
  names(overFPU)[5]='country'
  eval(parse(text=paste('crop_',treat[t],' <- data.frame(crop_',treat[t],', overFPU)',sep='')))
  
  eval(parse(text=paste("Coincidencias = match(paste(round(crop_",treat[t],"[,'x'],2),round(crop_",treat[t],"[,'y'],2)),paste(round(Coordenadas[,'x'],2),round(Coordenadas[,'y'],2)))",sep="")))
  eval(parse(text=paste("crop_",treat[t]," <- data.frame(crop_",treat[t],", Coincidencias)",sep="")))
}

###Add region classification
regiones = list(MEX=c('MIM_MEX','RIG_MEX','UME_MEX'),
                CEN=c('YUC_MEX','GTM_GTM','BLZ_BLZ','SLV_SLV','HND_HND','NIC_NIC','CRI_CRI','PAN_PAN','CRB_CRB','DOM_DOM','HTI_HTI','CUB_CUB','JAM_JAM'),
                AND=c('AMA_BOL','PAR_BOL','AMA_COL','NWS_COL','ORI_COL','AMA_ECU','NWS_ECU','AMA_PER','PEC_PER','ORI_VEN'),
                BRA=c('AMA_BRA','NEB_BRA','SAN_BRA','TOC_BRA','PAR_BRA','GSA_GSA','RVE_VEN'),
                SUR=c('PAR_ARG','RIC_ARG','SAL_ARG','TIE_ARG','URU_BRA','CHC_CHL','PAR_PRY','URU_URY'))

for (t in 1:2)  {
  eval(parse(text=paste("crop_",treat[t],"$Region = NA",sep="")))
  for(j in 1:length(regiones)){
    eval(parse(text=paste("crop_",treat[t],"$Region[which(crop_",treat[t],"$New_FPU %in% regiones[[j]])] = names(regiones)[j]",sep="")))
  }
}

#Add elevation
elevacion <-raster(paste(path,"/01-climate-data/elevation/WFDEI-elevation.nc",sep=""))

elevacion <- crop(elevacion, Map_LatinAmerica)
elevacion <- resample(elevacion, area.LA.5) 
extent(elevacion) <- extent(area.LA.5)
elevacion = rasterToPoints(elevacion)
elevacion <- round(elevacion, 2)
colnames(elevacion) <- c("x", "y", "elev")
crop_secano <- merge(crop_secano, elevacion, by = c("x", "y"))
crop_riego <- merge(crop_riego, elevacion, by = c("x", "y"))

# #Add varieties by region
# eval(parse(text=paste("variedades.riego = read.csv('",path,"_documentos/Cultivares_porRegion2_R_riego.csv',header=T,stringsAsFactors=F)",sep="")))
# eval(parse(text=paste("variedades.secano = read.csv('",path,"_documentos/Cultivares_porRegion2_R_secano.csv',header=T,stringsAsFactors=F)",sep="")))
# 
# for (t in 1:2)  {  #loop through treatments
#   
#   ind.var = grep(cultivos[c],variedades.riego$cultivares,ignore.case=T)  #filas para cultivo c, asumir mismo orden para riego y secano
#   #falla para bean/ soybean!!  
#   ind.var = 10:12  #luego fijan las filas para ahora
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
variedades.alt = read.csv(paste(path,'/_documentos/Cultivares_porAltitud.csv',sep=''),header=T,stringsAsFactors=F)
crop_secano$variedad.1 = NA
crop_riego$variedad.1 = NA
for (t in 1:2) {  #riego/ secano
  ind.v = which(variedades.alt$Cultivo=='frijol'&variedades.alt$Treat==treat[t])
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

#Descartar filas afuera de 45N a 45S y pixeles de la costa sin datos
ind.r = which(crop_riego$y<(-45)|crop_riego$y>45)
ind.s = which(crop_secano$y<(-45)|crop_secano$y>45)
if (length(ind.r)>=1) {crop_riego = crop_riego[-ind.r,]}
if (length(ind.s)>=1) {crop_secano = crop_secano[-ind.s,]}

ind.na.r = which(is.na(crop_riego$Region))
ind.na.s = which(is.na(crop_secano$Region))
if (length(ind.na.r)>=1) {crop_riego = crop_riego[-ind.na.r,]}
if (length(ind.na.s)>=1) {crop_secano = crop_secano[-ind.na.s,]}

#get rid of factor for country
crop_riego$country = as.character(crop_riego$country)
crop_secano$country = as.character(crop_secano$country)

#Save results
eval(parse(text=paste('save(crop_secano, file = "',path,'08-Cells_toRun/matrices_cultivo/Bean_secano.RDat")',sep='')))
eval(parse(text=paste('save(crop_riego, file = "',path,'08-Cells_toRun/matrices_cultivo/Bean_riego.RDat")',sep='')))









