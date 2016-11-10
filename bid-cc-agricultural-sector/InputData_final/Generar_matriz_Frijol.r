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


path <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"  #Z:/ for Windows; /mnt/workspace_cluster_3/bid-cc-agricultural-sector/

Map_LatinAmerica <- shapefile(paste0(path, "03-Map_LatinAmerica/Latino_America.shp"))  ## Map Latin America

#Set static variables
cultivos = c('Maize','Bean','Potato','Rice','Soybean','Wheat', 'Bean')
vars = c('area','prod')
cultivos[2]
j <- 2
cultivos[j]  ## Cultivo para frijol

## Load Monfreda data
eval(parse(text=paste0( "area.prop<-raster(", "'", path, "06-Monfreda data (Rendimiento, Area)/", cultivos[j],'/',cultivos[j],"_5min.nc", "'", ",", "level=1)", sep="")))  #harvested area, proportion of grid cell area
area.prop.LA <- crop(area.prop, Map_LatinAmerica)  ##harvested area as proportion of grid cell area

eval(parse(text=paste0( "rend<-raster(", "'", path, "06-Monfreda data (Rendimiento, Area)/", cultivos[j],'/',cultivos[j],"_5min.nc", "'", ",", "level=2)", sep="")))   #yield
rend.LA<-crop(rend,Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??

areas = area(area.prop.LA)  # Area de cada pixel en km2
areas = areas*100    ## Representamos el area en hectareas
area.LA = area.prop.LA * areas  #calculate harvested area in hectares (multiply proportion by cell area)
prod.LA = rend.LA * area.LA  #calculate production as yield * harvested area (doesn't account for double cropping!)
## plot(area.LA,main=cultivos[j])  #try graphing harvested area map (in hectares)

## Agregar a la resolucion del 0.5 grados (de 5 minutos o 0.083 grados?) por sumar
area.LA.5 = aggregate(area.LA, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)  
prod.LA.5 = aggregate(prod.LA, fact=6, fun=sum, expand=TRUE, na.rm=TRUE) 


for (v in 1:2)  {
  #pull out map of areas/ prod clipped to Latin America
  eval(parse(text=paste('mascara<-mask(',vars[v],'.LA.5,Map_LatinAmerica)',sep='')))  
  
  #Ordenar area/ prod
  eval(parse(text=paste('pos_=which(',vars[v],'.LA.5[]!="NA")',sep='')))   ## Posiciones donde se Encuentran los valores
  eval(parse(text=paste('valores=',vars[v],'.LA.5[][pos_]',sep='')))                     ## Valores sin NA del Raster
  Base=cbind(valores,pos_)                        ## Matriz de valores asociadas con las posiciones en el raster   
  valores.sort=Base[order(Base[,1],decreasing = T),]   ## Se ordenan los valores de mayor a menor
  valores.sort2 = cumsum(valores.sort[,1])         ## Suma acumulada de los valores de area
  
  #       #Plot suma acumulada con tope en 95 y 99% area
  #       win.graph()
  #       eval(parse(text=paste('plot(valores.sort2,ylab="Cumsum ',vars[v],'",xlab="",main=paste("Cumulative sum de ',vars[v],': ",cultivos[j]))',sep='')))
  #       abline(h=sum(valores)*0.95,col="orange")
  #       abline(h=sum(valores)*0.99,col='red')
  #       eval(parse(text=paste('legend("bottomright",c("',vars[v],'","95%","99%"),col=c("black","orange","red"),lty=c(NA,1,1),pch=c(1,NA,NA))',sep='')))
  
  Cortar.95=which(valores.sort2<sum(valores)*0.95)  #Find cells with highest area until 95% cutoff value
  Cortar.99=which(valores.sort2<sum(valores)*0.99)  #Find cells with highest area until 99% cutoff value
  Cortar.95.2=which(valores.sort2>sum(valores)*0.95)  #Values outside 95% cutoff
  Cortar.99.2=which(valores.sort2>sum(valores)*0.99)  #Values outside 99% cutoff
  
  #add 95 & 99% masks to valores.sort
  valores.sort = data.frame(valores.sort)
  valores.sort$var.95 = valores.sort$valores
  valores.sort$var.99 = valores.sort$valores
  valores.sort$var.95[Cortar.95.2]<-NA  #set var values outside 95% cutoff to NA
  valores.sort$var.95[Cortar.95]<-1  #set values inside 95% cutoff to 1
  valores.sort$var.99[Cortar.99.2]<-NA  #set var values outside 99% cutoff to NA
  valores.sort$var.99[Cortar.99]<-1  #set values inside 99% cutoff to 1
  
  #create binary maps for 95 & 99% cutoffs
  eval(parse(text=paste('mascara.95.',vars[v],' = mascara',sep='')))
  eval(parse(text=paste('mascara.95.',vars[v],'[][valores.sort$pos_]<-valores.sort$var.95',sep='')))  
  eval(parse(text=paste('mascara.99.',vars[v],' = mascara',sep='')))
  eval(parse(text=paste('mascara.99.',vars[v],'[][valores.sort$pos_]<-valores.sort$var.99',sep='')))
  
  print(paste("Celdas Latinoamerica para",cultivos[j],sum(valores.sort$var.95==1,na.rm=T),95,"%",vars[v]))
  print(paste("Celdas Latinoamerica para",cultivos[j],sum(valores.sort$var.99==1,na.rm=T),99,"%",vars[v]))
  
  #Pull out data with lat/ long (different data frames for prod & area)
  eval(parse(text=paste('map.p.95 <- rasterToPoints(mascara.95.',vars[v],')',sep='')))
  eval(parse(text=paste('map.p.99 <- rasterToPoints(mascara.99.',vars[v],')',sep='')))
  eval(parse(text=paste('df.95.',vars[v],' <- data.frame(map.p.95)',sep='')))
  eval(parse(text=paste('df.99.',vars[v],' <- data.frame(map.p.99)',sep='')))
  eval(parse(text=paste('colnames(df.95.',vars[v],') <- c("Longitude", "Latitude", "Include")',sep='')))
  eval(parse(text=paste('colnames(df.99.',vars[v],') <- c("Longitude", "Latitude", "Include")',sep='')))
  
}

colnames(df.95.area)[3] = 'Area.95'
colnames(df.99.area)[3] = 'Area.99'
colnames(df.95.prod)[3] = 'Prod.95'
colnames(df.99.prod)[3] = 'Prod.99'

merge1 = merge(df.95.prod,df.95.area,all.x=T,all.y=T,sort=T)
merge2 = merge(merge1,df.99.prod,all.x=T,all.y=T,sort=T)
merge3 = merge(merge2,df.99.area,all.x=T,all.y=T,sort=T)
merge3[is.na(merge3)] = 0  #reset NA's to 0
#test = apply(merge3[,3:6],1,sum)  #check for # of criteria met per location

eval(parse(text=paste(cultivos[j],'.loc=merge3',sep='')))  #rename by crop
eval(parse(text=paste('save(',cultivos[j],'.loc,mascara.95.area,mascara.95.prod,mascara.99.area,mascara.99.prod', ",",'file =', "'", path, '08-Cells_toRun/',cultivos[j],'.loc.Rdat', "'", ")",sep='')))  #save results

############################################################


#crops
crops = 'Bean'
crops2 = 'Bean'



  eval(parse(text = paste0('crop_plant<-raster(', "'", path, "05-Crop Calendar Sacks/bean-EcoCrop/", "common_bean_forest_exc_gssuit_rain_spam.tif", "'", ")")))
  plot(crop_plant)
  extent(crop_plant) <- extent(prod.LA.5)
 #plot(stack(crop_plant, prod.LA.5))

  crop_plant <- round(crop_plant)
  meses <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30)
  acumulado <- cumsum(meses)
  mes_siembra = rasterToPoints(crop_plant)[, 3]
  #y <- rasterToPoints(crop_plant)
  #raster::extract(prod.LA.5, y[, 1:2])
  mes_siembra <- round(mes_siembra)
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
  
  ## Cargar los raster para secano y riego se puede filtrar tanto para area como para rendimiento o Produccion
#plot(crop_plant)

bean_secano <- raster(paste0(path, "07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_bean_rainfed.nc"), level = 1)
area.LA.secano <- crop(bean_secano, Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??


## Agregar a la resolucion del 0.5 grados
area.LA.5 = aggregate(area.LA.secano, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)  

#Ordenar Produccion
pos_=which(area.LA.5[]!="NA")   ## Posiciones donde se Encuentran los valores
valores=area.LA.5[][pos_]                     ## Valores sin NA del Raster
Base=cbind(valores,pos_)                        ## Matriz de valores asociadas con las posciones en el raster   
valores.sort=Base[order(Base[,1],decreasing = T),]   ## Se ordenan los valores de mayor a menor
valores.sort2 = cumsum(valores.sort[,1])         ## Suma acumulada de los valores de produccion

##plot(valores.sort2,ylab="Cumsum Area",xlab="",main=paste("Datos Representativos al 95%: ",cultivos[j]))
##abline(h=sum(valores)*0.95,col="orange")
##abline(h=sum(valores)*0.99,col='red')

Cortar=which(valores.sort2<sum(valores)*0.95) 
Cortar2=which(valores.sort2>sum(valores)*0.95)

##print(paste("Area (95): ",cultivos[j],length(Cortar.95),sep=' '))
##print(paste("Area (99): ",cultivos[j],length(Cortar.99),sep=' '))


valores.sort[Cortar2,1]<-NA
add_areas <- valores.sort[Cortar]
valores.sort[Cortar]<-1


area.LA.5[][valores.sort[,2]]<-valores.sort[,1]


##area.LA.5 <- mask(area.LA.5, Map_LatinAmerica)
area.La.coord <- rasterToPoints(area.LA.5)
area.La.coord <- data.frame(area.La.coord, add_areas)
colnames(area.La.coord) <- c("x", "y", "id", "area")

area.La.coord <- round(area.La.coord, 2)
EcoCrop <- rasterToPoints(crop_plant)
EcoCrop <- round(EcoCrop, 2)
ind.cal.bean.secano = match(paste(area.La.coord[,'x'], area.La.coord[,'y'],sep=''), paste(EcoCrop[,'x'],EcoCrop[,'y'],sep=''))
## head(area.La.coord[which(!is.na(ind.cal.bean.secano)),])

siembra_secano <- EcoCrop[ind.cal.bean.secano, ]
siembra_secano<- na.omit(siembra_secano)
head(siembra_secano)
siembra_secano <- data.frame(siembra_secano)
dim(siembra_secano)


siembra_secano <- merge(siembra_secano, area.La.coord, by = c("x", "y"))
head(siembra_secano)


#Make the points a dataframe for ggplot
df <- data.frame(siembra_secano)
colnames(df) <- c("Longitude", "Latitude", "Plant", "Area")

Map_LatinAmerica1<- fortify(Map_LatinAmerica)
ggplot() +
  geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
  geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
  coord_equal() +
  geom_raster(data=df, aes(x=Longitude, y=Latitude, fill = Plant))+
  ggtitle("Dias de Siembra Secano para aquellos que cuenten con el 95% de las areas total")+
  scale_fill_gradient2(low = "yellow", mid = "darkgreen", high = "darkred")


################################################################# Para Riego

bean_riego <- raster(paste0(path, "07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_bean_irrigated.nc"), level = 1)
area.LA.riego <- crop(bean_riego, Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??


## Agregar a la resolucion del 0.5 grados
area.LA.5 = aggregate(area.LA.riego, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)  

#Ordenar Produccion
pos_ = which(area.LA.5[]!="NA")   ## Posiciones donde se Encuentran los valores
valores = area.LA.5[][pos_]                     ## Valores sin NA del Raster
Base = cbind(valores,pos_)                        ## Matriz de valores asociadas con las posciones en el raster   
valores.sort = Base[order(Base[,1],decreasing = T),]   ## Se ordenan los valores de mayor a menor
valores.sort2 = cumsum(valores.sort[,1])         ## Suma acumulada de los valores de produccion

##plot(valores.sort2,ylab="Cumsum Area",xlab="",main=paste("Datos Representativos al 95%: ",cultivos[j]))
##abline(h=sum(valores)*0.95,col="orange")
##abline(h=sum(valores)*0.99,col='red')

Cortar=which(valores.sort2<sum(valores)*0.95) 
Cortar2=which(valores.sort2>sum(valores)*0.95)

##print(paste("Area (95): ",cultivos[j],length(Cortar.95),sep=' '))
##print(paste("Area (99): ",cultivos[j],length(Cortar.99),sep=' '))


valores.sort[Cortar2,1]<-NA
valores.sort[Cortar]<-1


area.LA.5[][valores.sort[,2]]<-valores.sort[,1]


area.LA.5 <- mask(area.LA.5, Map_LatinAmerica)
area.La.coord <- rasterToPoints(area.LA.5)
area.La.coord <- round(area.La.coord, 2)
EcoCrop <- rasterToPoints(crop_plant)
EcoCrop <- round(EcoCrop, 2)
ind.cal.bean.riego = match(paste(area.La.coord[,'x'], area.La.coord[,'y'],sep=''), paste(EcoCrop[,'x'],EcoCrop[,'y'],sep=''))
siembra_riego <- EcoCrop[ind.cal.bean.riego, ]
siembra_riego <- na.omit(siembra_riego)
head(siembra_riego)
dim(siembra_riego)
siembra_riego <- merge(siembra_riego, area.La.coord, by = c("x", "y"))

#Make the points a dataframe for ggplot
df <- data.frame(siembra_riego)
colnames(df) <- c("Longitude", "Latitude", "Plant")

Map_LatinAmerica1<- fortify(Map_LatinAmerica)
ggplot() +
  geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
  geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
  coord_equal() +
  geom_raster(data=df, aes(x=Longitude, y=Latitude, fill = Plant))+
  ggtitle("Dias de Siembra Riego para aquellos que cuenten con el 95% de las areas total")+
  scale_fill_gradient2(low = "yellow", mid = "darkgreen", high = "darkred")



### Agregar fertilizantes


#Load GGCMI fertilizer data
library('ncdf4')
library('RColorBrewer')  #next option
library('lattice')
library('raster')

#static data
path2 <- paste(path,"10-GGCMI_fertilizers/")
K2O.app = raster(paste(path,"10-GGCMI_fertilizers/agmip_soybeans_apprate_fill_NPK_0.5.nc4",sep=""), varname = "K2Oapprate")
N.app = raster(paste(path,"10-GGCMI_fertilizers/agmip_soybeans_apprate_fill_NPK_0.5.nc4",sep=""), varname = "Napprate")
P2O5.app = raster(paste(path,"10-GGCMI_fertilizers/agmip_soybeans_apprate_fill_NPK_0.5.nc4",sep=""), varname = "P2O5apprate")


N.app <- crop(N.app, Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??
K2O.app <- crop(K2O.app, Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??
P2O5.app <- crop(P2O5.app, Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??

N.app <- resample(N.app, prod.LA.5)     ## Volver a recrear este objeto prod.LA.5 para que funcione 
K2O.app <- resample(K2O.app, prod.LA.5)     ## Volver a recrear este objeto prod.LA.5 para que funcione 
P2O5.app <- resample(P2O5.app, prod.LA.5)     ## Volver a recrear este objeto prod.LA.5 para que funcione 

extent(N.app) <- extent(prod.LA.5)
extent(K2O.app) <- extent(prod.LA.5)
extent(P2O5.app) <- extent(prod.LA.5)

K2O.app = rasterToPoints(K2O.app)
N.app = rasterToPoints(N.app)
P2O5.app = rasterToPoints(P2O5.app)


##################### Para secano

ind.N = match(paste(round(siembra_secano[, "x"],2),round(siembra_secano[, "y"],2),sep=''),paste(round(N.app[,'x'],2),round(N.app[,'y'],2),sep=''))
#siembra_riego$N.app = N.app[ind.N,3]

ind.P = match(paste(siembra_secano[, "x"], siembra_secano[, "y"], sep = ''), paste(round(P2O5.app[,'x'],2),round(P2O5.app[,'y'],2),sep=''))
#siembra_riego$P2O5.app = P2O5.app[ind.P,3]

ind.K = match(paste(siembra_secano[, "x"], siembra_secano[, "y"], sep = ''), paste(round(K2O.app[,'x'],2),round(K2O.app[,'y'],2),sep=''))
#siembra_riego$K2O.app = K2O.app[ind.K,3]

head(siembra_secano)

siembra_secano <- data.frame(siembra_secano, N.app[ind.N,3], P2O5.app[ind.P,3], K2O.app[ind.K,3])
head(siembra_secano)
siembra_secano <- siembra_secano[, -4]
head(siembra_secano)
colnames(siembra_secano) <- c("x", "y", "Plant", "Area","N.app", "P2O5.app", "K2O.app")
head(siembra_secano)
##siembra_riego[, "N.app"] <- siembra_riego[, "N.app"]*2   ## En riego la fertilizacion es el doble que secano
head(siembra_secano)

#Load libraries
library(maptools)
library(sp)

# Load FPU shape file
worldFPU <- readShapePoly(fn="Z:/15_FPUs/fpu_shp/fpu.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

xy <- cbind(siembra_secano$x, siembra_secano$y)
occ <- SpatialPoints(xy); rm(xy)
proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Extract FPU categories
overFPU <- sp::over(occ,worldFPU)
overFPU <- overFPU[,c("New_FPU","New_Basin","Basin_Name", "New_Region")]
head(overFPU)
siembra_secano <- data.frame(siembra_secano, overFPU)
head(siembra_secano)

## Objeto que contiene las coordenadas
coord_climate <- raster("Z:/01-climate-data/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_01.nc")
coord_climate <- resample(coord_climate, prod.LA.5)     ## Volver a recrear este objeto prod.LA.5 para que funcione 
coord_climate <- rasterToPoints(coord_climate)

Coincidencias = match(paste(round(siembra_secano[,'x'],2),round(siembra_secano[,'y'],2)),paste(round(coord_climate[,'x'],2),round(coord_climate[,'y'],2)))
## for Try
## siembra_secano[1, ]
## coord_climate[1476, ]
siembra_secano <- data.frame(siembra_secano, Coincidencias)
head(siembra_secano)
#Add region classification

regiones = list(MEX=c('MIM_MEX','RIG_MEX','UME_MEX'),
                CEN=c('YUC_MEX','GTM_GTM','BLZ_BLZ','SLV_SLV','HND_HND','NIC_NIC','CRI_CRI','PAN_PAN','CRB_CRB','DOM_DOM','HTI_HTI','CUB_CUB','JAM_JAM'),
                AND=c('AMA_BOL','PAR_BOL','AMA_COL','NWS_COL','ORI_COL','AMA_ECU','NWS_ECU','AMA_PER','PEC_PER','ORI_VEN'),
                BRA=c('AMA_BRA','NEB_BRA','SAN_BRA','TOC_BRA','PAR_BRA','GSA_GSA','RVE_VEN'),
                SUR=c('PAR_ARG','RIC_ARG','SAL_ARG','TIE_ARG','URU_BRA','CHC_CHL','PAR_PRY','URU_URY')
)
siembra_secano$Region = NA
for(j in 1:length(regiones)){
  
  siembra_secano$Region[which(siembra_secano$New_FPU %in% regiones[[j]])] = names(regiones)[j]
}

head(siembra_secano)
siembra_secano$N.app.0 <- NA
siembra_secano$N.app.20 <- NA
siembra_secano$N.app.0[which(siembra_secano$N.app<40)] <- siembra_secano$N.app[which(siembra_secano$N.app<40)]
siembra_secano$N.app.40[which(siembra_secano$N.app>=40)] <- siembra_secano$N.app[which(siembra_secano$N.app>=40)]*0.5
head(siembra_secano)


elevacion <-raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/elevation/WFDEI-elevation.nc")

elevacion <- crop(elevacion, Map_LatinAmerica)
elevacion <- resample(elevacion, prod.LA.5) 
extent(elevacion) <- extent(prod.LA.5)
elevacion = rasterToPoints(elevacion)
elevacion <- round(elevacion, 2)
colnames(elevacion) <- c("x", "y", "Elev")
siembra_secano <- merge(siembra_secano, elevacion, by = c("x", "y"))

head(siembra_secano)
save(siembra_secano, file = "Z:/08-Cells_toRun/matrices_cultivo/Bean_secano.RDat")

variedad.1 <- rep("IB0006", dim(siembra_secano)[1])

siembra_secano <- data.frame(siembra_secano, variedad.1)

siembra_secan <- siembra_secano[,-17]
dim(siembra_secano)


###################### Para riego


ind.N = match(paste(round(siembra_riego[, "x"],2),round(siembra_riego[, "y"],2),sep=''),paste(round(N.app[,'x'],2),round(N.app[,'y'],2),sep=''))
#siembra_riego$N.app = N.app[ind.N,3]

ind.P = match(paste(siembra_riego[, "x"], siembra_riego[, "y"], sep = ''), paste(round(P2O5.app[,'x'],2),round(P2O5.app[,'y'],2),sep=''))
#siembra_riego$P2O5.app = P2O5.app[ind.P,3]

ind.K = match(paste(siembra_riego[, "x"], siembra_riego[, "y"], sep = ''), paste(round(K2O.app[,'x'],2),round(K2O.app[,'y'],2),sep=''))
#siembra_riego$K2O.app = K2O.app[ind.K,3]

head(siembra_riego)

siembra_riego <- data.frame(siembra_riego, N.app[ind.N,3], P2O5.app[ind.P,3], K2O.app[ind.K,3])
head(siembra_riego)
siembra_riego <- siembra_riego[, -4]
head(siembra_riego)
colnames(siembra_riego) <- c("x", "y", "Plant", "Area","N.app", "P2O5.app", "K2O.app")
head(siembra_riego)
##siembra_riego[, "N.app"] <- siembra_riego[, "N.app"]*2   ## En riego la fertilizacion es el doble que secano
head(siembra_secano)

#Load libraries
library(maptools)
library(sp)

# Load FPU shape file
worldFPU <- readShapePoly(fn="Z:/15_FPUs/fpu_shp/fpu.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

xy <- cbind(siembra_secano$x, siembra_secano$y)
occ <- SpatialPoints(xy); rm(xy)
proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Extract FPU categories
overFPU <- sp::over(occ,worldFPU)
overFPU <- overFPU[,c("New_FPU","New_Basin","Basin_Name", "New_Region")]
head(overFPU)
siembra_secano <- data.frame(siembra_secano, overFPU)
head(siembra_secano)

## Objeto que contiene las coordenadas
coord_climate <- raster("Z:/01-climate-data/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month/tmax_1971_01.nc")
coord_climate <- resample(coord_climate, prod.LA.5)     ## Volver a recrear este objeto prod.LA.5 para que funcione 
coord_climate <- rasterToPoints(coord_climate)

Coincidencias = match(paste(round(siembra_secano[,'x'],2),round(siembra_secano[,'y'],2)),paste(round(coord_climate[,'x'],2),round(coord_climate[,'y'],2)))
## for Try
## siembra_secano[1, ]
## coord_climate[1476, ]
siembra_secano <- data.frame(siembra_secano, Coincidencias)
head(siembra_secano)
#Add region classification

regiones = list(MEX=c('MIM_MEX','RIG_MEX','UME_MEX'),
                CEN=c('YUC_MEX','GTM_GTM','BLZ_BLZ','SLV_SLV','HND_HND','NIC_NIC','CRI_CRI','PAN_PAN','CRB_CRB','DOM_DOM','HTI_HTI','CUB_CUB','JAM_JAM'),
                AND=c('AMA_BOL','PAR_BOL','AMA_COL','NWS_COL','ORI_COL','AMA_ECU','NWS_ECU','AMA_PER','PEC_PER','ORI_VEN'),
                BRA=c('AMA_BRA','NEB_BRA','SAN_BRA','TOC_BRA','PAR_BRA','GSA_GSA','RVE_VEN'),
                SUR=c('PAR_ARG','RIC_ARG','SAL_ARG','TIE_ARG','URU_BRA','CHC_CHL','PAR_PRY','URU_URY')
)
siembra_secano$Region = NA
for(j in 1:length(regiones)){
  
  siembra_secano$Region[which(siembra_secano$New_FPU %in% regiones[[j]])] = names(regiones)[j]
}

head(siembra_secano)
siembra_secano$N.app.0 <- NA
siembra_secano$N.app.20 <- NA
siembra_secano$N.app.0[which(siembra_secano$N.app<40)] <- siembra_secano$N.app[which(siembra_secano$N.app<40)]
siembra_secano$N.app.40[which(siembra_secano$N.app>=40)] <- siembra_secano$N.app[which(siembra_secano$N.app>=40)]*0.5
head(siembra_secano)


elevacion <-raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/elevation/WFDEI-elevation.nc")

elevacion <- crop(elevacion, Map_LatinAmerica)
elevacion <- resample(elevacion, prod.LA.5) 
extent(elevacion) <- extent(prod.LA.5)
elevacion = rasterToPoints(elevacion)
elevacion <- round(elevacion, 2)
colnames(elevacion) <- c("x", "y", "Elev")
siembra_secano <- merge(siembra_secano, elevacion, by = c("x", "y"))

head(siembra_secano)
save(siembra_secano, file = "Z:/08-Cells_toRun/matrices_cultivo/Bean_secano.RDat")

variedad.1 <- rep("IB0006", dim(siembra_secano)[1])

siembra_secano <- data.frame(siembra_secano, variedad.1)

siembra_secan <- siembra_secano[,-17]
dim(siembra_secano)







