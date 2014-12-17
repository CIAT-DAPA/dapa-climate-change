library('raster')
library('rgdal')
library('ggplot2')

getwd()
setwd('//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/')
rm(list=ls())

#Load map of Latin America
Map_LatinAmerica<-shapefile("03-Map_LatinAmerica/Latino_America.shp")  ## Mapa de Latinoamerica
Map_LatinAmerica1<- fortify(Map_LatinAmerica)  #to facilitate plotting in ggplot2

#Set static variables
cultivos = c('Maize','Bean','Potato','Rice','Soybean','Wheat')
vars = c('area','prod')

for (j in 1:6)  {

  ## Load Monfreda data
  eval(parse(text=paste('area.prop<-raster("06-Monfreda data (Rendimiento, Area)/',cultivos[j],'/',cultivos[j],'_5min.nc",level=1)',sep='')))  #harvested area, proportion of grid cell area
  area.prop.LA<-crop(area.prop,Map_LatinAmerica)  ##harvested area as proportion of grid cell area
  
  eval(parse(text=paste('rend<-raster("06-Monfreda data (Rendimiento, Area)/',cultivos[j],'/',cultivos[j],'_5min.nc",level=2)',sep='')))  #yield
  rend.LA<-crop(rend,Map_LatinAmerica)  ## Rendimiento Latinoamerica in tons/ha??
 
  areas = area(area.prop.LA)  # Area de cada pixel en km2
  areas = areas*100    ## Representamos el area en hectareas
  area.LA = area.prop.LA * areas  #calculate harvested area in hectares (multiply proportion by cell area)
  prod.LA = rend.LA * area.LA  #calculate production as yield * harvested area (doesn't account for double cropping!)
  ## plot(area.LA,main=cultivos[j])  #try graphing harvested area map (in hectares)
  
  ## Agregar a la resolucion del 0.5 grados (de 5 minutos o 0.083 grados?) por sumar
  area.LA.5 = aggregate(area.LA, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)  
  prod.LA.5 = aggregate(prod.LA, fact=6, fun=sum, expand=TRUE, na.rm=TRUE) 
  
  #resample to use cell centers at 0.25 & 0.75 degrees (using raster from Sacks)
  Maiz_Sacks<-raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/05-Crop Calendar Sacks/Maize.crop.calendar.nc",varname="plant")
  area.LA.5 = resample(area.LA.5,Maiz_Sacks,method="bilinear")
  prod.LA.5 = resample(prod.LA.5,Maiz_Sacks,method="bilinear")
  
  for (v in 1:2)  {
      #pull out map of areas/ prod clipped to Latin America
      eval(parse(text=paste('mascara<-mask(',vars[v],'.LA.5,Map_LatinAmerica)',sep='')))  
    
      #Ordenar area/ prod
      eval(parse(text=paste('pos_=which(',vars[v],'.LA.5[]!="NA")',sep='')))   ## Posiciones donde se Encuentran los valores
      eval(parse(text=paste('valores=',vars[v],'.LA.5[][pos_]',sep='')))                     ## Valores sin NA del Raster
      Base=cbind(valores,pos_)                        ## Matriz de valores asociadas con las posciones en el raster   
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
      
      #plot masks with ggplot2
#       cutoffs = c('95','99')
#       for (k in 1:2)  {
#         eval(parse(text = paste('y=ggplot() +
#         geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
#         geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
#         coord_equal() +
#         geom_raster(data=df.',cutoffs[k],'.',vars[v],',aes(x=Longitude, y=Latitude),fill="green4")+
#         ggtitle(paste("',cutoffs[k],'% ',vars[v],' mask at 0.5 degree",cultivos[j]))
#         win.graph()  
#         print(y)',sep='')))
#       }
  }
  
  #Merge dataframes with 4 criteria into superset with 1/0 for inclusion in each
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
  eval(parse(text=paste('save(',cultivos[j],'.loc,mascara.95.area,mascara.95.prod,mascara.99.area,mascara.99.prod,file="08-Cells_toRun/',cultivos[j],'.loc.Rdat")',sep='')))  #save results
}
