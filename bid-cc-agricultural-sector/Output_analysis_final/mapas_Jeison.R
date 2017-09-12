#Load libraries
library(ggplot2)
library(reshape)

#Load input data
load('Z:/12-Resultados/Maiz/1971-1998/IB0006_IRNA_Riego.Rdat')  #WFD results
load('Z:/08-Cells_toRun/matrices_cultivo/Maize_riego.Rdat')
Map_LatinAmerica <- shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")

#create multi-year mean
Presente <- lapply(Run,mean)  
Presente <- unlist(Presente)

#put data with lat/long in data frame
Maize <- cbind.data.frame(x=crop_riego[, "x"],y=crop_riego[, "y"],Presente)
df <- Maize
colnames(df) <- c("Longitude", "Latitude", "Produccion")

#calculate range of data
limits = quantile(df$Produccion,c(.05,.95))

Map_LatinAmerica1<- fortify(Map_LatinAmerica)
y=ggplot() +
geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="white", fill="white" )+
geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
coord_equal() +
geom_raster(data=df, aes(x=Longitude, y=Latitude,fill=Produccion))+
ggtitle(paste("Rendimiento Maiz"))+
scale_fill_gradient(low = "yellow",high = "red",limits=limits)
plot(y)  #create plot