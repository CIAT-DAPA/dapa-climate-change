#Create maps of harvested area from SPAM for secano & riego
rm(list=ls())

#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#fijar cultivo aquí
#c=1  #maize
#c=2  #rice
c=3  #soy
#c=4  #frijol
#c=5 #trigo

#Set paths
path.res = "D:/tobackup/BID/Resultados_DSSAT/"; #'Z:/12-Resultados/'  #results directory D:/tobackup/BID/Resultados_DSSAT/
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path

#otros datos fijos
treat = c('riego','secano')  #riego o secano (which to plot)
treat.en = c('Irrigated','Rainfed')
cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')

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

quantile(crop_riego$riego.area,c(.05,.95))
quantile(crop_secano$secano.area,c(.05,.95))
aggregate(crop_secano$secano.area,by=list(crop_secano$country),FUN=sum)
aggregate(crop_riego$riego.area,by=list(crop_riego$country),FUN=sum)

#plot harvested area for riego & secano
for (t in 1:2)  {  #loop through riego (t=1)/ secano (t=2)
  #t=2  #secano
  print(t)
    #extract SPAM data
    eval(parse(text=paste('promedio = crop_',treat[t],'$',treat[t],'.area',sep=''))) 
    promedio = promedio/1000  #convert to thousands ha
    
    #limits2 = quantile(c(crop_riego$riego.area,crop_secano$secano.area),c(.05,.95),na.rm=T)  #limites flexibles segun el cultivo
    labs2 = "Harvested area \n(000's ha)"
    if (t==1) {
      limits2 = c(0,5)  #riego
    } else {
      limits2 = c(0,35)  #secano
    }
    
    promedio[which(promedio<limits2[1] & promedio>0)] = limits2[1]  #reset end points of data
    promedio[which(promedio>limits2[2])] = limits2[2]
    
    eval(parse(text=paste("df = data.frame(Long=crop_",treat[t],"[,1],Lat=crop_",treat[t],"[,2],yield=promedio)",sep='')))
    #color_scale = colorRampPalette(c('yellow','orange','red'), space="rgb")(25) 
    color_scale =  brewer.pal(9,'Oranges')
    
    y=ggplot() +
      geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="gray90", fill="gray80" )+  #fill="gray70"
      geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      coord_equal() +
      geom_raster(data=df, aes(x=Long, y=Lat,fill=yield))+
      ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],')',sep=''))+
      scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "gray60",guide='none')+ # guide='none' turns off legend,lightskyblue, limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
      theme_bw()+
      labs(fill=labs2)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),  #element_text(angle=90, vjust=-0.5, size=16)
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=16),
            legend.text = element_text(size=16),
            legend.title = element_text(face="bold",size=17),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.key.size = unit(1,'cm'),
            legend.justification=c(0,0), legend.position=c(0.15,0.1),  
            plot.title = element_blank(),    #element_text(face="bold", size=18)
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.margin=unit(c(0,0,0,0),"mm")
      )
    #plot(y)
    ggsave(filename=paste(path.root,"resultados_graficas/",cultivos[c],"_",treat[t],"_SPAM_harvestedArea.png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
}  