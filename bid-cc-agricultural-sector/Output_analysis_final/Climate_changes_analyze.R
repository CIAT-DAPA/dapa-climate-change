#Look at climate changes for cropping areas

cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
path.root = "D:/tobackup/BID/"; #'Z:/'  #root path

for (c in 1:5)  {
  eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))  #08-Cells_toRun/
  eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))
  
  if (c==1) {
    secano.all = crop_secano[,c('x','y','country','secano.area')]
    riego.all = crop_riego[,c('x','y','country','riego.area')]
  }  else {
    secano.all = rbind(secano.all,crop_secano[,c('x','y','country','secano.area')])
    riego.all = rbind(riego.all,crop_riego[,c('x','y','country','riego.area')])
  }
}

#aggregate area across crops
secano.merge = aggregate(secano.all$secano.area,by=list(secano.all$x,secano.all$y,secano.all$country),FUN=sum)
riego.merge = aggregate(riego.all$riego.area,by=list(riego.all$x,riego.all$y,riego.all$country),FUN=sum)

#aggregate area across riego/ secano
areas.all = rbind(secano.merge,riego.merge)
areas.all=aggregate(areas.all$x,by=list(areas.all[,1],areas.all[,2],areas.all[,3]),FUN=sum)

#cut out low area cells (need <4000 for plotting with ggplot??)
ind.low = which(areas.all[,4]<500)  #get rid of pixels with <500 ha (experiment here?)
areas.all = areas.all[-ind.low,]

# secano.all =secano.all[!duplicated(secano.all[,1:2]),]
# riego.all =riego.all[!duplicated(riego.all[,1:2]),]
 
# areas.all = rbind(secano.all,riego.all)
# areas.all = areas.all[!duplicated(areas.all[,1:2]),]  #all growing locations across crops

#load climate variables from spreadsheet
var.name = 'prec'  #choose variable to analyze here
models = read.table(paste(path.root,'ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)  #_documentos/

for (m in 1:9)  {
  model.ch = read.csv(paste(path.root,'articulo_cientifico/climate_changes/',models[m,],'.changes-climate.csv',sep=''),header=T)
  if (m==1)  {latlon = model.ch[,2:3]
    ind.latlon = match(paste(round(areas.all[,1],2),round(areas.all[,2],2)),paste(round(latlon[,1],2),round(latlon[,2],2)))
    ind.var = which(substr(colnames(model.ch),1,4)==var.name)
    var.dat = array(NA,dim=c(length(ind.latlon),length(ind.var),9))  #create empty array
  }
  var.dat[,,m]=as.matrix(model.ch[ind.latlon,ind.var])  #subset data & place in larger array
}

#calculate multi-model mean
var.dat.mean=apply(var.dat,c(1,2),mean)
colnames(var.dat.mean)=colnames(model.ch[,ind.var])

#bootstrap multi-model mean across models
var.dat.mean.CI=array(NA,dim=c(dim(var.dat.mean)[1],length(ind.var),2))
for (j in 1:dim(var.dat.mean)[1]) {  #by pixel
  print(j)
  for (v in 1:length(ind.var))  {  #by variable
    boot = mat.or.vec(500,1)
    for (b in 1:500)  {boot.ind = sample(1:9,9,replace=T)
    boot[b] = mean(var.dat[j,v,boot.ind])
    }
    var.dat.mean.CI[j,v,1:2] = quantile(boot,c(.05,.95),na.rm=T)
  }
}

#calculate categorical changes
sig.increase = (var.dat.mean.CI[,,1]>0)*1
sig.decrease = (var.dat.mean.CI[,,2]<0)*-1
var.dat.cat = sig.increase+sig.decrease

#save results
save(var.dat.mean,var.dat.mean.CI,var.dat.cat,areas.all,file=paste(path.root,'articulo_cientifico/climate_changes/',var.name,'_changes.Rdat',sep=''))

#load matrices back in & make plots of categorical changes based on CI; Where are there consistencies across models?
load(paste(path.root,'articulo_cientifico/climate_changes/',var.name,'_changes.Rdat',sep=''))

#What % of area are significant increases & decreases for each season?
test=var.dat.cat[,1]*areas.all[,4]  #update index of variable here, double-check area
area.decrease=sum(test[test<0])/sum(areas.all[,4])*-100  #decreases
area.increase=sum(test[test>0])/sum(areas.all[,4])*100  #increases
area.increase
area.decrease
100-area.decrease-area.increase  #NS

#aggregate results to country-scale; where are areas of greatest change by country?  
simple.mean=aggregate(var.dat.mean[,4],by=list(areas.all[,3]),mean)  #update index of season here
simple.mean[order(simple.mean[,2]),]
#should really weight by harvested area or pixel-wise area


##create maps of categorical changes across all variables & seasons for appendix
#Load libraries
library(Hmisc)
library(raster)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#Load mapas de Latinoamerica 
eval(parse(text=paste('Map_LatinAmerica<-shapefile("',path.root,'Latino_America.shp")',sep='')))  #03-Map_LatinAmerica/
Map_LatinAmerica1<- fortify(Map_LatinAmerica)

#load data & rename variables
vars.name=c('prec','tmax','tmin','rsds')
seasons = c('DJF','MAM','JJA','SON')
for (v in 1:4)  {
  load(paste(path.root,'articulo_cientifico/climate_changes/',vars.name[v],'_changes.Rdat',sep=''))
  #rename relevant variables
  colnames(var.dat.cat) = colnames(var.dat.mean)
  var.dat.cat = var.dat.cat[,c(5,4,3,2)]  #re-order seasons
  eval(parse(text=paste(vars.name[v],'.cat=var.dat.cat',sep='')))
  if (v==1) {latlon = areas.all[,1:2]}
}

#loop through & save maps for each season & variable
#fix plot margins
par(mar=c(2.1, 2.1, 2.1, 2.1))
cols.inc = c('blue','red','red','yellow')  #colors for 4 crops
cols.dec = c('orange','blue','blue','brown')
for (v in 1:4) {
  for (s in 1:4)  {
    
    #get data ready for plotting
    eval(parse(text=paste('dat.cat =',vars.name[v],'.cat[,s]',sep='')))  #save data to generic variable
    
    #plot with points
    png(paste(path.root,"articulo_cientifico/climate_changes/CatChange_",vars.name[v],"_",seasons[s],".png", sep =""), width=1200, height=1400, pointsize=8, res=150)
    plot(Map_LatinAmerica)
    ind.NS = which(dat.cat==0)
    points(areas.all[ind.NS,1],areas.all[ind.NS,2],pch=20,cex=1.5,col='gray75')
    ind.increase = which(dat.cat==1)
    points(areas.all[ind.increase,1],areas.all[ind.increase,2],pch=20,cex=1.5,col=cols.inc[v])
    ind.decrease = which(dat.cat==-1)
    points(areas.all[ind.decrease,1],areas.all[ind.decrease,2],pch=20,cex=1.5,col=cols.dec[v])
    dev.off()
  }
}

#ggplot (doesn't work!!)
for (v in 1:4) {
  for (s in 1:4)  {
    
    #get data ready for plotting
    eval(parse(text=paste('dat.cat =',vars.name[v],'.cat[,s]',sep='')))  #save data to generic variable
    
    #preliminary stuff for ggplot
    dat.cat[dat.cat==0]=NA  #reset 0 value with NA to get gray color
    df= data.frame(Long=latlon[,1],Lat=latlon[,2],yield=dat.cat)
    # df = df[1:1000,]  #test
    limits2=c(-1,1)
    color_scale = brewer.pal(11,'RdYlGn')  
    labs2 = ''
    
    #create ggplot & save
    y=ggplot() +
      geom_polygon( data=Map_LatinAmerica1, aes(x=long, y=lat, group = group),colour="gray90", fill="white" )+  #fill="gray70"
      geom_path(data=Map_LatinAmerica1, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
      coord_equal() +
      geom_raster(data=df, aes(x=Long, y=Lat,fill=yield))+
      #ggtitle(paste(capitalize(cultivos.en[c]),' (',treat.en[t],'): \n',models[m,],sep=''))+
      scale_fill_gradientn(colours=color_scale,limits=limits2,na.value = "gray60")+ # guide='none' turns off legend,lightskyblue, limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
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
            legend.text = element_text(size=16),
            legend.title = element_text(face="bold",size=18),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.key.size = unit(1,'cm'),
            legend.justification=c(0,0), 
            legend.position=c(0.2,0.2),  #baseline yields only!
            plot.title = element_blank(),  #element_text(face="bold", size=18)  #this turns on or off plot title
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.margin=unit(c(0,0,0,0),"mm")
      )
    plot(y)
    ggsave(filename=paste(path.root,"articulo_cientifico/climate_changes/CatChange_",vars.name[v],"_",seasons[s],".png", sep=""), plot=y, width=5, height=5, dpi=400,scale=1.5)
  }
}


#try Carlos code
require(raster)
require(maptools)
library(rgeos)
library(rgdal)
# List of seasons
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
shp <- readOGR(paste(path.root,"articulo_cientifico/climate_changes/mask_lat_adm1_diss.shp",sep=""), layer="mask_lat_adm1_diss")

# # bottom, left, top, and right
# par(mfrow = c(2,2), mar=c(1, 0, 2, 0), oma=c(0, 0, 0, 0))

#create base raster
basRs <- raster(nrows=1080, ncols=2160, xmn=-180, xmx=180, ymn=-90, ymx=90)

# Loop throught seasons
for (i in 1:4){
  
  # brk <- c(-20, -14, -12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, 14, 20)
  brk <- c(-30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30)
  colfunc <- colorRampPalette(c("red", "white", "blue"))
  
  #place season in base raster
  dat.plot = cbind(areas.all[,1:2],prec.cat[,1])
  test = rasterize(dat.plot[,1:2],basRs,dat.plot[,3])
  test.plot = mask(crop(test,extent(shp)),shp)
  
  plot(test.plot, col=c('orange','gray75','blue'), axes=F, box=F, legend = F,pch=2)
  plot(shp, add=T)
  
  title(toupper(names(seasons[i])), line = -1, adj=0.5, cex.main = 2)
  
  
}

dev.off()