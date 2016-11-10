#Process irrigated/ secano area data; scale N app data to separate values for RF & irrigated

#Load libraries
library('raster')

#static data
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat','Bean')
crop_num = c('01','10','03','08','01','17')
months = c('001','002','003','004','005','006','007','008','009','010','011','012')
k=3  #set crop index (loop later)

#Load MIRCA growing area data by crop for irrigated & secano areas
riego_stack = stack()
secano_stack = stack()
for (j in 1:12)  {  #should do both irrigated & secano here
  eval(parse(text = paste("test = raster('Z:/09-MIRCA2000/Growing_area_grids/crop_",crop_num[k],"_irrigated_",months[j],".asc')",sep='')))
  riego_stack = stack(riego_stack,test)
  
  eval(parse(text = paste("test = raster('Z:/09-MIRCA2000/Growing_area_grids/crop_",crop_num[k],"_rainfed_",months[j],".asc')",sep='')))
  secano_stack = stack(secano_stack,test)
  
  print(j)
}

#aggregate from 5-arc minute (or 1/12 degree) to half degree
riego_stack_p5 = aggregate(riego_stack, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
secano_stack_p5 = aggregate(secano_stack, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
#plot(riego_stack_p5,7)  #look at results by month
#plot(secano_stack_p5,7)

#Cut out coordinates in Latin America by crop
load('Z:/14-ObjectsR/id_coordinates.Rdat')  #coordinates of DSSAT runs by crop
eval(parse(text=paste('crop_pixel = id_pixel[which(id_pixel$',crops[k],'==1),]',sep='')))
for (j in 1:12)  {  #loop through months
  riego_mirca = rasterToPoints(riego_stack_p5[[j]])  #jth layer in stack
  ind_LAM = match(paste(crop_pixel$x,crop_pixel$y),paste(riego_mirca[,'x'],riego_mirca[,'y']))
  if (j==1)  {
    riego_mirca_LAM = riego_mirca[ind_LAM,]
  }  else{
    riego_mirca_LAM = cbind(riego_mirca_LAM,riego_mirca[ind_LAM,3])  #create new monthly columns
  }
  
  secano_mirca = rasterToPoints(secano_stack_p5[[j]])  #jth layer in stack
  ind_LAM = match(paste(crop_pixel$x,crop_pixel$y),paste(secano_mirca[,'x'],secano_mirca[,'y']))
  if (j==1)  {
    secano_mirca_LAM = secano_mirca[ind_LAM,]
  }  else{
    secano_mirca_LAM = cbind(secano_mirca_LAM,secano_mirca[ind_LAM,3])  #create new monthly columns
  }
  
  print(j)
}

#find the maximum area across months
test = apply(riego_mirca_LAM[,3:14],1,max)  
riego_mirca_LAM = cbind(riego_mirca_LAM[,1:2],test)
test = apply(secano_mirca_LAM[,3:14],1,max)  
secano_mirca_LAM = cbind(secano_mirca_LAM[,1:2],test)

#calculate % irrigated by pixel
crop_RS = data.frame(x=riego_mirca_LAM[,1],y=riego_mirca_LAM[,2],riego.area=riego_mirca_LAM[,3],secano.area=secano_mirca_LAM[,3])
crop_RS$total.area = crop_RS$riego.area + crop_RS$secano.area  #calculate total area
crop_RS$riego.pct = crop_RS$riego.area/crop_RS$total.area*100
crop_RS$secano.pct = crop_RS$secano.area/crop_RS$total.area*100

#try ploting results (by area or percent)
Map_LatinAmerica<-shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")
par(mar=c(2.1,2.1,2.1,1.1))

data_seq = seq(0,100,5)  #for percentages
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))

cols = col_pal[cut(crop_RS$riego.pct,data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_RS[,1:2], col = cols, pch=16, cex=0.5)
title('% Irrigated')

cols = col_pal[cut(crop_RS$secano.pct,data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_RS[,1:2], col = cols, pch=16, cex=0.5)
title('% Rainfed')

data_seq = quantile(crop_RS$total.area,seq(0,1,.01))
data_seq = c(0,data_seq[which(data_seq>0)[1]:length(data_seq)])  #get rid of repeating zeros
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))
cols = col_pal[cut(crop_RS$total.area,data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_RS[,1:2], col = cols, pch=16, cex=0.5)
title('Total area')

#some cells that have sowing dates, but no growing area (re MIRCA,look at SPAM?)
none = which(crop_RS$total.area==0)
length(none)/dim(crop_RS)[1]
sum(crop_RS$riego.pct>0,na.rm=T)/sum(crop_RS$total.area>0)
sum(crop_RS$secano.pct>0,na.rm=T)/sum(crop_RS$total.area>0)

#Look at correlations b/w irrigated area & N.app
eval(parse(text=paste("load('Z:/08-Cells_toRun/",crops[k],".loc.cal.Rdat')",sep='')))
eval(parse(text=paste("ind_N = match(paste(crop_RS$x,crop_RS$y),paste(",crops[k],".loc.cal$Longitude,",crops[k],".loc.cal$Latitude))",sep='')))
eval(parse(text=paste("crop_RS$N.app = ",crops[k],".loc.cal[ind_N,'N.app']",sep='')))  #add N app to riego/ secano matrix
cor(crop_RS$riego.pct,crop_RS$N.app,use='pairwise.complete.obs')  #should be +

#map N.app
data_seq = quantile(crop_RS$N.app,seq(0,1,.12))
data_seq = c(0,data_seq[which(data_seq>0)[1]:length(data_seq)])  #get rid of repeating zeros
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))
cols = col_pal[cut(crop_RS$N.app,data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(crop_RS[,1:2], col = cols, pch=16, cex=0.5)
title('N app')

#also add MIRCA sowing dates to crop_RS
eval(parse(text=paste("crop_RS$mirca.irr.start = ",crops[k],".loc.cal[ind_N,'mirca.irr.start']",sep='')))  #add N app to riego/ secano matrix
eval(parse(text=paste("crop_RS$mirca.irr.end = ",crops[k],".loc.cal[ind_N,'mirca.irr.end']",sep='')))
eval(parse(text=paste("crop_RS$mirca.rf.start = ",crops[k],".loc.cal[ind_N,'mirca.rf.start']",sep='')))
eval(parse(text=paste("crop_RS$mirca.rf.end = ",crops[k],".loc.cal[ind_N,'mirca.rf.end']",sep='')))
ind_irr = which(crop_RS$mirca.irr.start>0 & crop_RS$total.area>0) #cells with irrigated sowing dates and some area
sum(crop_RS[ind_irr,'riego.pct']==0)/length(ind_irr)*100  #% of irrigated cells with sowing date but no area
ind_rf = which(crop_RS$mirca.rf.start>0 & crop_RS$total.area>0) #cells with irrigated sowing dates and some area
sum(crop_RS[ind_rf,'secano.pct']==0)/length(ind_rf)*100

#Scale N.app for riego/ secano
crop_RS$N.app.rf = crop_RS$N.app/(2*crop_RS$riego.pct + crop_RS$secano.pct)*100
crop_RS$N.app.irr = crop_RS$N.app.rf * 2

#Save results
#put pixel ID's back in before saving
eval(parse(text=paste("save(crop_RS,file='Z:/09-MIRCA2000/Growing_area_grids/RS/",crops[k],"_RS.Rdat')",sep='')))
