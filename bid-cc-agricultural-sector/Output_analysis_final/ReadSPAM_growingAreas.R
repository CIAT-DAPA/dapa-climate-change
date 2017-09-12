#Load SPAM data

#load libraries & static data
library('raster')
library('ncdf4')
crops = c('Maize','Potatoes','Rice','Soybeans','Wheat','Bean')
crops_spam = c('maize','potato','rice','soybean','wheat','bean')
k=3

#Load SPAM2005 rasters
# eval(parse(text=paste("riego.phys.area = raster('Z:/07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_",crops_spam[k],"_irrigated.nc')",sep='')))
# eval(parse(text=paste("secano.phys.area = raster('Z:/07-SPAM_data/SPAM2005/physical.area/spam2005v2r0_physical-area_",crops_spam[k],"_rainfed.nc')",sep='')))
eval(parse(text=paste("riego.phys.area = raster('D:/Tobackup/BID/SPAM_data/SPAM2005_physical.area/spam2005v2r0_physical-area_",crops_spam[k],"_irrigated.nc')",sep='')))
eval(parse(text=paste("secano.phys.area = raster('D:/Tobackup/BID/SPAM_data/SPAM2005_physical.area/spam2005v2r0_physical-area_",crops_spam[k],"_rainfed.nc')",sep='')))

#aggregate from 5-arc minute (or 1/12 degree) to half degree
riego.area.p5 = aggregate(riego.phys.area, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
riego.pts.p5 = rasterToPoints(riego.area.p5)  #pull out the points in SPAM data
secano.area.p5 = aggregate(secano.phys.area, fact=6, fun=sum, expand=TRUE, na.rm=TRUE)
secano.pts.p5 = rasterToPoints(secano.area.p5)  #pull out the points in SPAM data

#Cut out coordinates in Latin America by crop
#load pixels for climate data
load('Z:/14-ObjectsR/id_coordinates.Rdat')  #coordinates of DSSAT runs by crop
eval(parse(text=paste('crop_pixel = id_pixel[which(id_pixel$',crops[k],'==1),]',sep='')))

#match to SPAM data
ind_LAM_riego = match(paste(crop_pixel$x,crop_pixel$y),paste(round(riego.pts.p5[,'x'],2),round(riego.pts.p5[,'y'],2)))
riego.pts.LAM = riego.pts.p5[ind_LAM_riego,]
#ind_bad = which(is.na(ind_LAM_riego))
#riego.pts.LAM = riego.pts.LAM[-ind_bad,]  #get rid of "data" cells not found in SPAM
ind_LAM_secano = match(paste(crop_pixel$x,crop_pixel$y),paste(round(secano.pts.p5[,'x'],2),round(secano.pts.p5[,'y'],2)))
secano.pts.LAM = secano.pts.p5[ind_LAM_secano,]
#ind_bad = which(is.na(ind_LAM_secano))
#secano.pts.LAM = secano.pts.LAM[-ind_bad,]  #get rid of "data" cells not found in SPAM

#calculate % irrigated by pixel
crop_RS = data.frame(x=riego.pts.LAM[,1],y=riego.pts.LAM[,2],riego.area=riego.pts.LAM[,3],secano.area=secano.pts.LAM[,3])
crop_RS$total.area = crop_RS$riego.area + crop_RS$secano.area  #calculate total area
crop_RS$riego.pct = crop_RS$riego.area/crop_RS$total.area*100
crop_RS$secano.pct = crop_RS$secano.area/crop_RS$total.area*100

#some cells that have sowing dates, but no growing area (re MIRCA,look at SPAM?)
none = which(crop_RS$total.area==0)
length(none)/dim(crop_RS)[1]*100
sum(crop_RS$riego.pct>0,na.rm=T)/sum(crop_RS$total.area>0,na.rm=T)*100
sum(crop_RS$secano.pct>0,na.rm=T)/sum(crop_RS$total.area>0,na.rm=T)*100

#Look at correlations b/w irrigated area & N.app
eval(parse(text=paste("load('Z:/08-Cells_toRun/",crops[k],".loc.cal.Rdat')",sep='')))
eval(parse(text=paste("ind_N = match(paste(round(crop_RS$x,2),round(crop_RS$y,2)),paste(round(",crops[k],".loc.cal$Longitude,2),round(",crops[k],".loc.cal$Latitude,2)))",sep='')))
eval(parse(text=paste("crop_RS$N.app = ",crops[k],".loc.cal[ind_N,'N.app']",sep='')))  #add N app to riego/ secano matrix
cor(crop_RS$riego.pct,crop_RS$N.app,use='pairwise.complete.obs')  #with % riego, should be +
cor(crop_RS$riego.area,crop_RS$N.app,use='pairwise.complete.obs')  #with area riego, should be +

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
eval(parse(text=paste("save(crop_RS,file='Z:/07-SPAM_data/SPAM2005/physical.area/RS/",crops[k],"_RS.Rdat')",sep='')))

##COMPARE MIRCA & SPAM DATA FOR IRRIGATED/ RAINFED AREA
eval(parse(text=paste("load('Z:/07-SPAM_data/SPAM2005/physical.area/RS/",crops[k],"_RS.Rdat')",sep='')))
spam=crop_RS
eval(parse(text=paste("load('Z:/09-MIRCA2000/Growing_area_grids/RS/",crops[k],"_RS.Rdat')",sep='')))
mirca=crop_RS

#plot irrigated/ rainfed area for each dataset
Map_LatinAmerica<-shapefile("Z:/03-Map_LatinAmerica/Latino_America.shp")
par(mar=c(2.1,2.1,2.1,1.1))

data_seq = quantile(c(mirca$riego.area,mirca$secano.area,spam$riego.area,spam$secano.area),seq(0,1,.01),na.rm=T)
data_seq = c(0,data_seq[which(data_seq>0)[1]:length(data_seq)])  #get rid of repeating zeros
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))

ind.good = which(mirca$riego.area>0)
cols = col_pal[cut(mirca$riego.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(mirca[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Irrigated area: MIRCA')

ind.good = which(mirca$secano.area>0)
cols = col_pal[cut(mirca$secano.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(mirca[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Rainfed area: MIRCA')

ind.good = which(spam$riego.area>0)
cols = col_pal[cut(spam$riego.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(spam[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Irrigated area: SPAM')

ind.good = which(spam$secano.area>0)
cols = col_pal[cut(spam$secano.area[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(spam[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('Rainfed area: SPAM')

#plot % irrigated/ rainfed for each dataset
data_seq = seq(0,100,5)  #for percentages
col_pal = colorRampPalette(c('darkblue','orange'))(length(data_seq))

ind.good = which(mirca$riego.pct>0)
cols = col_pal[cut(mirca$riego.pct[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(mirca[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('% Irrigated: MIRCA')

ind.good = which(mirca$secano.pct>0)
cols = col_pal[cut(mirca$secano.pct[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(mirca[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('% Rainfed: MIRCA')

ind.good = which(spam$riego.pct>0)
cols = col_pal[cut(spam$riego.pct[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(spam[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('% Irrigated: SPAM')

ind.good = which(spam$secano.pct>0)
cols = col_pal[cut(spam$secano.pct[ind.good],data_seq,include.lowest=T)]
plot(Map_LatinAmerica,xlim=c(-110,-35),ylim=c(-50,30))
points(spam[ind.good,1:2], col = cols, pch=16, cex=0.5)
title('% Rainfed: SPAM')