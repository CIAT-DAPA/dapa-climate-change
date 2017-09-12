#Compare WFD yields to Iizumi for coincident years

library('raster')

#fijar cultivo aquí
#c=1  #maize
#c=2  #rice
c=3  #soy
#c=4  #frijol
#c=5 #trigo

cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')

#load DSSAT yields
load(paste('D:/Tobackup/BID/Resultados_DSSAT/ylds_evaluacion/',cultivos[c],'_yld_pixel_riego.Rdat',sep=''))
load(paste('D:/Tobackup/BID/Resultados_DSSAT/ylds_evaluacion/',cultivos[c],'_yld_pixel_secano.Rdat',sep=''))

#create weighted average of rainfed & irrigated yields from DSSAT for comparison
#find coincident pixels & do weighted average
#otherwise use just riego or secano
ind.match = match(paste(crop_riego$x,crop_riego$y),paste(crop_secano$x,crop_secano$y))
ind.riego = which(is.na(ind.match)==T)
ind.match.r = which(is.na(ind.match)==F)
ind.match.s = ind.match[is.na(ind.match)==F]  #reset to exclude NA's
ind.secano = match(1:dim(crop_secano)[1],ind.match)
ind.secano = which(is.na(ind.secano))  #indices not in ind.match

#area vectors for weighted averages
area.riego = replicate(dim(yld_riego)[2],crop_riego$riego.area)  #keep same dimension as yield matrix
area.secano = replicate(dim(yld_secano)[2],crop_secano$secano.area)

yld_all = array(NA,dim=c(length(ind.riego)+length(ind.secano)+length(ind.match.s),dim(yld_secano)[2]))
yld_all[1:length(ind.riego),] = yld_riego[ind.riego,]  #riego-only pixels
yld_all[(length(ind.riego)+1):(length(ind.riego)+length(ind.secano)),] = yld_secano[ind.secano,]  #secano-only pixels
yld_all[(length(ind.riego)+length(ind.secano)+1):dim(yld_all)[1],] = (yld_secano[ind.match.s,]*area.secano[ind.match.s,] + yld_riego[ind.match.r,]*area.riego[ind.match.r,])/(area.secano[ind.match.s,]+area.riego[ind.match.r,])
colnames(yld_all) = 1971:1997
apply(yld_all,2,function(x) sum(is.na(x)==F))  #identify "good" years

#create location vectors
crop_all = rbind(crop_riego[ind.riego,c('x','y','country')],crop_secano[ind.secano,c('x','y','country')],crop_riego[ind.match.r,c('x','y','country')])

#load Iizumi data, extract points & match to DSSAT pixels
iizumi = stack(paste('D:/Tobackup/BID/iizumi_raster_data/',cultivos.en[c],'_median.nc',sep=''))
iizumi.pts = rasterToPoints(iizumi)
colnames(iizumi.pts) = c('x','y',1982:2006)

#cut out coincident years (1982 to 1996)
yr.good = 1982:1996
dssat = yld_all[,match(yr.good,colnames(yld_all))]
iizumi.c = iizumi.pts[,match(yr.good,colnames(iizumi.pts))]

#find & extract coincident points
ind.iizumi = match(paste(round(crop_all$x,2),round(crop_all$y,2)),paste(round(iizumi.pts[,1],2),round(iizumi.pts[,2],2)))
ind.dssat = which(is.na(ind.iizumi)==F)
ind.iizumi = ind.iizumi[is.na(ind.iizumi)==F]

#check to make sure locations match
sum(crop_all[ind.dssat,1]!=iizumi.pts[ind.iizumi,1] | round(crop_all[ind.dssat,2],2)!=round(iizumi.pts[ind.iizumi,2],2))

#modify matrices
dssat = dssat[ind.dssat,]
iizumi.c = iizumi.c[ind.iizumi,]
dssat.pts = crop_all[ind.dssat,]
iizumi.pts.c = iizumi.pts[ind.iizumi,1:2]

#do comparison for spatial & temporal correlation 
cor(c(dssat),c(iizumi.c))  #full correlation
cor.pixel = mat.or.vec(dim(dssat)[1],1)  #pixel-wise temporal correlation
for (j in 1:dim(dssat)[1]) {
  cor.pixel[j] = cor(dssat[j,],iizumi.c[j,])
}
hist(cor.pixel)
t.test(cor.pixel)
dssat.mean = apply(dssat,1,mean)
iizumi.mean = apply(iizumi.c,1,mean)
cor(dssat.mean,iizumi.mean)  #spatial correlation

#calculate RMSE as % of mean yields
(sqrt(mean((dssat/1000 - iizumi.c)^2)))/mean(iizumi.c)  #method 1
sqrt(mean(((dssat/1000 - iizumi.c)/iizumi.c)^2))  #method 2
mean(dssat/1000)
mean(iizumi.c)
