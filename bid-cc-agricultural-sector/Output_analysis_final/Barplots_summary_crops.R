#Across-crop comparison with error bars

#Load libraries
library('gplots')

#static data
cultivos = c('maiz','arroz','trigo','soya','frijol')
cultivos.en = c('maize','rice','wheat','soybean','dry bean')

#Load input data
for (c in 1:length(cultivos))  {  #1:length(cultivos)
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".ch.r.Rdat')",sep="")))  #Z:/12-Resultados
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".ch.s.Rdat')",sep="")))
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".cv.r.Rdat')",sep="")))
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".cv.s.Rdat')",sep="")))
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".fail.r.Rdat')",sep="")))
  eval(parse(text=paste("load('D:/tobackup/BID/Resultados_DSSAT/summaries/",cultivos[c],".fail.s.Rdat')",sep="")))
}
regions = dimnames(arroz.ch.r)[[2]]

#Merge data frames by region & make plots
#Plot mean
par(ask=T)
for (r in 1:5)  {
  crop.all = data.frame(arroz.r = arroz.ch.r[,r], arroz.s = arroz.ch.s[,r],frijol.r = frijol.ch.r[,r], frijol.s = frijol.ch.s[,r],trigo.r = trigo.ch.r[,r], trigo.s = trigo.ch.s[,r], maiz.r = maiz.ch.r[,r], maiz.s = maiz.ch.s[,r], soya.r = soya.ch.r[,r], soya.s = soya.ch.s[,r])
  crop.all = as.matrix(crop.all)
  
  #create barplot with errorbars
  barplot2(crop.all[3,],plot.ci=T,ci.l=crop.all[6,],ci.u=crop.all[7,])
  title(paste('Area-weighted yield changes for ',regions[r],sep=''))
}

#CV plots
for (r in 1:5)  {
  crop.cv = data.frame(arroz.r = arroz.cv.r[,r], arroz.s = arroz.cv.s[,r],frijol.r = frijol.cv.r[,r], frijol.s = frijol.cv.s[,r],trigo.r = trigo.cv.r[,r], trigo.s = trigo.cv.s[,r], maiz.r = maiz.cv.r[,r], maiz.s = maiz.cv.s[,r], soya.r = soya.cv.r[,r], soya.s = soya.cv.s[,r])
  crop.cv = as.matrix(crop.cv)
  
  #create barplot with errorbars
  barplot2(crop.cv[1:2,],beside=T,plot.ci=T,ci.l=rbind(NA,crop.cv[5,]),ci.u=rbind(NA,crop.cv[6,]))
  title(paste('Inter-annual CV changes for ',regions[r],sep=''))
}

#Now merge data frames by crop system & make plots
#Plot mean
par(ask=T)
for (c in 1:5)  {
  eval(parse(text=paste("crop.ch.c = rbind(",cultivos[c],".ch.r[c(3,6:8),],",cultivos[c],".ch.s[c(3,6:8),])",sep="")))
  rownames(crop.ch.c)[c(1,5)] = c('Riego','Secano')
  ind.low.r = which(crop.ch.c[4,]<5000)
  ind.low.s = which(crop.ch.c[8,]<5000)
  if (length(ind.low.r)>0) {crop.ch.c[1:3,ind.low.r]=NA}
  if (length(ind.low.s)>0) {crop.ch.c[5:7,ind.low.s]=NA}
  
  #create barplot with errorbars
  barplot2(crop.ch.c[c(1,5),],beside=T,ylim=c(-30,30),plot.ci=T,ci.l=crop.ch.c[c(2,6),],ci.u=crop.ch.c[c(3,7),],col=c('blue','green'),ylab='% yield change')
  abline(h=0)
  title(paste(cultivos.en[c],sep=''))
  if (c==5)  {legend('topright',c('Irrigated','Rainfed'),fill=c('blue','green'))}
}

#CV plots
par(ask=T)
for (c in 1:5)  {
  
  eval(parse(text=paste("crop.cv.c = rbind(",cultivos[c],".cv.r[c(1:2,5:6),],",cultivos[c],".cv.s[c(1:2,5:6),])",sep="")))
  eval(parse(text=paste("crop.ch.c = rbind(",cultivos[c],".ch.r[c(3,6:8),],",cultivos[c],".ch.s[c(3,6:8),])",sep="")))
  ind.low.r = which(crop.ch.c[4,]<5000)
  ind.low.s = which(crop.ch.c[8,]<5000)
  if (length(ind.low.r)>0) {crop.cv.c[1:4,ind.low.r]=NA}
  if (length(ind.low.s)>0) {crop.cv.c[5:8,ind.low.s]=NA}
  
  #create barplot with errorbars
  barplot2(crop.cv.c[c(1:2,5:6),],beside=T,ylim=c(0,45),plot.ci=T,ci.l=rbind(NA,crop.cv.c[3,],NA,crop.cv.c[7,]),ci.u=rbind(NA,crop.cv.c[4,],NA,crop.cv.c[8,]),col=c('blue','blue','green','green'),density=c(-1,30,-1,30),ylab='Interannual CV (%)')
  title(paste(cultivos.en[c],sep=''))
  #if (c==5)  {
    legend('top',c('Irrigated - Baseline','Irrigated - Multi-model mean','Rainfed - Baseline','Rainfed - Multi-model mean'),fill=c('blue','blue','green','green'),density=c(-1,50,-1,50))
  #}
}

#Crop failures plots
for (c in 1:5)  {
  
  eval(parse(text=paste("crop.fail.c = rbind(",cultivos[c],".fail.r[c(1:2,5:6),],",cultivos[c],".fail.s[c(1:2,5:6),])",sep="")))
  eval(parse(text=paste("crop.ch.c = rbind(",cultivos[c],".ch.r[c(3,6:8),],",cultivos[c],".ch.s[c(3,6:8),])",sep="")))
  ind.low.r = which(crop.ch.c[4,]<5000)
  ind.low.s = which(crop.ch.c[8,]<5000)
  if (length(ind.low.r)>0) {crop.cv.c[1:4,ind.low.r]=NA}
  if (length(ind.low.s)>0) {crop.cv.c[5:8,ind.low.s]=NA}
  
  #create barplot with errorbars
  barplot2(crop.fail.c[c(1:2,5:6),],beside=T,ylim=c(0,20),plot.ci=T,ci.l=rbind(NA,crop.fail.c[3,],NA,crop.fail.c[7,]),ci.u=rbind(NA,crop.fail.c[4,],NA,crop.fail.c[8,]),col=c('blue','blue','green','green'),density=c(-1,30,-1,30),ylab='Crop failures (% area)')
  title(paste(cultivos.en[c],sep=''))
  if (c==5)  {
  legend('topright',c('Irrigated - Baseline','Irrigated - Multi-model mean','Rainfed - Baseline','Rainfed - Multi-model mean'),fill=c('blue','blue','green','green'),density=c(-1,50,-1,50))
  }
}

#Create plot comparing crops across Latin America
crop.all.r = data.frame(maiz.r = maiz.ch.r[,6], arroz.r = arroz.ch.r[,6], trigo.r = trigo.ch.r[,6], frijol.r = frijol.ch.r[,6], soya.r = soya.ch.r[,6])
crop.all.s = data.frame(maiz.s = maiz.ch.s[,6], arroz.s = arroz.ch.s[,6], trigo.s = trigo.ch.s[,6], frijol.s = frijol.ch.s[,6], soya.s = soya.ch.s[,6])
crop.all = rbind(as.matrix(crop.all.r),as.matrix(crop.all.s))
colnames(crop.all)=c('Maize','Rice','Wheat','Dry Bean','Soybean')
barplot2(crop.all[c(3,11),],beside=T,plot.ci=T,ci.l=crop.all[c(6,14),],ci.u=crop.all[c(7,15),],col=c('blue','green'),ylab='% yield change')
title('Area-weighted yield changes for Latin America')
legend('topleft',c('Irrigated','Rainfed'),fill=c('blue','green'))

#generate results for table
round(maiz.cv.r,2)
maiz.cv.r[2,6] - maiz.cv.r[1,6]  #MMM - WFD
#c(maiz.cv.r[3,6] - maiz.cv.r[1,6],maiz.cv.r[4,6] - maiz.cv.r[1,6])  #range of models relative to WFD
c(maiz.cv.r[5,6] - maiz.cv.r[1,6],maiz.cv.r[6,6] - maiz.cv.r[1,6])  #bootstrapped changes

round(maiz.cv.s,2)
maiz.cv.s[2,6] - maiz.cv.s[1,6]
#c(maiz.cv.s[3,6] - maiz.cv.s[1,6],maiz.cv.s[4,6] - maiz.cv.s[1,6])  #range
c(maiz.cv.s[5,6] - maiz.cv.s[1,6],maiz.cv.s[6,6] - maiz.cv.s[1,6])  #intervalos de confianza

#check to see if mean change > CV & significant
abs(maiz.ch.r[3,])>maiz.cv.r[1,]& maiz.ch.r[8,]>5000 & (sign(maiz.ch.r[6,])==sign(maiz.ch.r[7,]))
abs(arroz.ch.r[3,])>arroz.cv.r[1,]& arroz.ch.r[8,]>5000& (sign(arroz.ch.r[6,])==sign(arroz.ch.r[7,]))
abs(soya.ch.r[3,])>soya.cv.r[1,]& soya.ch.r[8,]>5000& (sign(soya.ch.r[6,])==sign(soya.ch.r[7,]))

abs(maiz.ch.s[3,])>maiz.cv.s[1,]& maiz.ch.s[8,]>5000& (sign(maiz.ch.s[6,])==sign(maiz.ch.s[7,]))
abs(arroz.ch.s[3,])>arroz.cv.s[1,]& arroz.ch.s[8,]>5000& (sign(arroz.ch.s[6,])==sign(arroz.ch.s[7,]))
abs(soya.ch.s[3,])>soya.cv.s[1,]& soya.ch.s[8,]>5000& (sign(soya.ch.s[6,])==sign(soya.ch.s[7,]))

#look for sig changes in CV
((maiz.cv.r[2,] - maiz.cv.r[1,])>5 & maiz.cv.r[5,]>maiz.cv.r[1,]& maiz.ch.r[8,]>5000) 
((arroz.cv.r[2,] - arroz.cv.r[1,])>5 & arroz.cv.r[5,]>arroz.cv.r[1,]& arroz.ch.r[8,]>5000)
((soya.cv.r[2,] - soya.cv.r[1,])>5 & soya.cv.r[5,]>soya.cv.r[1,]& soya.ch.r[8,]>5000)

((maiz.cv.s[2,] - maiz.cv.s[1,])>5 & maiz.cv.s[5,]>maiz.cv.s[1,]& maiz.ch.s[8,]>5000)
((arroz.cv.s[2,] - arroz.cv.s[1,])>5 & arroz.cv.s[5,]>arroz.cv.s[1,]& arroz.ch.s[8,]>5000)
((soya.cv.s[2,] - soya.cv.s[1,])>5 & soya.cv.s[5,]>soya.cv.s[1,]& soya.ch.s[8,]>5000)
