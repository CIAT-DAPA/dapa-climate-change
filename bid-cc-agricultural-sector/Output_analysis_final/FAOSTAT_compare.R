#load libraries
library('reshape2')
library('gplots')
library('boot')

#set crop of interest here
c=1  #maize
# material = 'IB0011_XL45'
#c=2  #rice
#c=3  #soy
#c=4  #frijol
#c=5 #trigo

cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybeans','Beans','Wheat')
cultivos.en2 = c('Maize','Rice','Soybean','Dry bean','Wheat')

faostat = read.table('D:/tobackup/BID/FAOSTAT_validation/FAOSTAT_yields_validation.csv',sep=',',header=T)

#update country names
faostat$AreaName = as.character(faostat$AreaName)
faostat$AreaName[faostat$AreaName=='Dominican Republic']='Dominican.Republic'
faostat$AreaName[faostat$AreaName=='El Salvador']='El.Salvador'
faostat$AreaName[faostat$AreaName=='Costa Rica']='Costa.Rica'
faostat$AreaName[faostat$AreaName=='Trinidad and Tobago']='Trinidad.and.Tobago'
faostat$AreaName[faostat$AreaName=='French Guiana']='French.Guiana'
faostat$AreaName[faostat$AreaName=='Bolivia (Plurinational State of)']='Bolivia'
faostat$AreaName[faostat$AreaName=='Venezuela (Bolivarian Republic of)']='Venezuela'

#extract data by crop
crop.f = faostat[faostat$ItemName==cultivos.en[c],]
crop.f = t(acast(crop.f,AreaName~Year))

#pull out relevant years & discard countries with NA's
# ind.wfd = which(rownames(crop.f)>=1972 & rownames(crop.f)<=1997)
ind.wfd = which(rownames(crop.f)>=1971 & rownames(crop.f)<=1998)
crop.f = crop.f[ind.wfd,]
crop.na = which(apply(crop.f,2,function(x) sum(is.na(x)))>0)
if (length(crop.na)>0) {crop.f = crop.f[,-crop.na]}

#remove time trend from FAOSTAT yields
yrs=1971:1998
crop.f.detrend = apply(crop.f,2,function(x) resid(lm(x~yrs))+mean(x))

#load DSSAT yields
dssat.yld = read.csv(paste("D:/tobackup/BID/FAOSTAT_validation/country.ylds_",cultivos[c],"_optVariety_cosecha.csv",sep=""),row.names=1)
dssat.yld=dssat.yld[which(rownames(dssat.yld)>=1971 & rownames(dssat.yld)<=1998),]

#calculate correlations & plot
ind.fao = match(colnames(dssat.yld),colnames(crop.f.detrend))  #match countries first
na.fao = which(is.na(ind.fao))
if (length(na.fao)>=1)  {
  country.na = colnames(dssat.yld)[na.fao]  #identify missing country
  ind.fao = ind.fao[is.na(ind.fao)==F]  #eliminate missing countries in FAO
  dssat.yld = dssat.yld[,-na.fao]
}

cor.country = mat.or.vec(length(ind.fao),1)
mean.ylds = mat.or.vec(length(ind.fao),2)
colnames(mean.ylds) = c('FAO','DSSAT')
rownames(mean.ylds) = colnames(dssat.yld)
names(cor.country) = colnames(dssat.yld)
par(ask=T)
for (j in 1:length(ind.fao))  {
  cor.x = cor(crop.f.detrend[,ind.fao[j]],dssat.yld[,j],use='pairwise.complete.obs')
  cor.country[j] = cor.x
  mean.ylds[j,] = c(mean(crop.f.detrend[,ind.fao[j]],na.rm=T),mean(dssat.yld[,j],na.rm=T))
  ylim.c = range(c(crop.f.detrend[,ind.fao[j]],dssat.yld[,j]),na.rm=T)
  plot(dssat.yld[,j],ylim=ylim.c,type='o',xlab='years',ylab='yields')
  points(crop.f.detrend[,ind.fao[j]],pch=20,col='red',type='o')
  #legend('right',c('DSSAT','FAO'),pch=c(1,20),col=c('black','red'),inset=0.02,cex=0.5)
  title(colnames(dssat.yld)[j])
  text(4,mean(ylim.c),paste('Cor =',round(cor.x,2)))
}

#area-weighted average of correlations
crop.areas = read.csv(paste('D:/tobackup/BID/FAOSTAT_validation/country.areas_',cultivos[c],'.csv',sep=''),row.names=1)
if (length(na.fao)>=1) {
  ind.bad = which(crop.areas$country==country.na)
  crop.areas = crop.areas[-ind.bad,]
}

cor.country = cor.country[order(names(cor.country))]
sum(cor.country*crop.areas$total)/sum(crop.areas$total)  #value in paper
cor.country[order(crop.areas[,4],decreasing = T)]  #by area
mean.ylds = mean.ylds[order(rownames(mean.ylds)),]
prod = mean.ylds[,1]*crop.areas[,4]
cor.country[order(prod,decreasing = T)]  #by production

#full spatiotemporal correlation
ind.fao = match(colnames(dssat.yld),colnames(crop.f.detrend))
cor(c(as.matrix(dssat.yld)),c(crop.f.detrend[,ind.fao]),use='pairwise.complete.obs')

#Calculate yield CV
fao.cv = apply(crop.f,2,function(x) sd(x)/mean(x)*100)  #calculate yield CV by country
dssat.cv = apply(dssat.yld[,order(colnames(dssat.yld))],2,function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)
crop.areas2 = data.frame(crop.areas$country,crop.areas$total)
crop.areas2$crop.areas.country = as.character(crop.areas2$crop.areas.country)
crop.areas2[,1] = gsub(" ", ".", crop.areas2[,1])
ind.area = match(names(fao.cv),crop.areas2[,1])
sum(fao.cv*crop.areas2[ind.area,2],na.rm=T)/sum(crop.areas2[ind.area,2],na.rm=T)  #faostat area-weighted CV

#Plot time series for 5 top area countries
ind.top5 = as.character(crop.areas[order(crop.areas[,4],decreasing = T),1][1:5])
ind.fao2 = match(ind.top5,colnames(crop.f.detrend))
ind.dssat2 = match(ind.top5,colnames(dssat.yld))
ylim.c = range(c(crop.f.detrend[,ind.fao2],dssat.yld[,ind.dssat2]),na.rm=T)
par(ask=F)
plot(dssat.yld[,ind.dssat2[1]],ylim=ylim.c,type='o',xlab='years',ylab='yields')
points(crop.f.detrend[,ind.fao2[1]],pch=20,type='o')
colors=c('red','blue','green','orange')
for (j in 2:5)  {
  points(dssat.yld[,ind.dssat2[j]],type='o',col=colors[j-1])
  points(crop.f.detrend[,ind.fao2[j]],pch=20,type='o',col=colors[j-1])
}
legend('top',colnames(dssat.yld[,ind.dssat2]),col=c('black',colors),lty=1,pch=20)

#barplot of mean yields (ordered by FAOSTAT ylds)
#Try color-coding country labels by region?
NOR = c('Mexico','Cuba','Belize','Guatemala','Honduras','El.Salvador','Nicaragua','Dominican.Republic','Jamaica','Haiti')
CEN = c('Costa.Rica','Panama','Colombia','Venezuela','Trinidad.and.Tobago','Guyana','French.Guiana','Suriname')
AND = c('Ecuador','Peru','Bolivia')
BRA = c('Brazil','Paraguay')
SUR = c('Argentina','Uruguay','Chile')

regions = character(dim(mean.ylds)[1])
ind.NOR = match(rownames(mean.ylds),NOR)
regions[which(is.na(ind.NOR)==F)]='orange'
ind.CEN = match(rownames(mean.ylds),CEN)
regions[which(is.na(ind.CEN)==F)]='yellow'
ind.BRA = match(rownames(mean.ylds),BRA)
regions[which(is.na(ind.BRA)==F)]='green'
ind.AND = match(rownames(mean.ylds),AND)
regions[which(is.na(ind.AND)==F)]='blue'
ind.SUR = match(rownames(mean.ylds),SUR)
regions[which(is.na(ind.SUR)==F)]='red'

par(ask=F)
par(mar=c(6.1, 4.1, 2.1, 2.1))
ind.fao = order(mean.ylds[,1],decreasing=T)
barplot2(t(mean.ylds[ind.fao,]),width=0.8,xlim=c(0,58),ylim=c(0,max(mean.ylds)+25),beside=T,col=rbind(regions[ind.fao],regions[ind.fao]),density=c(NA,30),las=2,yaxt='n',xaxt='n',xlab='')
axis(side = 2, at = seq(0,max(mean.ylds)+25,by=500))
abline(h=0)
text(seq(0.8,dim(mean.ylds)[1]*3*0.8,by=0.8*3), par("usr")[3]-50, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(rownames(mean.ylds[ind.fao,])), cex=0.8)
#legend('topright',c('FAO','DSSAT'),fill='black',density=c(NA,50),cex=0.8)
abline(h=(sum(mean.ylds[,1]*crop.areas$total)/sum(crop.areas$total)),col='black')
abline(h=(sum(mean.ylds[,2]*crop.areas$total)/sum(crop.areas$total)),lty=3,col='black')
title(cultivos.en[c])

cor(mean.ylds[,1],mean.ylds[,2],use='pairwise.complete.obs')  #correlation of mean yields across countries
mean.ylds = mean.ylds[order(rownames(mean.ylds)),]  #make sure yields alphabetized
corr(mean.ylds,w=crop.areas$total)  #spatial correlation weighted by harvested area (in paper)

#barplots for just 5 top countries
ind.top5 = order(crop.areas[,4],decreasing = T)[1:5]
cor.country = cor.country[order(names(cor.country))]  #make sure correlations alphabetized

#ylim.c = c(0,max(mean.ylds[ind.top5,])*1.1)
if (c<=2|c==5) {ylim.c=c(0,9000)}  else{ylim.c=c(0,4000)}
barplot(t(mean.ylds[ind.top5,]),beside=T,col=c('red','blue'),las=2,ylim=ylim.c,xaxt='n',xlab='')
abline(h=(sum(mean.ylds[,1]*crop.areas$total)/sum(crop.areas$total)),lty=2,col='red')
abline(h=(sum(mean.ylds[,2]*crop.areas$total)/sum(crop.areas$total)),lty=2,col='blue')
abline(h=0)
text(seq(1.1,5*3*1.1,by=1.1*3), par("usr")[3]-100, 
     srt = 55, adj= 1, xpd = TRUE,
     labels = paste(rownames(mean.ylds[ind.top5,])))
title(cultivos.en2[c])
text(seq(1.75,15,by=3),apply(mean.ylds[ind.top5,],1,max)+200,round(cor.country[ind.top5],2),cex=1)
#legend('topleft',c('FAO','DSSAT'),fill=c('red','blue'))

#check coefficient of variation (across countries) in mean yields
sd(mean.ylds[,1])/mean(mean.ylds[,1])*100  #FAO
sd(mean.ylds[,2])/mean(mean.ylds[,2])*100  #DSSAT
