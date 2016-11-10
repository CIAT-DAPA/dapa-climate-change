cultivos = c('maiz','arroz','soya','frijol','trigo')
c=1

#load DSSAT yield changes
load(paste("D:/tobackup/BID/FAOSTAT_validation/country.pctCh_",cultivos[c],"_optVariety.Rdat",sep=""))

#load FAOSTAT yields & calculate historical growth rates
fao.ylds=read.table('D:/Tobackup/BID/articulo_cientifico/FAOSTAT_yields/maize_ylds.csv',header=T,sep=',')
colnames(fao.ylds)[colnames(fao.ylds)=='Dominican.Republic']='Dominican Republic'
colnames(fao.ylds)[colnames(fao.ylds)=='El.Salvador']='El Salvador'
colnames(fao.ylds)[colnames(fao.ylds)=='Costa.Rica']='Costa Rica'
colnames(fao.ylds)[colnames(fao.ylds)=='Trinidad.and.Tobago']='Trinidad and Tobago'
colnames(fao.ylds)[colnames(fao.ylds)=='French.Guiana']='French Guiana'
p=dim(fao.ylds)[2]-1  #number of countries
growth.pais = mat.or.vec(p,1)
for (j in 1:(dim(fao.ylds)[2]-1))  {
  #reg.model=lm(fao.ylds[,j+1]~fao.ylds[,1])
  reg.model=lm(fao.ylds[,j+1]~fao.ylds[,1]+eval(fao.ylds[,1]^2))
  growth.pais[j]=(tail(reg.model$fitted,n=1)-reg.model$fitted[1])/reg.model$fitted[1]*100
}
names(growth.pais)=colnames(fao.ylds)[2:(p+1)]

#match countries
ind.fao=match(names(pct.ch.country),names(growth.pais))
dat.all=cbind(pct.ch.country,growth.pais[ind.fao])

#regress technology growth on CC
test = lm(dat.all[,2]~dat.all[,1],x=T)
plot(dat.all[,1],dat.all[,2],xlab='CC impact (%): 1971-2049',ylab='Yield growth: 1971-2014')
points(test$x[,2],test$fitted,pch=20,col='red')
text(20,100,paste('slope = ',round(summary(test)$coef[2,1],2),', p = ',round(summary(test)$coef[2,4],3)))
title('Maize')
