#Compare aggregated yields with & without CO2
cultivos = c('maiz','arroz','soya','frijol','trigo')
path.res = "E:/CIAT_backup/BID/Resultados_DSSAT/"
c=1
regions= c('MEX','CEN','AND','BRA','SUR','LAC')

eval(parse(text=paste("load('",path.res,"summaries/sinCO2/",cultivos[c],".ch.r.Rdat')",sep="")))  
eval(parse(text=paste("load('",path.res,"summaries/sinCO2/",cultivos[c],".ch.s.Rdat')",sep="")))  
eval(parse(text=paste("sinCO2.r = ",cultivos[c],".ch.r",sep="")))
eval(parse(text=paste("sinCO2.s = ",cultivos[c],".ch.s",sep="")))

eval(parse(text=paste("load('",path.res,"summaries/conCO2/",cultivos[c],".ch.r.Rdat')",sep="")))  
eval(parse(text=paste("load('",path.res,"summaries/conCO2/",cultivos[c],".ch.s.Rdat')",sep="")))  
eval(parse(text=paste("conCO2.r = ",cultivos[c],".ch.r",sep="")))
eval(parse(text=paste("conCO2.s = ",cultivos[c],".ch.s",sep="")))


#Graph results
library('gplots')

crop.ch = rbind(sinCO2.r[c(3,6:7),],conCO2.r[c(3,6:7),],sinCO2.s[c(3,6:7),],conCO2.s[c(3,6:7),])

#create barplot with errorbars
barplot2(crop.ch[c(1,4,7,10),],beside=T,plot.ci=T,ci.l=crop.ch[c(2,5,8,11),],ci.u=crop.ch[c(3,6,9,12),],col=c('blue','blue','green','green'),density=c(-1,30,-1,30),ylab='Yield change (%)')
title(paste(cultivos[c],sep=''))
legend('bottomright',c('Irrigated - CO2 380','Irrigated - Rising CO2','Rainfed - CO2 380','Rainfed - Rising CO2'),fill=c('blue','blue','green','green'),density=c(-1,50,-1,50))


