tmax<-read.csv("D:/col-cormacarena/estaciones cormacarena/estations_all/datos_seleccionadas/monthly_tmax.csv")
tmax=tmax[-2]

enero<-seq(1,420,12)
promedio<-apply(tmax[enero,],2,function(x) mean(x,na.rm=T))
a=cbind(promedio[2:138])

feb<-seq(2,420,12)
promedio<-apply(tmax[feb,],2,function(x) mean(x,na.rm=T))
b=cbind(promedio[2:138])

mar<-seq(3,420,12)
promedio<-apply(tmax[mar,],2,function(x) mean(x,na.rm=T))
c=cbind(promedio[2:138])

abril<-seq(4,420,12)
promedio<-apply(tmax[abril,],2,function(x) mean(x,na.rm=T))
d=cbind(promedio[2:138])

may<-seq(5,420,12)
promedio<-apply(tmax[may,],2,function(x) mean(x,na.rm=T))
e=cbind(promedio[2:138])

jun<-seq(6,420,12)
promedio<-apply(tmax[jun,],2,function(x) mean(x,na.rm=T))
f=cbind(promedio[2:138])

jul<-seq(7,420,12)
promedio<-apply(tmax[jul,],2,function(x) mean(x,na.rm=T))
g=cbind(promedio[2:138])

ago<-seq(8,420,12)
promedio<-apply(tmax[ago,],2,function(x) mean(x,na.rm=T))
h=cbind(promedio[2:138])

sep<-seq(9,420,12)
promedio<-apply(tmax[sep,],2,function(x) mean(x,na.rm=T))
i=cbind(promedio[2:138])

oct<-seq(10,420,12)
promedio<-apply(tmax[oct,],2,function(x) mean(x,na.rm=T))
j=cbind(promedio[2:138])

nov<-seq(11,420,12)
promedio<-apply(tmax[nov,],2,function(x) mean(x,na.rm=T))
k=cbind(promedio[2:138])

dic<-seq(12,420,12)
promedio<-apply(tmax[dic,],2,function(x) mean(x,na.rm=T))
l=cbind(promedio[2:138])

tmax_prom<-data.frame(a,b,c,d,e,f,g,h,i,j,k,l)
names(tmax_prom)=clmnames <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")


##########country
country=rep("Colombia",length(l))


##########id
ID<-seq(1,length(l),1)


##########source
SOURCE=rep("IDEAM",length(l))

##nyear
nyear=rep(35,length(l))

nombres<-c("ID","SOURCE","COUNTRY","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEAR")
base<-data.frame(ID,SOURCE,country,tmax_prom,nyear,row.names = NULL)
names(base)<-nombres

write.csv(base,"table_tmax.csv",row.names=F)

########################################################tmin
tmin<-read.csv("D:/col-cormacarena/estaciones cormacarena/estations_all/datos_seleccionadas/monthly_tmin.csv")
tmin=tmin[-2]

enero<-seq(1,420,12)
promedio<-apply(tmin[enero,],2,function(x) mean(x,na.rm=T))
a=cbind(promedio[2:138])

feb<-seq(2,420,12)
promedio<-apply(tmin[feb,],2,function(x) mean(x,na.rm=T))
b=cbind(promedio[2:138])

mar<-seq(3,420,12)
promedio<-apply(tmin[mar,],2,function(x) mean(x,na.rm=T))
c=cbind(promedio[2:138])

abril<-seq(4,420,12)
promedio<-apply(tmin[abril,],2,function(x) mean(x,na.rm=T))
d=cbind(promedio[2:138])

may<-seq(5,420,12)
promedio<-apply(tmin[may,],2,function(x) mean(x,na.rm=T))
e=cbind(promedio[2:138])

jun<-seq(6,420,12)
promedio<-apply(tmin[jun,],2,function(x) mean(x,na.rm=T))
f=cbind(promedio[2:138])

jul<-seq(7,420,12)
promedio<-apply(tmin[jul,],2,function(x) mean(x,na.rm=T))
g=cbind(promedio[2:138])

ago<-seq(8,420,12)
promedio<-apply(tmin[ago,],2,function(x) mean(x,na.rm=T))
h=cbind(promedio[2:138])

sep<-seq(9,420,12)
promedio<-apply(tmin[sep,],2,function(x) mean(x,na.rm=T))
i=cbind(promedio[2:138])

oct<-seq(10,420,12)
promedio<-apply(tmin[oct,],2,function(x) mean(x,na.rm=T))
j=cbind(promedio[2:138])

nov<-seq(11,420,12)
promedio<-apply(tmin[nov,],2,function(x) mean(x,na.rm=T))
k=cbind(promedio[2:138])

dic<-seq(12,420,12)
promedio<-apply(tmin[dic,],2,function(x) mean(x,na.rm=T))
l=cbind(promedio[2:138])

tmin_prom<-data.frame(a,b,c,d,e,f,g,h,i,j,k,l)
names(tmin_prom)=clmnames <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")


##########country
country=rep("Colombia",length(l))


##########id
ID<-seq(1,length(l),1)


##########source
SOURCE=rep("IDEAM",length(l))

##nyear
nyear=rep(35,length(l))

nombres<-c("ID","SOURCE","COUNTRY","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEAR")
base<-data.frame(ID,SOURCE,country,tmin_prom,nyear,row.names = NULL)
names(base)<-nombres

write.csv(base,"table_tmin.csv",row.names=F)
########################################################prec
prec<-read.csv("D:/col-cormacarena/estaciones cormacarena/estations_all/datos_seleccionadas/monthly_precip.csv")
prec=prec[-2]

enero<-seq(1,420,12)
promedio<-apply(prec[enero,],2,function(x) mean(x,na.rm=T))
a=cbind(promedio[2:582])

feb<-seq(2,420,12)
promedio<-apply(prec[feb,],2,function(x) mean(x,na.rm=T))
b=cbind(promedio[2:582])

mar<-seq(3,420,12)
promedio<-apply(prec[mar,],2,function(x) mean(x,na.rm=T))
c=cbind(promedio[2:582])

abril<-seq(4,420,12)
promedio<-apply(prec[abril,],2,function(x) mean(x,na.rm=T))
d=cbind(promedio[2:582])

may<-seq(5,420,12)
promedio<-apply(prec[may,],2,function(x) mean(x,na.rm=T))
e=cbind(promedio[2:582])

jun<-seq(6,420,12)
promedio<-apply(prec[jun,],2,function(x) mean(x,na.rm=T))
f=cbind(promedio[2:582])

jul<-seq(7,420,12)
promedio<-apply(prec[jul,],2,function(x) mean(x,na.rm=T))
g=cbind(promedio[2:582])

ago<-seq(8,420,12)
promedio<-apply(prec[ago,],2,function(x) mean(x,na.rm=T))
h=cbind(promedio[2:582])

sep<-seq(9,420,12)
promedio<-apply(prec[sep,],2,function(x) mean(x,na.rm=T))
i=cbind(promedio[2:582])

oct<-seq(10,420,12)
promedio<-apply(prec[oct,],2,function(x) mean(x,na.rm=T))
j=cbind(promedio[2:582])

nov<-seq(11,420,12)
promedio<-apply(prec[nov,],2,function(x) mean(x,na.rm=T))
k=cbind(promedio[2:582])

dic<-seq(12,420,12)
promedio<-apply(prec[dic,],2,function(x) mean(x,na.rm=T))
l=cbind(promedio[2:582])

prec_prom<-data.frame(a,b,c,d,e,f,g,h,i,j,k,l)
names(prec_prom)=clmnames <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")


##########country
country=rep("Colombia",length(l))


##########id
ID<-seq(1,length(l),1)


##########source
SOURCE=rep("IDEAM",length(l))

##nyear
nyear=rep(35,length(l))

nombres<-c("ID","SOURCE","COUNTRY","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEAR")
base<-data.frame(ID,SOURCE,country,prec_prom,nyear,row.names = NULL)
names(base)<-nombres

write.csv(base,"table_prec.csv",row.names=F)

