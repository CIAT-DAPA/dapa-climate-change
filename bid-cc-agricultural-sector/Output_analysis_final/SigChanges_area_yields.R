#Calculate percent area with significant changes in yields

cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
path.root = "D:/tobackup/BID/"

#Overall area changes
area.changes = array(NA,dim=c(5,6))
dimnames(area.changes)[[1]] = cultivos
dimnames(area.changes)[[2]] = c('Riego-pos','Riego-NS','Riego-neg','Secano-pos','Secano-NS','Secano-neg')
for (c in 1:5) {
  load(paste(path.root,"resultados_graficas/",cultivos[c],"_riego_pctCh_HWAH_zeros.Rdat", sep=""))
  crop.r = pct.ch.r2
  ind.pos = which(crop.r[,2]>0)
  ind.NS = which(is.na(crop.r[,2]))
  ind.neg = which(crop.r[,2]<0)
  area.changes[c,1]=sum(crop.r[ind.pos,6])/sum(crop.r[,6])*100  #%area with sig yield increases
  area.changes[c,2]=sum(crop.r[ind.NS,6])/sum(crop.r[,6])*100  #%area with insig yield changes
  area.changes[c,3]=sum(crop.r[ind.neg,6])/sum(crop.r[,6])*100  #%area with sig yield decreases
  
  load(paste(path.root,"resultados_graficas/",cultivos[c],"_secano_pctCh_HWAH_zeros.Rdat", sep=""))
  crop.s = pct.ch.s2
  ind.pos = which(crop.s[,2]>0)
  ind.NS = which(is.na(crop.s[,2]))
  ind.neg = which(crop.s[,2]<0)
  area.changes[c,4]=sum(crop.s[ind.pos,6])/sum(crop.s[,6])*100  #%area with sig yield increases
  area.changes[c,5]=sum(crop.s[ind.NS,6])/sum(crop.s[,6])*100  #%area with insig yield changes
  area.changes[c,6]=sum(crop.s[ind.neg,6])/sum(crop.s[,6])*100  #%area with sig yield decreases
  
}

#look at results
barplot(t(area.changes),beside=T)

#Area changes by country
c=3  #set crop here
treat = 'secano'  #riego/ secano here

load(paste(path.root,"resultados_graficas/",cultivos[c],"_",treat,"_pctCh_HWAH_zeros.Rdat", sep=""))
eval(parse(text=paste("crop.ch = pct.ch.",substr(treat,1,1),"2",sep="")))
ind.pos = which(crop.ch[,2]>0)
ind.NS = which(is.na(crop.ch[,2]))
ind.neg = which(crop.ch[,2]<0)
load(paste(path.root,'matrices_cultivo/',cultivos.en[c],'_',treat,'.Rdat',sep=''))
eval(parse(text=paste("ind.big = match(paste(crop.ch[,7],crop.ch[,8]),paste(crop_",treat,"$x,crop_",treat,"$y))",sep="")))
eval(parse(text=paste("crop.country = crop_",treat,"[ind.big,'country']",sep="")))
#table(crop.country[ind.pos])  #table of pixel-level changes

country.area.pos = aggregate(crop.ch[ind.pos,'Area'],by=list(crop.country[ind.pos]),FUN=sum)
country.area.pos = country.area.pos[order(country.area.pos$x,decreasing=T),]

country.area.NS = aggregate(crop.ch[ind.NS,'Area'],by=list(crop.country[ind.NS]),FUN=sum)
country.area.NS = country.area.NS[order(country.area.NS$x,decreasing=T),]

country.area.neg = aggregate(crop.ch[ind.neg,'Area'],by=list(crop.country[ind.neg]),FUN=sum)
country.area.neg = country.area.neg[order(country.area.neg$x,decreasing=T),]

#put in big matrix
countries.u = unique(crop.country)
country.area.change = data.frame(country=countries.u,pos=NA,NS=NA,neg=NA)
country.area.change[match(country.area.pos[,1],countries.u),2] = country.area.pos[,2]
country.area.change[match(country.area.NS[,1],countries.u),3] = country.area.NS[,2]
country.area.change[match(country.area.neg[,1],countries.u),4] = country.area.neg[,2]
