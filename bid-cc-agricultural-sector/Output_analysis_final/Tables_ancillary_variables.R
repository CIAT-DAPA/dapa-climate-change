#Create data for tables in paper
#Baseline range of ancillary variables
#Changes from past to future in ancillary variables
#Correlation between changes from past to future in yield & ancillary variables

# vars.anc = c('HWAH','NDCH','CWAM','H.AM','SRADA','SRADT','Stress_water_all','IRCM','ETCP','YPEM')
# vars.anc.r = c('HWAH','NDCH','H.AM','CWAM','SRADA','SRADT','YPIM','IRCM', 'ETCP','YPEM')  #
# vars.anc.s = c('HWAH','NDCH','H.AM','CWAM','SRADA','SRADT','Stress_water_all','ETCP','YPEM')
vars.anc.r = c('CWAM','H.AM','GRWT','NDCH','TMAXA','TMINA','SRADA','IRCM','YPIM','ADAT')  #
vars.anc.s = c('CWAM','H.AM','GRWT','NDCH','TMAXA','TMINA','SRADA','Stress_water_all','PRCPA','PDAT','ADAT')

cultivos = c('maiz','arroz','soya','frijol','trigo')
cultivos.en = c('Maize','Rice','Soybean','bean','wheat')
path.root = 'D:/Tobackup/BID/resultados_graficas/'
path.mat = 'D:/Tobackup/BID/matrices_cultivo/'

for (c in 5:5) {  #Loop through crops here
  load(paste(path.root,cultivos[c],'_riego_pctCh_HwAH_NA.Rdat',sep='')) 
  yld.r = pct.ch.r2
  ind.a = which(colnames(pct.ch.r2)=='Area')
  yld.r.mean = sum(pct.ch.r2[,1]*pct.ch.r2[,ind.a],na.rm=T)/sum(pct.ch.r2[,ind.a]*(is.na(pct.ch.r2[,1])==F)*1)  #average weighted by harvested area
  print(paste('Mean change in yield.r: ',round(yld.r.mean,2)))
  for (j in 1:length(vars.anc.r)) {
    load(paste(path.root,cultivos[c],'_riego_pctCh_',vars.anc.r[j],'.Rdat',sep='')) 
    ind.a = which(colnames(pct.ch.r2)=='Area')
    var.r.mean = sum(pct.ch.r2[,1]*pct.ch.r2[,ind.a],na.rm=T)/sum(pct.ch.r2[,ind.a]*(is.na(pct.ch.r2[,1])==F)*1)  #average weighted by harvested area
    print(paste('Mean change in ',vars.anc.r[j],'.r: ',round(var.r.mean,2)))
    print(paste('% changes: ',cultivos[c],' ',vars.anc.r[j],' ',round(quantile(pct.ch.r2[,1],na.rm=T),2),sep=''))
    print(paste('corr w/ yield changes: ',cultivos[c],' ',vars.anc.r[j],' ',round(cor(pct.ch.r2[,1],yld.r[,1],use='pairwise.complete.obs'),2),sep=''))
  }
}

for (c in 3:3) {
  load(paste(path.root,cultivos[c],'_secano_pctCh_HwAH_NA.Rdat',sep='')) 
  yld.s = pct.ch.s2
  ind.a = which(colnames(pct.ch.s2)=='Area')
  yld.s.mean = sum(pct.ch.s2[,1]*pct.ch.s2[,ind.a],na.rm=T)/sum(pct.ch.s2[,ind.a]*(is.na(pct.ch.s2[,1])==F)*1)  #average weighted by harvested area
  print(paste('Mean change in yield.s: ',round(yld.s.mean,2)))
  
  for (j in 1:length(vars.anc.s)) {
    load(paste(path.root,cultivos[c],'_secano_pctCh_',vars.anc.s[j],'.Rdat',sep='')) 
    ind.a = which(colnames(pct.ch.s2)=='Area')
    var.s.mean = sum(pct.ch.s2[,1]*pct.ch.s2[,ind.a],na.rm=T)/sum(pct.ch.s2[,ind.a]*(is.na(pct.ch.s2[,1])==F)*1)  #average weighted by harvested area
    print(paste('Mean change in ',vars.anc.s[j],'.s: ',round(var.s.mean,3)))
    print(paste('% changes: ',cultivos[c],' ',vars.anc.s[j],' ',round(quantile(pct.ch.s2[,1],na.rm=T),3),sep=''))
    print(paste('corr w/ yield changes: ',cultivos[c],' ',vars.anc.s[j],' ',round(cor(pct.ch.s2[,1],yld.s[,1],use='pairwise.complete.obs'),3),sep=''))
  }
}

#Try aggregating ancillary variable changes by region
#classification 3
NOR = c('Mexico','Cuba','Belize','Guatemala','Honduras','El Salvador','Nicaragua','Dominican Republic','Jamaica','Haiti')
CEN = c('Costa Rica','Panama','Colombia','Venezuela','Trinidad and Tobago','Guyana','French Guiana','Suriname')
AND = c('Ecuador','Peru','Bolivia')
BRA = c('Brazil','Paraguay')
SUR = c('Argentina','Uruguay','Chile')
region.code = c('NOR','CEN','AND','BRA','SUR')

for (c in 2:2) {
  load(paste(path.root,cultivos[c],'_secano_pctCh_HwAH_NA.Rdat',sep='')) 
  yld.s = pct.ch.s2
  ind.a = which(colnames(pct.ch.s2)=='Area')
  load(paste(path.mat,cultivos.en[c],'_Secano.Rdat',sep=''))
  ind.LL = match(paste(round(yld.s[,'Lon'],2),round(yld.s[,'Lat'],2)),paste(round(crop_secano$x,2),round(crop_secano$y,2)))
  countries = crop_secano$country[ind.LL]  #country vector
  regions = countries  #make copy & update with regions
  regions[which(is.na(match(countries,NOR))==F)] = 'NOR'
  regions[which(is.na(match(countries,CEN))==F)] = 'CEN'
  regions[which(is.na(match(countries,AND))==F)] = 'AND'
  regions[which(is.na(match(countries,BRA))==F)] = 'BRA'
  regions[which(is.na(match(countries,SUR))==F)] = 'SUR'
  
  var.ch.region = array(NA,dim=c(length(vars.anc.s),5))
  for (j in 1:length(vars.anc.s)) {
    load(paste(path.root,cultivos[c],'_secano_pctCh_',vars.anc.s[j],'.Rdat',sep='')) 
    ind.a = which(colnames(pct.ch.s2)=='Area')
    for (k in 1:5)  {  #regions
      ind.reg = which(regions==region.code[k])
      var.ch.region[j,k] = sum(pct.ch.s2[ind.reg,1]*pct.ch.s2[ind.reg,ind.a])/sum(pct.ch.s2[ind.reg,ind.a])  #average weighted by harvested area
    }
  }
}
dimnames(var.ch.region)[[1]] = vars.anc.s
dimnames(var.ch.region)[[2]] = region.code

#Look at climate variables in baseline period
for (j in 1:5)  {
  load(paste('D:/Tobackup/BID/resultados_graficas/',cultivos[j],'_WFD_TMAXA.Rdat',sep=''))
  print(paste('tmax:',cultivos[j],round(quantile(c(wfd.r,wfd.s)),2)))
  
  load(paste('D:/Tobackup/BID/resultados_graficas/',cultivos[j],'_WFD_TMINA.Rdat',sep=''))
  print(paste('tmin:',cultivos[j],round(quantile(c(wfd.r,wfd.s)),2)))
  
  load(paste('D:/Tobackup/BID/resultados_graficas/',cultivos[j],'_WFD_Stress_water_all.Rdat',sep=''))
  print(paste('water stress:',cultivos[j],round(quantile(c(wfd.s)),2)))
  
}

#Calculate temperature optima from baseline yield data
par(ask=T)
for (j in 1:5)  {
  load(paste('D:/Tobackup/BID/resultados_graficas/',cultivos[j],'_WFD_TMAXA.Rdat',sep=''))
  print(paste('tmax:',cultivos[j],round(quantile(c(wfd.r,wfd.s)),2)))
  temp = c(wfd.r,wfd.s)
  
  load(paste('D:/Tobackup/BID/resultados_graficas/',cultivos[j],'_WFD_HWAH.Rdat',sep=''))
  print(paste('yield:',cultivos[j],round(quantile(c(wfd.r,wfd.s)),2)))
  ylds = c(wfd.r,wfd.s)
  
  test = lm(ylds~temp+eval(temp^2))
  topt = test$coef[2]/-2/test$coef[3]
  print(round(topt,2))
  
  plot(temp,ylds)
  points(temp,test$fitted,pch=20,col='red')
}
