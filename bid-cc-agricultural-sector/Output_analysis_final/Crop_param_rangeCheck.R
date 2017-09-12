cultivos = c('maiz','arroz','soya','frijol','trigo')
path.root = "D:/tobackup/BID/resultados_graficas/"; #'Z:/'  #root path

#Water stress
for (j in 1:5) {
  load(paste(path.root,cultivos[j],'_secano_pctCh_Stress_water_all.Rdat',sep=''))
  if (j==1) {
    dat = pct.ch.s2[,2]
  }  else{
    dat = c(dat,pct.ch.s2[,2])
  }
}
quantile(dat,c(.05,.95),na.rm=T)

#Irrigation productivity
for (j in 1:5) {
  load(paste(path.root,cultivos[j],'_riego_pctCh_YPIM.Rdat',sep=''))
  if (j==1) {
    dat = pct.ch.r2[,2]
  }  else{
    dat = c(dat,pct.ch.r2[,2])
  }
}
quantile(dat,c(.05,.95),na.rm=T)

#Yields
for (j in 1:5) {
  load(paste(path.root,cultivos[j],'_secano_pctCh_HWAH.Rdat',sep=''))
  if (j==1) {
    dat = pct.ch.s2[,2]
  }  else{
    dat = c(dat,pct.ch.s2[,2])
  }
}
for (j in 1:5) {
  load(paste(path.root,cultivos[j],'_riego_pctCh_HWAH.Rdat',sep=''))
  dat = c(dat,pct.ch.r2[,2])
}
quantile(dat,c(.05,.95),na.rm=T)
