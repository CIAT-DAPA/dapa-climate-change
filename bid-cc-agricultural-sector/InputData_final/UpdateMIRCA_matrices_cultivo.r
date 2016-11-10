#Actualizar fechas de MIRCA de crop calendars en matrices de cultivo

crops = c('Maize','Rice','Wheat','Soybeans','Potatoes')

for (j in 1:5)  {
  load(paste('Z:/08-Cells_toRun/',crops[j],'.loc.cal.Rdat',sep=''))
  eval(parse(text=paste('crop.loc.cal = ',crops[j],'.loc.cal',sep='')))
  
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/old.mirca/',crops[j],'_riego.Rdat',sep=''))
  ind.match = match(paste(round(crop_riego[,'x'],2),round(crop_riego[,'y'],2)),paste(crop.loc.cal[,'Longitude'],crop.loc.cal[,'Latitude']))
  crop_riego$mirca.start = crop.loc.cal$mirca.irr.start[ind.match]
  crop_riego$mirca.end = crop.loc.cal$mirca.irr.end[ind.match]
  
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/old.mirca/',crops[j],'_secano.Rdat',sep=''))
  ind.match = match(paste(round(crop_secano[,'x'],2),round(crop_secano[,'y'],2)),paste(crop.loc.cal[,'Longitude'],crop.loc.cal[,'Latitude']))
  crop_secano$mirca.start = crop.loc.cal$mirca.rf.start[ind.match]
  crop_secano$mirca.end = crop.loc.cal$mirca.rf.end[ind.match]
  
  save(crop_riego,file=paste('Z:/08-Cells_toRun/matrices_cultivo/',crops[j],'_riego.Rdat',sep=''))
  save(crop_secano,file=paste('Z:/08-Cells_toRun/matrices_cultivo/',crops[j],'_secano.Rdat',sep=''))
}

#Compare new & old
for (j in 1:5)  {
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/old.mirca/',crops[j],'_riego.Rdat',sep=''))
  crop_riego.old = crop_riego
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/',crops[j],'_riego.Rdat',sep=''))
  
  a=sum(crop_riego$mirca.start != crop_riego.old$mirca.start)
  b=dim(crop_riego)[1]
  print(paste(crops[j],'riego',round(a/b*100,2)))
  
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/old.mirca/',crops[j],'_secano.Rdat',sep=''))
  crop_secano.old = crop_secano
  load(paste('Z:/08-Cells_toRun/matrices_cultivo/',crops[j],'_secano.Rdat',sep=''))
  
  a=sum(crop_secano$mirca.start != crop_secano.old$mirca.start,na.rm=T)
  b=dim(crop_secano)[1]
  print(paste(crops[j],'secano',round(a/b*100,2)))
}