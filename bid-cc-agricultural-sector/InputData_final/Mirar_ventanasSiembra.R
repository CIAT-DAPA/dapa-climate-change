load('Z:/08-Cells_toRun/matrices_cultivo/Maize_secano.Rdat')
probl = which(crop_secano$mirca.start > crop_secano$mirca.end)
crop_secano[probl,'mirca.end'] = crop_secano[probl,'mirca.end'] + 365
ventana = crop_secano[,'mirca.end'] - crop_secano[,'mirca.start']
hist(ventana)
quantile(ventana)

load('Z:/08-Cells_toRun/matrices_cultivo/Maize_riego.Rdat')
probl = which(crop_riego$mirca.start > crop_riego$mirca.end)
crop_riego[probl,'mirca.end'] = crop_riego[probl,'mirca.end'] + 365
ventana = crop_riego[,'mirca.end'] - crop_riego[,'mirca.start']
hist(ventana)
quantile(ventana)

#Original files - MIRCA
load('Z:/08-Cells_toRun/Maize.loc.cal.Rdat')
probl = which(Maize.loc.cal$mirca.rf.start > Maize.loc.cal$mirca.rf.end)
Maize.loc.cal[probl,'mirca.rf.end'] = Maize.loc.cal[probl,'mirca.rf.end'] + 365
ventana = Maize.loc.cal[,'mirca.rf.end'] - Maize.loc.cal[,'mirca.rf.start']
hist(ventana)
quantile(ventana,na.rm=T)

probl = which(Maize.loc.cal$mirca.irr.start > Maize.loc.cal$mirca.irr.end)
Maize.loc.cal[probl,'mirca.irr.end'] = Maize.loc.cal[probl,'mirca.irr.end'] + 365
ventana = Maize.loc.cal[,'mirca.irr.end'] - Maize.loc.cal[,'mirca.irr.start']
hist(ventana)
quantile(ventana,na.rm=T)

#Original files - Sacks
load('Z:/08-Cells_toRun/Maize.loc.cal.Rdat')
probl = which(Maize.loc.cal$sacks.plant > Maize.loc.cal$sacks.harvest)
Maize.loc.cal[probl,'sacks.harvest'] = Maize.loc.cal[probl,'sacks.harvest'] + 365
ventana = Maize.loc.cal[,'sacks.harvest'] - Maize.loc.cal[,'sacks.plant']
hist(ventana)
quantile(ventana,na.rm=T)

