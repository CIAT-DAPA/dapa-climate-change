#Cargar fechas de MIRCA
load('D:/Tobackup/BID/matrices_cultivo/maize_secano.Rdat')
mirca = crop_secano$mirca.start

#crear ventana de siembra
sdate = mirca - 70
pfrst =  mirca - 42
plast = mirca + 56

#indices con cambio de ano
ind.sdate = which(sdate<1)
ind.pfrst = which(pfrst<1)
ind.plast = which(plast>365)

#ajustar dia para cambio de ano
sdate[ind.sdate] = sdate[ind.sdate] + 365
pfrst[ind.pfrst] = pfrst[ind.pfrst] + 365
plast[ind.plast] = plast[ind.plast] - 365

#crear anos
anos = 71:98
anos2 = t(replicate(length(sdate),anos))  #repetir por pixel

#combinar anos y dias
sdate.yr = array(NA,dim=c(length(sdate),length(anos)))
pfrst.yr = array(NA,dim=c(length(pfrst),length(anos)))
plast.yr = array(NA,dim=c(length(plast),length(anos)))
for (j in 1:length(anos))  {
  sdate.yr[,j] = paste(anos2[,j],sprintf("%03d",sdate),sep='')
  pfrst.yr[,j] = paste(anos2[,j],sprintf("%03d",pfrst),sep='')  #agregar 0's a 3 caracteres
  plast.yr[,j] = paste(anos2[,j],sprintf("%03d",plast),sep='')
}

#ajustar ano para cambio de ano
sdate.yr[ind.sdate,] = paste(anos2[ind.sdate,]-1,sprintf("%03d",sdate[ind.sdate]),sep='')
pfrst.yr[ind.pfrst,] = paste(anos2[ind.pfrst,]-1,sprintf("%03d",pfrst[ind.pfrst]),sep='')
plast.yr[ind.plast,] = paste(anos2[ind.plast,]+1,sprintf("%03d",plast[ind.plast]),sep='')

