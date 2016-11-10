#Assign FPU's to regions

path.root = 'D:/tobackup/BID/'  #root path
cultivos = c('maiz','arroz','soya','papa','trigo')
cultivos.en = c('Maize','Rice','Soybeans','Potatoes','Wheat')
treat = c('riego','secano')
c=5  #have to set up crop manually here

eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#Assign missing FPU's (on coastlines)
na.riego = which(is.na(crop_riego$New_FPU))
na.secano = which(is.na(crop_secano$New_FPU))

#HAVE TO FIX THESE MANUALLY!
crop_riego[which(round(crop_riego$x,2)==-97.75 & round(crop_riego$y,2)==23.25),'New_FPU'] = 'MIM_MEX'
crop_riego[which(round(crop_riego$x,2)==-105.25 & round(crop_riego$y,2)==21.25),'New_FPU'] = 'MIM_MEX'
crop_riego[which(round(crop_riego$x,2)==-88.25 & round(crop_riego$y,2)==15.75),'New_FPU'] = 'GTM_GTM'
crop_riego[which(round(crop_riego$x,2)==-71.25 & round(crop_riego$y,2)==9.25),'New_FPU'] = 'ORI_VEN'
crop_riego[which(round(crop_riego$x,2)==-59.25 & round(crop_riego$y,2)==-38.75),'New_FPU'] = 'SAL_ARG'

crop_secano[which(round(crop_secano$x,2)==-97.75 & round(crop_secano$y,2)==23.25),'New_FPU'] = 'MIM_MEX'
crop_secano[which(round(crop_secano$x,2)==-105.25 & round(crop_secano$y,2)==21.25),'New_FPU'] = 'MIM_MEX'
crop_secano[which(round(crop_secano$x,2)==-88.25 & round(crop_secano$y,2)==15.75),'New_FPU'] = 'GTM_GTM'
crop_secano[which(round(crop_secano$x,2)==-65.75 & round(crop_secano$y,2)==10.25),'New_FPU'] = 'ORI_VEN'
crop_secano[which(round(crop_secano$x,2)==-71.25 & round(crop_secano$y,2)==9.25),'New_FPU'] = 'ORI_VEN'
crop_secano[which(round(crop_secano$x,2)==-59.25 & round(crop_secano$y,2)==-38.75),'New_FPU'] = 'SAL_ARG'

#Add region classification
regiones = list(MEX=c('MIM_MEX','RIG_MEX','UME_MEX'),
                CEN=c('YUC_MEX','GTM_GTM','BLZ_BLZ','SLV_SLV','HND_HND','NIC_NIC','CRI_CRI','PAN_PAN','CRB_CRB','DOM_DOM','HTI_HTI','CUB_CUB','JAM_JAM'),
                AND=c('AMA_BOL','PAR_BOL','AMA_COL','NWS_COL','ORI_COL','AMA_ECU','NWS_ECU','AMA_PER','PEC_PER','ORI_VEN'),
                BRA=c('AMA_BRA','NEB_BRA','SAN_BRA','TOC_BRA','PAR_BRA','GSA_GSA','RVE_VEN'),
                SUR=c('PAR_ARG','RIC_ARG','SAL_ARG','TIE_ARG','URU_BRA','CHC_CHL','PAR_PRY','URU_URY')
                )

#add region column to data frames
crop_riego$Region = NA
crop_secano$Region = NA

for (j in 1:5)  {  #loop through regions
  for (t in 1:2)  {
    eval(parse(text=paste("reg.T = crop_",treat[t],"$New_FPU %in% regiones[[",j,"]]",sep="")))
    eval(parse(text=paste("crop_",treat[t],"[which(reg.T==T),'Region'] = names(regiones)[",j,"]",sep="")))
  }
}

eval(parse(text=paste('save(crop_riego,file="',path.root,'matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('save(crop_secano,file="',path.root,'matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))
