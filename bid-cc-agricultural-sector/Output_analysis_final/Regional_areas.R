load('D:/tobackup/BID/matrices_cultivo/maize_riego.Rdat')
maiz_riego = crop_riego
load('D:/tobackup/BID/matrices_cultivo/maize_secano.Rdat')
maiz_secano = crop_secano

load('D:/tobackup/BID/matrices_cultivo/rice_secano.Rdat')
load('D:/tobackup/BID/matrices_cultivo/rice_riego.Rdat')
rice_riego = crop_riego
rice_secano = crop_secano

load('D:/tobackup/BID/matrices_cultivo/soybeans_riego.Rdat')
load('D:/tobackup/BID/matrices_cultivo/soybeans_secano.Rdat')
soy_riego = crop_riego
soy_secano = crop_secano

load('D:/tobackup/BID/matrices_cultivo/Potatoes_secano.Rdat')
load('D:/tobackup/BID/matrices_cultivo/Potatoes_riego.Rdat')
potato_riego = crop_riego
potato_secano = crop_secano

load('D:/tobackup/BID/matrices_cultivo/Wheat_riego.Rdat')
load('D:/tobackup/BID/matrices_cultivo/Wheat_secano.Rdat')
wheat_riego = crop_riego
wheat_secano = crop_secano

regions = c('MEX','CEN','AND','BRA','SUR')
for (j in 1:5)  {
  print(paste(regions[j],'riego',round(sum(wheat_riego$riego.area[wheat_riego$Region==regions[j]])/1000,2)))
  print(paste(regions[j],'secano',round(sum(wheat_secano$secano.area[wheat_secano$Region==regions[j]])/1000,2)))
}