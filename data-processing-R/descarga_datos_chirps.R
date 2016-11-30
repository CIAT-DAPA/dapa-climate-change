# _____        _               _____ _    _ _____ _____  _____   _____ 
#|  __ \      | |             / ____| |  | |_   _|  __ \|  __ \ / ____|
#| |  | | __ _| |_ ___  ___  | |    | |__| | | | | |__) | |__) | (___  
#| |  | |/ _` | __/ _ \/ __| | |    |  __  | | | |  _  /|  ___/ \___ \ 
#| |__| | (_| | || (_) \__ \ | |____| |  | |_| |_| | \ \| |     ____) |
#|_____/ \__,_|\__\___/|___/  \_____|_|  |_|_____|_|  \_\_|    |_____/ 
#                                                                     
# Jaime Tarapues
#
#####################
### Load packages ###
#####################

require(raster)
require(rgdal)
require(R.utils)
require(stringr)
require("RCurl")

########################
## Function download daily ###
########################
output="S:/observed/gridded_products/chirps/daily"
setwd(output)
year="2016"
files=getURL(paste0("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/",year,"/"),verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
files=unlist(strsplit(files, "\r\n"))
for(file in files){
  url <- paste("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/",file,sep="")
  monfile <- basename(file)
  if(!file.exists(paste0(output,'/',gsub(".gz","",monfile)))){
    download.file(url, monfile)
    system(paste0("7z e ",output,"/",monfile," -o",output))
    unlink(monfile)  
  }
}

# download_daily=function(x,y){
#   z=seq(as.Date(x), as.Date(y), "days")
#   fechas=str_replace_all(z, "-", ".")  
#   year=substr(fechas,1,4)
#   tmpdir <- tempdir()
#   for(i in 1:length(fechas)){
#     url <- paste('ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/',year[i],'/chirps-v2.0.',fechas[i],'.tif.gz',sep="")
#     file <- basename(url)
#     if(!file.exists(paste0(output,'/',gsub(".gz","",file)))){
#       download.file(url, file)
#       system(paste0("7z e ",output,"/",file," -o",output))
#       unlink(file)  
#       
#     }
#   }
#   return("Succesfful Download")
# }
# download_daily("2016/03/01","2016/10/31")

########################
## Function download monthly ###
########################

output="S:/observed/gridded_products/chirps/monthly/world"
setwd(output)

format="bils" #tifs
files=getURL(paste0("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/",format,"/"),verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
files=unlist(strsplit(files, "\r\n"))
for(file in files){
  url <- paste("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/",format,"/",file,sep="")
  monfile <- basename(file)
  if(!file.exists(paste0(output,'/',gsub(".tar.gz","",monfile),".bil"))){
    download.file(url, monfile)
    #p=gunzip(file, exdir = tempdir)
    system(paste0("7z e ",output,"/",monfile," -o",output))
    system(paste0("7z e ",output,"/",gsub(".gz","",monfile)," -o",output))
    unlink(monfile)  
    unlink(gsub(".gz","",monfile))
  }
}

########################
## Function download pentad ###
########################

output="S:/observed/gridded_products/chirps/pendaily"
setwd(output)

format="bils" #tifs
files=getURL(paste0("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_pentad/",format,"/"),verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
files=unlist(strsplit(files, "\r\n"))
for(file in files){
  url <- paste("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_pentad/",format,"/",file,sep="")
  monfile <- basename(file)
  if(!file.exists(paste0(output,'/',gsub(".tar.gz","",monfile),".bil"))){
    download.file(url, monfile)
    #p=gunzip(file, exdir = tempdir)
    system(paste0("7z e ",output,"/",monfile," -o",output))
    system(paste0("7z e ",output,"/",gsub(".gz","",monfile)," -o",output))
    unlink(monfile)  
    unlink(gsub(".gz","",monfile))
  }
}


















