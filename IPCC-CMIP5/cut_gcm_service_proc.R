require(raster)
require(rgdal)
require(R.utils)
require(stringr)
require("RCurl"); library(ggplot2);library(lubridate)

dirbase = "/mnt/data_cluster_2/gcm/cmip5"
workspasce= "/mnt/GCMPage/data/data_requests/"#"/home/jetarapues/Request/"
dirout = "jaime"
mask = "-81.5,7.5,-80.5,9" #  xmin,ymin,xmax,ymax
rcps = "rcp26,rcp45" # rcp45, rcp60, rcp85
resolution = "30s" # 1km
models = "ensemble,bcc_csm1_1" # bcc_csm1_1, bcc_csm1_1_m ...
periods = "2020_2049,2060_2089" # 2040_2069, 2060_2089, 2070_2099
variable = "prec" #tmin, tmax, bio, cons_mths
descfile = "YES" # YES | NO
wcl = "wcl"# cut worldclim: wcl|nowcl
toaddr = "jaime.tm8@gmail.com" # send email: name@email.com|NO
calcAnom= "anom" # option: anom|noanom
path2script= "/mnt/data_climatewizard/temp/cut_gcm_service.py"#"/home/jetarapues/scripts/cut_gcm_service.py"
command = "python2.7"

files=getURL(paste0("http://maprooms.ciat.cgiar.org/CCAFS-Climate/chirps/cut_gcm_service_request.php?dirbase=",dirbase ,"&workspasce=",workspasce,"&dirout=",dirout ,"&mask=",mask ,"&rcps=",rcps ,"&resolution=",resolution ,"&models=",models ,"&periods=",periods ,"&variable=",variable ,"&descfile=",descfile ,"&wcl=",wcl ,"&toaddr=",toaddr ,"&calcAnom=",calcAnom,"&path2script=",path2script,"&command=",command),verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
