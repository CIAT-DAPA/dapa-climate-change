#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2015
stop("!")

#directories
#wd <- "~/Leeds-work/p4s-csa/climate-impacts"
wd <- "/mnt/BCSD/climate-impacts_bcsd"
bcsd_dir <- paste(wd,"/bcsd_data",sep="")
if (!file.exists(bcsd_dir)) {dir.create(bcsd_dir)}

#list of GCMs and scenarios
gcm_list <- c("GFDL-ESM2M","BNU-ESM","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M","CCSM4","bcc-csm1-1")
sce_list <- c("historical","rcp85","rcp45")
#gcm_list <- "GFDL-ESM2M"
#sce_list <- c("historical","rcp85")

#base url
base_url <- "ftp://ftp.nccs.nasa.gov"

#for (sce in sce_list) {
  sce <- sce_list[2]
  sce_dir <- paste(bcsd_dir,"/",sce,sep="")
  if (!file.exists(sce_dir)) {dir.create(sce_dir)}
  #if (sce == "historical") {years <- c(2000:2005)} else {years <- c(2015:2020)}
  if (sce == "historical") {years <- c(1980:2005)} else {years <- c(2015:2070)}
  for (gcm in gcm_list) {
    #gcm <- gcm_list[1]
    for (yr in years) {
      #yr <- years[1]
      for (vname in c("pr","tasmax","tasmin")) {
        #vname <- "pr"
        setwd(sce_dir)
        if (!file.exists(paste(vname,"_day_BCSD_",sce,"_r1i1p1_",gcm,"_",yr,".nc",sep=""))) {
          system(paste("wget -t 1 --no-dns-cache -T 1 --read-timeout=1 --ftp-user=NEXGDDP --ftp-password='' ",
                       base_url,"/BCSD/",sce,"/day/atmos/",vname,"/r1i1p1/v1.0/",vname,
                       "_day_BCSD_",sce,"_r1i1p1_",gcm,"_",yr,".nc",sep=""))
          cat("...file done xxx!:!xxx\n")
        }
        setwd("~")
      }
    }
  }
#}


