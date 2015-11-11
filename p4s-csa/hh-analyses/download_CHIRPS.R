#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#download CHIRPS Africa data
wd <- "~/Leeds-work/p4s-csa/hh-analyses"
chirps_dir <- paste(wd,"/CHIRPS_data",sep="")

base_url <- "ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05"

setwd(chirps_dir)
for (year in 2007:2015) {
  system(paste("wget -r ",base_url,"/",year,sep=""))
}

