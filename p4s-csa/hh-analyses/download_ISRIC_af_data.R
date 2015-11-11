#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#download ISRIC Africa data

#directories
wd <- "~/Leeds-work/p4s-csa/hh-analyses"
soil_dir <- paste(wd,"/ISRIC_soil",sep="")

#server and data details
base_url <- "ftp://soilgrids:soilgrids@ftp.soilgrids.org"
layernames <- c("af_ERDICM__M_","af_BLD_T__M_sd","af_CEC_T__M_sd","af_CLYPPT_T__M_sd",
                "af_ORCDRC_T__M_sd","af_CRFVOL_T__M_sd","af_SLTPPT_T__M_sd","af_SNDPPT_T__M_sd",
                "af_LRI_T__M_sd","af_AWCh2__M_sd","af_tetaS__M_sd","af_WWP__M_sd","af_PHIHOX_T__M_sd")

#loop layers
setwd(soil_dir)
for (layer in layernames) {
  #layer <- layernames[1]
  cat("...processing layer=",layer,"\n")
  if (layer == "af_ERDICM__M_" | layer == "af_ERDICM_LIMFACTOR__M_") {
    fname <- paste(layer,"1km.tif",sep="")
    int_url <- "/data/AF/GYGA"
    if (!file.exists(fname)) {system(paste("wget ",base_url,int_url,"/",fname,sep=""))}
  } else {
    for (i in 1:6) {
      #i <- 1
      fname <- paste(layer,i,"_1km.tif",sep="")
      int_url <- "/data/AF/aggregated/1km"
      if (!file.exists(fname)) {system(paste("wget ",base_url,int_url,"/",fname,sep=""))}
    }
  }
}

system("wget ftp://soilgrids:soilgrids@ftp.soilgrids.org/data/AF/GYGA/af_ERDICM_LIMFACTOR_legend_complete.csv")
system("wget ftp://soilgrids:soilgrids@ftp.soilgrids.org/data/AF/aggregated/1km/README")
system("wget ftp://soilgrids:soilgrids@ftp.soilgrids.org/data/AF/GYGA/README")
system("mv README.1 README_af_ERDICM__M_1km")
