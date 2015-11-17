#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015

library(raster)

#working dir
wd <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/modelling/Cul_de_sacs"

#output directories
climdir_out <- paste(wd,"/meteorology",sep="")
if (!file.exists(climdir_out)) {dir.create(climdir_out)}
cru_odir <- paste(climdir_out,"/cru_hist",sep="")
if (!file.exists(cru_odir)) {dir.create(cru_odir)}
wcl_odir <- paste(climdir_out,"/wcl_hist",sep="")
if (!file.exists(wcl_odir)) {dir.create(wcl_odir)}

#climate data dirs
climdir_in <- "/nfs/data_cluster_4/observed/gridded_products"
cru_dir <- paste(climdir_in,"/cru-ts-v3-21/30yr_averages/1971_2000",sep="")
wcl_dir <- paste(climdir_in,"/worldclim/Global_30min",sep="")

#load Africa mask
msk <- raster(paste(wd,"/model_data/mask.tif",sep=""))
msk[which(!is.na(msk[]))] <- 1

for (dset in c("cru","wcl")) {
  #dset <- "cru"
  idir <- get(paste(dset,"_dir",sep=""))
  odir <- get(paste(dset,"_odir",sep=""))
  if (dset == "cru") {ext <- ".asc"} else {ext <- ""}
  
  cat("...processing dataset=",dset,"\n")
  
  for (ivar in c("prec","tmean","tmin")) {
    #ivar <- "tmean"
    cat("...variable=",ivar,"\n")
    for (imth in 1:12) {
      #imth <- 1
      if (!file.exists(paste(odir,"/",ivar,"_",imth,".tif",sep=""))) {
        rsi <- raster(paste(idir,"/",ivar,"_",imth,ext,sep=""))
        if (dset == "cru" & ivar != "prec") {
          rso <- crop(rsi*10, msk)
        } else {
          rso <- crop(rsi, msk)
        }
        rso <- writeRaster(rso, paste(odir,"/",ivar,"_",imth,".tif",sep=""),format="GTiff")
      }
    }
  }
}




