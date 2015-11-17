#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015

library(raster)

#working dir
wd <- "~/Leeds-work/cul-de-sacs"

#output directories
climdir_out <- paste(wd,"/meteorology",sep="")
if (!file.exists(climdir_out)) {dir.create(climdir_out)}
cru_odir <- paste(climdir_out,"/cru_hist_yearly",sep="")
if (!file.exists(cru_odir)) {dir.create(cru_odir)}

#climate data dirs
cru_dir <- "~/Leeds-work/p4s-csa/hh-analyses/CRU_data"

#load Africa mask
msk <- raster(paste(wd,"/model_data/mask.tif",sep=""))
msk[which(!is.na(msk[]))] <- 1

dset <- "cru"
idir <- get(paste(dset,"_dir",sep=""))
odir <- get(paste(dset,"_odir",sep=""))

cat("...processing dataset=",dset,"\n")

for (ivar in c("prec","tmean","tmin")) {
  #ivar <- "tmean"
  if (ivar == "tmean") {cruvar <- "tmp"}
  if (ivar == "tmin") {cruvar <- "tmn"}
  if (ivar == "prec") {cruvar <- "pre"}
  cat("...variable=",ivar,"\n")
  
  #load data for all years
  flist <- list.files(idir,pattern=paste(cruvar,".dat.nc.gz",sep=""))
  for (fl in flist) {
    #fl <- flist[1]
    cat("...processing file=",fl,"\n")
    setwd(idir)
    system(paste("gunzip ",fl,sep=""))
    stk <- stack(gsub("\\.gz","",fl))
    for (i in 1:nlayers(stk)) {
      #i <- 1
      iname <- names(stk[[i]])
      year <- as.numeric(substr(iname,2,5))
      month <- as.numeric(substr(iname,7,8))
      if (!file.exists(paste(odir,"/",year,sep=""))) {dir.create(paste(odir,"/",year,sep=""))}
      if (!file.exists(paste(odir,"/",year,"/",ivar,"_",month,".tif",sep=""))) {
        rs <- crop(stk[[i]], msk)
        if (ivar != "prec") {rs <- rs*10}
        rs <- writeRaster(rs, paste(odir,"/",year,"/",ivar,"_",month,".tif",sep=""),format="GTiff")
      }
    }
    system(paste("gzip ",gsub("\\.gz","",fl),sep=""))
  }
}





