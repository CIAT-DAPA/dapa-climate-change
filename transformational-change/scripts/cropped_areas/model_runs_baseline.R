#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015

#load packages
library(raster); library(maptools); data(wrld_simpl)

#working dir
bdir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
wd <- paste(bdir,"/modelling/Cul_de_sacs",sep="")

#source model
src_dir <- paste(wd,"/_scripts",sep="")
source(paste(src_dir,"/EcoCrop-model.R",sep=""))

#output directories
data_dir <- paste(wd,"/model_data",sep="")
climdir_out <- paste(wd,"/meteorology",sep="")
wcl_inp <- paste(climdir_out,"/wcl_hist",sep="")
cru_inp <- paste(climdir_out,"/cru_hist",sep="")
runs_dir <- paste(wd,"/model_runs",sep="")
chk_dir <- paste(wd,"/check_figs",sep="")
if (!file.exists(chk_dir)) {dir.create(chk_dir)}

#model parameters
params <- read.table(paste(data_dir,"/crop_parameters.tab",sep=""),header=T)

#load and resample maize mask
mmask <- raster(paste(data_dir,"/ME_mask.tif",sep=""))
mmask <- resample(mmask, raster(paste(climdir_out,"/cru_hist/prec_1.tif",sep="")),method="ngb")
maiz_id <- c("wuma","wl","wlma","h","dma","dl")

#loop input met and crops
for (dset in c("cru","wcl")) {
  #dset <- "cru"
  idir <- get(paste(dset,"_inp",sep=""))
  odir <- paste(runs_dir,"/",dset,"_hist",sep="")
  if (!file.exists(odir)) {dir.create(odir,recursive=T)}
  
  for (icrop in 1:nrow(params)) {
    #icrop <- 1
    crop_name <- paste(params$Crop[icrop])
    
    cat("...processing crop=",crop_name,"for dataset=",dset,"\n")
    
    #run model or calculate (maize, sorghum)
    if (crop_name == "sorghum") {
      if (!file.exists(paste(odir,"/",crop_name,"_suit.tif",sep=""))) {
        tsfun <- function(x) {
          xn <- x[1]; xx <- x[2]
          if (is.na(xn) | is.na(xx)) {
            xf <- NA
          } else if (xn == 0 & xx != 0) {
            xf <- xx
          } else if (xn != 0 & xx == 0) {
            xf <- xn
          } else if (xn != 0 & xx != 0) {
            xf <- (xx^2 + xn^2) / (xx + xn)
          } else  if (xn == 0 & xx == 0) {
            xf <- 0
          }
          return(xf)
        }
        
        rsx <- raster(paste(odir,"/sorghum_h_suit.tif",sep=""))
        rsn <- raster(paste(odir,"/sorghum_l_suit.tif",sep=""))
        rsf <- calc(stack(rsn,rsx),fun=tsfun)
        rsf <- writeRaster(rsf, paste(odir,"/",crop_name,"_suit.tif",sep=""), format="GTiff")
        rm(rsf)
      }
    } else if (crop_name == "maize") {
      if (!file.exists(paste(odir,"/",crop_name,"_suit.tif",sep=""))) {
        me_his <- ome_his <- list()
        for (mci in 1:length(maiz_id)) {
          #mci <- 1
          tmask <- raster(mmask); tmask[] <- NA; tmask[which(mmask[] == mci)] <- 1 #; plot(tmask)
          trs_his <- raster(paste(odir,"/maize_",maiz_id[mci],"_suit.tif",sep=""))
          me_his[[mci]] <- trs_his * tmask #pieced MEs
          ome_his[[mci]] <- trs_his #for outside areas
        }
        
        pcd_his <- merge(stack(me_his))
        unpcd_his <- calc(stack(ome_his), max)
        mai_his <- merge(pcd_his, unpcd_his)
        mai_his <- writeRaster(mai_his,paste(odir,"/",crop_name,"_suit.tif",sep=""), format="GTiff")
      }
    } else {
      if (!file.exists(paste(odir,"/",crop_name,"_suit.tif",sep=""))) {
        ecorun <- suitCalc(climPath=idir,
                           Gmin=params$Gmin[icrop],
                           Gmax=params$Gmax[icrop],
                           Tkmp=params$Tkmp[icrop],
                           Tmin=params$Tmin[icrop],
                           Topmin=params$Topmin[icrop],
                           Topmax=params$Topmax[icrop],
                           Tmax=params$Tmax[icrop],
                           Rmin=params$Rmin[icrop],
                           Ropmin=params$Ropmin[icrop],
                           Ropmax=params$Ropmax[icrop],
                           Rmax=params$Rmax[icrop],
                           outfolder=odir,
                           cropname=crop_name,
                           ext=".tif")
      }
    }
  }
  
  #plot all data
  all_crops <- paste(params$Crop[c(1:6,9:10,17)])
  rsall <- stack(paste(odir,"/",all_crops,"_suit.tif",sep=""))
  pdf(paste(chk_dir,"/eco_hist_",dset,".pdf",sep=""),width=10,height=10)
  plot(rsall,zlim=c(0,100),col=rev(terrain.colors(20)),legend=F)
  dev.off()
}




