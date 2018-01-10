#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015
stop("!")

#load packages
library(raster); library(maptools); data(wrld_simpl)

#takes into account the use of yearly time series for future simulations, the former version only iterates through
#the same year, the new script considers the actual year as well as the next year for cases where GS might fall in the
#end of the first year and the beginning of the following year

###chose base route, for Linux its "/mnt"
nfs_base <- "/mnt" #"//dapadfs" "/mnt" "/nfs"
#nfs_base <- "//dapadfs" #"//dapadfs" "/mnt" "/nfs"

#working dir
bdir <- paste(nfs_base,"/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM",sep="")
wd <- paste(bdir,"/modelling/Cul_de_sacs",sep="")

#output directories
data_dir <- paste(wd,"/model_data",sep="")
met_dir <- paste(wd,"/meteorology",sep="")
runs_dir <- paste(wd,"/model_runs",sep="")

#source model
src_dir <- paste(wd,"/_scripts",sep="")
source(paste(src_dir,"/EcoCrop-model_fut_yearly.R",sep=""))

#model parameters
params <- read.table(paste(data_dir,"/crop_parameters.tab",sep=""),header=T)
ncrops <- nrow(params) #number of crops

#load and resample maize mask
mmask <- raster(paste(data_dir,"/ME_mask.tif",sep=""))
mmask <- resample(mmask, raster(paste(met_dir,"/cru_hist/prec_1.tif",sep="")),method="ngb")
maiz_id <- c("wuma","wl","wlma","h","dma","dl")

####specify rcps
dset <- "wcl"; rcp <- "rcp60"
rcp_dir <- paste(met_dir,"/",dset,"_futclim_bc/",rcp, sep="")
crop_dir <- paste(runs_dir,"/",dset,"_futclim_bc/",rcp, sep="")
if (!file.exists(crop_dir)) {dir.create(crop_dir,recursive=T)}

#global climate models - this is now the link to the folder with all the model folders
gcm_list <- list.files(rcp_dir)

#############  Projection onto future  ##################
#Run principal function for each GCM
cat("...calculate suitability projected onto future\n")
  
#loop aroudn GCMs
for (gcm in gcm_list[16:19]) {
  #gcm <- gcm_list[1]
  
  #end in 2098 because for 2099 next year does not exist so cannot simulate
  for(ye in c(2006:2098)){
    #ye <- year_list[1]
    #but then later he takes in aDir2 ye+1 so it acutally also uses 2099 potentially
    run_dir <- paste(crop_dir,  "/",gcm,"/r1i1p1/",ye, sep="") #Output folder for each GCM
    tmet_dir <- paste(rcp_dir, "/", gcm,"/r1i1p1/", ye, sep="")  #Folder of future climate data
    tmet_dir2 <- paste(rcp_dir, "/", gcm,"/r1i1p1/", (ye+1), sep="")  #Folder of future climate data
    
    #calculate monthly indices corresponding to year
    ind.clim = ((ye-2020)*12+1) : ((ye-2019)*12)
    
    #########################################################
    ############ loop through the crops #####################
    for (n in 1:ncrops) {
      #n <- 1
      crop_name <- paste(params$Crop)[n] #Name of the last test
      if (!file.exists(paste(run_dir, "/", crop_name, "_suit.tif", sep=""))) {
        cat("...suitability sim for",crop_name,"/" ,rcp,"/",gcm, ye,"\n")
        
        #run model or calculate (maize, sorghum)
        if (crop_name == "sorghum") {
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
          
          rsx <- raster(paste(run_dir,"/sorghum_h_suit.tif",sep=""))
          rsn <- raster(paste(run_dir,"/sorghum_l_suit.tif",sep=""))
          rsf <- calc(stack(rsn,rsx),fun=tsfun)
          rsf <- writeRaster(rsf, paste(run_dir,"/",crop_name,"_suit.tif",sep=""), format="GTiff")
          rm(rsf)
        } else if (crop_name == "maize") {
          me_his <- ome_his <- list()
          for (mci in 1:length(maiz_id)) {
            #mci <- 1
            tmask <- raster(mmask); tmask[] <- NA; tmask[which(mmask[] == mci)] <- 1 #; plot(tmask)
            trs_his <- raster(paste(run_dir,"/maize_",maiz_id[mci],"_suit.tif",sep=""))
            me_his[[mci]] <- trs_his * tmask #pieced MEs
            ome_his[[mci]] <- trs_his #for outside areas
          }
          pcd_his <- merge(stack(me_his))
          unpcd_his <- calc(stack(ome_his), max)
          mai_his <- merge(pcd_his, unpcd_his)
          mai_his <- writeRaster(mai_his,paste(run_dir,"/",crop_name,"_suit.tif",sep=""), format="GTiff")
        } else {
          #run the function. note: we have now two climate directories and input files
          fut <- suitCalc(climPath = tmet_dir, 
                          climPath2 = tmet_dir2,
                          yearly=T,#turn this into FALSE if u dont want the script to do the yearly time series calculation
                          Gmin=params$Gmin[n],
                          Gmax=params$Gmax[n],
                          Tkmp=params$Tkmp[n],
                          Tmin=params$Tmin[n],
                          Topmin=params$Topmin[n],
                          Topmax=params$Topmax[n],
                          Tmax=params$Tmax[n],
                          Rmin=params$Rmin[n],
                          Ropmin=params$Ropmin[n],
                          Ropmax=params$Ropmax[n],
                          Rmax=params$Rmax[n],
                          outfolder = run_dir, 
                          cropname = crop_name,
                          ext=".tif")
        }
      } else {
        cat("...suitability sim for",crop_name,"/" ,rcp,"/",gcm, ye,"done\n")
      }
    }
  }
}


