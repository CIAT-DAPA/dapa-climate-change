#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Jun 2013


#wrapper function to downscale GCMs
gcm_downscale <- function(gcm_i) {
  gcm <- gcm_list$GCM[gcm_i]
  ens <- gcm_list$ENS[gcm_i]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("XXXXXXXXXXXXX downscaling GCM:",gcm,"and ENS:",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #i/o gcm dirs
  hisgcmDir <- paste(hisDir,"/",gcm,"/",ens,"_monthly",sep="")
  rcpgcmDir <- paste(rcpDir,"/",gcm,"/",ens,"_monthly",sep="")
  
  ogcmLOCI_b <- paste(outLOCI_b,"/",gcm,"_ENS_",ens,sep="")
  ogcmraw_b <- paste(outraw_b,"/",gcm,"_ENS_",ens,sep="")
  ogcmLOCI_p <- paste(outLOCI_p,"/",gcm,"_ENS_",ens,sep="")
  ogcmdel_p <- paste(outdel_p,"/",gcm,"_ENS_",ens,sep="")
  ogcmraw_p <- paste(outraw_p,"/",gcm,"_ENS_",ens,sep="")
  if (!file.exists(ogcmLOCI_b)) {dir.create(ogcmLOCI_b)}
  if (!file.exists(ogcmraw_b)) {dir.create(ogcmraw_b)}
  if (!file.exists(ogcmLOCI_p)) {dir.create(ogcmLOCI_p)}
  if (!file.exists(ogcmdel_p)) {dir.create(ogcmdel_p)}
  if (!file.exists(ogcmraw_p)) {dir.create(ogcmraw_p)}
  
  #list of output directories
  dirtoproc <- c("ogcmLOCI_b","ogcmraw_b","ogcmLOCI_p","ogcmraw_p","ogcmdel_p")
  
  #2. for each variable: load the 1966_1993 data (obs, hist, rcp)
  for (vn in c("prec","tmin","tmax","tmean")) {
    #vn <- "prec"
    cat("processing monthly data for:",vn,"\n")
    
    #checking if all files are done
    flist <- c()
    for (dtp in dirtoproc) {flist <- c(flist,list.files(get(dtp),pattern=paste(vn,"_",sep="")))}
    
    if (length(flist) < 12*length(dirtoproc)) {
      #load mask
      msk <- raster(paste(envDir,"/mask/mask_1dd.tif",sep=""))
      
      #observed
      obs_stk <- stack(paste(mthDir,"/",vn,"_",1:12,".tif",sep=""))
      
      #historical GCM
      if (vn == "prec") {
        his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/pr_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmax") {
        his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tasmax_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmin") {
        his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tasmin_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmean") {
        his_stk <- lapply(1:12,FUN=function(x) {stack(paste(hisgcmDir,"/",1966:1993,"/tas_",sprintf("%02d",x),".tif",sep=""))})
      }
      
      his_stk <- lapply(his_stk,FUN=function(x) {calc(x,fun=function(y) {mean(y,na.rm=T)})})
      his_stk <- lapply(his_stk,FUN=function(x) {rotate(x)})
      his_stk <- lapply(his_stk,FUN=function(x,y) {resample(x,y,method="ngb")},msk)
      his_stk <- stack(his_stk)
      if (vn != "prec") {his_stk <- his_stk * 10} #to make temperatures comparable
      
      #rcp45 GCM
      if (vn == "prec") {
        rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/pr_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmax") {
        rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tasmax_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmin") {
        rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tasmin_",sprintf("%02d",x),".tif",sep=""))})
      } else if (vn == "tmean") {
        rcp_stk <- lapply(1:12,FUN=function(x) {stack(paste(rcpgcmDir,"/",2022:2049,"/tas_",sprintf("%02d",x),".tif",sep=""))})
      }
      
      #load and crop
      rcp_stk <- lapply(rcp_stk,FUN=function(x) {calc(x,fun=function(y) {mean(y,na.rm=T)})})
      rcp_stk <- lapply(rcp_stk,FUN=function(x) {rotate(x)})
      rcp_stk <- lapply(rcp_stk,FUN=function(x,y) {resample(x,y,method="ngb")},msk)
      rcp_stk <- stack(rcp_stk)
      if (vn != "prec") {rcp_stk <- rcp_stk * 10} #to make temperatures comparable
      
      #3. write baseline for: 
      #   loci (bc but only for rain, i.e., replace obs temperature)
      #   no need to write other baselines because that == obs
      if (vn == "prec") {wrtstk <- obs_stk} else {wrtstk <- his_stk}
      for (m in 1:12) {
        if (!file.exists(paste(ogcmLOCI_b,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(wrtstk[[m]],paste(ogcmLOCI_b,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
        if (!file.exists(paste(ogcmraw_b,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(his_stk[[m]],paste(ogcmraw_b,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
      }
      rm(wrtstk)
      
      #4. calculate future climate for each method: del, bc, loci
      #calculate bias factors
      del_stk <- list()
      for (m in 1:12) {del_stk[[m]] <- biasfun(obs_stk[[m]],his_stk[[m]],rcp_stk[[m]],vn)}
      
      #5. write everything put the 'raw' in there as well (but running this may not be necessary)
      #if variable is precip then i write delta stuff in LOCI, otherwise i write raw (rcp_stk)
      if (vn == "prec") {wrtstk <- del_stk} else {wrtstk <- rcp_stk}
      #loop months
      for (m in 1:12) {
        if (!file.exists(paste(ogcmLOCI_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(wrtstk[[m]],paste(ogcmLOCI_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
        if (!file.exists(paste(ogcmdel_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(del_stk[[m]],paste(ogcmdel_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
        if (!file.exists(paste(ogcmraw_p,"/",vn,"_",m,".tif",sep=""))) {aa <- writeRaster(rcp_stk[[m]],paste(ogcmraw_p,"/",vn,"_",m,".tif",sep=""),format="GTiff"); rm(aa)}
      }
    }
  }
  
  #6. calculate the 'bio' variables for each one: 
  #   baseline_loci, baseline_raw, rcp_loci, rcp_raw, rcp_del
  #load sowing and harvest dates
  sowd <- raster(paste(nbDir,"/calendar/plant_doy_ind_jt_1dd.tif",sep=""))
  hard <- raster(paste(nbDir,"/calendar/harvest_doy_ind_jt_1dd.tif",sep=""))
  
  for (dtp in dirtoproc) {
    #dtp <- dirtoproc[1]
    this_dir <- get(dtp)
    tbioDir <- paste(this_dir,"/biovars",sep="")
    if (!file.exists(tbioDir)) {dir.create(tbioDir)}
    
    #load monthly stacks
    rain_stk <- stack(paste(this_dir,"/prec_",1:12,".tif",sep=""))
    tmen_stk <- stack(paste(this_dir,"/tmean_",1:12,".tif",sep=""))
    tmax_stk <- stack(paste(this_dir,"/tmax_",1:12,".tif",sep=""))
    tmin_stk <- stack(paste(this_dir,"/tmin_",1:12,".tif",sep=""))
    tnx_stk <- stack(c(paste(this_dir,"/tmin_",1:12,".tif",sep=""),
                       paste(this_dir,"/tmax_",1:12,".tif",sep="")))
    
    #### annual total rainfall
    cat("\ntotal annual rainfall\n")
    if (!file.exists(paste(tbioDir,"/annrain.tif",sep=""))) {
      totrain <- calc(rain_stk,fun=sum)
      totrain <- writeRaster(totrain,paste(tbioDir,"/annrain.tif",sep=""),format="GTiff")
    }
    
    #### 1. Total seasonal rainfall
    cat("\ntotal seasonal rainfall")
    if (!file.exists(paste(tbioDir,"/seasrain.tif",sep=""))) {
      seasrain <- apply_by_blocks(rain_stk,sowd,hard,calc_totrain)
      seasrain <- writeRaster(seasrain,paste(tbioDir,"/seasrain.tif",sep=""),format="GTiff")
    } else {seasrain <- raster(paste(tbioDir,"/seasrain.tif",sep=""))}
    
    #get maximum rainfall for normalising
    rx <- seasrain@data@max
    
    #### 2. Feng et al. (2013)'s seasonality index
    cat("\nFeng seasonality index")
    if (!file.exists(paste(tbioDir,"/sindex.tif",sep=""))) {
      sindex <- apply_by_blocks(rain_stk,sowd,hard,calc_sfeng,rx)
      sindex <- writeRaster(sindex,paste(tbioDir,"/sindex.tif",sep=""),format="GTiff")
      rm(sindex); g=gc(); rm(g)
    }
    
    #### 3. minimum rainfall during growing season
    cat("\nminimum gs rainfall")
    if (!file.exists(paste(tbioDir,"/minrain.tif",sep=""))) {
      sminrain <- apply_by_blocks(rain_stk,sowd,hard,calc_minrain)
      sminrain <- writeRaster(sminrain,paste(tbioDir,"/minrain.tif",sep=""),format="GTiff")
      rm(sminrain); g=gc(); rm(g)
    }
    
    #### 4. mean/min/max temperature growing season
    cat("\ntemperatures")
    if (!file.exists(paste(tbioDir,"/minmintemp.tif",sep=""))) {
      #average of monthly mean temperatures
      smeantemp <- apply_by_blocks(tmen_stk,sowd,hard,calc_meantemp,"mean")
      smeantemp <- writeRaster(smeantemp,paste(tbioDir,"/meanmeantemp.tif",sep=""),format="GTiff")
      rm(smeantemp); g=gc(); rm(g)
      #maximum of monthly maximum temperatures
      smaxtemp <- apply_by_blocks(tmax_stk,sowd,hard,calc_meantemp,"max")
      smaxtemp <- writeRaster(smaxtemp,paste(tbioDir,"/maxmaxtemp.tif",sep=""),format="GTiff")
      rm(smaxtemp); g=gc(); rm(g)
      #minimum of monthly minimum temperatures
      smintemp <- apply_by_blocks(tmin_stk,sowd,hard,calc_meantemp,"min")
      smintemp <- writeRaster(smintemp,paste(tbioDir,"/minmintemp.tif",sep=""),format="GTiff")
      rm(smintemp); g=gc(); rm(g)
    }
    
    #### 5. number of days with Tmax > 34C
    cat("\ndays above tcrit")
    if (!file.exists(paste(tbioDir,"/daystcrit.tif",sep=""))) {
      daystcrit <- apply_by_blocks(tmax_stk,sowd,hard,calc_tcdays)
      daystcrit <- writeRaster(daystcrit,paste(tbioDir,"/daystcrit.tif",sep=""),format="GTiff")
      rm(daystcrit); g=gc(); rm(g)
    }
    
    #### 6. calculate growing season GDD
    cat("\ngrowing season days")
    if (!file.exists(paste(tbioDir,"/totgdd.tif",sep=""))) {
      totgdd <- apply_by_blocks(tmen_stk,sowd,hard,calc_gdd)
      totgdd <- writeRaster(totgdd,paste(tbioDir,"/totgdd.tif",sep=""),format="GTiff")
      rm(totgdd); g=gc(); rm(g)
    }
    
    #### 7. calculate growing season total VPD
    cat("\ntotal seasonal VPD")
    if (!file.exists(paste(tbioDir,"/totvpd.tif",sep=""))) {
      totvpd <- apply_by_blocks(tnx_stk,sowd,hard,calc_vdp)
      totvpd <- writeRaster(totvpd,paste(tbioDir,"/totvpd.tif",sep=""),format="GTiff")
      rm(totvpd); g=gc(); rm(g)
    }
    
    #### 8. calculate total potential evapotranspiration
    cat("\ntotal potential ET")
    if (!file.exists(paste(tbioDir,"/setmax.tif",sep=""))) {
      totetmax <- apply_by_blocks(tnx_stk,sowd,hard,calc_etmax)
      totetmax <- writeRaster(totetmax,paste(tbioDir,"/setmax.tif",sep=""),format="GTiff")
      rm(totetmax); g=gc(); rm(g)
    }
    
    #### 9. calculate drought index
    cat("\ndrought index\n")
    if (!file.exists(paste(tbioDir,"/dindex.tif",sep=""))) {
      totetmax <- raster(paste(tbioDir,"/setmax.tif",sep=""))
      seasrain <- raster(paste(tbioDir,"/seasrain.tif",sep=""))
      dindex <- (totetmax-seasrain) / totetmax * 100
      dindex <- writeRaster(dindex,paste(tbioDir,"/dindex.tif",sep=""),format="GTiff")
    }
  }
  return("done!")
}



#bias correct data for ecocrop and sdm runs
#x=obs, y=his, z=rcp, varname
biasfun <- function(x,y,z,varname) {
  xy <- as.data.frame(xyFromCell(x,1:ncell(x)))
  xy <- cbind(cell=1:ncell(x),xy)
  xy$obs <- extract(x,xy[,c("x","y")])
  xy$his <- extract(y,xy[,c("x","y")])
  xy$rcp <- extract(z,xy[,c("x","y")])
  if (varname == "prec") {
    #calculate factors
    xy$his <- sapply(xy$his,FUN=function(x) {if (x == 0) {x <- 1}; return(x)})
    xy$del <- (xy$rcp-xy$his)/(xy$his)
    #xy$bc <- (xy$obs-xy$his)/(xy$his)
    
    #calculate climate
    xy$del_rcp <- xy$obs * (1 + xy$del)
    xy$del_rcp <- sapply(xy$del_rcp,FUN=function(x) max(c(0,x)))
    #xy$bc_rcp <- xy$rcp * (1 + xy$bc)
    #xy$bc_rcp <- sapply(xy$bc_rcp,FUN=function(x) max(c(0,x)))
  } else {
    #calculate factors
    xy$del <- xy$rcp-xy$his
    #xy$bc <- xy$obs-xy$his
    
    #calculate climate
    xy$del_rcp <- xy$obs + xy$del
    #xy$bc_rcp <- xy$rcp + xy$bc
  }
  #bcr <- raster(x); bcr[xy$cell] <- xy$bc_rcp
  delr <- raster(x); delr[xy$cell] <- xy$del_rcp
  #obj <- list(delr,bcr)
  return(delr)
}

