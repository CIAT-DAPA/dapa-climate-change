#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#assess the accuracy of EcoCrop's spatial prediction using a gridded dataset
#of presence and absence
eval_ecocrop <- function(rsl,eval_rs) {
  pa_rsl <- rsl; pa_rsl[which(rsl[]>0)] <- 1 #bin the prediction
  
  met <- xyFromCell(eval_rs,1:ncell(eval_rs))
  met <- cbind(met,PRE=extract(pa_rsl,met))
  met <- cbind(met,OBS=extract(eval_rs,1:ncell(eval_rs))); met <- as.data.frame(met)
  met <- met[which(!is.na(met$PRE)),]; met <- met[which(!is.na(met$OBS)),] #get rid of NAs
  
  #get the values *1 is observed and 2 is prediction
  ntp <- length(which(met$PRE > 0 & met$OBS == 1))
  tpr <- ntp/length(which(met$OBS == 1))
  #false negative rate
  nfp <- length(which(met$PRE == 0 & met$OBS == 1))
  fpr <- nfp/length(which(met$OBS == 1))
  #true negative rate (if absences are available)
  if (length(which(met$OBS == 0)) != 0) {
    ntn <- length(which(met$PRE > 0 & met$OBS == 0))
    tnr <- ntn / length(which(met$OBS == 0))
  } else {tnr <- NA}
  
  rm(met); g=gc(); rm(g)
  met.final <- data.frame(TPR=tpr, FPR=fpr, TNR=tnr)
  return(met.final)
}



#function to copy the relevant cru and iitm data for an ecocrop run
#everything will be in GeoTiff
copy_clim_data <- function(clm_type,iitm_dir,cru_dir,oclm_dir,cru_prefix=NA) {
  #create output folder if it does not exist
  oclm_dir <- paste(oclm_dir,"/",clm_type,sep="")
  if (!file.exists(oclm_dir)) {dir.create(oclm_dir,recursive=T)}
  
  if (!is.na(cru_prefix)) {cru_prefix <- paste(cru_prefix,"_",sep="")} else {cru_prefix <- ""}
  
  #loop through months
  for (i in 1:12) {
    #copying rainfall
    rf <- raster(paste(iitm_dir,"/rain-",i,".tif",sep=""))
    rf <- writeRaster(rf,paste(oclm_dir,"/prec_",i,".tif",sep=""),format="GTiff",overwrite=T)
    
    #copying min. temperature
    tn <- raster(paste(cru_dir,"/tmn_1dd/tmn_",cru_prefix,i,".asc",sep=""))
    tn <- crop(tn,rf)
    tn <- resample(tn,rf,method="ngb")
    tn[which(is.na(rf[]))] <- NA
    tn <- writeRaster(tn,paste(oclm_dir,"/tmin_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tn)
    
    #copying max. temperature
    tx <- raster(paste(cru_dir,"/tmx_1dd/tmx_",cru_prefix,i,".asc",sep=""))
    tx <- crop(tx,rf)
    tx <- resample(tx,rf,method="ngb")
    tx[which(is.na(rf[]))] <- NA
    tx <- writeRaster(tx,paste(oclm_dir,"/tmax_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tx)
    
    #copying min. temperature
    tm <- raster(paste(cru_dir,"/tmp_1dd/tmp_",cru_prefix,i,".asc",sep=""))
    tm <- crop(tm,rf)
    tm <- resample(tm,rf,method="ngb")
    tm[which(is.na(rf[]))] <- NA
    tm <- writeRaster(tm,paste(oclm_dir,"/tmean_",i,".tif",sep=""),format="GTiff",overwrite=T)
    rm(tm)
  }
  return(oclm_dir)
}


#produce growing season parameters based on sowing date and duration
get_gs_data <- function(i,rs) {
  x <- rs[i,] #get that row
  x2 <- calibrationParameters(x, gs=x$DUR, verbose=F) #get growing season data for that duration
  this_gs <- x$SOW_MTH #which month the crop was planted
  fields <- c(paste("GS",this_gs,"_P",sep=""),paste("GS",this_gs,"_T",sep=""),
              paste("GS",this_gs,"_N",sep=""),paste("GS",this_gs,"_X",sep=""))
  gs_data <- x2[,fields] #get only data for the growing season when crop was planted
  names(gs_data) <- c("GS1_P","GS1_T","GS1_N","GS1_X") #assign new names to fields for final merging
  x <- cbind(x,gs_data) #merge with original input data
  return(x)
}


#function to find corresponding fraction of month for a given Julian day
find_month <- function(jd,dg) {
  mth <- dg$MTH[which(dg$JD==jd)]
  day <- dg$DAY[which(dg$JD==jd)] / length(dg$DAY[which(dg$MTH==mth)])
  mth <- mth+day
  return(mth)
}


