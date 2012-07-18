#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL


#function to regress the data (remainder of perfect fit)
#of a given gridcell, against key growing season weather variables
regress_cell <- function(cell,cell_data,crop_dir,wth_dir,exp) {
  cat("\ngricell",cell,"...\n")
  
  #extract yield and suitability values
  yield <- as.numeric(cell_data$YIELD[which(cell_data$CELL==cell)])
  suit <- as.numeric(cell_data$SUIT[which(cell_data$CELL==cell)])
  
  #preliminary plots of historical yield and suitability, commented
  # plot(1966:1993,yield/max(yield),ty="l",col="red")
  # lines(1966:1993,suit/100,ty="l",col="blue")
  # plot(yield/max(yield),suit/100,pch=20,ylim=c(0,1),xlim=c(0,1))
  # abline(0,1,col="red")
  
  #make data frame with data
  data_cell <- data.frame(YEAR=1966:1993,YIELD=yield,SUIT=suit)
  data_cell$SUIT_NORM <- data_cell$SUIT/100
  data_cell$YIELD_NORM <- data_cell$YIELD/max(data_cell$YIELD)
  
  #calculate the difference between each point and the perfect fit (0,1)
  x <- c(0,1)
  y <- c(0,1)
  p_fit <- lm(y~x)
  data_cell$PFIT <- as.numeric(predict(p_fit,newdata=data.frame(x=data_cell$YIELD_NORM)))
  data_cell$DIFF <- data_cell$SUIT_NORM-data_cell$PFIT
  
  #some additional plots, commented
  # plot(data_cell$YIELD_NORM,data_cell$SUIT_NORM,pch=20,ylim=c(0,1),xlim=c(0,1))
  # abline(0,1,col="red")
  # points(data_cell$YIELD_NORM,data_cell$PFIT,col="red",pch=20)
  # points(data_cell$YIELD_NORM,data_cell$DIFF,col="red",pch=20)
  
  #calculate growing season variables
  #loop through years
  cat("calculating yearly data...\n")
  out_df <- data.frame()
  for (yr in 1966:1993) {
    #yr <- 1966 #year i want to get
    #cat("year",yr,"\n")
    #read weather data from *.wth file
    wth_dir <- paste(crop_dir,"/inputs/ascii/wth/rfd_",cell,sep="")
    wth_fil <- paste(wth_dir,"/ingc001001",yr,".wth",sep="")
    wth <- read.fortran(wth_fil,format=c("I5","F6","3F7"),skip=4)
    names(wth) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
    wth$YEAR <- as.numeric(substr(wth$DATE,1,2))
    wth$JDAY <- as.numeric(substr(wth$DATE,3,5))
    
    #get planting, idur, harvest date date from glam run
    run_dir <- paste(crop_dir,"/calib/exp-",exp,"_outputs/gridcells/fcal_",cell,sep="")
    
    #get harvest date (from ygp=1 run, rainfed)
    glam_file <- paste(run_dir,"/ygp_1/groundnut_RFD.out",sep="")
    glam_run <- read.table(glam_file,sep="\t",header=F)
    names(glam_run) <- vnames$EOS
    
    sow <- glam_run$IPDATE[which(glam_run$YEAR == yr)]
    har <- sow + glam_run$IDUR[which(glam_run$YEAR == yr)]
    
    #Calculate the water balance
    wth_out <- wth
    wth_out$ETMAX <- NA; wth_out$AVAIL <- NA; wth_out$ERATIO <- NA
    wth_out$CUM_RAIN <- NA; wth_out$RUNOFF <- NA; wth_out$DEMAND <- NA
    wth_out <- watbal_wrapper(wth_out)
    
    #remove not-needed dates in the wth file
    wth_out <- wth_out[which(wth_out$JDAY >= sow & wth_out$JDAY <= har),]
    
    #here try to correlate that difference with some metrics of the growing season, such as:
    #number of days with rain > 0mm, 2mm, 5mm, 10mm, 15mm, 20mm
    rain <- sum(wth_out$RAIN)
    rstd <- sd(wth_out$RAIN)
    rd_0 <- length(which(wth_out$RAIN>0))
    rd_2 <- length(which(wth_out$RAIN>2))
    rd_5 <- length(which(wth_out$RAIN>5))
    rd_10 <- length(which(wth_out$RAIN>10))
    rd_15 <- length(which(wth_out$RAIN>15))
    rd_20 <- length(which(wth_out$RAIN>20))
    
    #days exceeding thresholds of HTS and TETRS
    hts_34 <- length(which(wth_out$TMAX>34))
    hts_40 <- length(which(wth_out$TMAX>40))
    
    tetr_35 <- length(which(wth_out$TMAX>35))
    tetr_47 <- length(which(wth_out$TMAX>47))
    
    #water stress days, calculated from simple WATBAL of PJones
    #number of days with Ea/Ep ratio < 0.25, 0.5, 0.75
    eratio_25 <- length(which(wth_out$ERATIO<0.25))
    eratio_50 <- length(which(wth_out$ERATIO<0.5))
    eratio_75 <- length(which(wth_out$ERATIO<0.75))
    
    #output row
    orow <- data.frame(YEAR=yr,SOW=sow,HAR=har,RAIN=rain,RSTD=rstd,RD.0=rd_0,
                       RD.2=rd_2,RD.5=rd_5,RD.10=rd_10,RD.15=rd_15,RD.20=rd_20,
                       HTS1=hts_34,HTS2=hts_40,TETR1=tetr_35,TETR2=tetr_47,
                       ERATIO.25=eratio_25,ERATIO.50=eratio_50,ERATIO.75=eratio_75)
    out_df <- rbind(out_df,orow)
  }
  
  #calculate water use efficiency
  data_cell$WUE <- data_cell$YIELD/out_df$RAIN
  dcell <- data_cell
  dcell <- merge(dcell,out_df,by=c("YEAR"),sort=F)
  
  #correlation matrix to explore potential explanatory power for that difference
  cor_mx <- as.data.frame(cor(dcell))
  
  #remove HTS thresholds if required
  if (is.na(cor_mx$HTS1[1])) {
    cor_mx$HTS1 <- NULL
    w_rem <- which(row.names(cor_mx) == "HTS1")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  if (is.na(cor_mx$HTS2[1])) {
    cor_mx$HTS2 <- NULL
    w_rem <- which(row.names(cor_mx) == "HTS2")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  
  #remove TETR thresholds if required
  if (is.na(cor_mx$TETR1[1])) {
    cor_mx$TETR1 <- NULL
    w_rem <- which(row.names(cor_mx) == "TETR1")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  if (is.na(cor_mx$TETR2[1])) {
    cor_mx$TETR2 <- NULL
    w_rem <- which(row.names(cor_mx) == "TETR2")
    cor_mx <- cor_mx[c(1:(w_rem-1),(w_rem+1):nrow(cor_mx)),]
  }
  
  #remove useless fields
  cor_mx$YEAR <- NULL; cor_mx$YIELD <- NULL; cor_mx$SUIT <- NULL
  cor_mx$SUIT_NORM <- NULL; cor_mx$YIELD_NORM <- NULL; cor_mx$PFIT <- NULL
  cor_mx$WUE <- NULL; cor_mx$SOW <- NULL; cor_mx$HAR <- NULL; cor_mx$RAIN <- NULL
  
  cat("fitting multiple regression\n")
  #data for fitting
  fit_data <- dcell[,names(cor_mx)]
  
  #fit the multiple regression
  reg_fit <- lm(DIFF ~ .,data=fit_data)
  
  #stepwise the regression based on AIC
  reg_stp <- stepAIC(reg_fit,direction="both",trace=F)
  anv_stp <- reg_stp$anova
  sum_stp <- summary(reg_stp)
  
  #calculate the response and do some plots (commented)
  diffp <- predict(reg_stp,dcell)
  #plot(diffp,dcell$DIFF)
  ccoef <- cor(diffp,dcell$DIFF)
  #length(which(dcell$DIFF<0)) #number of years with diff < 0
  
  #put this all into a matrix with the coefficients
  reg_df <- data.frame(CELL=cell,INT=0,RSTD=0,RD.0=0,RD.2=0,RD.5=0,RD.10=0,
                       RD.15=0,RD.20=0,HTS1=0,HTS2=0,TETR1=0,TETR2=0,ERATIO.25=0,
                       ERATIO.50=0,ERATIO.75=0,CCOEF=ccoef)
  
  coef_mx <- as.data.frame(sum_stp$coefficients)
  row.names(coef_mx)[1] <- "INT"
  rnames <- row.names(coef_mx)
  for (rn in rnames) {
    #rn <- rnames[1]
    reg_df[which(reg_df$CELL == cell),rn] <- coef_mx$Estimate[which(rnames==rn)]
  }
  #out_all <- rbind(out_all,reg_df)
  return(reg_df)
}



###get data for a given cell
get_cell_data <- function(cell,xy,glam_data,eco_data) {
  #coordinates of gridcell
  x_coord <- xy$x[which(xy$CELL == cell)]
  y_coord <- xy$y[which(xy$CELL == cell)]
  posit <- which(xy$CELL == cell)
  
  #yield and suitability data
  yield <- as.numeric(glam_data[posit,])
  suit <- as.numeric(eco_data[posit,])
  dfp <- data.frame(CELL=cell,YEAR=1966:1993,YIELD=yield,SUIT=suit)
  
  #dfo <- rbind(dfo,dfp)
  #plot(1966:1993,(yield-mean(yield))/sd(yield),ty="l",col="red")
  #lines(1966:1993,(suit-mean(suit))/sd(suit),ty="l",col="blue")
  #plot(yield,suit,pch=20,ylim=c(0,100),xlim=c(0,5000))
  return(dfp)
}


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


