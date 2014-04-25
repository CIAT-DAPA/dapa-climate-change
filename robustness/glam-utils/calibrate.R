#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014 #borrows from PhD script called "glam-optimise-functions.R"

##############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided), each one individually
##############################################################################################

#this (first) function should
#1. use number of steps to choose the values to iterate
#2. calibrate (i.e. find optimum YGP) for each grid cell individually
#3. return table of grid cell * ygp values, RMSE value, and crop yield

#note: a second function will deal with optimisation of parameters
#note: this function should be applicable to both the hypercube and the normal 
#      optimisation procedure

# #example:
# #---------------------------------------------------------------
# src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
# source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
# source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
# source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
# 
# wd <- "~/Leeds-work/quest-for-robustness"
# runsDir <- paste(wd,"/crop_model_runs",sep="")
# calibDir <- paste(runsDir,"/ppe_optimisation",sep="")
# mdataDir <- paste(wd,"/data/model_data",sep="")
# metDir <- paste(wd,"/data/meteorology",sep="")
# binDir <- paste(wd,"/bin/glam-maize-osx",sep="")
# 
# #load objects
# load(paste(mdataDir,"/initial_conditions_major.RData",sep=""))
# load(paste(mdataDir,"/yield_major.RData",sep=""))
# 
# #arguments
# opt_data <- list()
# opt_data$CROP <- "maize"
# opt_data$MODEL <- "glam-maiz"
# opt_data$BASE_DIR <- calibDir
# opt_data$BIN_DIR <- binDir
# opt_data$PAR_DIR <- mdataDir
# opt_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw",sep="") #for reading .wth files
# opt_data$WTH_ROOT <- "obs_hist_WFD"
# opt_data$LOC <- c(680,681,682)
# opt_data$ISYR <- 1981
# opt_data$IEYR <- 2000
# opt_data$INI_COND <- xy_main
# opt_data$YLD_DATA <- xy_main_yield
# opt_data$PARAMS <- GLAM_get_default(opt_data$PAR_DIR)
# opt_data$SIM_NAME <- "optim1"
# #opt_data$PARAM <- "YGP"
# #opt_data$SECT <- "glam_param.ygp"
# opt_data$NSTEPS <- 100
# opt_data$RUN_TYPE <- "RFD"
# opt_data$METHOD <- "RMSE"
# opt_data$USE_SCRATCH <- F
# opt_data$SCRATCH <- NA
# 
# #modify parameter value to avoid model failure
# opt_data$PARAMS$glam_param.maize$TLIMJUV$Value <- 280
# 
# ygpcalib <- GLAM_calibrate(opt_data)
# #---------------------------------------------------------------

# #plotting some of the results
# xx <- ygpcalib$RAW_DATA[which(ygpcalib$RAW_DATA$VALUE==0.87 & ygpcalib$RAW_DATA$LOC==680),]
# plot(xx$YEAR,xx$OBS_ADJ,ty="l",ylim=c(0,1200))
# lines(xx$YEAR,xx$PRED_ADJ,col="red")
# 
# yy <- ygpcalib$CALIBRATION[which(ygpcalib$CALIBRATION$LOC==680),]
# plot(yy$VALUE, yy$RMSE/yy$YOBS_ADJ*100, ty='l',ylim=c(0,100))

### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing

#calibrate ygp
GLAM_calibrate <- function(opt_data) {
  param <- "YGP" #toupper(opt_data$PARAM)
  sect <- "glam_param.ygp" #tolower(opt_data$SECT)
  params <- opt_data$PARAMS
  
  #put years into parameter set
  params$glam_param.mod_mgt$ISYR <- opt_data$ISYR
  params$glam_param.mod_mgt$IEYR <- opt_data$IEYR
  
  #here is the optimisation method
  #RMSE: is yearly root mean square error (classical)
  #CH07: is the MSE method proposed in Challinor et al. (2007) AGEE, that optimises based on
  #      the differences between mean and standard deviations of the simulated time series
  #CH10: is the MSE method proposed in Challinor et al. (2010) ERL, that optimises based
  #      on the difference between mean yields only. I guess this method is only valid when
  #      an insufficiently large observed yield + weather time series is available.
  if (is.null(opt_data$METHOD)) {
    opt_meth <- "RMSE" #defaulting to RMSE if missing in input list
  } else {
    opt_meth <- toupper(opt_data$METHOD)
  }
  
  if (!opt_meth %in% c("RMSE","CH07","CH10")) {
    opt_meth <- "RMSE" #defaulting the RMSE
  }
  
  #input directories and model
  exec_name <- opt_data$MODEL
  
  #running command
  glam_cmd <- paste("./",exec_name,sep="")
  
  #output directories
  if (opt_data$USE_SCRATCH) {
    cal_dir <- opt_data$SCRATCH #calibration directory
    nfs_dir <- paste(opt_data$BASE_DIR,"/",opt_data$SIM_NAME,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- opt_data$BASE_DIR #calibration directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",opt_data$SIM_NAME,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #create optimisation folder if it does not exist
  opt_dir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #create sequence of values
  vals <- seq(params[[sect]][[param]][,"Min"],params[[sect]][[param]][,"Max"],length.out=opt_data$NSTEPS)
  
  #type of run
  #params$glam_param.mod_mgt$SEASON <- opt_data$RUN_TYPE
  
  #params config
  #params$glam_param.mod_mgt$IASCII <- 1 #output only to season file
  
  #loop through desired locations
  for (loc in opt_data$LOC) {
    #loc <- opt_data$LOC[1]
    cat("\n...loc= ",loc,sep="","\n")
    
    #sowing window
    sow_date1 <- opt_data$INI_COND$SOW_DATE1[which(opt_data$INI_COND$LOC == loc)]
    sow_date2 <- opt_data$INI_COND$SOW_DATE2[which(opt_data$INI_COND$LOC == loc)]
    sow_window <- sow_date1 - sow_date2
    params$glam_param.mod_mgt$ISDAY$Value <- min(c(sow_window,-30))
    
    #prepare input object
    run_data <- list()
    run_data$CROP <- opt_data$CROP
    run_data$MODEL <- opt_data$MODEL
    run_data$BASE_DIR <- opt_dir
    run_data$BIN_DIR <- opt_data$BIN_DIR
    run_data$PAR_DIR <- NA
    run_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw/",opt_data$WTH_ROOT,sep="") #to be specified
    run_data$LOC <- loc
    run_data$LON <- opt_data$INI_COND$x[which(opt_data$INI_COND$LOC == loc)]
    run_data$LAT <- opt_data$INI_COND$y[which(opt_data$INI_COND$LOC == loc)]
    run_data$ME <- opt_data$INI_COND$ME[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$SOW_DATE <- opt_data$INI_COND$SOW_DATE1[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$RLL <- opt_data$INI_COND$RLL[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$DUL <- opt_data$INI_COND$DUL[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$SAT <- opt_data$INI_COND$SAT[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$ISYR <- opt_data$ISYR
    run_data$IEYR <- opt_data$IEYR
    run_data$PARAMS <- params
    
    #file of output
    cal_outfile <- paste(opt_dir,"/cal-",run_data$LOC,".txt",sep="") #summary
    raw_outfile <- paste(opt_dir,"/cal-",run_data$LOC,"_raw.txt",sep="") #raw
    
    if (!file.exists(cal_outfile)) {
      #loop through sequence of values
      for (i in 1:length(vals)) {
        #i <- 1
        cat("performing ygp calibration run ",opt_data$RUN_TYPE," ",i," value = ",vals[i],sep="","\n")
        
        #run id
        run_data$RUN_ID <- paste("run-",i,"_val-",vals[i],"_loc-",run_data$LOC,sep="")
        
        #assign values to parameter set
        run_data$PARAMS[[sect]][[param]][,"Value"] <- vals[i]
        
        #check whether the *.out already exists
        outfile <- list.files(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/output",sep=""),pattern="\\.out")
        if (length(outfile) == 0) {
          #run the model
          run_data <- run_glam(run_data)
        } else {
          run_data$SEAS_FILES <- outfile
          run_data$RUN_DIR <- paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")
        }
        
        #read in the predicted yield
        if (length(run_data$SEAS_FILES) == 1 | length(outfile) == 1) {
          pred <- read.table(paste(run_data$RUN_DIR,"/output/",run_data$SEAS_FILES,sep=""),header=F,sep="\t")
          names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT",
                           "IPLANT","LETHAL_YIELD","LETHAL_HI","LETHAL_BMASS","LETHAL_BMASS","LETHAL_DAP",
                           "SWFAC_TOT","SWFAC_MEAN","SWFAC_COUNT")
          y_p <- pred$YIELD #predicted yield
          
          #grab observed yield
          y_o <- as.data.frame(t(opt_data$YLD_DATA[which(opt_data$YLD_DATA$x == run_data$LON & opt_data$YLD_DATA$y == run_data$LAT),3:ncol(opt_data$YLD_DATA)]))
          y_o <- cbind(YEAR=1982:2005,y_o)
          names(y_o)[2] <- "YIELD"
          row.names(y_o) <- 1:nrow(y_o)
          y_o <- y_o[which(y_o$YEAR >= opt_data$ISYR & y_o$YEAR <= opt_data$IEYR),]
          y_o <- y_o$YIELD
          
          #get simulated yield, depending on which year the crop was actually harvested:
          #** if planted and harvested this year then use simulation from this year to end
          #** if planted this and harvested next year then use simulation from year-1 to end-1
          har_date <- mean((pred$PLANTING_DATE + pred$DUR)) #get harvest date first
          if (har_date<365) {y_p <- y_p[2:length(y_p)]} else {y_p <- y_p[1:(length(y_p)-1)]}
          odf <- data.frame(YEAR=(opt_data$ISYR+1):opt_data$IEYR,VALUE=vals[i],OBS=y_o,PRED=y_p)
          
          ## detrending (borrows from detrender-functions.R)
          #detrend observed yield
          fit_loess <- loess(odf$OBS ~ odf$YEAR) #compute lowess fit
          y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
          odf$LOESS_PRED <- y_loess$fit
          rd_loess <- (odf$OBS - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
          odf$OBS_ADJ <- (rd_loess+1) * odf$OBS[nrow(odf)] #loess
          
          #detrend simulated yield
          fit_loess <- loess(odf$PRED ~ odf$YEAR, degree=1, span=2) #compute lowess fit
          y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
          odf$LOESS_PRED <- y_loess$fit
          rd_loess <- (odf$PRED - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
          odf$PRED_ADJ <- (rd_loess+1) * odf$PRED[nrow(odf)] #loess
          odf$LOESS_PRED <- NULL
          
          #choose optimisation method (RMSE, CH07, CH10)
          if (opt_meth == "RMSE") {
            rmse <- sqrt(sum((odf$OBS_ADJ-odf$PRED_ADJ)^2,na.rm=T) / (length(which(!is.na(odf$OBS_ADJ)))))
          } else if (opt_meth == "CH07") {
            rmse <- (mean(odf$OBS_ADJ,na.rm=T)-mean(odf$PRED_ADJ,na.rm=T))^2 + (sd(odf$OBS_ADJ,na.rm=T)-sd(odf$PRED_ADJ,na.rm=T))^2
          } else if (opt_meth == "CH10") {
            rmse <- (mean(odf$OBS_ADJ,na.rm=T)-mean(odf$PRED_ADJ,na.rm=T))^2
          }
          
          #remove junk
          system(paste("rm -rf ",run_data$RUN_DIR,sep=""))
        } else {
          rmse <- NA
        }
        out_row <- data.frame(VALUE=vals[i], RMSE=rmse, YOBS=mean(odf$OBS,na.rm=T), YPRED=mean(odf$PRED,na.rm=T), 
                              YOBS_ADJ=mean(odf$OBS_ADJ,na.rm=T), YPRED_ADJ=mean(odf$PRED_ADJ,na.rm=T))
        
        if (i == 1) {
          out_all <- out_row
          raw_all <- odf
        } else {
          out_all <- rbind(out_all,out_row)
          raw_all <- rbind(raw_all, odf)
        }
      }
      #write outputs for this grid cell
      write.table(out_all,sep="\t",quote=F,file=cal_outfile,row.names=F)
      write.table(raw_all,sep="\t",quote=F,file=raw_outfile,row.names=F)
    } else {
      out_all <- read.table(cal_outfile,sep="\t",header=T)
      raw_all <- read.table(raw_outfile,sep="\t",header=T)
    }
    
    #append location data
    out_all <- cbind(LOC=loc,out_all)
    raw_all <- cbind(LOC=loc,raw_all)
    if (loc == opt_data$LOC[1]) {
      cal_all <- out_all
      raw_cal <- raw_all
    } else {
      cal_all <- rbind(cal_all, out_all)
      raw_cal <- rbind(raw_cal, raw_all)
    }
  }
  
  #remove junk from scratch
  if (opt_data$USE_SCRATCH) {
    system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  
  #return object
  r_list <- list(CALIBRATION=cal_all, RAW_DATA=raw_cal)
  return(r_list)
}
